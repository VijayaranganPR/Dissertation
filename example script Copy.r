################################################################################
#Simple example of empirical analyses

#This is just some basic analyses and you are expected to expand on it

# In the empirical part you will be testing the hypotheses that you have already developed in the litreature review.
# The analyses should be guided by the hypotheses and the data that you have.

# you should not copy the code as it is (but you can use it as a guide).
#//1- You need to have a story/ reasoning for each table (e.g. why (how) misreporting affects pricing? why (how) misreporting affects maturity? etc.) This should have been already part of your litrature review and hypotheses!!!!!
#//2- what control variables to include?
####################

#/* Assume that you are interested in examining the effect of misreporting on loan price So:

#- Dependent variable: Loan pricing (allindrawn and is measured in basis points - 100 basis points is equal to 1%)
#- Independent variable: Misreporting  (total_restatements)

#Also, Assume that our Control Variables:
# - firm size (measured by log total assets)
# - firm ROA (measured by Ebitda /sales)
# - firm leverage (measured by (total debt)/total assets)
# - market to book ratio of assets (measured by (market value of assets)/total assets)



##########################################################################

# Install/load key packages

library(tidyverse)  #for reading data, wrangling, visualization etc.
library(DescTools)  # to winsorize outliers
library(modelsummary) #for summarizing data/descriptive stataistics
library(corx)         # for the correlation matrix
library(fixest)       # for the regression analysis
###

#

# Read the data (Make sure that you see the variables definitions table)

loan_data <- read_csv("final_data_702.csv")

view(loan_data) # view the data

# at this stage you may select the variables of interest [this list may change as you progress in your analysis and dependign on yout topic]
my_loan_data <- loan_data %>%
  select(gvkey, facilityid, contract_year, total_restatements, allindrawn,total_assets , profitability, leverage,mtb)

##############################################
#-1- check your sample e.g. number of firms/loans and sample period and see if you have a strong reason to look at a subset of the time range
##############################################

n_distinct(loan_data$gvkey) #* how many firms?

n_distinct(loan_data$facilityid) #* how many loans?

table(loan_data$contract_year) # sample period

unique(loan_data$contract_year) # unique years

# suppose you are interested in the recent period (2010-2020)
loan_data_recent <-  loan_data %>%
  filter(between(contract_year, 2010, 2020))

n_distinct(loan_data_recent$gvkey)
n_distinct(loan_data_recent$facilityid)
table(loan_data_recent$contract_year) # sample period
unique(loan_data_recent$contract_year) # unique years


##############################################
#*-2- you may want to drop borrowers who belongs to the finance industry because they are regulated differently (note that financial industry code=6)
##############################################


# if you need granular industry  data, you should merge with Industry.csv available on Moodle 
#(where sic2 is the first two digits of the sic code and category is the industry category)

# Load the detailed Industry data
Industry <- read_csv("Industry.csv") %>% 
  select(gvkey, sic2, category) 

# Merge the loan data with the Industry data
loan_data=loan_data %>% 
  inner_join(Industry, by = "gvkey") 




table(loan_data$industry)  # which industry codes we have in the data
unique(loan_data$industry) # unique years

unique(loan_data$category) # what industries we have in the data


## you may want to drop borrowers from the finance industry 

loan_data_non <- loan_data %>%
  # Filter out records where industry is 6 or sic2 is between 59 and 70
  filter(!(industry == 6 | (sic2 >= 59 & sic2 <= 70)))


# Check the distribution of the 'industry' field to see if the finance industry ( coded as 6) has been appropriately filtered based on the 'sic2' condition
table(loan_data_non$industry)

# View unique categories to understand what types of industries are represented in the filtered data
unique(loan_data_non$category)

# Check specific 'sic2' codes remaining in the data to confirm the range 60 to 69 has been handled as expected
unique(loan_data_non$sic2)


# how many firms and loans after dropping finance industry
n_distinct(loan_data_non$gvkey)
n_distinct(loan_data_non$facilityid)
table(loan_data_non$contract_year) # sample period
unique(loan_data_non$contract_year)

##############
#*3- look at/create your main (and control) variables and check skewness outliers [you may then need to rename some variables at this stage]
####################

# view the first few observations from total_restatements and allindrawn
loan_data_non %>%
  select(total_restatements, allindrawn) %>%
  head()


# check the values of total_restatements
table(loan_data_non$total_restatements) #apparently total_restatements ranges from zero to 4, so it is a categorical/ordinal variable.

# check the values of allindrawn
table(loan_data_non$allindrawn) #apparently allindrawn is continuous variable


# you will notice that most (if not all) of variables are skewed. Below is just one example: Size

loan_data_non %>%
  ggplot(aes(total_assets))+
  geom_histogram()


loan_data_non %>%
  ggplot(aes(profitability))+
  geom_histogram()

#* Note - it is generally  acceptable in this literature  to winsorize ratio variables (e.g. ROA, leverage, mtb) at 1% and 99% and
#* #log transform non-ratio variables (e.g. size, spread) to reduce skewness.


# loan_data_non <- loan_data_non %>%
#   mutate(across(c(profitability, leverage,mtb),
#                 ~Winsorize(.x,probs = c(0.01, 0.99), na.rm=T),
#                 .names = "{col}_w")) %>%
#   mutate(across(c(allindrawn, total_assets), ~log(1+.x),  # we have added 1 to avoid log(0)
#                 .names = "log_{col}"))



loan_data_non <- loan_data_non %>%
  mutate(across(c(profitability, leverage, mtb), ~{
    lower_bound <- quantile(.x, 0.01, na.rm = TRUE)
    upper_bound <- quantile(.x, 0.99, na.rm = TRUE)
    pmin(pmax(.x, lower_bound), upper_bound)
  }, .names = "{col}_w")) %>%
  mutate(across(c(allindrawn, total_assets), ~log(1 + .x), .names = "log_{col}"))





# now look at the distribution of the variables after winsorizing and log transforming
loan_data_non %>%
  ggplot(aes(log_total_assets))+
  geom_histogram()


loan_data_non %>%
  ggplot(aes(profitability_w))+
  geom_histogram()



# rename some variables to make the name self explanatory

loan_data_non <- loan_data_non %>%
  rename(size=total_assets) %>%
  rename(spread=allindrawn)%>%
  rename(log_size=log_total_assets) %>%
  rename(log_spread=log_allindrawn)


#################################################################
# 6-now lets describe the data
#################################################################

# Note that in the descriptive tables, it may better to show unlogged values to make sense of the values


# Table 1: Summary Statistics

#start with the main variables of interest

data_analysis <-loan_data_non %>%
  select(total_restatements, spread, log_spread, size, log_size, profitability_w, leverage_w, mtb_w)

datasummary(total_restatements+ spread+ log_spread+ size+ log_size+ profitability_w+ leverage_w+ mtb_w ~ N+ Mean + Median + SD +Min + P25 + P75 + Max,
            title = "Table 1: Summary Statistics",
            data = data_analysis,
            fmt = 4,   #decimal places
            output ="Table1.docx")

#################

# balance table [Should be done based on an independent indicator  variable]

data_analysis <-data_analysis %>%
  mutate(misreport=ifelse(total_restatements>0, 1, 0))

datasummary_balance(~misreport,
                    data=data_analysis,
                    fmt = 4,
                    dinm_statistic = "p.value",
                    output = "Table2.docx")


###############################

# correlation matrix


data_analysis <- as.data.frame(data_analysis) #convert to data frame

corr_matrix <- corx(data_analysis,
                    stars = c(0.1, 0.05, 0.01),
                    triangle = "lower",
                    caption = "Table 3 Correlation matrix")

to_clipboard(corr_matrix) #copy to clipboard and then to excel

################################
###############################

# regression results

#######################
# note that with regression it may be better to use the full dataset not the descriptive dataset,
#in case you have an additional analysis that involves other variables




est1 <- loan_data_non %>%
  feols(log_spread ~ total_restatements)
est2 <- loan_data_non %>%
  feols(log_spread ~ total_restatements + profitability_w + leverage_w + mtb_w + log_size)

modelsummary(list(est1, est2),stars = c('*' = .1, '**' = .05, '***' = .01),  statistic ="p.value")


modelsummary(list(est1, est2),stars = c('*' = .1, '**' = .05, '***' = .01),  statistic ="p.value", output = "Table4.docx")


### how do you interpret the coefficient on misreporting? see [Miller van der Meulen Rodgers, 2008] lecture 4 materials



### Since our data is panel data, there are three important specifications to consider
#1- we should control for time-trend (time fixed effects)
#2- we should control for industry heterogeneity (industry fixed effects)
#3- we should cluster the standard errors  */


# it is common in the literature to use the first digit of industry codes (sic) to proxy for industry type


est1 <- loan_data_non %>%
  feols(log_spread ~ total_restatements| contract_year+industry, cluster = ~ gvkey)
est2 <- loan_data_non %>%
  feols(log_spread ~ total_restatements + profitability_w + leverage_w + mtb_w + log_size| contract_year+industry, cluster = ~ gvkey)

modelsummary(list(est1, est2),stars = c('*' = .1, '**' = .05, '***' = .01),  statistic ="p.value")



############################################
##  *additional analysis
############################################

# A # does auditor quality matters?


est3 <- loan_data_non %>%
  filter(BigN==1) %>%
  feols(log_spread ~ total_restatements + profitability_w + leverage_w + mtb_w + log_size| contract_year+industry, cluster = ~ gvkey)
est4 <- loan_data_non %>%
  filter(BigN==0) %>%
  feols(log_spread ~ total_restatements + profitability_w + leverage_w + mtb_w + log_size| contract_year+industry, cluster = ~ gvkey)

modelsummary(list(est3, est4),stars = c('*' = .1, '**' = .05, '***' = .01))

####################

# B # How about lenders own monitoring through covenants?

est5 <- loan_data_non %>%
  filter(covenant_lite==1) %>%
  feols(log_spread ~ total_restatements + profitability_w + leverage_w + mtb_w + log_size| contract_year+industry, cluster = ~ gvkey)
est6 <- loan_data_non %>%
  filter(covenant_lite==0) %>%
  feols(log_spread ~ total_restatements + profitability_w + leverage_w + mtb_w + log_size| contract_year+industry, cluster = ~ gvkey)

modelsummary(list(est5, est6),stars = c('*' = .1, '**' = .05, '***' = .01))



# C # How about credit rating?



est7 <- loan_data_non %>%
  filter(investmnt_grade==1) %>%
  feols(log_spread ~ total_restatements + profitability_w + leverage_w + mtb_w + log_size| contract_year+industry, cluster = ~ gvkey)
est8 <- loan_data_non %>%
  filter(investmnt_grade==0) %>%
  feols(log_spread ~ total_restatements + profitability_w + leverage_w + mtb_w + log_size| contract_year+industry, cluster = ~ gvkey)

modelsummary(list(est7, est8),stars = c('*' = .1, '**' = .05, '***' = .01))

# D # if lenders care about restatements then more serious restatements should matter more

est1 <- loan_data_non %>%
  feols(log_spread ~ total_serious_restatements| contract_year+industry, cluster = ~ gvkey)
est2 <- loan_data_non %>%
  feols(log_spread ~ total_serious_restatements + profitability_w + leverage_w + mtb_w + log_size| contract_year+industry, cluster = ~ gvkey)

modelsummary(list(est1, est2),stars = c('*' = .1, '**' = .05, '***' = .01),  statistic ="p.value")



# Is our result robust to adding loan controls?


est9 <- loan_data_non %>%
  feols(log_spread ~ total_restatements + profitability_w + leverage_w + mtb_w + log_size| contract_year+industry, cluster = ~ gvkey)
est10 <- loan_data_non %>%
  feols(log_spread ~ total_restatements + profitability_w + leverage_w + mtb_w + log_size+ log(facilityamt)+maturity| contract_year+industry, cluster = ~ gvkey)

modelsummary(list(est9, est10),stars = c('*' = .1, '**' = .05, '***' = .01),  statistic ="p.value")

###############################################
## Different topic
## ESG rating and loan contracting
###########################################

#https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3865147
#https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3838462
#https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4103883
#https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4287295

est1 <- loan_data_non %>%
  feols(log_spread ~ total_esg_score| contract_year, cluster = ~ gvkey)
est2 <- loan_data_non %>%
  feols(log_spread ~ total_esg_score + profitability_w + leverage_w + mtb_w + log_size| contract_year, cluster = ~ gvkey)

modelsummary(list(est1, est2),stars = c('*' = .1, '**' = .05, '***' = .01),  statistic ="p.value")



# where does this come from? E or S or G?
est1 <- loan_data_non %>%
  feols(log_spread ~ governance_score+profitability_w + leverage_w + mtb_w + log_size| contract_year, cluster = ~ gvkey)
est2 <- loan_data_non %>%
  feols(log_spread ~ social_score+profitability_w + leverage_w + mtb_w + log_size| contract_year, cluster = ~ gvkey)
est3 <- loan_data_non %>%
  feols(log_spread ~ environment_score+profitability_w + leverage_w + mtb_w + log_size| contract_year, cluster = ~ gvkey)
modelsummary(list(est1, est2, est3),stars = c('*' = .1, '**' = .05, '***' = .01),  statistic ="p.value")


# Is our main result robust to adding loan controls?
est1 <- loan_data_non %>%
  feols(log_spread ~ total_esg_score+ log(facilityamt)+maturity| contract_year, cluster = ~ gvkey)
est2 <- loan_data_non %>%
  feols(log_spread ~ total_esg_score + profitability_w + leverage_w + mtb_w + log_size+ log(facilityamt)+maturity| contract_year, cluster = ~ gvkey)
modelsummary(list(est1, est2),stars = c('*' = .1, '**' = .05, '***' = .01),  statistic ="p.value")


###############################################
## Different topic
## Lead bank reputation and loan contracting
###########################################

# Does lead bank reputation matters for loan spread? see https://academic.oup.com/rfs/article/23/7/2730/1585863?login=true

est1 <- loan_data_non %>%
  feols(log_spread ~ AvgPrevYearMarketShare| contract_year, cluster = ~ gvkey)
est2 <- loan_data_non %>%
  feols(log_spread ~ AvgPrevYearMarketShare + profitability_w + leverage_w + mtb_w + log_size| contract_year, cluster = ~ gvkey)

modelsummary(list(est1, est2),stars = c('*' = .1, '**' = .05, '***' = .01),  statistic ="p.value")



## Does lead bank reputation matters for the syndicate structure? see https://www.sciencedirect.com/science/article/pii/S1042443115000748#sec0020


est1 <- loan_data_non %>%
  feols(num_lenders ~ AvgPrevYearMarketShare + profitability_w + leverage_w + mtb_w + log_size| contract_year, cluster = ~ gvkey)
est2 <- loan_data_non %>%
  feols(meanshare ~ AvgPrevYearMarketShare + profitability_w + leverage_w + mtb_w + log_size| contract_year, cluster = ~ gvkey)

modelsummary(list(est1, est2),stars = c('*' = .1, '**' = .05, '***' = .01),  statistic ="p.value")




###############################################
## Different topic
## Relationship lending and loan contracting
###########################################

# Does Relationship lending matters for loan spread? see https://academic.oup.com/rfs/article/24/4/1141/1577241

est1 <- loan_data_non %>%
  feols(log_spread ~ REL_Number + profitability_w + leverage_w + mtb_w + log_size| contract_year, cluster = ~ gvkey)
est2 <- loan_data_non %>%
  feols(log_spread ~ REL_Dummy + profitability_w + leverage_w + mtb_w + log_size| contract_year, cluster = ~ gvkey)
est3 <- loan_data_non %>%
  feols(log_spread ~ REL_Amount + profitability_w + leverage_w + mtb_w + log_size| contract_year, cluster = ~ gvkey)

modelsummary(list(est1, est2, est3),stars = c('*' = .1, '**' = .05, '***' = .01),  statistic ="p.value")




## Does Relationship lending matters for the syndicate structure? see https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3716948
# https://onlinelibrary.wiley.com/doi/10.1111/fire.12146

est1 <- loan_data_non %>%
  feols(num_lenders ~ REL_Dummy + profitability_w + leverage_w + mtb_w + log_size| contract_year, cluster = ~ gvkey)
est2 <- loan_data_non %>%
  feols(meanshare ~ REL_Dummy + profitability_w + leverage_w + mtb_w + log_size| contract_year, cluster = ~ gvkey)

modelsummary(list(est1, est2),stars = c('*' = .1, '**' = .05, '***' = .01))


###############################################
## Different topic
## Credit rating and loan contracting
###########################################


est1 <- loan_data_non %>%
  feols(log_spread ~ Credit_Rating_Score + profitability_w + leverage_w + mtb_w + log_size| contract_year, cluster = ~ gvkey)
est2 <- loan_data_non %>%
  feols(log_spread ~ Credit_Rating_Score + profitability_w + leverage_w + mtb_w + log_size| contract_year, cluster = ~ gvkey)

modelsummary(list(est1, est2),stars = c('*' = .1, '**' = .05, '***' = .01))

