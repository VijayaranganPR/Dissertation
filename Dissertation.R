# importing Packages
library(tidyverse)  #for reading data, wrangling, visualization etc.
library(DescTools)  # to winsorize outliers
library(dplyr)
library(pandoc)
library(modelsummary) #for summarizing data/descriptive stataistics
library(corx)         # for the correlation matrix
library(fixest)       # for the regression analysis
library(ggplot2)
library(rmarkdown)
library(officer)


# loading the data

loan_data <- read_csv("final_data_702.csv")

# view(loan_data)


# extracting the variables we needed
my_loan_data <- loan_data %>%
  select(gvkey, facilityid, contract_year,
         total_esg_score, allindrawn,
         total_assets, profitability, leverage, industry)

view(my_loan_data)
###############
# Handling the missing data
##############
#
# # First we check the independent variable (total_esg_score)
#
# #creating a table of how much non NA values in total_esg_score by each year
# non_na_count_by_year <- aggregate(!is.na(my_loan_data$total_esg_score),
# by = list(my_loan_data$contract_year), FUN = sum)
#
# colnames(non_na_count_by_year) <- c("contract_year", "total_esg_score_count")
#
# # Creating a bar chart to visualize where the ESG score values are
# barplot(non_na_count_by_year$total_esg_score_count, names.arg = non_na_count_by_year$contract_year,
#         xlab = "Contract Year", ylab = "Number of Non NA Values", main = "Non NA Values by Year",
#         col = "skyblue")
# 
# # from the graph we can see that ESG score values starting at 2009 and ending at 2019
# # so we are filtering it from 2009 to 2019
loan_data_clean <-  my_loan_data %>%
  filter(between(contract_year, 2009, 2019))

view(loan_data_clean)
# 
# # removing all the rows where NA is present in total_esg_score
# loan_data_clean <- loan_data_recent[!is.na(loan_data_recent$total_esg_score), ]
# # view(loan_data_clean)
# 
# 
# # Second we check the for leverage
# # removing all the rows where NA is present in leverage
# loan_data_clean <- loan_data_clean[!is.na(loan_data_clean$leverage), ]
# # view(loan_data_clean)
# 
# # third we check the for leverage
# # removing all the rows where NA is present in profitability
# loan_data_clean <- loan_data_clean[!is.na(loan_data_clean$profitability), ]
# # view(loan_data_clean)
# 
# # fourth we check the for leverage
# # removing all the rows where NA is present in profitability
# loan_data_clean <- loan_data_clean[!is.na(loan_data_clean$industry), ]
# # view(loan_data_clean)
# 

#####################
# Check the sample
#####################

n_distinct(loan_data_clean$gvkey)

n_distinct(loan_data_clean$facilityid)

table(loan_data_clean$contract_year)

sort(unique(loan_data_clean$contract_year))


##############################################
# drop borrowers who belongs to the finance industry because they are regulated differently (note that financial industry code=6)
##############################################


#loading industry data
Industry <- read_csv("Industry.csv") %>%
  select(gvkey, sic2, category)

#view(Industry)

loan_data_clean_with_industry <- loan_data_clean %>%
  inner_join(Industry, by = "gvkey")

view(loan_data_clean_with_industry)

# which industry codes we have in the data
table(loan_data_clean_with_industry$industry)


# sum(!is.na(loan_data_clean_with_industry$industry))

# unique years
unique(loan_data_clean_with_industry$industry)

# what industries we have in the data
unique(loan_data_clean_with_industry$category)


loan_data_clean_non <- loan_data_clean_with_industry %>%
  # Filter out records where industry is 6 or sic2 is between 59 and 70
  filter(!(industry == 6 | (sic2 >= 59 & sic2 <= 70)))


table(loan_data_clean_non$industry)

# sum(!is.na(loan_data_clean_non$industry))

unique(loan_data_clean_non$category)

unique(loan_data_clean_non$sic2)

n_distinct(loan_data_clean_non$gvkey)
n_distinct(loan_data_clean_non$facilityid)

table(loan_data_clean_non$contract_year)
unique(loan_data_clean_non$contract_year)


#############
# skewness and outliners
#############
plot(loan_data_clean_non$total_assets, loan_data_clean_non$allindrawn)

# checking the first few records
loan_data_clean_non %>%
  select(total_esg_score, allindrawn) %>%
  head()


# checking the type
table(loan_data_clean_non$total_esg_score)
table(loan_data_clean_non$allindrawn)


loan_data_clean_non %>%
  ggplot(aes(total_assets)) +
  geom_histogram()

loan_data_clean_non %>%
  ggplot(aes(profitability)) +
  geom_histogram()


loan_data_clean_non %>%
  ggplot(aes(total_esg_score)) +
  geom_histogram()

# winsorize

loan_data_non <- loan_data_clean_non %>%
  mutate(across(c(profitability, leverage), ~{
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



view(loan_data_non)


# rename some variables to make the name self explanatory

loan_data_non <- loan_data_non %>%
  rename(size=total_assets) %>%
  rename(spread=allindrawn)%>%
  rename(log_size=log_total_assets) %>%
  rename(log_spread=log_allindrawn)


##############
# describe the data
##############

# Table 1: Summary Statistics

data_analysis <- loan_data_non %>%
  select(total_esg_score, spread, log_spread, size, log_size, profitability_w, leverage_w)

datasummary(total_esg_score+ spread+ log_spread+ size+ log_size+ profitability_w+ leverage_w 
            ~ N + Mean + Median + SD + Min + P25 + P75 + Max,
            title = "Table 1: Summary Statistics",
            data = data_analysis,
            fmt = 4,   #decimal places
            output = "Table1.docx")

# correlation matrix


data_analysis <- as.data.frame(data_analysis) #convert to data frame

corr_matrix <- corx(data_analysis,
                    stars = c(0.1, 0.05, 0.01),
                    triangle = "lower",
                    caption = "Table 3 Correlation matrix")

to_clipboard(corr_matrix) #copy to clipboard and then to excel



est1 <- loan_data_non %>%
  feols(log_spread ~ total_esg_score)
est2 <- loan_data_non %>%
  feols(log_spread ~ total_esg_score + profitability_w + leverage_w + log_size)

modelsummary(list(est1, est2),stars = c('*' = .1, '**' = .05, '***' = .01),  statistic ="p.value")


modelsummary(list(est1, est2),stars = c('*' = .1, '**' = .05, '***' = .01),  statistic ="p.value", output = "Table4.docx")
