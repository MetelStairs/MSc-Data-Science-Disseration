library(DBI)
library(dplyr)
library(dbplyr)
library(tidyverse)
library(eeptools)


con <- dbConnect(odbc::odbc(), .connection_string = "Driver={MySQL ODBC 8.0 Unicode Driver};",
                 Server= "localhost", Database = "epsmon", UID = "root", PWD = "root",
                 Port = 3306)


dbListTables(con)


assessments <- tbl(con, "assessments")
oauth_users <- tbl(con, "oauth_users")


assessments

total_risk_score <- assessments %>%
  as_tibble() %>%
  count(risk_score)

total_risk_score

# 1 male, 2 female??
sex_diff <- oauth_users %>%
  as_tibble() %>%
  count(sex)
sex_diff

#find age out of date of birth

test <- age_calc(csv_test$date_of_birth, enddate = Sys.Date(), units = "years")


library(readr)
library(dplyr)

oauth_users <- read.csv("C:/Users/ELISW/Desktop/Disseration/oauth_users.csv", colClasses = "character", na.strings = "?")

oauth_users$date_of_birth <- as.Date(oauth_users$date_of_birth, format = "%Y-%m-%d")


oauth_users$age <- floor(age_calc(oauth_users$date_of_birth, units = 'years'))

oauth_users$age_range <- oauth_users %>% mutate(age_range = case_when(age < 16 ~ "Under 16",
                                                                age >= 16 & age <= 19 ~ "16-19",
                                                                age >= 20 & age <= 29 ~ "20-29",
                                                                age >= 30 & age <= 39 ~ "30-39",
                                                                age >= 40 & age <= 49 ~ "40-49",
                                                                age >= 50 & age <= 60 ~ "50-60",
                                                                age >61 ~ "More than 60"), 
                                          age_range = factor( age_range, level = c("Under 16", "16-19", "20-29", "30-39", "40-49", "50-60", "More than 60")))
women_only <- filter(oauth_users, sex =="2")
women_only

str(women_only)

men_only <- filter(csv_test, sex == "1")
men_only 

