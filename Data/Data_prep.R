library(tidyverse)
library(eeptools)
library(readr)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(ggpattern)

options("scipen" = 10)



#install.packages("remotes")
#remotes::install_github("coolbutuseless/ggpattern")

##TO DO
#1. Make graphs pretty



##_______________________________________________________________________________________________________##

#Read CSV files into dataframe
oauth_users <- read.csv("C:/Users/ELISW/Desktop/Disseration/oauth_users.csv",  na.strings = "?")
assessments <- read.csv("C:/Users/ELISW/Desktop/Disseration/assessments.csv",  na.strings = "?")
assessments_answers <- read.csv("C:/Users/ELISW/Desktop/Disseration/assessments_answers.csv",  na.strings = "?")
assessment_meds <- read.csv("C:/Users/ELISW/Desktop/Disseration/assessments_meds.csv",  na.strings = "?")
med_cat <- read.csv("C:/Users/ELISW/Desktop/Disseration/med__category.csv",  na.strings = "?")
med_cat10n  <- read.csv("C:/Users/ELISW/Desktop/Disseration/med__category10n.csv",  na.strings = "?")
meds  <- read.csv("C:/Users/ELISW/Desktop/Disseration/meds.csv",  na.strings = "?")
meds10n  <- read.csv("C:/Users/ELISW/Desktop/Disseration/meds10n.csv",  na.strings = "?")
question_cat  <- read.csv("C:/Users/ELISW/Desktop/Disseration/question_category.csv",  na.strings = "?")
question_cat10n <- read.csv("C:/Users/ELISW/Desktop/Disseration/question_category10n.csv",  na.strings = "?")
questions <- read.csv("C:/Users/ELISW/Desktop/Disseration/questions.csv",  na.strings = "?")
questionsn10n <- read.csv("C:/Users/ELISW/Desktop/Disseration/questionsn10.csv",  na.strings = "?")

 

#Formats the data of birth so i can be used to calculate the age of user
oauth_users$date_of_birth <- as.Date(oauth_users$date_of_birth, format = "%Y-%m-%d")

#uses date if birth to find user age
oauth_users$age <- floor(age_calc(oauth_users$date_of_birth, units = 'years'))

#Factorizes and groups age ranges 
factor_users <- oauth_users%>% na.omit() %>% mutate(age_range = case_when(age < 16 ~ "Under 16",
                                                                      age >= 16 & age <= 19 ~ "16-19",
                                                                      age >= 20 & age <= 29 ~ "20-29",
                                                                      age >= 30 & age <= 39 ~ "30-39",
                                                                      age >= 40 & age <= 49 ~ "40-49",
                                                                      age >= 50 & age <= 60 ~ "50-60",
                                                                      age >61 ~ "More than 60"), 
                                                age_range = factor( age_range, level = c("Under 16", "16-19", "20-29", "30-39", "40-49", "50-60", "More than 60")))

##_____________________________________________________________________________________________________##
#Getting risk score from assessment_answers


#Adds all the risk scores up to a total per assessment
risk_score_ans <- assessments_answers %>% group_by(assessment_id) %>% summarise(risk_score = sum(risk_score)) %>% rename(id = assessment_id)

#renames columm 
assessments <- assessments %>% rename(defunct_risk_score = risk_score)

#merges two df
assessments <- merge(x = assessments, y =risk_score_ans, by = "id")



##___________________________________________________________________________________________________##
##Epilesy type

# Mean of users age of onset of epilepsy
mean(factor_users$age_of_onset_of_epilepsy)


factor_users

#Creates a new dataframe using only women, gets rids of NA values
women_only <- filter(factor_users, sex =="2") %>% na.omit()
women_only

str(women_only)


# Total amount of female users in age ranges
women_age_range <- women_only %>% group_by(age_range) %>% summarise(length(age_range))

women_age_range

# Graph to show the age groups of female users on the app
ggplot(women_age_range, aes( x= age_range, y = `length(age_range)`)) +
  geom_bar(stat = "identity") +
  xlab("Age Range") +
  ylab("Number of Users") + 
  theme_clean()



# Counts the amount of users with seizure type in their age group
women_seiz_count <- women_only %>% group_by(age_range) %>% summarise( sum(seizures_focal), sum(seizures_tonic_clonic), sum(seizures_tonic),
                                                                       sum(seizures_atonic), sum(seizures_myoclonic), sum(seizures_absence), sum(seizures_non_epilepsy), sum(seizures_other))


# Renames the seizure type columns so its easier to refer to them
women_seiz_count <- women_seiz_count %>% rename(Focal = `sum(seizures_focal)`, Tonic_Clonic = `sum(seizures_tonic_clonic)`, Tonic = `sum(seizures_tonic)`,
                                                Atonic = `sum(seizures_atonic)`, Myoclonic = `sum(seizures_myoclonic)`, Absence = `sum(seizures_absence)`,
                                                Non_Epilepsy = `sum(seizures_non_epilepsy)`, Other = `sum(seizures_other)`)

women_seiz_count

# Changes the table format for further analysis
women_seiz_count_long <- women_seiz_count%>% gather(seizure_type, count, 2:9) 
women_seiz_count_long

#Graph to show what age groups have what types of epilepsy
ggplot(women_seiz_count_long, aes(fill = age_range, x =seizure_type, y = count))+
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_grey(start = 0, end = .9) +
  labs(fill = "Age Range") +
  xlab("Seizure Type") +
  ylab("Number of Users") + 
  theme_clean()


#______________________________________________________________________________________________________#
## Merging datasets ##


#This merges the fields we want from the oauth-users table into the assessment table
factor_users <- factor_users %>% rename(user_id = username)
prep_users <- factor_users %>%
  select(user_id, sex, age, age_range)
mrg_user_asses <- merge(x = assessments, y =prep_users, by = "user_id")
mrg_user_asses

#This merges the medication into the assessment table
meds10n <- meds10n %>% rename(med_id = medication_id)
prep_meds <- meds10n %>%
  select(med_id, medication_name)
mrg_meds_asses <- merge(x = assessment_meds, y = prep_meds, by = "med_id")

mrg_user_asses <- mrg_user_asses %>% rename(assessment_id = id)
prep_meds_asses <- mrg_meds_asses %>%
  select(medication_name, assessment_id, dosage)
mrg_user_meds_asses <- merge(x = mrg_user_asses, y = mrg_meds_asses, by = "assessment_id")

##________________________________________________________________________
##Finding out drig characteristics
#One assessment can have multiple medication, could be problematic, come back later!!!
mrg_user_meds_asses 

mrg_women_only_meds <- filter(mrg_user_meds_asses, sex ==2)

mrg_women_only_meds


mrg_women_meds_grouped <- mrg_women_only_meds %>% group_by(age_range,medication_name) %>% tally(sort = TRUE)

#Takes the top 20 15 medication
mrg_women_meds_grouped_top <- mrg_women_only_meds %>% na.omit() %>% group_by(age_range, medication_name) %>% tally(sort = TRUE) %>% slice(1:15)

mrg_women_meds_grouped<- mrg_women_only_meds %>% na.omit() %>% group_by(age_range, medication_name) %>% tally(sort = TRUE)

mrg_women_only_meds <- mrg_women_only_meds %>% rename(count = n)

#Shows number of prescriptions!!! Not number of people!!!

mrg_women_meds_grouped_top %>%
  ggplot(., aes(fill = age_range, x = medication_name, y = n)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(fill = "Age Range") +
  scale_fill_grey(start = 0, end = .9) +
  xlab("Medication Name") +
  ylab("Number of Prescriptions") + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


mrg_women_meds_grouped %>%
  ggplot(., aes(fill = age_range, x = medication_name, y = n)) +
  geom_bar(position = "stack", stat = "identity") +
  labs( fill = "Age Range") +
  scale_fill_grey(start = 0, end = .9) +
  xlab("Medication Name") +
  ylab("Number of Prescriptions") + 
  theme_clean() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
##___________________________________________________________________________
##Risk score for each age group

head(mrg_user_asses)

women_risk_score <- filter(mrg_user_asses, sex == 2)
women_risk_score

women_risk_score_grouped <- women_risk_score %>% na.omit() %>% group_by(age_range, risk_score) %>% tally(sort = TRUE)
women_risk_score_grouped

women_risk_score_grouped %>%
  ggplot(.,aes(fill = age_range, x = risk_score, y = n, label = n)) +
  geom_bar(position = "stack", stat = "identity") +
  labs( fill = "Age Range") +
  scale_fill_grey(start = 0, end = .9) +
  xlab("Risk Score") +
  ylab("Number of Assessments") + 
  theme_clean() 

women_risk_score_grouped %>%
  ggplot(.,aes(fill = age_range, x = risk_score, y = n, label = n)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_grey(start = 0, end = .9) +
  labs(fill = "Age Range") +
  xlab("Risk Score") +
  ylab("Number of Assessments") + 
  theme_clean() +
  scale_y_log10()

ggplot(women_risk_score_grouped, aes(x = risk_score, y = n, fill = age_range)) +
  geom_point() +   scale_y_continuous(trans = "log10")

##__________________________________________________________________
#Splits all the medical categories
#split_cats <- mrg_user_asses %>% separate(medication_categories,c("cat1", "cat2", "cat3", "cat4", "cat5", "cat6", "cat7", "cat8", "cat9", "cat10", "cat11", "cat12", "cat13", "cat14"), ",")
split_cats_v2 <- separate_rows(mrg_user_asses, medication_categories, sep = ",")
grouped_medical_cat <- split_cats_v2 %>% na.omit() %>% group_by(age_range, medication_categories) %>% tally(sort = TRUE)
grouped_medical_cat

grouped_medical_cat <- grouped_medical_cat[!(is.na(grouped_medical_cat$medication_categories) | grouped_medical_cat$medication_categories==""), ]


grouped_medical_cat
#


grouped_medical_cat %>%
  ggplot(., aes(fill = age_range, x = medication_categories, y = n, label = n)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(fill = "Age Range") +
  scale_fill_grey(start = 0, end = .9) +
  xlab("Medical Category") +
  ylab("Number of Medical Problems") + 
  theme_clean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
##________________________________________________________________________
#Q2; Users who use the app more frequently than others 
# Using 5 as a divider between frequent, due it supposed to be done once every 4 months
mrg_user_asses
freq_women <- filter(mrg_user_asses, sex == 2)

freq_users <- freq_women %>% group_by(user_id) %>% tally(sort = TRUE)

freq_users

median(freq_users)

mrg_freq <- merge(x =factor_users, y = freq_users, by = "user_id")

mrg_freq

mrg_freq <- mrg_freq %>% mutate(frequent_user = ifelse(n  >= 5, "Frequent", "Infrequent"))

#age range of frequent assessments users  

mrg_freq_age_range <- mrg_freq %>% na.omit() %>% group_by(age_range, frequent_user) %>% summarise(length(age_range))

mrg_freq

mrg_freq_age_range

mrg_freq_age_range
ggplot(mrg_freq_age_range, aes( x= age_range, y = `length(age_range)`, fill = frequent_user)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(fill = "Frequent User") +
  scale_fill_grey(start = 0, end = .5) +
  xlab("Age Range") +
  ylab("Number of Users") + 
  theme_clean()



mrg_risk <-mrg_risk <- merge(x =mrg_user_asses, y = freq_users, by = "user_id")

mrg_risk <- mrg_risk %>% mutate(frequent_user = ifelse(n  >= 5, "Frequent", "Infrequent"))

freq_risk_score <- mrg_risk %>% na.omit() %>% group_by(frequent_user, risk_score) %>% tally(sort = TRUE)

freq_risk_score %>%
  ggplot(.,aes(fill = frequent_user, x = risk_score, y = n)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(fill = "Age Range") +
  scale_fill_grey(start = 0, end = .5) +
  xlab("Risk Score") +
  ylab("Number of Assessments") + 
  theme_clean() +
  scale_y_log10()


freq_risk_score %>%
  ggplot(.,aes(fill = frequent_user, x = risk_score, y = n)) +
  geom_bar(position = "stack", stat = "identity") +
  labs( fill = "Age Range") +
  scale_fill_grey(start = 0, end = .5) +
  xlab("Risk Score") +
  ylab("Number of Assessments") + 
  theme_clean()
##ONCE AGAIN, NUMBER OF PRESCRIPTION, NOT NUMBER OF USERS, ONE USER MAY HAVE MORE THAN ONE PRESCRIPTION
##medication 
mrg_user_meds_asses

mrg_freq_meds <- merge(x =mrg_user_meds_asses, y = freq_users, by = "user_id")

mrg_freq_meds <- mrg_freq_meds %>% mutate(frequent_user = ifelse(n  >= 5, "Frequent", "Infrequent"))


mrg_freq_meds_plot <- mrg_freq_meds %>% na.omit() %>% group_by(frequent_user, medication_name) %>% tally(sort = TRUE) %>% slice(1:15)
mrg_freq_meds_plot %>%
  ggplot(., aes(fill = frequent_user, x = medication_name, y = n)) +
  geom_bar(position = "stack", stat = "identity") +  
  scale_fill_grey(start = 0, end = .5) +
  labs(fill = "Frequent User") +
  xlab("Medication Name") +
  ylab("Number Of Prescriptions") + 
  theme_clean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

split_cats_v3 <- separate_rows(mrg_freq_meds, medication_categories, sep = ",")

freq_grouped_medical_cat <- split_cats_v3 %>% na.omit() %>% group_by(frequent_user, medication_categories) %>% tally(sort = TRUE)

freq_grouped_medical_cat <- freq_grouped_medical_cat[!(is.na(freq_grouped_medical_cat$medication_categories) | freq_grouped_medical_cat$medication_categories==""), ]
##Number of assessments medication categories 
freq_grouped_medical_cat %>%
  ggplot(., aes(fill = frequent_user, x = medication_categories, y = n)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_grey(start = 0, end = .5) +
  labs(fill = "Frequent User") +
  ylab("Number Of Medical Problems ") +
  xlab("Medical Catergory") + 
  theme_clean() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
##_______________________________________________________________________________________________________##
##Q3-SUDEP 
## Epsmon SUDEP related question is in the assessment_answers, question ID 32

q32 <- filter(assessments_answers, question_id == 32)

q32$answer_given<-ifelse(q32$answer_given== 0 , "Not Aware", "Aware")


q32


ggplot(q32,aes( x = answer_given, fill = answer_given)) +
  geom_bar()+ 
  scale_fill_grey(start = 0, end = .5) +
  xlab("User Aware of SUDEP Risk") + 
  ylab("Numer of Users") +
  theme_clean() + 
  theme(legend.position = "none") 

