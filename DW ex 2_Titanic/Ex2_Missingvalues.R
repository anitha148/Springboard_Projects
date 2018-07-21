# Data Wrangling Exercise 2 : Dealing with missing values 

## 0: Load the data in RStudio

titanic_original <- read.csv("~/Data Science course/Data Wrangling/Data Wrangling Projects/Springboard_Projects/DW ex 2_Titanic/titanic_original.csv")
View(titanic_original)

install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)

## 1: Port of embarkation

filter(titanic_original,embarked=="")
titanic_clean <- titanic_original[-c(1310),]
titanic_clean$embarked <-gsub(pattern="^$", replacement = "S" ,x=titanic_clean$embarked)

## 2: Age

View(filter(titanic_clean,is.na(age)))

titanic_clean$age <- titanic_clean$age %>% replace_na(mean(titanic_clean$age, na.rm=T))

### Think about other ways you could have populated the missing values in the age column. Why would you pick any of those over the mean (or not)?

# Answer : 
  
#1. I would use median value is a better estimate here as it ignores extreme values and considers a central value in this context. This is because there are a lot of children and also aged passengers
#2. We can use a regression model to fit the missing values (prediction)
#3. On other thoughts I would have grouped the data based on name search seeking the match "master" this would help me to recognise children replace the median value (of the child group) for these matches and the rest with adult median values 

## 3: Lifeboat

titanic_clean$boat <-gsub(pattern="^$", replacement = "None" ,x=titanic_clean$boat)

## 4: Cabin

### Does it make sense to fill missing cabin numbers with a value?

# As almost 80% of the data is missing and also it contains a large number of unique values. Hence filling with a value would not improve the dataframe for analysis in anyway.

### What does a missing value here mean?

# When i filtered the data for missing values in cabin using

View(filter(titanic_clean,cabin==""))

# I noticed a link between "Pclass" and missing values in "cabin"
# The higher the pclass number the more the number of cabin values missing.

#This may indicate that most of the missing cabin numbers belonged to Pclass = 3, which is the A proxy for socio-economic status (SES) and number 3 is the lower class.

#So most of the people from lower class might not have a cabin for themselves.

### Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.

titanic_clean <- titanic_clean %>% 
  mutate(has_cabin_number = case_when(cabin == "" ~ 0, cabin != "" ~ 1))

View(titanic_clean)

## 5: Submit the project on Github

write.csv(titanic_clean,"~/Data Science course/Data Wrangling/Data Wrangling Projects/Springboard_Projects/DW ex 2_Titanic/titanic_clean.csv")