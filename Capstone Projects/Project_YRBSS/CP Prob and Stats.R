# Capstone project : statistical analysis
## Data Visualization

install.packages("tidyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse") 

library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)

### Loading Data file

yrbss <- read.csv("~/Data Science course/Data Wrangling/Data Wrangling Projects/Springboard_Projects/Capstone Projects/Project_YRBSS/YRBSS.csv")

##********************* EDA of smoking in Youth ***************************

### converting factors with labels of smoker

yrbss$smoker[yrbss$Q9 == "1"] <- "Yes"
yrbss$smoker[yrbss$Q9 == "2"] <- "No"
yrbss$smoker[yrbss$Q9 == "0"] <- "Missing"
yrbss$smoker <- factor(yrbss$smoker)

### converting factors with labels of Sex

yrbss$sex[yrbss$Q2 == "1"] <- "Female"
yrbss$sex[yrbss$Q2 == "2"] <- "Male"
yrbss$sex[yrbss$Q2 == "0"] <- "Missing"
yrbss$sex <- factor(yrbss$sex)

### converting factors with labels of grade they study

yrbss$grade[yrbss$Q3 == "1"] <- "09th grade"
yrbss$grade[yrbss$Q3 == "2"] <- "10th grade"
yrbss$grade[yrbss$Q3 == "3"] <- "11th grade"
yrbss$grade[yrbss$Q3 == "4"] <- "12th grade"
yrbss$grade[yrbss$Q3 == "5"] <- "Ungraded/Other"
yrbss$grade[yrbss$Q3 == "0"] <- "Missing"
yrbss$grade <- factor(yrbss$grade)

### converting factors with labels of electronic vapor product users

yrbss$evp[yrbss$Q14 == "1"] <- "Used"
yrbss$evp[yrbss$Q14 == "2"] <- "Not Used"
yrbss$evp[yrbss$Q14 == "0"] <- "Missing"
yrbss$evp<- factor(yrbss$evp)

### Count and Percentage of Smokers

yrbss %>%
      filter(smoker != "Missing") %>% 
      group_by(smoker) %>% 
      summarize(count = n()) %>% 
      mutate(percentage = 100*count/sum(count))
##      smoker    count     percentage
##      No        47535      58.1 
##      Yes       34227      41.9 

### Bar Graph of Percentage of smokers
 
yrbss %>%
      filter(smoker != "Missing") %>% 
      ggplot(aes(x = smoker, fill = smoker)) + 
      geom_bar(aes(y = (..count..)/sum(..count..))) + 
      labs(x = 'Smoker', title = 'Ever tried smoking', y = 'Percentage') +
      scale_y_continuous(labels = scales::percent) 
### Comment : The proportion of smokers are less as compared to non smokers 

 
### Percentage Smokers grouped by Sex

 yrbss %>%
       filter(smoker == "Yes" & sex != "Missing") %>% 
       group_by(sex) %>% 
       summarize(freq = n()) %>% 
       mutate(percentage = 100*freq/sum(freq))
##`      sex`        freq    percentage
##   1   Female      16309     47.8  
##   2   Male        17777     52.2  

### Bar Graph grouped by Sex for Smokers

yrbss %>%
  filter(smoker == "Yes" & sex != "Missing") %>% 
  ggplot(aes(x = sex, fill = sex)) +
  geom_bar(aes(y =prop.table(..count..))) + 
  labs(x = 'Smoker', title = 'Ever tried smoking', y = 'Percentage') +
  scale_y_continuous(labels = scales::percent) 
### Comment : The percentage of female smokers are less as compared to male smokers 
           

### Bar Chart to show all Smokers and Non Smokers grouped by Sex

yrbss %>%
  filter(smoker != "Missing" & sex != "Missing") %>%
  ggplot(aes(x = smoker, fill = sex)) +
  geom_bar(position = "dodge") +
  geom_text(aes(y = (..count..)+400,label = paste0(..count..)),stat = 'count',size = 3, 
            position = position_dodge(1)) + 
  labs(x = 'Smoker', y = 'Number of Students', fill = 'Sex',title = 'Ever tried smoking')
### Comment : The number of male smokers are more compared to female smokers and 
###           The number of male non smokers are less as compared to female smokers

### Bar Chart to show Proportion of Smokers and Non Smokers based on Sex

yrbss %>%
  filter(smoker != "Missing" & sex != "Missing") %>%
  ggplot(aes(x = smoker, fill = sex)) +
  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count',position = position_dodge(1),size = 3) + 
  labs(x = 'Smoker', y = 'Percentage', fill = 'Sex',title = 'Ever tried smoking')
### Comment : The percentage male smokers are more compared to female smokers and 
###           The percentage male non smokers are less as compared to female smokers

### Smokers Filtered by Grades

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & smoker == "Yes") %>% 
  group_by(grade) %>% 
  summarize(freq = n()) %>% 
  mutate(percentage = 100*freq/sum(freq))
##   grade         freq     percentage
##
##   09th grade    7045       20.8
##   10th grade    7750       22.8
##   11th grade    9151       27.0
##   12th grade    9991       29.4


### Number of smokers in each grade

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & smoker == "Yes") %>%
  ggplot(aes(x = grade, fill = grade)) +
  geom_bar(position = "dodge") +
  geom_text(aes(y = (..count..)+300,label = paste0(..count..)),stat = 'count',size = 3, 
            position = position_dodge(1)) + 
  labs(x = 'Grade', y = 'Number of Smokers', fill = 'Student Grade',title = 'Ever tried smoking')
### Comment : The number of smokers increase as the grade increases, suggesting that 
###           higher the grade the more the number of smokers

### Percentage of smokers in each grade


yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & smoker == "Yes")  %>%
  ggplot(aes(x = grade, fill = grade)) +
           geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
           geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                         label = paste0(round(prop.table(..count..) * 100,2), '%')), 
                     stat = 'count',position = position_dodge(1),size = 3) + 
           labs(x = 'Grade', y = 'Percentage Smoker',title = 'Ever tried smoking')
### Comment : The percentage of smokers increase as the grade increases, suggesting that 
###           higher the grade the greater the number of smokers


### Number of smokers and Non Smokers in each grade

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & smoker != "Missing") %>%
  ggplot(aes(x = grade, fill = smoker)) +
  geom_bar(position = "dodge") +
  geom_text(aes(y = (..count..)+400,label = paste0(..count..)),stat = 'count',size = 3, 
            position = position_dodge(1)) + 
  labs(x = 'Grade', y = 'Numbers of Smokers', fill = 'Smoker',title = 'Ever tried smoking')
### Comment : The number of smokers increase as the grade increases and the non smokers decrease,
###           suggesting that with grade we have more users of cigarettes

### Percentage of smokers and Non Smokers in each grade

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & smoker != "Missing")  %>%
  group_by(grade) %>%
  count(smoker) %>%
  mutate(percent=round(n/sum(n)*100, 2)) %>%
  ungroup(grade) %>%
  ggplot(aes(x = grade, y = percent, fill = smoker)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(y = percent + 1, 
                label = paste0(round(percent,2), '%')),position = position_dodge(1),size = 3) + 
  labs(x = 'Grade', y = 'Percentage Smokers',title = 'Ever tried smoking')
### Comment : As the grade increases there is gradual increase in  percentage smokers
###           and decrease in non smokers. Eventually almost reaching the same percentage
###           by grade 12. No gap in users and non users


### Proportion of smokers and Non Smokers across all grades

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & smoker != "Missing")  %>%
  ggplot(aes(x = grade, fill = smoker)) +
  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count',position = position_dodge(1),size = 3) + 
  labs(x = 'Grade', y = 'Percentage Smokers',title = 'Ever tried smoking')
### Comment : In the lower grade the percentage of non smokers is higher- 17% 
###           where as smokers is 9%, of the total smoker data. 
###           But as the grade increases there is gradual increase in  percentage of smokers 
###           and decrease in non smokers. Eventually almost filling the gap by grade 12.


## Time Series plot for smokers from 2007 to 2017

### Percentages trend across the years grouped by smoker

yrbss %>%
  filter(smoker != "Missing")  %>%
  group_by(YEAR) %>%
  count(smoker) %>%
  mutate(percent=round(n/sum(n)*100, 2)) %>%
  ungroup(YEAR) %>%
  group_by(smoker) %>%
  ggplot(aes(x = YEAR, y = percent, group = smoker, color = smoker)) + 
  geom_point(shape = 22, fill="white", color="blue", size = 3) +
  geom_line(size = 1) + 
  geom_text(aes( label = paste0(percent,"%")),position = position_nudge(0.4), size = 4) +
  labs( y = 'Percentage',title = 'Ever tried smoking')
### Comment : On a happier note, studying the smoker pattern over ten years shows a decline
###           in percentage on smokers from 53% in 2007 to 28% in 2017.  
###           The time series trend also show a good increase in percentage of non smokers
###           from 47% in 2007 to 72% in 2017.
###           This can suggest that effective measures and steps might be showing a positive effect
 
### Percentage of students 13 years and younger who are smokers

yrbss %>%
  subset(Q1 ==1:2) %>%
  filter(smoker != "Missing") %>% 
  ggplot(aes(x = smoker, fill = smoker)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  labs(x = 'Smoker', title = 'Ever tried smoking aged 13 years and less', y = 'Percentage') +
  scale_y_continuous(labels = scales::percent)
### Comment : When grouped for smokers and non smokers below the age of 13, 
###           there is an alarmingly higher percentage of smokers in this age group. 
###           Highlighting that more preventive actions should be diverted towards this age group.

yrbss %>%
  subset(Q1 ==1:2) %>%
  filter(smoker != "Missing" & sex != "Missing") %>%
  ggplot(aes(x = smoker, fill = sex)) +
  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count',position = position_dodge(1),size = 3) + 
  labs(x = 'Smoker', y = 'Percentage', fill = 'Sex',title = 'Ever tried smoking aged 13 years and less')
### Comment : When smokers and non smokers below the age of 13 are grouped for sex, 
###           there is a higher percentage of male smokers compared to female smokers. 
###           This is the same finding with the overall users data when not group under this criteria

## Electronic Vapor product users (e-cigarettes)

### TOTAL Users from 2015 and 2017

yrbss %>%
  filter(evp != "Missing") %>%
  ggplot(aes(x = evp, fill = evp)) + 
  geom_bar(aes(y = (..count..)/sum(..count..) * 100), position = "dodge") + 
  geom_text(aes(y = (..count..)/sum(..count..) * 100 + 1, 
                label = paste0(round((..count..)/sum(..count..) * 100,2), '%')), 
            stat = 'count',position = position_dodge(1),size = 4) +
  labs(x = 'Electronic Vapor product', title = 'Ever Used an Electronic Vapor Product', y = 'Percentage')
### Comment : The new fab in recent years which is taking over the teenagers is the vaping plague.
###           The data exists only for 2015 and 2017
###           There is a high percentage of users of this product which is an area of concern.


### Percentage Users of Electronic Vapor Product in Both 2015 and 2017
  
yrbss %>%
  filter(evp != "Missing") %>%
  group_by(YEAR) %>%
  count(evp) %>%
  mutate(percent=round(n/sum(n)*100, 2)) %>%
  ungroup(YEAR) %>%
  group_by(evp) %>%
  ggplot(aes(x = evp, y = percent,fill=evp)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(y = percent + 1, label = paste0(round(percent,2), '%')), 
            position = position_dodge(1),size = 4) + 
  labs(x = 'Electronic Vapor product', title = 'Ever Used an Electronic Vapor Product', y = 'Percentage') +
  facet_grid(.~ YEAR)

### Proportion of Users and non users of electronic cigarettes across both 2015 and 2017

yrbss %>%
  filter(evp != "Missing") %>%
  ggplot(aes(x = evp, fill = evp)) + 
  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count',position = position_dodge(1),size = 3) + 
  labs(x = 'Electronic Vapor product', title = 'Ever Used an Electronic Vapor Product', y = 'Percentage') +
  facet_grid(.~ YEAR)
  
### Trend of Vaping across 2015 and 2017

yrbss %>%
  filter(evp != "Missing")  %>%
  group_by(YEAR) %>%
  count(evp) %>%
  mutate(percent=round(n/sum(n)*100, 2)) %>%
  ungroup(YEAR) %>%
  group_by(evp) %>%
  ggplot(aes(x = YEAR, y = percent, group = evp, color = evp)) + 
  geom_point(shape = 22, fill="lime green", color="navy", size = 3) +
  geom_line(size = 1) + 
  geom_text(aes( label = paste0(percent,"%")),position = position_nudge(0.155), size = 4) +
  labs( y = 'Percentage',title = 'Ever Used an Electronic Vapor Product')

### Comment : Though there is a drop in users of vaping over the two compared years this drop 
###           may not be of significant and can be investigated further.
###           Also the sample of two years may not be a good sample to come to conclusions.
###           The authorities concerned should take effect actions against this new threat!

##*************************** EDA of Alcohol Usage in Youth  ***************************


### converting factors with labels for  alcohol users

yrbss$alcoholic[yrbss$Q21 == "1"] <- "No"
yrbss$alcoholic[yrbss$Q21 >= "2"] <- "Yes"
yrbss$alcoholic[yrbss$Q21 == "0"] <- "Missing"
yrbss$alcoholic<- factor(yrbss$alcoholic)


### Bar Graph of Percentage of Alcoholics

yrbss %>%
  filter(alcoholic != "Missing") %>% 
  ggplot(aes(x = alcoholic, fill = alcoholic)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  labs(x = 'Alcoholic', title = 'Drank Alcohol', y = 'Percentage') +
  scale_y_continuous(labels = scales::percent) 

### Comment : The percentage of Alcohol users are more as compared to non users 

### Percentage Alcoholics grouped by Sex

yrbss %>%
  filter(alcoholic == "Yes" & sex != "Missing") %>% 
  group_by(sex) %>% 
  summarize(freq = n()) %>% 
  mutate(percentage = 100*freq/sum(freq))
##`      sex`        freq    percentage
##   1   Female      25684     51.1  
##   2   Male        24566     48.9  

### Bar Graph grouped by Sex for alcoholics

yrbss %>%
  filter(alcoholic == "Yes" & sex != "Missing") %>% 
  ggplot(aes(x = sex, fill = sex)) +
  geom_bar(aes(y =prop.table(..count..))) + 
  labs(x = 'alcoholic', title = 'Drank Alcohol', y = 'Percentage') +
  scale_y_continuous(labels = scales::percent) 

### Bar Chart to show alcoholics and Non alcoholics based on Sex

yrbss %>%
  filter(alcoholic != "Missing" & sex != "Missing") %>%
  ggplot(aes(x = alcoholic, fill = sex)) +
  geom_bar(position = "dodge") +
  geom_text(aes(y = (..count..)+400,label = paste0(..count..)),stat = 'count',size = 3, 
            position = position_dodge(1)) + 
  labs(x = 'alcoholic', y = 'Number of Students', fill = 'Sex',title = 'Drank Alcohol')

### Bar Chart to show Proportion of alcoholics and Non alcoholics based on Sex

yrbss %>%
  filter(alcoholic != "Missing" & sex != "Missing") %>%
  ggplot(aes(x = alcoholic, fill = sex)) +
  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count',position = position_dodge(1),size = 3) + 
  labs(x = 'alcoholic', y = 'Percentage', fill = 'Sex',title = 'Drank Alcohol')
### Comment : The percentage of female Alcohol users are slightly more than male users 
###           suggesting no significant change in percentage users grouped by sex.

### alcoholics Filtered by Grades

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & alcoholic == "Yes") %>% 
  group_by(grade) %>% 
  summarize(freq = n()) %>% 
  mutate(percentage = 100*freq/sum(freq))

##   grade         freq     percentage
##
##   09th grade    10919       21.8
##   10th grade    11785       23.5
##   11th grade    13483       26.9
##   12th grade    13892       27.7

### Number of alcoholics in each grade

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & alcoholic == "Yes") %>%
  ggplot(aes(x = grade, fill = grade)) +
  geom_bar(position = "dodge") +
  geom_text(aes(y = (..count..)+300,label = paste0(..count..)),stat = 'count',size = 3, 
            position = position_dodge(1)) + 
  labs(x = 'Grade', y = 'Number of alcoholics', fill = 'Student Grade',title = 'Drank Alcohol')


### Percentage of alcoholics in each grade

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & alcoholic == "Yes")  %>%
  ggplot(aes(x = grade, fill = grade)) +
  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count',position = position_dodge(1),size = 3) + 
  labs(x = 'Grade', y = 'Percentage alcoholic',title = 'Drank Alcohol')
### Comment : The percentage of alcohol users increase gradually as the grade increases, 
###           suggesting that higher the grade the greater the percentage of alcohol users

### Number of alcoholics and Non alcoholics in each grade

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & alcoholic != "Missing") %>%
  ggplot(aes(x = grade, fill = alcoholic)) +
  geom_bar(position = "dodge") +
  geom_text(aes(y = (..count..)+400,label = paste0(..count..)),stat = 'count',size = 3, 
            position = position_dodge(1)) + 
  labs(x = 'Grade', y = 'Numbers of alcoholics', fill = 'alcoholic',title = 'Drank Alcohol')

### Percentage of alcoholics and Non alcoholics in each grade

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & alcoholic != "Missing")  %>%
  group_by(grade) %>%
  count(alcoholic) %>%
  mutate(percent=round(n/sum(n)*100, 2)) %>%
  ungroup(grade) %>%
  ggplot(aes(x = grade, y = percent, fill = alcoholic)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(y = percent + 1, 
                label = paste0(round(percent,2), '%')),position = position_dodge(1),size = 3) + 
  labs(x = 'Grade', y = 'Percentage alcoholics',title = 'Drank Alcohol')
### Comment : In the lower grade the percentage of alcohol users is almost the same as  
###           non users and this gap drastically increases with increase in grades.
###           This behavior is alarming and needs to be addressed at an earlier stage with 
###           more motivational contents such as videos and flyers

### Proportion of alcoholics and Non alcoholics across all grades

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & alcoholic != "Missing")  %>%
  ggplot(aes(x = grade, fill = alcoholic)) +
  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count',position = position_dodge(1),size = 3) + 
  labs(x = 'Grade', y = 'Percentage alcoholics',title = 'Drank Alcohol')



## Time Series plot for alcoholics from 2009 to 2017

### Percentages trend across the years grouped by alcoholic

yrbss %>%
  filter(alcoholic != "Missing" & YEAR != 2007)  %>%
  group_by(YEAR) %>%
  count(alcoholic) %>%
  mutate(percent=round(n/sum(n)*100, 2)) %>%
  ungroup(YEAR) %>%
  group_by(alcoholic) %>%
  ggplot(aes(x = YEAR, y = percent, group = alcoholic, color = alcoholic)) + 
  geom_point(shape = 22, fill="white", color="blue", size = 3) +
  geom_line(size = 1) + 
  geom_text(aes( label = paste0(percent,"%")),position = position_nudge(0.5), size = 4) +
  labs( y = 'Percentage',title = 'Drank Alcohol')
### Comment : Studying the Alcohol users pattern over eight years shows a decline
###           in percentageon  of Alcohol users from 74% in 2009 to 60% in 2017 and non users from
###           26% to 40%.  
###           But what needs to be seen is that there is still a significant high percentage
###           of users of Alcohol and we need effective measures to reduce the gap.

### Percentage of students 13 years and younger who are alcoholics

yrbss %>%
  subset(Q1 ==1:2) %>%
  filter(alcoholic != "Missing") %>% 
  ggplot(aes(x = alcoholic, fill = alcoholic)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  labs(x = 'alcoholic', title = 'Drank Alcohol- aged 13 years and less', y = 'Percentage') +
  scale_y_continuous(labels = scales::percent)
### Comment : When grouped for Alcohol users below the age of 13, 
###           there is an alarmingly higher percentage of users in this age group. 
###           Highlighting that this behavior needs to be nipped at the bud to see positive
###           change at a higher age group.

## Percent of students grouped by sex, who are 13 years and younger and alcoholics  

yrbss %>%
  subset(Q1 ==1:2) %>%
  filter(alcoholic != "Missing" & sex != "Missing") %>%
  ggplot(aes(x = alcoholic, fill = sex)) +
  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count',position = position_dodge(1),size = 3) + 
  labs(x = 'alcoholic', y = 'Percentage', fill = 'Sex',title = 'Drank Alcohol - aged 13 years and less')
### Comment : When Alcohol users and non users below the age of 13 are grouped by sex, 
###           there is a higher percentage of male alcohol users as compared to female users. 
###           This contrasts with the overall users based on sex.




##********************* EDA of Drugs usage in Youth  ***********************************


### converting factors with labels for Marijuana users

yrbss$marijuana[yrbss$Q26 == "1"] <- "No"
yrbss$marijuana[yrbss$Q26 >= "2"] <- "Yes"
yrbss$marijuana[yrbss$Q26 == "0"] <- "Missing"
yrbss$marijuana<- factor(yrbss$marijuana)

### converting factors with labels for cocaine users

yrbss$cocaine[yrbss$Q29 == "1"] <- "No"
yrbss$cocaine[yrbss$Q29 >= "2"] <- "Yes"
yrbss$cocaine[yrbss$Q29 == "0"] <- "Missing"
yrbss$cocaine<- factor(yrbss$cocaine)

### converting factors with labels for Glue users

yrbss$glue[yrbss$Q30 == "1"] <- "No"
yrbss$glue[yrbss$Q30 >= "2"] <- "Yes"
yrbss$glue[yrbss$Q30 == "0"] <- "Missing"
yrbss$glue<- factor(yrbss$glue)

### converting factors with labels for heroin users

yrbss$heroin[yrbss$Q31 == "1"] <- "No"
yrbss$heroin[yrbss$Q31 >= "2"] <- "Yes"
yrbss$heroin[yrbss$Q31 == "0"] <- "Missing"
yrbss$heroin<- factor(yrbss$heroin)

### converting factors with labels for Ecstasy users

yrbss$ecstasy[yrbss$Q33 == "1"] <- "No"
yrbss$ecstasy[yrbss$Q33 >= "2"] <- "Yes"
yrbss$ecstasy[yrbss$Q33 == "0"] <- "Missing"
yrbss$ecstasy<- factor(yrbss$ecstasy)

### converting factors with labels for prescription drugs users without doctor's permission

yrbss$predrug[yrbss$Q35 == "1"] <- "No"
yrbss$predrug[yrbss$Q35 >= "2"] <- "Yes"
yrbss$predrug[yrbss$Q35 == "0"] <- "Missing"
yrbss$predrug<- factor(yrbss$predrug)

### converting factors with labels for needle drug users 

yrbss$needle[yrbss$Q36 == "1"] <- "No"
yrbss$needle[yrbss$Q36 >= "2"] <- "Yes"
yrbss$needle[yrbss$Q36 == "0"] <- "Missing"
yrbss$needle<- factor(yrbss$needle)

### converting factors with labels for LSD users 

yrbss$lsd[yrbss$Q38 == "1"] <- "No"
yrbss$lsd[yrbss$Q38 >= "2"] <- "Yes"
yrbss$lsd[yrbss$Q38 == "0"] <- "Missing"
yrbss$lsd<- factor(yrbss$lsd)

### Creating combined drug users variable

yrbss$drugaddict[yrbss$marijuana == "No" & yrbss$cocaine == "No" & yrbss$glue == "No" & 
                   yrbss$heroin == "No" & yrbss$ecstasy == "No" & yrbss$predrug == "No" & 
                   yrbss$needle == "No" & yrbss$lsd == "No"] <- "No"
yrbss$drugaddict[yrbss$marijuana == "Yes" | yrbss$cocaine == "Yes" | yrbss$glue == "Yes" | 
                   yrbss$heroin == "Yes" | yrbss$ecstasy == "Yes" | yrbss$predrug == "Yes" | 
                   yrbss$needle == "Yes" | yrbss$lsd == "Yes"] <- "Yes"
yrbss$drugaddict[is.na(yrbss$drugaddict)] <- "Missing"
yrbss$drugaddict<- factor(yrbss$drugaddict)

### Creating a new variable for different drug users
yrbss$drugused[yrbss$marijuana == "Yes"] <- "Marijuana"
yrbss$drugused[yrbss$cocaine == "Yes"] <- "Cocaine "
yrbss$drugused[yrbss$glue == "Yes"] <- "Glue"
yrbss$drugused[yrbss$heroin == "Yes"] <- "Heroin"
yrbss$drugused[yrbss$ecstasy == "Yes"] <- "Ecstasy"
yrbss$drugused[yrbss$predrug == "Yes"] <- "Predrug"
yrbss$drugused[yrbss$needle == "Yes"] <- "Needle"
yrbss$drugused[yrbss$lsd == "Yes"] <- "LSD"
yrbss$drugused[is.na(yrbss$drugused)] <- "Missing"
yrbss$drugused <- factor(yrbss$drugused)

### Bar Graph of Percentage of Drug Addicts

yrbss %>%
  filter(drugaddict != "Missing") %>% 
  ggplot(aes(x = drugaddict, fill = drugaddict)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  labs(x = 'Drug Addicts', title = 'Used Drugs', y = 'Percentage') +
  scale_y_continuous(labels = scales::percent) 
### Comment : The percentage of Drug users are significantly higher as compared to non users 

### Percentage Drug Addicts grouped by Sex

yrbss %>%
  filter(drugaddict == "Yes" & sex != "Missing") %>% 
  group_by(sex) %>% 
  summarize(freq = n()) %>% 
  mutate(percentage = 100*freq/sum(freq))
##`      sex`        freq    percentage
##   1   Female      22412     48.7  
##   2   Male        23579     51.3  

### Bar Graph grouped by Sex for Drug Addicts

yrbss %>%
  filter(drugaddict == "Yes" & sex != "Missing") %>% 
  ggplot(aes(x = sex, fill = sex)) +
  geom_bar(aes(y =prop.table(..count..))) + 
  labs(x = 'Drug Addicts', title = 'Used Drugs', y = 'Percentage') +
  scale_y_continuous(labels = scales::percent) 
### Comment : The percentage of Drug users are almost the same in male and females

### Bar Chart to show Drug Addicts and Non Drug Addicts based on Sex

yrbss %>%
  filter(drugaddict != "Missing" & sex != "Missing") %>%
  ggplot(aes(x = drugaddict, fill = sex)) +
  geom_bar(position = "dodge") +
  geom_text(aes(y = (..count..)+400,label = paste0(..count..)),stat = 'count',size = 3, 
            position = position_dodge(1)) + 
  labs(x = 'Drug Addicts', y = 'Number of Students', fill = 'Sex',title = 'Used Drugs')


### Bar Chart to show Proportion of Drug Addicts and Non Drug Addicts based on Sex

yrbss %>%
  filter(drugaddict != "Missing" & sex != "Missing") %>%
  ggplot(aes(x = drugaddict, fill = sex)) +
  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count',position = position_dodge(1),size = 3) + 
  labs(x = 'Drug Addicts', y = 'Percentage', fill = 'Sex',title = 'Used Drugs')
### Comment : The percentage of Drug users and non users does not vary significantly by sex.

### Drug Addicts Filtered by Grades

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & drugaddict == "Yes") %>% 
  group_by(grade) %>% 
  summarize(freq = n()) %>% 
  mutate(percentage = 100*freq/sum(freq))
##   grade         freq     percentage
##
##   09th grade     9660       21.1
##   10th grade    10783       23.5
##   11th grade    12437       27.2
##   12th grade    12928       28.2

### Number of Drug Addicts in each grade

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & drugaddict == "Yes") %>%
  ggplot(aes(x = grade, fill = grade)) +
  geom_bar(position = "dodge") +
  geom_text(aes(y = (..count..)+300,label = paste0(..count..)),stat = 'count',size = 3, 
            position = position_dodge(1)) + 
  labs(x = 'Grade', y = 'Number of Drug Addicts', fill = 'Student Grade',title = 'Used Drugs')


### Percentage of Drug Addicts in each grade

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & drugaddict == "Yes")  %>%
  ggplot(aes(x = grade, fill = grade)) +
  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count',position = position_dodge(1),size = 3) + 
  labs(x = 'Grade', y = 'Percentage Drug addicts',title = 'Used Drugs')
### Comment : The percentage of Drug users increases gradually with grades.


### Number of Drug Addicts and Non Drug Addicts in each grade

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & drugaddict != "Missing") %>%
  ggplot(aes(x = grade, fill = drugaddict)) +
  geom_bar(position = "dodge") +
  geom_text(aes(y = (..count..)+400,label = paste0(..count..)),stat = 'count',size = 3, 
            position = position_dodge(1)) + 
  labs(x = 'Grade', y = 'Numbers of Drug Addicts', fill = 'drugaddict',title = 'Used Drugs')



### Percentage of Drug Addicts and Non Drug Addicts in each grade

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & drugaddict != "Missing")  %>%
  group_by(grade) %>%
  count(drugaddict) %>%
  mutate(percent=round(n/sum(n)*100, 2)) %>%
  ungroup(grade) %>%
  ggplot(aes(x = grade, y = percent, fill = drugaddict)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(y = percent + 1, 
                label = paste0(round(percent,2), '%')),position = position_dodge(1),size = 3) + 
  labs(x = 'Grade', y = 'Percentage Drug Addicts',title = 'Used Drugs')
### Comment : There is a significant increase in percentage of Drug users as
###           the grades increase and the the gap is way larger at the higher grade.


### Proportion of Drug Addicts and Non Drug Addicts across all grades

yrbss %>%
  filter(grade != "Ungraded/Other" & grade != "Missing" & drugaddict != "Missing")  %>%
  ggplot(aes(x = grade, fill = drugaddict)) +
  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.3, 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count',position = position_dodge(1),size = 3) + 
  labs(x = 'Grade', y = 'Percentage Drug Addicts',title = 'Used Drugs')
### Comment : In the lower grade the percentage of drug users is almost the same as  
###           non users and this gap drastically increases with increase in grades.
###           This behavior is alarming and needs to be addressed at an earlier stage with 
###           more motivational contents such as videos and flyers

## Time Series plot for Drug Addicts from 2007 to 2017

### Percentages trend across the years grouped by drugaddict

yrbss %>%
  filter(drugaddict != "Missing")  %>%
  group_by(YEAR) %>%
  count(drugaddict) %>%
  mutate(percent=round(n/sum(n)*100, 2)) %>%
  ungroup(YEAR) %>%
  group_by(drugaddict) %>%
  ggplot(aes(x = YEAR, y = percent, group = drugaddict, color = drugaddict)) + 
  geom_point(shape = 22, fill="white", color="blue", size = 3) +
  geom_line(size = 1) + 
  geom_text(aes( label = paste0(percent,"%")),position = position_nudge(0.5), size = 4) +
  labs( y = 'Percentage',title = 'Used Drugs')
### Comment : Studying the Alcohol users pattern over the years does not show any direction 
###           in percentage increase or decrease. 
###           But when compared between 2007 and 2017 there is a drop in number of users from
###           63%  to 55%. Whether this drop in numbers is significant needs to be investigated. 
###           To overcome the disparity in graph a different group method is adopted.


### Percentage of Different Drugs users

yrbss %>%
  filter(drugused!= "Missing" )%>%
  ggplot(aes(x = drugused, fill = drugused)) +
  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge")+ 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.6, 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count',position = position_dodge(1),size = 3.5) +
  labs(x = 'Drugs', y = 'Percentage users',title = 'Different Drug Users')
### Comment : This graph clearly shows that Marijuana is the most popular kind of drug 
###           amongst the youth followed by presription drugs not prescribed by the doctor.


## Table for Drug Addicts from 2007 to 2017 of different kinds of Drugs

reshape <- yrbss %>%
              filter(drugused != "Missing")  %>%
              group_by(YEAR) %>%
              count(drugused) %>%
              mutate(percent=round(n/sum(n)*100, 2)) %>%
              as.data.frame(reshape) %>%
              mutate(n = NULL) %>%
              spread(YEAR,percent) 
reshape 

##       drugused     2007     2009    2011   2013   2015   2017
##  1    Cocaine       3.03    2.91    2.38   1.33   2.02   2.16
##  2    Ecstasy       3.76    4.83    5.57   4.04   3.91   2.45
##  3    Glue         12.06   13.07    9.32   7.83   6.33   6.18
##  4    Heroin        0.45    0.38    0.54   0.30   0.76   0.46
##  5    LSD          11.49   14.05   11.31  12.14   9.53  10.10
##  6    Marijuana    37.59   39.17   38.57  44.68  44.65  48.17
##  7    Needle        1.98    1.14    1.71   0.85   2.07   2.28
##  8    Predrug      29.65   24.46   30.60  28.82  30.73  28.21

## Time Series plot for Drug Addicts from 2007 to 2017 of different kinds of Drugs

yrbss %>%
  filter(drugused != "Missing")  %>%
  group_by(YEAR) %>%
  count(drugused) %>%
  mutate(percent=round(n/sum(n)*100, 2)) %>%
  ungroup(YEAR) %>%
  group_by(drugused) %>%
  ggplot(aes(x = YEAR, y = percent, group = drugused, color = drugused)) + 
  geom_point(shape = 23, fill="grey", color="grey", size = 2) +
  geom_line(size = 1) + 
  labs( y = 'Percentage',title = 'Used Drugs')
### Comment : Studying the Different drug users pattern over the years we can conclude the following 
###           - The Percentage of Marijuana users have increased over the years 
###             from 38% in 2007 to 48% in 2017. 
###           - There is no change in the users of Heroin
###           - The percentage of Glue users has decreased from 12% in 2007 to 6% in 2017
###           - No significant difference seen in other kinds of drugs over the years.

### Percentage of students 13 years and younger who are Drug Addicts

yrbss %>%
  subset(Q1 ==1:2) %>%
  filter(drugaddict != "Missing") %>% 
  ggplot(aes(x = drugaddict, fill = drugaddict)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  labs(x = 'Drugs Addict', title = 'Used Drugs - aged 13 years and less', y = 'Percentage') +
  scale_y_continuous(labels = scales::percent)
### Comment : When grouped for Drug users of all kinds below the age of 13, 
###           there is an alarmingly higher percentage of users in this age group. 
###           Highlighting that this behavior needs to be nipped at the bud to see positive
###           change at a higher age group and a greater awarness campaign should be implemented.

## Percent of students grouped by sex, who are 13 years and younger and Drug Addicts  

yrbss %>%
  subset(Q1 ==1:2) %>%
  filter(drugaddict != "Missing" & sex != "Missing") %>%
  ggplot(aes(x = drugaddict, fill = sex)) +
  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.7, 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count',position = position_dodge(1),size = 3) + 
  labs(x = 'Drugs Addict', y = 'Percentage', fill = 'Sex',title = 'Used Drugs - aged 13 years and less')
### Comment : When Drug users below the age of 13 are grouped by sex, 
###           there is a higher percentage of male Drug addicts as compared to female. 
###           This contrasts with the overall users based on sex.

## Popular Drug type amongst 13 years and below
yrbss %>%
  subset(Q1 ==1:2) %>%
  filter(drugused != "Missing") %>% 
  ggplot(aes(x = drugused, fill = drugused)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  labs(x = 'Drugs Addict', title = 'Used Drugs - aged 13 years and less', y = 'Percentage') +
  scale_y_continuous(labels = scales::percent)

### Comment : Contrary to the overall Popular drug type (Marijuana) this age group 
###           finds popularity in LSD [Lysergic acid diethylamide] a hallucinogenic drug
###           with a high percentage of users of 46%



