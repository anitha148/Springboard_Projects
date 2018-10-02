install.packages("tidyr")
install.packages("dplyr")
library(dplyr)
library(tidyr)
yrbss_original <- read.csv("~/Data Science course/CAPSTONE PROJECT/CP YRBSS/wrangling/CP_YRBSS_original.csv")

yrbss <- yrbss_original
#yrbss[is.na(yrbss)] = 0
#View(yrbss)
yrbss <- yrbss %>% replace(is.na(.), 0)
write.csv(yrbss,"~/Data Science course/Data Wrangling/Data Wrangling Projects/Springboard_Projects/Capstone Projects/Project_YRBSS/YRBSS.csv")