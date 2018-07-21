#Exercise 1 - Data Wrangling

# 0: Load the data in RStudio

install.packages("tidyr")
install.packages("dplyr")
library(dplyr)
library(tidyr)

refine_original <- read.csv("~/Data Science course/Data Wrangling/Data Wrangling Projects/refine_original.csv")
View(refine_original)
write.csv(refine_original,'refine_original.csv')

## 1: Clean up brand names

idx <-grep(pattern="^ak",x=refine_original$company,ignore.case = TRUE,value = FALSE )
refine_original[idx,1]<- "akzo"

idx<-grep(pattern="ps$",x=refine_original$company,ignore.case = TRUE,value = FALSE )
refine_original[idx,1]<- "philips"

idx<-grep(pattern="en$",x=refine_original$company,ignore.case = TRUE,value = FALSE )
refine_original[idx,1]<- "van houten"

idx <-grep(pattern="er$",x=refine_original$company,ignore.case = TRUE,value = FALSE )
refine_original[idx,1]<- "unilever"


## 2: Separate product code and number

refine_clean<-separate(refine_original,Product.code...number,c("product_code",
                                                               "product_number"),sep ="-")

## 3: Add product categories

refine_clean <- refine_clean %>% 
  mutate(Product_category = case_when(product_code == "p" ~ " 
                                      Smartphone", product_code == "v" ~ "TV",product_code == "x" 
                                      ~ "Laptop",product_code == "q" ~ "Tablet"))

### To rearrange the columns so that Product category lies between code and number

refine_clean <- select(refine_clean,1:2,8,3:7)

## 4: Add full address for geocoding

refine_clean <-unite(refine_clean,"full_address",address, city, country,sep=",")

## 5: Create dummy variables for company and product category

### adding the binary values to company and product

refine_clean <- mutate(refine_clean,company_binary =1,product_binary=1) 


### Renaming  the column header of products to match with the required output

colnames(refine_clean) <- sub("Product_category", "product", colnames(refine_clean))


### creating the 4 binary company columns based on values in key column- company

refine_clean <- spread(refine_clean,company,company_binary,fill=0,sep="_")


### creating the 4 binary product columns based on values in key column - product

refine_clean <- spread(refine_clean,product,product_binary,fill=0,sep="_")

View(refine_clean)

## To view along with the company name 

refine_clean<-View(cbind(refine_original[,1,drop=FALSE],refine_clean))


## To save the cleaned dataframe as a .csv file

write.csv(refine_clean,'refine_clean.csv')