rm(list=ls())


#install.packages("summarytools")
#install.packages("Hmisc")
#install.packages("tidyverse")
#install.packages("skimr")

library(tidyverse)
library(dplyr)
library(summarytools)
library(skimr)
library(Hmisc)





#question to ask--# 159 SSU
#n=42 n_distinct(PSU_df$gbtownship_2021)
#n=40  n_distinct(location_df$gbtownship)
#18 Values in PSU_df$gbtownship_2021 but not in location_df$gbtownship


PSU_df<-read.csv("PSU_SSU_20200623_rev_20220501.csv",header=T)

location_df<-read.csv("location_share_20190124.csv")


#summaryTools::dfSummary(location_df$gbtownship)
skimr::skim(location_df$gbtownship)
skim(PSU_df$gbtownship_2021)

Hmisc::describe(PSU_df$gbtownship_2021)

table1(~as.factor(gbtownship),location_df)

length(unique(PSU_df$SSU))



# Glimpse of the data
glimpse(location_df)
glimpse(PSU_df)

names(PSU_df)
names(location_df)


# Summarize data
#data %>% summarise(mean_column1 = mean(column1, na.rm = TRUE))


# Count occurrences
location_df %>% 
  count(match_strata) %>% 
  summarise(total_count=sum(n))


gbcode<-PSU_df %>% 
  filter(gbcode_2021 %in% location_df$gbcode_2021)

#group by PSU and SSU
PSU_df


location_df$pilott_cat

# 20 unique strata match_strata
count(location_df$match_strata)
unique(location_df$match_strata)
count(unique(location_df$match_strata))

xtabs(~match_strata+pilott_cat, data=location_df)

# Count unique occurrences
match_2000<-location_df %>% 
  filter(match_strata>2000)

  
names(PSU_df)

## n=40 gbcode_2021  40 unique PSU
PSU_df<-PSU_df[-(160:166),]

count(unique(PSU_df$gbcode_2021))

location_df$gb

# N=40
n_distinct(PSU_df$gbcode_2021)

# N=42 unique gbtownship_2021

length(unique(PSU_df$gbtownship_2021))


count_fun<-function(df,column){
  count_column<-df %>% 
    count(column) #%>% 
  #summarise(total_count=sum(n))
  return(count_column)
}

names(location_df)
count_fun(location_df,"gbtownship")

#R interprets column as a literal column name rather than the variable you passed in ("gbtownship_2021"). 
#To fix this, use the double-bracket notation ([[ ]]) to dynamically reference the column by its name.

fre_less <- function(df, column,n) {
  # Create a frequency table as a data frame
  fre_table <- as.data.frame(table(df[[column]]))
  #print(fre_table)
  
  # Filter values with frequency less than 4
  values_with_n_freq <- fre_table %>% 
    filter(Freq!=n)
  #print(values_with_n_freq)
  
  # Extract the column of values with frequency less than 4
  values_less_freq <- values_with_n_freq[[1]]
  #print(values_less_freq)
  
  # Filter the rows in df where the specified column has those values
  filtered_df <- df %>%
    filter(.[[column]] %in% values_less_freq)
  
  return(filtered_df)
}


# 江西 吉安市 万安县 沙坪镇 360828107 only has 3 SSU
frq_less4_gbtownship_PSU<-fre_less4(PSU_df,"gbtownship_2021",4)

fre_less4_gbcode<-fre_less4(PSU_df,"gbcode_2021",4)


#n=40 
n_distinct(PSU_df$gbcode_2021)

# 159 SSU
length(unique(PSU_df$SSU))


#n=42
n_distinct(PSU_df$gbtownship_2021)
# n=40 
n_distinct(location_df$gbtownship)


#why PSU_df$gbtownship_2021!=location_df$gbtownship

# Find unique values in each column
unique_gbtownship_2021 <- unique(PSU_df$gbtownship_2021)
unique_location_gbtownship <- unique(location_df$gbtownship)

# n=18 Values in PSU_df$gbtownship_2021 but not in location_df$gbtownship
diff1 <- setdiff(unique_gbtownship_2021, unique_location_gbtownship)
print("Values in PSU_df$gbtownship_2021 but not in location_df$gbtownship:")

diff_gbcode_PSU<-PSU_df %>% 
  filter(gbtownship_2021 %in% diff1)

n_distinct(diff_gbcode_PSU$gbtownship_2021)


#n=16 Values in location_df$gbtownship but not in PSU_df$gbtownship_2021:
diff2 <- setdiff(unique_location_gbtownship,unique_gbtownship_2021)
print("Values in location_df$gbtownship but not in PSU_df$gbtownship_2021:")

diff_gbcode_location<-location_df %>% 
  filter(gbtownship %in% diff2)

n_distinct(diff_gbcode_location$gbtownship)

#inner_join(): Returns the matching rows plus columns from the right data frame.
#semi_join(): Returns only the rows from the left data frame that have a match in the right data frame, without adding any columns from the right.

#n=24 find the common values in column gbtownship_2021 and gbtownship
common_semi<-PSU_df %>% 
  semi_join(location_df, by = c("gbtownship_2021" = "gbtownship"))
n_distinct(common_semi$gbtownship_2021)

#Returns all rows from the left data frame and the matched rows from the right, filling in NA where there are no matches.
common_left<-PSU_df %>% 
  left_join(location_df, by = c("gbtownship_2021" = "gbtownship"))

n_distinct(common_left$gbtownship_2021)


is.na(location_df[["match_strata"]])

#select rows where match_strata == NA in common_left

na_gbtown<-common_left %>% 
  filter(is.na(match_strata))

#n=18
n_distinct(na_gbtown$gbtownship_2021)

#This will return rows from df1 where the values in df1$column1 do not have a match in df2$column2.
anti<-PSU_df %>%
  anti_join(location_df, by = c("gbtownship_2021" = "gbtownship"))

n_distinct(anti1$gbtownship_2021)

##n=16 Values in location_df$gbtownship but not in PSU_df$gbtownship_2021:
anti2<-location_df %>%
  anti_join(PSU_df, by = c( "gbtownship" = "gbtownship_2021"))

n_distinct(anti2$gbtownship)


diff_gbcode_location$gbtownship

# Values in location_df$gbtownship but not in PSU_df$gbtownship_2021
diff2 <- setdiff(unique_location_gbtownship, unique_gbtownship_2021)
print("Values in location_df$gbtownship but not in PSU_df$gbtownship_2021:")
print(diff2)


table(location_df$gbtownship)

# n=20
n_distinct(location_df$match_strata)


PSU_df[is.na(PSU_df$统计用区划代码),]

#method1:prevent scientific notation
options(scipen = 999)  # This will prevent scientific notation in output
PSU_df$统计用区划代码[1]  # Now this should print the number in full

#method2
format(PSU_df$统计用区划代码[1], scientific = FALSE)  # This will format the number as a string

#method3
as.character(PSU_df$统计用区划代码[1])

nchar(PSU_df$统计用区划代码[1])

#统计用区划代码和城乡划分代码分为两段17位，其代码结构为：
#1  2  3  4  5  6  7  8  9  10 11 12   统计用区划代码 
#13 14 15 16 17  城乡划分代码 

#第1～2位，为省级代码；

#第3～4 位，为地级代码；

#第5～6位，为县级代码；

#第7～9位，为乡级代码；

#第10～12位，为村级代码。


#初级抽样单位（PSU）：乡、镇、街道，采用PPS方法共抽取40个乡镇街道。
#2、次级抽样单位（SSU）：30″*30″经纬度组成的单元格，简称半分格。每个乡镇街道内抽取4个半分格，合计160个半分格。

##country level--6digits 第5～6位，为县级代码； 
PSU_df$gbcode_2021[1]

## township level--9 digits 第7～9位，为乡级代码；
PSU_df$gbtownship_2021[1]

substring_fun<-function(x,start,stop){
  substr(x,start,stop) }

substr(PSU_df$gbcode_2021[1],1,6)

# 12 digits of the first element
PSU_df$统计用区划代码[1]

#dispaly the last 3 digit village code #第10～12位，为村级代码。
start<-10
stop<-nchar(PSU_df$统计用区划代码[1])            
PSU_df$region_code <- as.character(format(PSU_df$统计用区划代码, scientific = FALSE))  # Convert to full character format
PSU_df$region_code <- substr(PSU_df$region_code, start, stop)             # Extract characters after the first 12
PSU_df$region_code[1]  # This will show the first 12 digits

nchar(PSU_df$region_code[1]) # This will show the number of characters in the first element of the column

PSU_df$region_code <-sapply(PSU_df$region_code,function (x) {
  if (!is.na(x)) {
    substr(x, 13, nchar(x))}
          else {
            NA
            }})

PSU_df$region_code[1]

PSU_df$region_code <- sapply(PSU_df$region_code, function(x) substr(x, 13, nchar(x)))




library(dplyr)

# Example data frame
df <- data.frame(
  column1 = c(0, 0, 0, 0),
  column2 = c("A", "B", "C", "D"),
  column3 = c("A", "X", "C", "Y")
)

# Update column1 when column2 equals column3
df <- df %>%
  mutate(column1 = if_else(column2 == column3, 1, column1))

print(df)

