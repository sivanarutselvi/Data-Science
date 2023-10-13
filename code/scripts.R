
# Adding basic libraries 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)

# import data into R

smdata <- read_csv('data/supermarket_sales.csv',show_col_types = FALSE)

# Getting to know the data

dim(smdata) # 1000 * 17
head(smdata)
str(smdata)
names(smdata)
unique(smdata$branch)
unique(smdata$customer_type)
unique(smdata$product_line)
unique(smdata$cogs)
#revenue = unit cost * quantity + 5pct_markup

# rename some columns
colnames(smdata)[5]="customer_gender"
colnames(smdata)[9]="pct5_markup"

# Checking for missing data
sum(is.na(smdata)) # 0

# Descriptive Statistics
summary(smdata)

# Univariate Data Exploration

# 1. Histograms of unit cost, revenue and rating

num_cols = data.frame(subset(smdata, select=c(7,10,14,16,17)))
h_df = data.frame(subset(num_cols, select=c(1,2,5)))
par(mfrow = c(1, 3))
invisible(lapply(1:ncol(num_cols), function(i) hist(num_cols[, i],main=names(num_cols)[i],fill="#fde0dd")))

# 2. Boxplot of unit cost, revenue, cogs, gross income and rating


par(mfrow = c(1, ncol(num_cols)))
invisible(lapply(1:ncol(num_cols), function(i) boxplot(num_cols[, i],main=names(num_cols)[i],col="#fde0dd")))

# Categorical variables are branch, city, type of customer, 
# customer gender and kind of product they buy and method of payment

table(smdata$branch)
table(smdata$city)
table(smdata$customer_type)
table(smdata$customer_gender)
table(smdata$product_line)
table(smdata$payment_method)

# Bar plot of product lines

plcg = ggplot(smdata, aes(x=fct_rev(fct_infreq(product_line)))) + 
  geom_bar(color="black",fill="#fde0dd")+ 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  ggtitle("Counts of product line catagories")

plcg

#  Bivariate Analysis

# 1)	City Vs Product line

pink = c("#fde0dd","#fa9fb5","#c51b8a")

cpl <- ggplot(smdata) + 
  geom_bar(aes(city,fill = city)) +scale_fill_manual(values = c(pink))+
  xlab(NULL) + 
  ylab(NULL)+ facet_wrap(~product_line)+theme_bw()

cpl

# 2) customer gender Vs Product line

blue = c("#deebf7","#9ecae1","#3182bd")
cgpl =  ggplot(smdata) + 
  geom_bar(mapping=aes(city,fill = customer_gender),position="dodge") +
  scale_fill_manual(values = c("#fa9fb5","#9ecae1"))+xlab(NULL) + 
  ylab(NULL)+ facet_wrap(~product_line)+theme_bw()

cgpl

# Customer Gender and Product line

gp = ggplot(smdata) + geom_bar(mapping=aes(payment_method,fill = customer_gender),position="dodge") +
  scale_fill_manual(values = c("#fa9fb5","#9ecae1"))+xlab(NULL) + 
  ylab(NULL)+ facet_wrap(~product_line)+theme_bw()

gp




