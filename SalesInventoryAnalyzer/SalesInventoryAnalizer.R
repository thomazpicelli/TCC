#House preparation ####

# Clear R workspace
rm(list = ls() )

getwd()

#system("ls ../input")
#setwd('../input')

library(class)
library(readr) # CSV file I/O, e.g. the read_csv function
library(grid)
library(dplyr)
library(magrittr)

library(ggplot2) # Data visualization
library(data.table)
library(caret)
library(fields)
library(plot3D)



#Heading Name####
sales <- read.csv("./data/SalesKaggle3.csv")

# preparando dados
sales$SoldFlag <- as.factor(sales$SoldFlag)
sales$New_Release_Flag <- as.factor(sales$New_Release_Flag)
sales$ReleaseYear <- as.factor(sales$ReleaseYear)

str(sales)
summary(sales)

# split historical and active data
hist_sales <- head(sales[sales$File_Type == 'Historical',])
active_sales <- sales %>% filter(File_Type == 'Active')
head(hist_sales)


# linear regression ####

hist_sales.lm <- lm(hist_sales$ItemCount ~ hist_sales$SoldFlag + hist_sales$SoldCount + hist_sales$ReleaseNumber + hist_sales$LowUserPrice + hist_sales$LowNetPrice, data = hist_sales)
hist_sales.lm

summary(hist_sales.lm)

hist_sales%>%ggplot(aes(x=hist_sales$ItemCount, y=hist_sales$SoldCount))+
geom_point(color="blue",size=4,alpha=0.5)+
ylab('Profit in $10,000s')+          
xlab('Population of City in 10,000s')+ggtitle ('Figure 1: Scatter plot of training data')+
theme(plot.title = element_text(size = 16,colour="red"))


coefficients(hist_sales.lm) # model coefficients
confint(hist_sales.lm, level=0.95) # CIs for model parameters 
fitted(hist_sales.lm) # predicted values
residuals(hist_sales.lm) # residuals
anova(hist_sales.lm) # anova table 
vcov(hist_sales.lm) # covariance matrix for model parameters 
influence(hist_sales.lm) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(hist_sales.lm)

# knn ####

test <- cbind(hist_sales$SoldFlag,hist_sales$MarketingType,hist_sales$New_Release_Flag,hist_sales$ItemCount)
colnames(test) <- c("SoldFlag", "MarketingType", "New_Release_Flag","ItemCount")
head(test)
knn <- knn(hist_sales,test,hist_sales$SoldCount,10)

#Explore historical sales data based on the marketing type, grouped by SoldFlag

# Saving an R workspace file
save.image(file="CO2 Project Data.RData")
# Clear your memory
rm(list = ls())
# Reload your data
Load("CO2 Project Data.RData")
head(CO2) 
# looking good! 


# export
write.csv(CO2,file="./Data/CO2_new.csv")