#House preparation ####

# Clear R workspace
rm(list = ls() )

getwd()

#system("ls ../input")
#setwd('../input')

library(readr) # CSV file I/O, e.g. the read_csv function
library(grid)
library(dplyr)
library(magrittr)

library(ggplot2) # Data visualization
library(data.table)
library(caret)
library(fields)
library(plot3D)

install.packages("C:/Users/tpicelli/Downloads/colorspace_1.3-2.zip")



#Heading Name####
sales <- read.csv("./data/SalesKaggle3.csv")

# preparando dados
sales$SoldFlag <- as.factor(sales$SoldFlag)
sales$New_Release_Flag <- as.factor(sales$New_Release_Flag)
sales$ReleaseYear <- as.factor(sales$ReleaseYear)

str(sales)
summary(sales)

# split historical and active data
hist_sales <- sales %>% filter(File_Type == 'Historical')
active_sales <- sales %>% filter(File_Type == 'Active')


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