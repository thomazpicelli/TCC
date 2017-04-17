# SALES INVENTORY ANALIZER - TPICELLI 2017

# Housekeeping ####

# Clear R workspace
rm(list = ls() )

# Load Workspace
load("C:/TCC/TCC/workspace.RData")

# Load All Packages
library(class)
library(curl)
library(readr)
library(grid)
library(gridExtra)
library(ggplot2)
library(data.table)
library(caret)
library(fields)
library(plot3D)
library(magrittr)  # to use piping %>%
library(MASS)      # to calculate the peseudo-inverse of a matrix
library(reshape2)  # for data manipulation
library(Cairo)
library(cowplot)


# Load All Functions
source("./R/functions.R")

# First dataset load ####
d1_sales <- read.csv("./data/SalesKaggle3.csv")

# preparando dados
d1_sales$SoldFlag <- as.factor(d1_sales$SoldFlag)
d1_sales$New_Release_Flag <- as.factor(d1_sales$New_Release_Flag)
d1_sales$ReleaseYear <- as.factor(d1_sales$ReleaseYear)

# split historical and active data
d2_hist_sales <- d1_sales[d1_sales$File_Type == 'Historical',]
d3_actv_sales <- d1_sales[d1_sales$File_Type == 'Active',]

# historico com venda + divisao mercado
d4_hist_sales_mkt_d <- d2_hist_sales[d2_hist_sales$MarketingType == "D",]
d5_hist_sales_mkt_s <- d2_hist_sales[d2_hist_sales$MarketingType == "S",]
d6_actv_sales_mkt_d <- d3_actv_sales[d3_actv_sales$MarketingType == "D",]
d7_actv_sales_mkt_s <- d3_actv_sales[d3_actv_sales$MarketingType == "S",]

# historico com venda + divisao mercado + 60 dias
d8_hist_sales_mkt_d_sold <- d4_hist_sales_mkt_d[d4_hist_sales_mkt_d$SoldFlag == 1,]
d9_hist_sales_mkt_d_not_sold <- d4_hist_sales_mkt_d[d4_hist_sales_mkt_d$SoldFlag == 0,]
d10_hist_sales_mkt_s_sold <- d5_hist_sales_mkt_s[d5_hist_sales_mkt_s$SoldFlag == 1,]
d11_hist_sales_mkt_s_not_sold <- d5_hist_sales_mkt_s[d5_hist_sales_mkt_s$SoldFlag == 0,]
d12_actv_sales_mkt_d_sold <- d6_actv_sales_mkt_d[d6_actv_sales_mkt_d$SoldFlag == 1,]
d13_actv_sales_mkt_d_not_sold <- d6_actv_sales_mkt_d[d6_actv_sales_mkt_d$SoldFlag == 0,]
d14_actv_sales_mkt_s_sold <- d7_actv_sales_mkt_s[d7_actv_sales_mkt_s$SoldFlag == 1,]
d15_actv_sales_mkt_s_not_sold <- d7_actv_sales_mkt_s[d7_actv_sales_mkt_s$SoldFlag == 0,]

# historico com venda + divisao mercado + 60 dias + releaseFlag
d16_hist_sales_mkt_d_sold_release <- d8_hist_sales_mkt_d_sold[d8_hist_sales_mkt_d_sold$New_Release_Flag == 1,]
d17_hist_sales_mkt_d_sold_not_release <- d8_hist_sales_mkt_d_sold[d8_hist_sales_mkt_d_sold$New_Release_Flag == 0,]
d18_hist_sales_mkt_d_not_sold_release <- d9_hist_sales_mkt_d_not_sold[d9_hist_sales_mkt_d_not_sold$New_Release_Flag == 1,]
d19_hist_sales_mkt_d_not_sold_not_release <- d9_hist_sales_mkt_d_not_sold[d9_hist_sales_mkt_d_not_sold$New_Release_Flag == 0,]

D20_hist_sales_mkt_s_sold_release <- d10_hist_sales_mkt_s_sold[d10_hist_sales_mkt_s_sold$New_Release_Flag == 1,]
D21_hist_sales_mkt_s_sold_not_release <- d10_hist_sales_mkt_s_sold[d10_hist_sales_mkt_s_sold$New_Release_Flag == 0,]
d22_hist_sales_mkt_s_not_sold_release <- d11_hist_sales_mkt_s_not_sold[d11_hist_sales_mkt_s_not_sold$New_Release_Flag == 1,]
d23_hist_sales_mkt_s_not_sold_not_release <- d11_hist_sales_mkt_s_not_sold[d11_hist_sales_mkt_s_not_sold$New_Release_Flag == 0,]

d24_actv_sales_mkt_d_sold_release <- d12_actv_sales_mkt_d_sold[d12_actv_sales_mkt_d_sold$New_Release_Flag == 1,]
d25_actv_sales_mkt_d_sold_not_release <- d12_actv_sales_mkt_d_sold[d12_actv_sales_mkt_d_sold$New_Release_Flag == 0,]
d26_actv_sales_mkt_d_not_sold_release <- d13_actv_sales_mkt_d_not_sold[d13_actv_sales_mkt_d_not_sold$New_Release_Flag == 1,]
d27_actv_sales_mkt_d_not_sold_not_release <- d13_actv_sales_mkt_d_not_sold[d13_actv_sales_mkt_d_not_sold$New_Release_Flag == 0,]

d28_actv_sales_mkt_s_sold_release <- d14_actv_sales_mkt_s_sold[d14_actv_sales_mkt_s_sold$New_Release_Flag == 1,]
d29_actv_sales_mkt_s_sold_not_release <- d14_actv_sales_mkt_s_sold[d14_actv_sales_mkt_s_sold$New_Release_Flag == 0,]
d30_actv_sales_mkt_s_not_sold_release <- d15_actv_sales_mkt_s_not_sold[d15_actv_sales_mkt_s_not_sold$New_Release_Flag == 1,]
d31_actv_sales_mkt_s_not_sold_not_release <- d15_actv_sales_mkt_s_not_sold[d15_actv_sales_mkt_s_not_sold$New_Release_Flag == 0,]

---------------------------------------
  
d16_hist_sales_mkt_d_sold_release <- d8_hist_sales_mkt_d_sold[d8_hist_sales_mkt_d_sold$New_Release_Flag == 1,]
d17_hist_sales_mkt_d_sold_not_release <- d8_hist_sales_mkt_d_sold[d8_hist_sales_mkt_d_sold$New_Release_Flag == 0,]
d18_hist_sales_mkt_d_not_sold_release <- d9_hist_sales_mkt_d_not_sold[d9_hist_sales_mkt_d_not_sold$New_Release_Flag == 1,]
d19_hist_sales_mkt_d_not_sold_not_release <- d9_hist_sales_mkt_d_not_sold[d9_hist_sales_mkt_d_not_sold$New_Release_Flag == 0,]

D20_hist_sales_mkt_s_sold_release <- d10_hist_sales_mkt_s_sold[d10_hist_sales_mkt_s_sold$New_Release_Flag == 1,]
D21_hist_sales_mkt_s_sold_not_release <- d10_hist_sales_mkt_s_sold[d10_hist_sales_mkt_s_sold$New_Release_Flag == 0,]
d22_hist_sales_mkt_s_not_sold_release <- d11_hist_sales_mkt_s_not_sold[d11_hist_sales_mkt_s_not_sold$New_Release_Flag == 1,]
d23_hist_sales_mkt_s_not_sold_not_release <- d11_hist_sales_mkt_s_not_sold[d11_hist_sales_mkt_s_not_sold$New_Release_Flag == 0,]

d24_actv_sales_mkt_d_sold_release <- d12_actv_sales_mkt_d_sold[d12_actv_sales_mkt_d_sold$New_Release_Flag == 1,]
d25_actv_sales_mkt_d_sold_not_release <- d12_actv_sales_mkt_d_sold[d12_actv_sales_mkt_d_sold$New_Release_Flag == 0,]
d26_actv_sales_mkt_d_not_sold_release <- d13_actv_sales_mkt_d_not_sold[d13_actv_sales_mkt_d_not_sold$New_Release_Flag == 1,]
d27_actv_sales_mkt_d_not_sold_not_release <- d13_actv_sales_mkt_d_not_sold[d13_actv_sales_mkt_d_not_sold$New_Release_Flag == 0,]

d28_actv_sales_mkt_s_sold_release <- d14_actv_sales_mkt_s_sold[d14_actv_sales_mkt_s_sold$New_Release_Flag == 1,]
d29_actv_sales_mkt_s_sold_not_release <- d14_actv_sales_mkt_s_sold[d14_actv_sales_mkt_s_sold$New_Release_Flag == 0,]
d30_actv_sales_mkt_s_not_sold_release <- d15_actv_sales_mkt_s_not_sold[d15_actv_sales_mkt_s_not_sold$New_Release_Flag == 1,]
d31_actv_sales_mkt_s_not_sold_not_release <- d15_actv_sales_mkt_s_not_sold[d15_actv_sales_mkt_s_not_sold$New_Release_Flag == 0,]



hist_sales_mkt_d_sold_release <- d1_sales[d1_sales$File_Type == 'Historical' & d1_sales$MarketingType == "D" & d1_sales$SoldFlag == 1 & d1_sales$New_Release_Flag == 1,]
hist_sales_mkt_d_sold_not_release <- d1_sales[d1_sales$File_Type == 'Historical' & d1_sales$MarketingType == "D" & d1_sales$SoldFlag == 1 & d1_sales$New_Release_Flag == 0,]
hist_sales_mkt_d_not_sold_release <- d1_sales[d1_sales$File_Type == 'Historical' & d1_sales$MarketingType == "D" & d1_sales$SoldFlag == 0 & d1_sales$New_Release_Flag == 1,]
hist_sales_mkt_d_not_sold_not_release <- d1_sales[d1_sales$File_Type == 'Historical' & d1_sales$MarketingType == "D" & d1_sales$SoldFlag == 0 & d1_sales$New_Release_Flag == 0,]

hist_sales_mkt_s_sold_release <- d1_sales[d1_sales$File_Type == 'Historical' & d1_sales$MarketingType == "S" & d1_sales$SoldFlag == 1 & d1_sales$New_Release_Flag == 1,]
hist_sales_mkt_s_sold_not_release <- d1_sales[d1_sales$File_Type == 'Historical' & d1_sales$MarketingType == "S" & d1_sales$SoldFlag == 1 & d1_sales$New_Release_Flag == 0,]
hist_sales_mkt_s_not_sold_release <- d1_sales[d1_sales$File_Type == 'Historical' & d1_sales$MarketingType == "S" & d1_sales$SoldFlag == 0 & d1_sales$New_Release_Flag == 1,]
hist_sales_mkt_s_not_sold_not_release <- d1_sales[d1_sales$File_Type == 'Historical' & d1_sales$MarketingType == "S" & d1_sales$SoldFlag == 0 & d1_sales$New_Release_Flag == 0,]

actv_sales_mkt_d_release <- d1_sales[d1_sales$File_Type == 'Active' & d1_sales$MarketingType == "D" & d1_sales$New_Release_Flag == 1,]
actv_sales_mkt_d_not_release <- d1_sales[d1_sales$File_Type == 'Active' & d1_sales$MarketingType == "D" & d1_sales$New_Release_Flag == 0,]
actv_sales_mkt_s_release <- d1_sales[d1_sales$File_Type == 'Active' & d1_sales$MarketingType == "S" & d1_sales$New_Release_Flag == 1,]
actv_sales_mkt_s_not_release <- d1_sales[d1_sales$File_Type == 'Active' & d1_sales$MarketingType == "S" & d1_sales$New_Release_Flag == 0,]


# First analises ####
hist(hist_sales_mkt_d_sold_release$PriceReg, breaks=Intervalo(nrow(hist_sales_mkt_d_sold_release),min(hist_sales_mkt_d_sold_release$PriceReg, na.rm=TRUE),max(hist_sales_mkt_d_sold_release$PriceReg, na.rm=TRUE)))

# comparacao do preco de historico do D (vendas e release)
P_1 <- ggplot(data=hist_sales_mkt_d_sold_release, aes(hist_sales_mkt_d_sold_release$PriceReg)) + geom_histogram(aes(y =..density..),breaks=seq(min(hist_sales_mkt_d_sold_release$PriceReg, na.rm=TRUE), max(hist_sales_mkt_d_sold_release$PriceReg, na.rm=TRUE), by = Intervalo(nrow(hist_sales_mkt_d_sold_release),min(hist_sales_mkt_d_sold_release$PriceReg, na.rm=TRUE),max(hist_sales_mkt_d_sold_release$PriceReg, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="hist_sales_mkt_d_sold_release") + labs(x="", y="Price")
P_2 <- ggplot(data=hist_sales_mkt_d_sold_not_release, aes(hist_sales_mkt_d_sold_not_release$PriceReg)) + geom_histogram(aes(y =..density..),breaks=seq(min(hist_sales_mkt_d_sold_not_release$PriceReg, na.rm=TRUE), max(hist_sales_mkt_d_sold_not_release$PriceReg, na.rm=TRUE), by = Intervalo(nrow(hist_sales_mkt_d_sold_not_release),min(hist_sales_mkt_d_sold_not_release$PriceReg, na.rm=TRUE),max(hist_sales_mkt_d_sold_not_release$PriceReg, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="hist_sales_mkt_d_sold_not_release") + labs(x="", y="Price")
P_3 <- ggplot(data=hist_sales_mkt_d_not_sold_release, aes(hist_sales_mkt_d_not_sold_release$PriceReg)) + geom_histogram(aes(y =..density..),breaks=seq(min(hist_sales_mkt_d_not_sold_release$PriceReg, na.rm=TRUE), max(hist_sales_mkt_d_not_sold_release$PriceReg, na.rm=TRUE), by = Intervalo(nrow(hist_sales_mkt_d_not_sold_release),min(hist_sales_mkt_d_not_sold_release$PriceReg, na.rm=TRUE),max(hist_sales_mkt_d_not_sold_release$PriceReg, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="hist_sales_mkt_d_not_sold_release") + labs(x="", y="Price")
P_4 <- ggplot(data=hist_sales_mkt_d_not_sold_not_release, aes(hist_sales_mkt_d_not_sold_not_release$PriceReg)) + geom_histogram(aes(y =..density..),breaks=seq(min(hist_sales_mkt_d_not_sold_not_release$PriceReg, na.rm=TRUE), max(hist_sales_mkt_d_not_sold_not_release$PriceReg, na.rm=TRUE), by = Intervalo(nrow(hist_sales_mkt_d_not_sold_not_release),min(hist_sales_mkt_d_not_sold_not_release$PriceReg, na.rm=TRUE),max(hist_sales_mkt_d_not_sold_not_release$PriceReg, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="hist_sales_mkt_d_not_sold_not_release") + labs(x="", y="Price")

hist_preco_mkt_D <- plot_grid(P_1, P_2, P_3, P_4, ncol = 2, nrow = 2)
savePlot(hist_preco_mkt_D, "./output/hist_preco_mkt_D.pdf")

# comparacao das vendas por mercado
a_1 <- ggplot(data=hist_sales_mkt_d_sold_release, aes(hist_sales_mkt_d_sold_release$SoldCount)) + geom_histogram(aes(y =..density..),breaks=seq(min(hist_sales_mkt_d_sold_release$SoldCount, na.rm=TRUE), max(hist_sales_mkt_d_sold_release$SoldCount, na.rm=TRUE), by = Intervalo(nrow(hist_sales_mkt_d_sold_release),min(hist_sales_mkt_d_sold_release$SoldCount, na.rm=TRUE),max(hist_sales_mkt_d_sold_release$SoldCount, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="hist_sales_mkt_d_sold_release") + labs(x="", y="Sold")
a_2 <- ggplot(data=hist_sales_mkt_d_sold_not_release, aes(hist_sales_mkt_d_sold_not_release$SoldCount)) + geom_histogram(aes(y =..density..),breaks=seq(min(hist_sales_mkt_d_sold_not_release$SoldCount, na.rm=TRUE), max(hist_sales_mkt_d_sold_not_release$SoldCount, na.rm=TRUE), by = Intervalo(nrow(hist_sales_mkt_d_sold_not_release),min(hist_sales_mkt_d_sold_not_release$SoldCount, na.rm=TRUE),max(hist_sales_mkt_d_sold_not_release$SoldCount, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="hist_sales_mkt_d_sold_not_release") + labs(x="", y="Sold")
a_3 <- ggplot(data=hist_sales_mkt_s_sold_release, aes(hist_sales_mkt_s_sold_release$SoldCount)) + geom_histogram(aes(y =..density..),breaks=seq(min(hist_sales_mkt_s_sold_release$SoldCount, na.rm=TRUE), max(hist_sales_mkt_s_sold_release$SoldCount, na.rm=TRUE), by = Intervalo(nrow(hist_sales_mkt_s_sold_release),min(hist_sales_mkt_s_sold_release$SoldCount, na.rm=TRUE),max(hist_sales_mkt_s_sold_release$SoldCount, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="hist_sales_mkt_s_sold_release") + labs(x="", y="Sold")
a_4 <- ggplot(data=hist_sales_mkt_s_sold_not_release, aes(hist_sales_mkt_s_sold_not_release$SoldCount)) + geom_histogram(aes(y =..density..),breaks=seq(min(hist_sales_mkt_s_sold_not_release$SoldCount, na.rm=TRUE), max(hist_sales_mkt_s_sold_not_release$SoldCount, na.rm=TRUE), by = Intervalo(nrow(hist_sales_mkt_s_sold_not_release),min(hist_sales_mkt_s_sold_not_release$SoldCount, na.rm=TRUE),max(hist_sales_mkt_s_sold_not_release$SoldCount, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="hist_sales_mkt_s_sold_not_release") + labs(x="", y="Sold")

hist_sold_market <- plot_grid(a_1, a_2, a_3, a_4,ncol = 2, nrow = 2)
savePlot(hist_sold_market, "./output/hist_sold_market.pdf")

----
b_1 <- ggplot(data=actv_sales_mkt_d_release, aes(actv_sales_mkt_d_release$ItemCount)) + geom_histogram(aes(y =..density..),breaks=seq(min(actv_sales_mkt_d_release$ItemCount, na.rm=TRUE), max(actv_sales_mkt_d_release$ItemCount, na.rm=TRUE), by = Intervalo(nrow(actv_sales_mkt_d_release),min(hist_sales_mkt_d_sold_release$ItemCount, na.rm=TRUE),max(actv_sales_mkt_d_release$ItemCount, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="actv_sales_mkt_d_release") + labs(x="", y="")
b_2 <- ggplot(data=actv_sales_mkt_d_not_release, aes(actv_sales_mkt_d_not_release$ItemCount)) + geom_histogram(aes(y =..density..),breaks=seq(min(actv_sales_mkt_d_not_release$ItemCount, na.rm=TRUE), max(actv_sales_mkt_d_not_release$ItemCount, na.rm=TRUE), by = Intervalo(nrow(actv_sales_mkt_d_not_release),min(actv_sales_mkt_d_not_release$ItemCount, na.rm=TRUE),max(actv_sales_mkt_d_not_release$ItemCount, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="actv_sales_mkt_d_not_release") + labs(x="", y="")
b_3 <- ggplot(data=actv_sales_mkt_s_release, aes(actv_sales_mkt_s_release$ItemCount)) + geom_histogram(aes(y =..density..),breaks=seq(min(actv_sales_mkt_s_release$ItemCount, na.rm=TRUE), max(actv_sales_mkt_s_release$ItemCount, na.rm=TRUE), by = Intervalo(nrow(actv_sales_mkt_s_release),min(actv_sales_mkt_s_release$ItemCount, na.rm=TRUE),max(actv_sales_mkt_s_release$ItemCount, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="actv_sales_mkt_s_release") + labs(x="", y="")
b_4 <- ggplot(data=actv_sales_mkt_s_not_release, aes(actv_sales_mkt_s_not_release$ItemCount)) + geom_histogram(aes(y =..density..),breaks=seq(min(actv_sales_mkt_s_not_release$ItemCount, na.rm=TRUE), max(actv_sales_mkt_s_not_release$ItemCount, na.rm=TRUE), by = Intervalo(nrow(actv_sales_mkt_s_not_release),min(actv_sales_mkt_s_not_release$ItemCount, na.rm=TRUE),max(actv_sales_mkt_s_not_release$ItemCount, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="actv_sales_mkt_s_not_release") + labs(x="", y="")


actv_item_analise <- plot_grid(b_1, b_2, b_3, b_4,ncol = 2, nrow = 2)
savePlot(actv_item_analise, "./output/histograma_estoque_ativo.pdf")
-----

c_1 <- ggplot(data=actv_sales_mkt_d_release, aes(actv_sales_mkt_d_release$StrengthFactor)) + geom_histogram(aes(y =..density..),breaks=seq(min(actv_sales_mkt_d_release$StrengthFactor, na.rm=TRUE), max(actv_sales_mkt_d_release$StrengthFactor, na.rm=TRUE), by = Intervalo(nrow(actv_sales_mkt_d_release),min(hist_sales_mkt_d_sold_release$StrengthFactor, na.rm=TRUE),max(actv_sales_mkt_d_release$StrengthFactor, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="actv_sales_mkt_d_release") + labs(x="", y="")
c_2 <- ggplot(data=actv_sales_mkt_d_not_release, aes(actv_sales_mkt_d_not_release$StrengthFactor)) + geom_histogram(aes(y =..density..),breaks=seq(min(actv_sales_mkt_d_not_release$StrengthFactor, na.rm=TRUE), max(actv_sales_mkt_d_not_release$StrengthFactor, na.rm=TRUE), by = Intervalo(nrow(actv_sales_mkt_d_not_release),min(actv_sales_mkt_d_not_release$StrengthFactor, na.rm=TRUE),max(actv_sales_mkt_d_not_release$StrengthFactor, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="actv_sales_mkt_d_not_release") + labs(x="", y="")
c_3 <- ggplot(data=actv_sales_mkt_s_release, aes(actv_sales_mkt_s_release$StrengthFactor)) + geom_histogram(aes(y =..density..),breaks=seq(min(actv_sales_mkt_s_release$StrengthFactor, na.rm=TRUE), max(actv_sales_mkt_s_release$StrengthFactor, na.rm=TRUE), by = Intervalo(nrow(actv_sales_mkt_s_release),min(actv_sales_mkt_s_release$StrengthFactor, na.rm=TRUE),max(actv_sales_mkt_s_release$StrengthFactor, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="actv_sales_mkt_s_release") + labs(x="", y="")
c_4 <- ggplot(data=actv_sales_mkt_s_not_release, aes(actv_sales_mkt_s_not_release$StrengthFactor)) + geom_histogram(aes(y =..density..),breaks=seq(min(actv_sales_mkt_s_not_release$StrengthFactor, na.rm=TRUE), max(actv_sales_mkt_s_not_release$StrengthFactor, na.rm=TRUE), by = Intervalo(nrow(actv_sales_mkt_s_not_release),min(actv_sales_mkt_s_not_release$StrengthFactor, na.rm=TRUE),max(actv_sales_mkt_s_not_release$StrengthFactor, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="actv_sales_mkt_s_not_release") + labs(x="", y="")


actv_peso <- plot_grid(c_1, c_2, c_3, c_4,ncol = 2, nrow = 2)
savePlot(actv_peso, "./output/histograma_peso_ativo.pdf")


------
  
  
c_1 <- ggplot(data=actv_sales_mkt_d_release, aes(actv_sales_mkt_d_release$StrengthFactor)) + geom_histogram(aes(y =..density..),breaks=seq(min(actv_sales_mkt_d_release$StrengthFactor, na.rm=TRUE), max(actv_sales_mkt_d_release$StrengthFactor, na.rm=TRUE), by = Intervalo(nrow(actv_sales_mkt_d_release),min(hist_sales_mkt_d_sold_release$StrengthFactor, na.rm=TRUE),max(actv_sales_mkt_d_release$StrengthFactor, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="actv_sales_mkt_d_release") + labs(x="", y="")
c_2 <- ggplot(data=actv_sales_mkt_d_not_release, aes(actv_sales_mkt_d_not_release$StrengthFactor)) + geom_histogram(aes(y =..density..),breaks=seq(min(actv_sales_mkt_d_not_release$StrengthFactor, na.rm=TRUE), max(actv_sales_mkt_d_not_release$StrengthFactor, na.rm=TRUE), by = Intervalo(nrow(actv_sales_mkt_d_not_release),min(actv_sales_mkt_d_not_release$StrengthFactor, na.rm=TRUE),max(actv_sales_mkt_d_not_release$StrengthFactor, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="actv_sales_mkt_d_not_release") + labs(x="", y="")
c_3 <- ggplot(data=actv_sales_mkt_s_release, aes(actv_sales_mkt_s_release$StrengthFactor)) + geom_histogram(aes(y =..density..),breaks=seq(min(actv_sales_mkt_s_release$StrengthFactor, na.rm=TRUE), max(actv_sales_mkt_s_release$StrengthFactor, na.rm=TRUE), by = Intervalo(nrow(actv_sales_mkt_s_release),min(actv_sales_mkt_s_release$StrengthFactor, na.rm=TRUE),max(actv_sales_mkt_s_release$StrengthFactor, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="actv_sales_mkt_s_release") + labs(x="", y="")
c_4 <- ggplot(data=actv_sales_mkt_s_not_release, aes(actv_sales_mkt_s_not_release$StrengthFactor)) + geom_histogram(aes(y =..density..),breaks=seq(min(actv_sales_mkt_s_not_release$StrengthFactor, na.rm=TRUE), max(actv_sales_mkt_s_not_release$StrengthFactor, na.rm=TRUE), by = Intervalo(nrow(actv_sales_mkt_s_not_release),min(actv_sales_mkt_s_not_release$StrengthFactor, na.rm=TRUE),max(actv_sales_mkt_s_not_release$StrengthFactor, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="actv_sales_mkt_s_not_release") + labs(x="", y="")


actv_peso <- plot_grid(c_1, c_2, c_3, c_4,ncol = 2, nrow = 2)
savePlot(actv_item_analise, "./output/histograma_peso_ativo.pdf")




actv_sales_mkt_d_release%>%ggplot(aes(x=StrengthFactor, y=ItemCount))+
  geom_point(color="blue",size=1,alpha=1)+
  xlab('Itens em estoque')+          
  ylab('Quantidade em estoque')+
  ggtitle('titulo')+
  theme(plot.title = element_text(size = 14,colour="red"))


#BOXPLOT
ggplot(actv_sales_mkt_d_release, aes(x=ReleaseYear, y=SoldCount)) + 
  geom_boxplot(notch=FALSE)


#DIVISAO 

# first plot
d5_hist_sales_sold_mkt_d%>%ggplot(aes(x=ReleaseYear, y=ItemCount))+
  geom_point(color="blue",size=1,alpha=1)+
  xlab('Itens em estoque')+          
  ylab('Quantidade em estoque')+
  ggtitle('titulo')+
  theme(plot.title = element_text(size = 14,colour="red"))

d5_hist_sales_sold_mkt_d%>%ggplot(aes(x=StrengthFactor, y=ItemCount))+
  geom_point(color="blue",size=1,alpha=1)+
  xlab('fator de forca')+          
  ylab('quantidade em estoque')+
  ggtitle('titulo')+
  theme(plot.title = element_text(size = 14,colour="red"))

d5_hist_sales_sold_mkt_d%>%ggplot(aes(x=ReleaseYear, y=StrengthFactor))+
  geom_point(color="blue",size=1,alpha=1)+
  xlab('ReleaseYear')+          
  ylab('fator de forca')+
  ggtitle('titulo')+
  theme(plot.title = element_text(size = 14,colour="red"))


savePlot(hist_sales_sold_mkt_d_plot, "./output/market_d_release.pdf")

# distribuicao de probabilidade
histogram=as.data.frame(cbind(d5_hist_sales_sold_mkt_d$LowNetPrice,d5_hist_sales_sold_mkt_d$LowUserPrice, d5_hist_sales_sold_mkt_d$PriceReg))
names(histogram)=c("LowNetPrice","LowUserPrice", "PriceReg")
histogram_melt=melt(histogram)
head(histogram)
 histogram_melt%>%ggplot(aes(x=value,fill=variable, color=variable))+geom_density(alpha = 0.5)+ggtitle('Histogram')

histogram2=as.data.frame(cbind(d5_hist_sales_sold_mkt_d$StrengthFactor,d5_hist_sales_sold_mkt_d$ReleaseYear))
names(histogram2)=c("StrengthFactor","LowUserPrice", "ReleaseYear")
histogram_melt2=melt(histogram2)
head(histogram2)
histogram_melt2_plot <- histogram_melt2%>%ggplot(aes(x=value,fill=variable, color=variable))+geom_density(alpha = 0.5)+ggtitle('Histogram')


savePlot(histogram_melt_plot, "./output/histogram.pdf")

# linear regression 2 variable ####
# separar variaveis
X=cbind(1,d4_hist_sales_sold$ItemCount)
y=d4_hist_sales_sold$StrengthFactor

## calculo j(0)
#repetir quantidade de colunas e transpor em registros - rodar uncao e arredondar
theta=matrix(rep(0,ncol(X)))
round(computeCost(X,y,theta),2)

# usar funcao gradientDescent
iterations = 200
alpha = 0.0001
theta= matrix(rep(0, ncol(X)))
gradientDescent_results=gradientDescent(X,y,theta,alpha,iterations)
theta=gradientDescent_results$theta

# plot custo X interacao
data.frame(Cost=gradientDescent_results$cost,Iterations=1:iterations)%>%
  ggplot(aes(x=Iterations,y=Cost))+geom_line(color="blue")+
  ggtitle("Cost as a function of number of iteration")+
  theme(plot.title = element_text(size = 16,colour="red"))

# plot final
 d4_hist_sales_sold%>%ggplot(aes(x=ItemCount, y=StrengthFactor))+
  geom_point(color="blue",size=1,alpha=1)+
  ylab('Profit in $10,000s')+          
  xlab('Population of City in 10,000s')+
  ggtitle ('Figure 1: Scatter plot of training data') +
  geom_abline(intercept = theta[1], slope = theta[2],col="red",show.legend=TRUE)+
  theme(plot.title = element_text(size = 16,colour="red"))+
  annotate("text", x = 12, y = 20, label = paste0("Profit = ",round(theta[1],4),"+",round(theta[2],4),"tendencia"))

savePlot(hist_sales_sold_plot, "./output/linear_regression.pdf")



# linear regression 3 variable ####
X=cbind(1,d4_hist_sales_sold$ItemCount,d4_hist_sales_sold$SoldCount)
y=d4_hist_sales_sold$LowUserPrice

theta=matrix(rep(0,ncol(X)))
round(computeCost(X,y,theta),2)

iterations = 150
alpha = 0.0001
theta= matrix(rep(0, ncol(X)))
gradientDescent_results=gradientDescent(X,y,theta,alpha,iterations)
theta=gradientDescent_results$theta
theta

data.frame(Cost=gradientDescent_results$cost,Iterations=1:iterations)%>%
  ggplot(aes(x=Iterations,y=Cost))+geom_line(color="blue")+
  ggtitle("Cost as a function of number of iteration")+
  theme(plot.title = element_text(size = 16,colour="red"))



# other ####

d5_hist_sales_sold_mkt_d.lm <- lm(d5_hist_sales_sold_mkt_d$ItemCount ~ d5_hist_sales_sold_mkt_d$SoldCount)
d5_hist_sales_sold_mkt_d.lm

summary(d5_hist_sales_sold_mkt_d.lm)


coefficients(d5_hist_sales_sold_mkt_d.lm) # model coefficients
confint(d5_hist_sales_sold_mkt_d.lm, level=0.95) # CIs for model parameters 
fitted(d5_hist_sales_sold_mkt_d.lm) # predicted values
residuals(d5_hist_sales_sold_mkt_d.lm) # residuals
anova(d5_hist_sales_sold_mkt_d.lm) # anova table 
vcov(d5_hist_sales_sold_mkt_d.lm) # covariance matrix for model parameters 
influence(d5_hist_sales_sold_mkt_d.lm) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(d5_hist_sales_sold_mkt_d.lm)

# knn ####

# Determine number of clusters
d5_hist_sales_sold_mkt_d.stand <- scale(d5_hist_sales_sold_mkt_d[-1])  # To standarize the variables
k.means.fit <- kmeans(d5_hist_sales_sold_mkt_d, 3) # k = 3

wss <- (nrow(d5_hist_sales_sold_mkt_d)-1)*sum(apply(d5_hist_sales_sold_mkt_d,2,d5_hist_sales_sold_mkt_d$Order))
for (i in 2:15) wss[i] <- sum(kmeans(d5_hist_sales_sold_mkt_d, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


test <- cbind(d5_hist_sales_sold_mkt_d$SoldFlag,d5_hist_sales_sold_mkt_d$MarketingType,d5_hist_sales_sold_mkt_d$New_Release_Flag,d5_hist_sales_sold_mkt_d$ItemCount)
colnames(test) <- c("SoldFlag", "MarketingType", "New_Release_Flag","ItemCount")
head(test)
knn <- knn(d5_hist_sales_sold_mkt_d,test,d5_hist_sales_sold_mkt_d$SoldCount,10)

#Explore historical sales data based on the marketing type, grouped by SoldFlag

# End ####

# Saving an R workspace file
save.image(file="C:/TCC/TCC/workspace.RData")
# Clear your memory
rm(list = ls())

# export
#write.csv(dataset,file="./Data/dataset.csv")
# Working diretory
#getwd()
#system("ls ../input")
#setwd('../')



summary(d5_hist_sales_sold_mkt_d2$StrengthFactor)
exponential.model <- lm(log(d5_hist_sales_sold_mkt_d2$StrengthFactor)~ d5_hist_sales_sold_mkt_d2$ItemCount)
timevalues <- seq(0, 9154, 0.1)
timevalues2 <- seq(10040, 1265000)
d5_hist_sales_sold_mkt_d2$StrengthFactor.exponential2 <- exp(predict(exponential.model,list(Time=timevalues)))
plot(d5_hist_sales_sold_mkt_d2$StrengthFactor, d5_hist_sales_sold_mkt_d2$ItemCount,pch=20)
lines(timevalues, d5_hist_sales_sold_mkt_d2$StrengthFactor.exponential2,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")



d5_hist_sales_sold_mkt_d2 <- d5_hist_sales_sold_mkt_d[d5_hist_sales_sold_mkt_d$ItemCount > 150 & d5_hist_sales_sold_mkt_d$StrengthFactor > 10000,]
savePlot(hist_sales_sold_plot, "./output/exponencial.pdf")

#############################

A <- structure(list(Time = c(0, 1, 2, 4, 6, 8, 9, 10, 11, 12, 13, 
                             14, 15, 16, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30), 
                    Counts = c(126.6, 101.8, 71.6, 101.6, 68.1, 62.9, 45.5, 41.9, 
                               46.3, 34.1, 38.2, 41.7, 24.7, 41.5, 36.6, 19.6, 
                               22.8, 29.6, 23.5, 15.3, 13.4, 26.8, 9.8, 18.8, 25.9, 19.3)), 
                .Names = c("Time", "Counts"), 
               row.names = c(1L, 2L,3L, 5L, 7L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 
                            16L, 17L, 19L, 20L, 21L, 22L, 23L, 25L, 26L, 27L, 28L, 29L, 30L,31L), 
               class = "data.frame")
A
attach(A)
names(A)
exponential.model <- lm(log(Counts)~ Time)
summary(Counts.exponential2)
timevalues <- seq(0, 30, 0.1)
Counts.exponential2 <- exp(predict(exponential.model,list(Time=timevalues)))
plot(Time, Counts,pch=16)
lines(timevalues, Counts.exponential2,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")