# SALES INVENTORY ANALIZER - TPICELLI 2017

# Housekeeping ####

# Clear R workspace
rm(list = ls() )

# Load Workspace
load("C:/TCC/TCC/workspace.RData")

# Load All Packages
library(class)
library(readr)
library(grid)
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

# historico com venda + 60 dias
d4_hist_sales_sold <- d2_hist_sales[d2_hist_sales$SoldFlag == 1,]
d45_actv_sales_sold <- d3_actv_sales[d3_actv_sales$SoldFlag == 1,]

# historico com venda + 60 dias + divisao mercado
d5_hist_sales_sold_mkt_d <- d4_hist_sales_sold[d4_hist_sales_sold$MarketingType == "D",]
d6_hist_sales_sold_mkt_s <- d4_hist_sales_sold[d4_hist_sales_sold$MarketingType == "S",]
d7_actv_sales_sold_mkt_d <- d3_actv_sales[d4_hist_sales_sold$MarketingType == "D",]
d8_actv_sales_sold_mkt_s <- d3_actv_sales[d4_hist_sales_sold$MarketingType == "S",]

d9_hist_sales_sold_mkt_d_release_true <- d5_hist_sales_sold_mkt_d[d5_hist_sales_sold_mkt_d$New_Release_Flag == 1,]
d10_hist_sales_sold_mkt_d_release_false <- d5_hist_sales_sold_mkt_d[d5_hist_sales_sold_mkt_d$New_Release_Flag == 0,]
d11_hist_sales_sold_mkt_s_release_true <- d6_hist_sales_sold_mkt_s[d6_hist_sales_sold_mkt_s$New_Release_Flag == 1,]
d12_hist_sales_sold_mkt_s_release_false <- d6_hist_sales_sold_mkt_s[d6_hist_sales_sold_mkt_s$New_Release_Flag == 0,]

d13_actv_sales_sold_mkt_d_release_true <- d7_actv_sales_sold_mkt_d[d7_actv_sales_sold_mkt_d$New_Release_Flag == 1,]
d14_actv_sales_sold_mkt_d_release_false <- d7_actv_sales_sold_mkt_d[d7_actv_sales_sold_mkt_d$New_Release_Flag == 0,]
d15_actv_sales_sold_mkt_s_release_true <- d8_actv_sales_sold_mkt_s[d8_actv_sales_sold_mkt_s$New_Release_Flag == 1,]
d16_actv_sales_sold_mkt_s_release_false <- d8_actv_sales_sold_mkt_s[d8_actv_sales_sold_mkt_s$New_Release_Flag == 0,]


histogram=as.data.frame(
    cbind(d10_hist_sales_sold_mkt_d_release_false$SoldCount
        ##,
        ##d14_actv_sales_sold_mkt_d_release_false$LowNetPrice, 
        ##d14_actv_sales_sold_mkt_d_release_false$LowNetPrice
        ))
names(histogram)=c("LowNetPrice","LowUserPrice", "PriceReg")
histogram_melt=melt(histogram)
histogram_melt%>%ggplot(aes(x=value,fill=variable, color=variable))+geom_density(alpha = 0.4)+ggtitle('Histogram')

grid.arrange(g1, g2, g3, ncol=1, top="The iris data")
plot_grid(sp, bp, labels=c("A", "B"), ncol = 2, nrow = 1)

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