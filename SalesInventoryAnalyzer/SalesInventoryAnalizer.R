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

# Load All Functions
source("./R/functions.R")

# First dataset load ####
d1_sales <- read.csv("./data/SalesKaggle3.csv")

# preparando dados
d1_sales$SoldFlag <- as.factor(d1_sales$SoldFlag)
d1_sales$New_Release_Flag <- as.factor(d1_sales$New_Release_Flag)
d1_sales$ReleaseYear <- as.factor(d1_sales$ReleaseYear)

# pre visualizacao dos dados
str(d1_sales)
summary(d1_sales)

# split historical and active data
d2_hist_sales <- d1_sales[d1_sales$File_Type == 'Historical',]
d3_actv_sales <- d1_sales[d1_sales$File_Type == 'Active',]

# historico com venda + 60 dias
d4_hist_sales_sold <- d2_hist_sales[d2_hist_sales$SoldFlag == 1,]

# historico com venda + 60 dias + divisao mercado
d5_hist_sales_sold_mkt_d <- d4_hist_sales_sold[d4_hist_sales_sold$MarketingType == "D",]
d6_hist_sales_sold_mkt_s <- d4_hist_sales_sold[d4_hist_sales_sold$MarketingType == "S",]


# first plot
hist_sales_sold_mkt_d_plot <- d5_hist_sales_sold_mkt_d%>%ggplot(aes(x=ReleaseYear, y=SoldCount))+
  geom_point(color="blue",size=2,alpha=1)+
  xlab('Itens em estoque')+          
  ylab('Quantidade vendida')+
  ggtitle('vendas X ano lancamento')+
  theme(plot.title = element_text(size = 14,colour="red"))

savePlot(hist_sales_sold_mkt_d_plot, "./output/market_d_release.pdf")

# distribuicao de probabilidade
histogram=as.data.frame(cbind(d5_hist_sales_sold_mkt_d$LowNetPrice,d5_hist_sales_sold_mkt_d$LowUserPrice, d5_hist_sales_sold_mkt_d$PriceReg))
names(histogram)=c("LowNetPrice","LowUserPrice", "PriceReg")
histogram_melt=melt(histogram)
head(histogram)
histogram_melt_plot <- histogram_melt%>%ggplot(aes(x=value,fill=variable, color=variable))+geom_density(alpha = 0.5)+ggtitle('Histogram')

savePlot(histogram_melt_plot, "./output/histogram.pdf")

# linear regression 2 variable ####
# separar variaveis
X=cbind(1,d4_hist_sales_sold$ItemCount)
y=d4_hist_sales_sold$SoldCount

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
hist_sales_sold_plot <- d4_hist_sales_sold%>%ggplot(aes(x=ItemCount, y=SoldCount))+
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

d2_hist_sales.lm <- lm(d2_hist_sales$ItemCount ~ d2_hist_sales$SoldFlag + d2_hist_sales$SoldCount + d2_hist_sales$ReleaseNumber + d2_hist_sales$LowUserPrice + d2_hist_sales$LowNetPrice, data = d2_hist_sales)
d2_hist_sales.lm

summary(d2_hist_sales.lm)


coefficients(d2_hist_sales.lm) # model coefficients
confint(d2_hist_sales.lm, level=0.95) # CIs for model parameters 
fitted(d2_hist_sales.lm) # predicted values
residuals(d2_hist_sales.lm) # residuals
anova(d2_hist_sales.lm) # anova table 
vcov(d2_hist_sales.lm) # covariance matrix for model parameters 
influence(d2_hist_sales.lm) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(d2_hist_sales.lm)

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
