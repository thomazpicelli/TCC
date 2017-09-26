# SALES INVENTORY ANALIZER - TPICELLI 2017

# Housekeeping ####

# Clear R workspace
rm(list = ls() )

# Load Workspace
load("C:/TCC/TCC/workspace.RData")

# Load All Packages
library(class)
library(ggplot2)
library(Cairo)
library(cluster)
library(factoextra)
library(cowplot)
library(NbClust)
library(magrittr)

# Load All Functions
source("./R/functions.R")



# First dataset load ####
inventory <- read.csv("./data/SalesKaggle3.csv", header = T, sep = ",")

# data formating
inventory$SoldFlag <- as.factor(inventory$SoldFlag)
inventory$New_Release_Flag <- as.factor(inventory$New_Release_Flag)
inventory$ReleaseYear <- as.factor(inventory$ReleaseYear)

summary(inventory)

# defining new colluns
inventory$expense <- inventory$ItemCount * inventory$PriceReg
inventory$NormalizeSoldCount <- NA
inventory$NormalizeItemCount <- NA
inventory$NormalizeExpense <- NA
inventory$NormalizeStrengthFactor <- NA
inventory$NormalizeSomado <- NA
inventory$NormalizePonderado <- NA

# data filter -- divisao de mercado e tipo (Active X Historical)

# historico e D
hist_inventory_mkt_d <- inventory[inventory$File_Type == 'Historical' & inventory$MarketingType == "D" & inventory$PriceReg > 0,]
  hist_inventory_mkt_d$NormalizeSoldCount <- Normalize(hist_inventory_mkt_d$SoldCount)
  hist_inventory_mkt_d$NormalizeItemCount <- Normalize(hist_inventory_mkt_d$ItemCount)
  hist_inventory_mkt_d$NormalizeStrengthFactor <- Normalize(hist_inventory_mkt_d$StrengthFactor)
  hist_inventory_mkt_d$NormalizeExpense <- Normalize(hist_inventory_mkt_d$expense)
  
# historico e S
hist_inventory_mkt_s <- inventory[inventory$File_Type == 'Historical' & inventory$MarketingType == "S" & inventory$PriceReg > 0,]
  hist_inventory_mkt_s$NormalizeSoldCount <- Normalize(hist_inventory_mkt_s$SoldCount)
  hist_inventory_mkt_s$NormalizeItemCount <- Normalize(hist_inventory_mkt_s$ItemCount)
  hist_inventory_mkt_s$NormalizeStrengthFactor <- Normalize(hist_inventory_mkt_s$StrengthFactor)
  hist_inventory_mkt_s$NormalizeExpense <- Normalize(hist_inventory_mkt_s$expense)
  
# Ativo e D
actv_inventory_mkt_d <- inventory[inventory$File_Type == 'Active' & inventory$MarketingType == "D" & inventory$PriceReg > 0,]
  actv_inventory_mkt_d$NormalizeSoldCount <- Normalize(actv_inventory_mkt_d$SoldCount)
  actv_inventory_mkt_d$NormalizeItemCount <- Normalize(actv_inventory_mkt_d$ItemCount)
  actv_inventory_mkt_d$NormalizeStrengthFactor <- Normalize(actv_inventory_mkt_d$StrengthFactor)
  actv_inventory_mkt_d$NormalizeExpense <- Normalize(actv_inventory_mkt_d$expense)
  
# Ativo e S
actv_inventory_mkt_s <- inventory[inventory$File_Type == 'Active' & inventory$MarketingType == "S" & inventory$PriceReg > 0,]
  actv_inventory_mkt_s$NormalizeSoldCount <- Normalize(actv_inventory_mkt_s$SoldCount)
  actv_inventory_mkt_s$NormalizeItemCount <- Normalize(actv_inventory_mkt_s$ItemCount)
  actv_inventory_mkt_s$NormalizeStrengthFactor <- Normalize(actv_inventory_mkt_s$StrengthFactor)
  actv_inventory_mkt_s$NormalizeExpense <- Normalize(actv_inventory_mkt_s$expense)
  

# Exploring analitics ####

##  histogramas(distribuicao de frequencia) para analisar o comportamento dos dados normalizados.
hist_hist_inventory_mkt_d <- 
    ggplot(data=hist_inventory_mkt_d, 
           aes(hist_inventory_mkt_d$NormalizeStrengthFactor)) + 
    geom_histogram(aes(y = ..count..),breaks=seq(min(hist_inventory_mkt_d$NormalizeStrengthFactor, na.rm=TRUE), max(hist_inventory_mkt_d$NormalizeStrengthFactor, na.rm=TRUE), by = 0.025), col="blue", fill="#4db2ff", alpha = .5) + 
    geom_density(col=2) + ylim(0, 15000) +
    labs(x="Fator de Força Normalizado", y="Frequência", title="Mercado D") + 
    geom_vline(data = hist_inventory_mkt_d, aes(xintercept = mean(hist_inventory_mkt_d$NormalizeStrengthFactor)), col="#4db255", linetype = "dashed", size = 1) + 
    geom_vline(data = hist_inventory_mkt_d, aes(xintercept = median(hist_inventory_mkt_d$NormalizeStrengthFactor)), col="red", linetype = "dashed", size = 1) + 
    theme(plot.title = element_text(size=36),axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"), panel.background = element_rect(fill = "#eeeeee"), panel.grid.minor = element_line(colour="white", size=0.5))

hist_hist_inventory_mkt_s <- 
    ggplot(data=hist_inventory_mkt_s, 
           aes(hist_inventory_mkt_s$NormalizeStrengthFactor)) + 
    geom_histogram(aes(y = ..count..),breaks=seq(min(hist_inventory_mkt_s$NormalizeStrengthFactor, na.rm=TRUE), max(hist_inventory_mkt_s$NormalizeStrengthFactor, na.rm=TRUE), by = 0.025), col="blue", fill="#4db2ff", alpha = .5) + 
    geom_density(col=2) + ylim(0, 15000) +
    labs(x="Fator de Força Normalizado", y="Frequência", title="Mercado S") + 
    geom_vline(data = hist_inventory_mkt_s, aes(xintercept = mean(hist_inventory_mkt_s$NormalizeStrengthFactor)), col="#4db255", linetype = "dashed", size = 1) + 
    geom_vline(data = hist_inventory_mkt_s, aes(xintercept = median(hist_inventory_mkt_s$NormalizeStrengthFactor)), col="red", linetype = "dashed", size = 1) + 
    theme(plot.title = element_text(size=36),axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold"), panel.background = element_rect(fill = "#eeeeee"), panel.grid.minor = element_line(colour="white", size=0.5))
  
hist_peso <- 
  plot_grid(hist_hist_inventory_mkt_d, hist_hist_inventory_mkt_s, ncol = 2, nrow = 1)
savePlot(hist_peso, "./output/hist_peso.pdf")


hist2_hist_inventory_mkt_d <- ggplot(data=hist_inventory_mkt_d, aes(hist_inventory_mkt_d$NormalizeExpense)) + geom_histogram(aes(y = ..count..),breaks=seq(min(hist_inventory_mkt_d$NormalizeExpense, na.rm=TRUE), max(hist_inventory_mkt_d$NormalizeExpense, na.rm=TRUE), by = 0.025), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="") + labs(x="", y="frequency")
hist2_hist_inventory_mkt_s <- ggplot(data=hist_inventory_mkt_s, aes(hist_inventory_mkt_s$NormalizeExpense)) + geom_histogram(aes(y = ..count..),breaks=seq(min(hist_inventory_mkt_s$NormalizeExpense, na.rm=TRUE), max(hist_inventory_mkt_s$NormalizeExpense, na.rm=TRUE), by = 0.025), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="") + labs(x="", y="frequency")
hist2_actv_inventory_mkt_d <- ggplot(data=actv_inventory_mkt_d, aes(actv_inventory_mkt_d$NormalizeExpense)) + geom_histogram(aes(y = ..count..),breaks=seq(min(actv_inventory_mkt_d$NormalizeExpense, na.rm=TRUE), max(actv_inventory_mkt_d$NormalizeExpense, na.rm=TRUE), by = 0.025), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="") + labs(x="", y="frequency")
hist2_actv_inventory_mkt_s <- ggplot(data=actv_inventory_mkt_s, aes(actv_inventory_mkt_s$NormalizeExpense)) + geom_histogram(aes(y = ..count..),breaks=seq(min(actv_inventory_mkt_s$NormalizeExpense, na.rm=TRUE), max(actv_inventory_mkt_s$NormalizeExpense, na.rm=TRUE), by = 0.025), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="") + labs(x="", y="frequency")

hist_custo <- plot_grid(hist2_hist_inventory_mkt_d, hist2_hist_inventory_mkt_s, hist2_actv_inventory_mkt_d, hist2_actv_inventory_mkt_s,ncol = 2, nrow = 2)
savePlot(hist_custo, "./output/hist_custo.pdf")


#BOXPLOT
summary(inventory)
box_1 <- ggplot(aes(y = NormalizeStrengthFactor, x = MarketingType), data = hist_inventory_mkt_d) + geom_boxplot() + facet_wrap(~MarketingType)
box_2 <- ggplot(aes(y = NormalizeStrengthFactor, x = MarketingType), data = hist_inventory_mkt_s) + geom_boxplot()
box_3 <- ggplot(aes(y = NormalizeStrengthFactor, x = MarketingType), data = actv_inventory_mkt_d) + geom_boxplot()
box_4 <- ggplot(aes(y = NormalizeStrengthFactor, x = MarketingType), data = actv_inventory_mkt_s) + geom_boxplot()
plot_grid(box_1, box_2, box_3, box_4, ncol = 2, nrow = 2)


# 1.Model - Classificação ABC Teórica ####

#MERCADO D
# exportacao para criacao do ABC direto no excel 
write.csv2(hist_inventory_mkt_d,file="./Data/hist_D_export.csv")

# importacao do ABC teórico já classificado
hist_inventory_mkt_d_ABC_1 <- read.csv("./data/hist_D_import.csv", header = T, sep = ";")
hist_inventory_mkt_d_ABC_1$NormalizeStrengthFactor <- as.numeric(gsub(",", ".", gsub("\\.", "", hist_inventory_mkt_d_ABC_1$NormalizeStrengthFactor)))
hist_inventory_mkt_d_ABC_1$NormalizeExpense <- as.numeric(gsub(",", ".", gsub("\\.", "", hist_inventory_mkt_d_ABC_1$NormalizeExpense)))

ggplot(hist_inventory_mkt_d_ABC_1, aes(hist_inventory_mkt_d_ABC_1$NormalizeExpense, hist_inventory_mkt_d_ABC_1$NormalizeStrengthFactor, color = hist_inventory_mkt_d_ABC_1$classe)) +
    geom_point() + 
    scale_colour_manual(values = c("#51C0FF","#51C0FF", "#00A2FF", "#00517F")) +
    theme(plot.title = element_text(size=0),axis.text=element_text(size=14), axis.title=element_text(size=0), legend.position = "none") + labs(x="", y="", title="", colour = "Classe")


#MERCADO S
# exportacao para criacao do ABC direto no excel 
write.csv2(hist_inventory_mkt_s,file="./Data/hist_S_export.csv")

# importacao do ABC teórico já classificado
hist_inventory_mkt_s_ABC_1 <- read.csv("./data/hist_S_import.csv", header = T, sep = ";")
hist_inventory_mkt_s_ABC_1$NormalizeStrengthFactor <- as.numeric(gsub(",", ".", gsub("\\.", "", hist_inventory_mkt_s_ABC_1$NormalizeStrengthFactor)))
hist_inventory_mkt_s_ABC_1$NormalizeExpense <- as.numeric(gsub(",", ".", gsub("\\.", "", hist_inventory_mkt_s_ABC_1$NormalizeExpense)))

ggplot(hist_inventory_mkt_s_ABC_1, aes(hist_inventory_mkt_s_ABC_1$NormalizeExpense, hist_inventory_mkt_s_ABC_1$NormalizeStrengthFactor, color = hist_inventory_mkt_s_ABC_1$classe)) +
  geom_point() + 
  scale_colour_manual(values = c("#51C0FF","#51C0FF", "#00A2FF", "#00517F")) +
  theme(plot.title = element_text(size=0),axis.text=element_text(size=14), axis.title=element_text(size=0), legend.position = "none") + labs(x="", y="", title="", colour = "Classe")



# 2.Model - KMeans ####
set.seed(76964057) #Set the seed for reproducibility

hist_inventory_mkt_d$NormalizeSomado <- (hist_inventory_mkt_d$NormalizeExpense) + (hist_inventory_mkt_d$NormalizeStrengthFactor)
hist_inventory_mkt_s$NormalizeSomado <- (hist_inventory_mkt_s$NormalizeExpense) + (hist_inventory_mkt_s$NormalizeStrengthFactor)


# CUSTO
k1 <-kmeans(hist_inventory_mkt_d[,18], centers=3)
k2 <-kmeans(hist_inventory_mkt_s[,18], centers=3)
# CUSTO + STRENGTH FACTOR
k1 <-kmeans(hist_inventory_mkt_d[,20], centers=3)
k2 <-kmeans(hist_inventory_mkt_s[,20], centers=3)

# Results
table(k$cluster)
k$centers

hist_inventory_mkt_d$cluster1 <- as.character(k1$cluster)
hist_inventory_mkt_s$cluster1 <- as.character(k2$cluster)

#PLOT
ggplot(hist_inventory_mkt_d, aes(hist_inventory_mkt_d$NormalizeExpense, hist_inventory_mkt_d$NormalizeStrengthFactor, color = hist_inventory_mkt_d$cluster1)) +
  geom_point() + 
  scale_colour_manual(values = c("#51C0FF", "#00A2FF", "#00517F")) +
  theme(plot.title = element_text(size=0),axis.text=element_text(size=14), axis.title=element_text(size=0), legend.position = "none") + labs(x="", y="", title="", colour = "Classe")

ggplot(hist_inventory_mkt_s, aes(hist_inventory_mkt_s$NormalizeExpense, hist_inventory_mkt_s$NormalizeStrengthFactor, color = hist_inventory_mkt_s$cluster1)) +
  geom_point() + 
  scale_colour_manual(values = c("#51C0FF", "#00A2FF", "#00517F")) +
  theme(plot.title = element_text(size=0),axis.text=element_text(size=14), axis.title=element_text(size=0), legend.position = "none") + labs(x="", y="", title="", colour = "Classe")



#3.Model - MKeans ponderado ####
hist_inventory_mkt_d$NormalizePonderado <- (hist_inventory_mkt_d$NormalizeExpense * 0.7) + (hist_inventory_mkt_d$NormalizeStrengthFactor * 0.3)
hist_inventory_mkt_s$NormalizePonderado <- (hist_inventory_mkt_s$NormalizeExpense * 0.7) + (hist_inventory_mkt_s$NormalizeStrengthFactor * 0.3)


# CUSTO + STRENGTH FACTOR
k3 <-kmeans(hist_inventory_mkt_d[,21], centers=3)
k4 <-kmeans(hist_inventory_mkt_s[,21], centers=3)


# Results
table(k$cluster)
k$centers

hist_inventory_mkt_d$cluster2 <- as.character(k3$cluster)
hist_inventory_mkt_s$cluster2 <- as.character(k4$cluster)

#PLOT
ggplot(hist_inventory_mkt_d, aes(hist_inventory_mkt_d$NormalizeExpense, hist_inventory_mkt_d$NormalizeStrengthFactor, color = hist_inventory_mkt_d$cluster2)) +
  geom_point() + 
  scale_colour_manual(values = c("#51C0FF", "#00A2FF", "#00517F")) +
  theme(plot.title = element_text(size=0),axis.text=element_text(size=14), axis.title=element_text(size=0), legend.position = "none") + labs(x="", y="", title="", colour = "Classe")

ggplot(hist_inventory_mkt_s, aes(hist_inventory_mkt_s$NormalizeExpense, hist_inventory_mkt_s$NormalizeStrengthFactor, color = hist_inventory_mkt_s$cluster2)) +
  geom_point() + 
  scale_colour_manual(values = c("#51C0FF", "#00A2FF", "#00517F")) +
  theme(plot.title = element_text(size=0),axis.text=element_text(size=14), axis.title=element_text(size=0), legend.position = "none") + labs(x="", y="", title="", colour = "Classe")






#determining optimum number of clusters for k-means
rng<-2:20 #K from 2 to 20
tries <-10 #Run the K Means algorithm 100 times
avg.totw.ss <-integer(length(rng)) #Set up an empty vector to hold all of points
for(v in rng){ # For each value of the range variable
  v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
  for(i in 1:tries){
    k.temp <-kmeans(hist_inventory_mkt_d[,15],centers=v) #Run kmeans
    v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
  }
  avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss
}
plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")



# 4.Model - KNN ####
train <- hist_inventory_mkt_d[-i,15]
test <- hist_inventory_mkt_d[i,15]


prc_test_pred <- knn(train, test,train, k=10)






# 5.Model - LinearRegration ####

hist_inventory_mkt_d.lm <- lm(hist_inventory_mkt_d$custo ~ hist_inventory_mkt_d$StrengthFactor)
hist_inventory_mkt_d.lm

summary(hist_inventory_mkt_d.lm)


coefficients(hist_inventory_mkt_d.lm) # model coefficients
confint(d5_hist_sales_sold_mkt_d.lm, level=0.95) # CIs for model parameters 
fitted(hist_inventory_mkt_d.lm) # predicted values
residuals(d5_hist_sales_sold_mkt_d.lm) # residuals
anova(d5_hist_sales_sold_mkt_d.lm) # anova table 
vcov(d5_hist_sales_sold_mkt_d.lm) # covariance matrix for model parameters 
influence(d5_hist_sales_sold_mkt_d.lm) # regression diagnostics

# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(hist_inventory_mkt_d.lm)




hist_inventory_mkt_d.lm = lm(hist_inventory_mkt_d$custo ~ hist_inventory_mkt_d$StrengthFactor, data=hist_inventory_mkt_d)
coeffs <- coefficients(hist_inventory_mkt_d.lm) 
A <- coeffs[2] 
B <- coeffs[1]

ggplot(aes(x=hist_inventory_mkt_d$custo, y=hist_inventory_mkt_d$StrengthFactor))+
  geom_point(color="blue",size=4,alpha=0.5)+
  ylab('Profit')+xlab('Population of City')+ 
  ggtitle('Scatter plot of training data')+theme(plot.title = element_text(size = 16,colour="red"))


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






# Examples ####

actv_sales_mkt_d_release%>%ggplot(aes(x=StrengthFactor, y=ItemCount))+
  geom_point(color="blue",size=1,alpha=1)+
  xlab('Itens em estoque')+          
  ylab('')+
  ggtitle('titulo')+
  theme(plot.title = element_text(size = 14,colour="red"))


# comparacao das vendas por mercado
a_1 <- ggplot(data=hist_sales_mkt_d_sold_release, aes(hist_sales_mkt_d_sold_release$SoldCount)) + geom_histogram(aes(y = ..count..),breaks=seq(min(hist_sales_mkt_d_sold_release$SoldCount, na.rm=TRUE), max(hist_sales_mkt_d_sold_release$SoldCount, na.rm=TRUE), by = Intervalo(nrow(hist_sales_mkt_d_sold_release),min(hist_sales_mkt_d_sold_release$SoldCount, na.rm=TRUE),max(hist_sales_mkt_d_sold_release$SoldCount, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="hist_sales_mkt_d_sold_release") + labs(x="", y="Sold")
a_2 <- ggplot(data=hist_sales_mkt_d_sold_not_release, aes(hist_sales_mkt_d_sold_not_release$SoldCount)) + geom_histogram(aes(y = ..count..),breaks=seq(min(hist_sales_mkt_d_sold_not_release$SoldCount, na.rm=TRUE), max(hist_sales_mkt_d_sold_not_release$SoldCount, na.rm=TRUE), by = Intervalo(nrow(hist_sales_mkt_d_sold_not_release),min(hist_sales_mkt_d_sold_not_release$SoldCount, na.rm=TRUE),max(hist_sales_mkt_d_sold_not_release$SoldCount, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="hist_sales_mkt_d_sold_not_release") + labs(x="", y="Sold")
a_3 <- ggplot(data=hist_sales_mkt_s_sold_release, aes(hist_sales_mkt_s_sold_release$SoldCount)) + geom_histogram(aes(y = ..count..),breaks=seq(min(hist_sales_mkt_s_sold_release$SoldCount, na.rm=TRUE), max(hist_sales_mkt_s_sold_release$SoldCount, na.rm=TRUE), by = Intervalo(nrow(hist_sales_mkt_s_sold_release),min(hist_sales_mkt_s_sold_release$SoldCount, na.rm=TRUE),max(hist_sales_mkt_s_sold_release$SoldCount, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="hist_sales_mkt_s_sold_release") + labs(x="", y="Sold")
a_4 <- ggplot(data=hist_sales_mkt_s_sold_not_release, aes(hist_sales_mkt_s_sold_not_release$SoldCount)) + geom_histogram(aes(y = ..count..),breaks=seq(min(hist_sales_mkt_s_sold_not_release$SoldCount, na.rm=TRUE), max(hist_sales_mkt_s_sold_not_release$SoldCount, na.rm=TRUE), by = Intervalo(nrow(hist_sales_mkt_s_sold_not_release),min(hist_sales_mkt_s_sold_not_release$SoldCount, na.rm=TRUE),max(hist_sales_mkt_s_sold_not_release$SoldCount, na.rm=TRUE))), col="blue", fill="blue", alpha = .2) + geom_density(col=2) + labs(title="hist_sales_mkt_s_sold_not_release") + labs(x="", y="Sold")

hist_sold_market <- plot_grid(a_1, a_2, a_3, a_4,ncol = 2, nrow = 2)
savePlot(hist_sold_market, "./output/hist_sold_market.pdf")




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
