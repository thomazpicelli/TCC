# FUNCTIONS
computeCost=function(X,y,theta){
  z=((X%*%theta)-y)^2
  return(sum(z)/(2*nrow(X)))
}

gradientDescent=function(X, y, theta, alpha, iters){
  gd=list()
  cost=rep(0,iters)
  for(k in 1:iters){
    z=rep(0,ncol(X))
    for(i in 1:ncol(X)){
      for(j in 1:nrow(X)){
        z[i]=z[i]+(((X[j,]%*%theta)-y[j])*X[j,i])
      }
    }
    theta= theta-((alpha/nrow(X))*z)
    cost[k]=computeCost(X,y,theta)
  }
  gd$theta= theta
  gd$cost=cost
  gd 
}

savePlot <- function(myPlot, name) {
  pdf(name)
  print(myPlot)
  dev.off()
}

Sturge <- function(n){
  return(1+(3.322*log10(n)))
}

Intervalo <- function(n, min, max){
  return(round((max-min)/Sturge(n)))
}

#sempre entre 0 e 1
Normalize <- function(x){
  return((x-min(x))/(max(x)-min(x))) 
}

#Min e Max dinamico
NormalizeDinamic <- function(x){
  return((x-min(x))/(max(x)-min(x))*((max-min)+max)) 
}
