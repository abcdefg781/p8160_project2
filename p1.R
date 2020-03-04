# Write a function that generate log-likelihood, gradient and Hessian
# Inputs: 
# x - data variables 
# y - outcome
# par - vector of beta parameters
func = function(x, y, par) {
  # Log link x*beta
  u = x %*% par
  expu = exp(u)
  
  loglik = vector(mode = "numeric", nrow(x))
  for(i in 1:nrow(x))
    loglik[i] = y[i]*u[i] - log(1 + expu[i])
  loglik_value = sum(loglik)
  
  # Log-likelihood at betavec
  p <- expu / (1 + expu)
  
  # P(Y_i=1|x_i)
  grad = vector(mode = "numeric", ncol(x))
  
  #grad[1] = sum(y - p)
  for(i in 1:ncol(x))
    grad[i] = sum(t(x[,i])%*%(y - p))
  
  #Hess <- -t(x)%*%p%*%t(1-p)%*%x
  Hess = hess_cal(x, p)
  return(list(loglik = loglik_value, grad = grad, Hess = Hess)) 
}

# Function to return the Hessian matrix
hess_cal = function(x,p){
  len = length(p)
  hess = matrix(0, ncol(x), ncol(x))
  for (i in 1:len) {
    x_t = t(x[i,])
    unit = t(x_t)%*%x_t*p[i]*(1-p[i])
    #unit = t(x[i,])%*%x[i,]*p[i]*(1-p[i])
    hess = hess + unit
  }
  return(-hess)
}

library(tidyverse)

par <- rep(0,31)
breast_cancer <- read_csv("breast-cancer.csv")
x <- cbind(rep(1,569), as.matrix(breast_cancer[,3:32]))
y <- ifelse(breast_cancer[,2]=="M", 0, 1)
func(x,y,par)
