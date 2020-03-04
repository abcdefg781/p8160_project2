# Write a function that generate log-likelihood, gradient and Hessian
# Inputs: 
# x - data variables 
# y - outcome
# par - vector of beta parameters
func = function(x, y, par) {
  # Log link x*beta
  u = x %*% par
  expu = exp(u)
  
  loglik = vector(mode = "numeric", length(y))
  for(i in 1:length(y))
    loglik[i] = y[i]*u[i] - log(1 + expu[i])
  loglik_value = sum(loglik)
  
  # Log-likelihood at betavec
  p <- expu / (1 + expu)
  
  # P(Y_i=1|x_i)
  grad = vector(mode = "numeric", length(par))
  
  #grad[1] = sum(y - p)
  for(i in 1:length(par))
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