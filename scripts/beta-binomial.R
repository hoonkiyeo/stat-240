library(tidyverse)

## beta-binomial density
dbb = function(x,n,a,b,log=FALSE)
{
  log_d = lchoose(n, x) +
    lbeta(x+a, n-x+b) -
    lbeta(a, b)
  if ( log )
    return ( log_d )
  return ( exp( log_d ) )
}

## This function assumes that the sample x_1,\ldots,x_m
## (all assumed from the same beta-binomial distribution)
## has been summarized into a vector of length n+1
## with the tabulated counts for each outcome from 0 to n
## The function returns the MLEs of the mean and variance
mbb = function(x)
{
  n = length(x) - 1
  m = sum(x)
  mx = sum((0:n)*x)/m
  vx = sum(x*(0:n - mx)^2)/m
  return(tibble(mx,vx))
}

## Log-likelihood function for (mu,phi)
## x are the counts from 0 to n
## theta = c(mu,phi)
lmpbb = function(theta,x)
{
  mu = theta[1]
  phi = theta[2]
  alpha = mu*phi
  beta = (1-mu)*phi
  n = length(x) - 1
  return( sum(x*dbb(0:n,n,alpha,beta,log=TRUE)) )
}

## Use optim to find mle estimates of alpha and beta from counts
## Use method of moments to start.
## Find mu and phi. Then translate to alpha and beta.
## If the returned convergence is not 0,
##   then there was an error in the optimization
mlebb = function(x)
{
  n = length(x)-1
  moments = mbb(x)
  mx = moments$mx
  vx = moments$vx
  mu_0 = mx/n
  phi_0 = (n*n*mu_0*(1-mu_0) - vx)/(vx - n*mu_0*(1-mu_0))
  tol = 1e-7
  opt = optim(c(mu_0,phi_0),lmpbb,x=x,
              control = list(fnscale=-1),
              method = "L-BFGS-B",
              lower = c(tol,tol),
              upper = c(1-tol,Inf))
  df = tibble(
    mu = opt$par[1],
    phi = opt$par[2],
    alpha = mu*phi,
    beta = (1-mu)*phi,
    logl = opt$value,
    convergence = opt$convergence)

  return( df )
}
