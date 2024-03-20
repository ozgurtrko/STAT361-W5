## Sum

chisquare <- function(n,df){   
  # Generate random variables,and compute chi square with degrees of freedom df
  
  x <- matrix(rnorm(n*df),n,df)^2 # generate a matrix with random variables
   
  y <- apply(x, MARGIN = 1, FUN = sum)  # Row sum of x
  
  a <- mean(y)
  b <- var(y)
  return(list(a,b))
}
chisquare(10000,2)
 
## Mixtures

conv_mix <- function(n, mu1, mu2, sig){
  x1 <- rnorm(n, mu1, sig)
  x2 <- rnorm(n, mu2, sig)
  conv <- x1 + x2 #convolution
  
  u <- runif(n)
  k <- as.integer(u > 0.5) # vector of 0's and 1's
  
  mixture <- k*x1 + (1-k)*x2 #the mixture (if k is 1, take x1; if k is 0, take x2)
  
  
  par(mfcol = c(1,2)) #two graphs per page
  
  hist(conv, prob = T)
  hist(mixture, prob = T)
  par(mfcol = c(1,1)) #restore display
}
conv_mix(1000, 0, 4, 1)


# Poisson - Gamma Mixture

paisson_gamma <- function(n, lambda, r, beta){
  
  #generate lambda from gamma dist.
  lambda <- rgamma(n, r, beta) #generate random lambda
  
  # supply the sample of lambda's as the poisson mean
  x <- rpois(n, lambda)
  
  #compare with negative binomial
  mix <- tabulate(x+1)/n
  negbin <- round(dnbinom(0:max(x), r, beta/(1 + beta)), 3)
  se <- sqrt(negbin * (1- negbin)/n)
  round(rbind(mix, negbin, se), 3)
}
paisson_gamma(10000, 4, 3)


## Mixture of several gamma distb.

dilara <- function(n,k){
  k <- sample(1:5, size = n, replace = T,
              prob = (1:5)/15)
  rate <- 1/k
  x <- rgamma(n, shape = 3, rate = rate)
  
  plot(density(x), xlim = c(0,40), ylim = c(0, 0.3), lwd = 3)
  for(i in 1:5){
    lines(density(rgamma(n, 3, 1/i)))
  }
}





















