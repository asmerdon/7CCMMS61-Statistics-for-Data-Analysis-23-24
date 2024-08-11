# In this practical we are going to experiment with sampling and simulation in R. This relies on
# two set of functions:

#   • sample extract a sample of a specified size from a set.
#   • rnorm, rbinom, rpois, etc... simulate from the corresponding random variables.

# Since in this exercise we are using random number generators, each of us can get different
# numbers out of them. To be able to replicate exactly the solutions, we can set the seed of the
# random number generator using the command

## Set seed to 
set.seed(1000)
################
## Exercise 1 ##
################

# Let us simulate a population of 1000 units from a uniform random variable between 0 and
# 1, look at its histogram and compare it with the uniform probability density function:

comp_random_sim_dens <-function(){
  set.seed(1000)
  X <<- runif(1000,0,1)
  hist(X,probability = TRUE)
  x <<- seq(0,1,len=100)
  points(x,dunif(x,0,1),type="l",lwd=2)
}
comp_random_sim_dens()
# Compute the mean and the variance for this population and save them for future reference.

mean_var <- function(){
  mu <<- mean(X)
  cat("The mean is:", mu)
  cat("\n")
  sigma2 <<-((1000-1)/1000)*var(X)
  cat("The variance is:", sigma2)
}
mean_var()

# Now repeat the sampling 100 times, save in two vectors the sample means and sample
# variances for all these samples and plot their histograms. Any comment?

hundred_samples_mean <- function(){
  set.seed(1000)
  mu_hat <- rep(NA,100)
  sigma2_hat <<- rep(NA,100)
  for (k in 1:100){
    S <- sample(X,100, replace = FALSE)
    mu_hat[k] <- mean(S)
    sigma2_hat[k] <<- var(S)
  }
  hist(mu_hat,probability = TRUE)
  points(x,dnorm(x,mu,sqrt(sigma2)/10),type="l",lwd=2)
}
hundred_samples_mean()

hundred_samples_var <- function(){
  hist(sigma2_hat, probability = TRUE)
}
hundred_samples_var()

# Let us now explore how the sample mean behave when the sample size increase. Define
# a vector of sample sizes from 10 to 1000 and plot the sample mean as function of the
# sample size. What do you see?

increas_sample_size <- function(){
  set.seed(1000)
  n <- 10:1000
  mean_hat <- rep(NA,length(n))
  for (k in 1:length(n)){
    S <- sample(X,n[k],replace = FALSE)
    mean_hat[k] <- mean(S)
  }
  plot(n, mean_hat,type="l")
  abline(h=mu)
  # The sample mean gets closer and closer to the population mean
  # when the sample size increases.
}
increas_sample_size()

# Repeat the exercise in the case the population is simulated from a normal distribution
# with zero mean and variance 1. Which difierences can you see?

normal_population <- function(){
  set.seed(1000)
  X <<- rnorm(1000,0,1)
  hist(X, probability = TRUE)
  x <<- seq(-3, 3, len=1000)
  points(x,dnorm(x, 0, 1), type="l", lwd=2)
}
normal_population()

mean_var_norm <- function(){
  mu <<- mean(X)
  cat("The mean is:", mu)
  cat("\n")
  sigma2 <<-((1000-1)/1000)*var(X)
  cat("The variance is:", sigma2)
}
mean_var_norm()

hundred_samples_mean_norm <- function(){
  set.seed(1000)
  mu_hat <- rep(NA,100)
  sigma2_hat <<- rep(NA,100)
  for (k in 1:100){
    S <- sample(X,100, replace = FALSE)
    mu_hat[k] <- mean(S)
    sigma2_hat[k] <<- var(S)
  }
  hist(mu_hat,probability = TRUE)
  points(x,dnorm(x,mu,sqrt(sigma2)/10),type="l",lwd=2)
}
hundred_samples_mean_norm()

hundred_samples_var_norm <- function(){
  hist(sigma2_hat, probability = TRUE)
}
hundred_samples_var_norm()

sigma2_hat_sigma2_norm <- function(){
  hist(99*sigma2_hat/sigma2, probability = TRUE)
  z <- 60:140
  points(z,dchisq(z,99),type="l",lwd=2)
}
sigma2_hat_sigma2_norm()

increas_sample_size_norm <- function(){
  set.seed(1000)
  n<-10:1000
  mean_hat<-rep(NA,length(n))
  for (k in 1:length(n)){
    S<-sample(X,n[k],replace = FALSE)
    mean_hat[k]<-mean(S)
  }
  plot(n, mean_hat,type="l")
  abline(h=mu)
  # The main difference is that now (n-1)S/sigma^2
  # is distributed as a chi-squared with n-1 df
}
increas_sample_size_norm()

################
## Exercise 2 ##
################

# The probability for a patient to recover after receiving a drug is 0.5. 200 experiments are
# performed with 50 patients each. Simulate the outcome of these experiments using the
# rbinom function and draw a histogram of the sample proportions from the experiments.
# Repeat the procedure when the probability of success of the drug is 0.95 and comment
# on the difference.

simulation_prob_50_pct <- function(){
  set.seed(1000)
  n <- 50
  p <- 0.5
  X <- rbinom(200,n,p)
  sample_prop <- X/n
  hist(sample_prop,probability = TRUE, main=paste("p=",p))
  z <- seq(0,1,len=1000)
  points(z,dnorm(z,mean=p,sd=sqrt(p*(1-p)/n)),type='l')
}
simulation_prob_50_pct()

simulation_prob_95_pct <- function(){
  # set.seed(1000)
  n <- 50
  p <- 0.95
  X <- rbinom(200,n,p)
  sample_prop <- X/n
  hist(sample_prop,breaks=6,probability = TRUE,main=paste("p=",p))
  z <- seq(0,1,len=1000)
  points(z,dnorm(z,mean=p,sd=sqrt(p*(1-p)/n)),type='l')
}
simulation_prob_95_pct()