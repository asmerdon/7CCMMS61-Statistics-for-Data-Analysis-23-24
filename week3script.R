#########################################################################
#BINOMIAL

#dbinom is the probability mass function of the binomial distribution. Its inputs are x, n, and the probability
# of `success'.
# X~Bin(10, 0.2) 

# Generating Prob(X=1) to Prob(X=10)
dbinom(x = 1:10, size = 10, prob = 0.2) 

# we can plot the binomial mass function as follows:
# Grid of X-axis values
x <- 1:80

# size = 80, prob = 0.2
plot(dbinom(x, size = 80, prob = 0.2), type = "h", lwd = 2,
     main = "Binomial probability function",
     ylab = "P(X = x)", xlab = "Number of successes")
# do notice that plot is a 'Stick Graph' there is discontinuity between each value of X because all values of X are integers.

# Illustration on how an increase to the "prob of success" shifts the E(X) and leads to higher levels of kurtosis.
# size = 80, prob = 0.3. 
lines(dbinom(x, size = 80, prob = 0.3), type = "h",
      lwd = 2, col = rgb(1,0,0, 0.7))

# size = 80, prob = 0.4
lines(dbinom(x, size = 80, prob = 0.4), type = "h",
      lwd = 2, col = rgb(0, 1, 0, 0.7))

# Add a legend
legend("topright", legend = c("80  0.2", "80  0.3", "80  0.4"),
       title = "size  prob", title.adj = 0.95,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)
#======================================================================
#pbinom is the cumulative distribution function of the binomial.Its inputs are q (the quantile), n (number of trials)
# probability of `success', lower.tail = TRUE (If TRUE, probabilities are P(X <= x), or P(X > x) otherwise)

#Find the probability of the success occurring less than 3 times if the number of trials is 10 
#and the probability of success is 0.3. So we want Prob(X<3)

pbinom(3, size = 10, prob = 0.3) 

#The probability above can also be calculated adding each value of the probability function up to three:

sum(dbinom(0:3, size = 10, prob = 0.3))

# either way will give you the same answer.


# another example of using pbinom (cumulative distribution function)
#Consider that a basketball player scores 4 out of 10 baskets (prob=0.4). 
#If the player throws 20 baskets (20 trials) what is the probability of scoring 6 or less baskets--Prob(X<=6)

pbinom(6, size = 20, prob = 0.4) # or alternatively find Prob(X>6) and subtract from 1.

1 - pbinom(6, size = 20, prob = 0.4, lower.tail = FALSE) 

# if instead we wanted to find  probability of scoring less than 6 baskets --- Prob(X<6)
pbinom(5, size = 20, prob = 0.4) #or alternatively

1 - pbinom(5, size = 20, prob = 0.4, lower.tail = FALSE)

#Plot of the binomial cumulative distribution in R using the plot function, setting type = "s" 
#and passing the output of the pbinom function for a specific number of experiments and a probability of success.
#The following block of code can be used to plot the binomial cumulative distribution functions
#for 80 trials and different probabilities.

# Grid of X-axis values
x <- 1:80

# size = 80, prob = 0.2
plot(pbinom(x, size = 80, prob = 0.2), type = "s", lwd = 2,
     main = "Binomial distribution function",
     xlab = "Number of successes", ylab = "F(x)")

# size = 80, prob = 0.3
lines(pbinom(x, size = 80, prob = 0.3), type = "s",
      lwd = 2, col = 2)

# size = 80, prob = 0.4
lines(pbinom(x, size = 80, prob = 0.4), type = "s",
      lwd = 2, col = 3)

# Add a legend
legend("bottomright", legend = c("80  0.2", "80  0.3", "80  0.4"),
       title = "size  prob", title.adj = 0.95,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)

#The qbinom function. This is the quantile function of the binomial.
#Given a probability or a set of probabilities, the qbinom function allows you 
#to obtain the corresponding binomial quantile. 
#The following  describes briefly the inputs of the function:
#qbinom(p,                 # Probability or vector of probabilities
#size,              # Number of trials (n > = 0)
#prob,              # The probability of success on each trial
#lower.tail = TRUE) # If TRUE, probabilities are P(X <= x), or P(X > x) otherwise


#As an example, the binomial quantile for the probability 0.4 if n=5 and prob=0.7 is:

qbinom(p = 0.4, size = 5, prob = 0.7) 

# Plot of the binomial quantile function in R
#The binomial quantile function can be plotted in R for a set of probabilities, 
#a number of trials and a probability of success with the following code:

# Grid of X-axis values
x <- 1:80

# size = 80, prob = 0.2
plot(qbinom(seq(0, 1, 0.001), size = 80, prob = 0.2),
     main = "Binomial quantile function",
     ylab = "Q(p)", xlab = "p",
     type = "s", col = 3, xaxt = "n")
axis(1, labels = seq(0, 1, 0.1), at = 0:10 * 100)

# size = 80, prob = 0.3
lines(qbinom(seq(0, 1, 0.001), size = 80, prob = 0.3), type = "s", col = 2)

# size = 80, prob = 0.4
lines(qbinom(seq(0, 1, 0.001), size = 80, prob = 0.4), type = "s", col = 1)

# Add a legend
legend("topleft", legend = c("80  0.2", "80  0.3", "80  0.4"),
       title = "size  prob", title.adj = 0.95,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)

# Generating binomila random values. The rbinom function allows you to drawn random observations 
#from a binomial distribution in R. The arguments of the function are:
#rbinom(n,    # Number of random observations to be generated
#     size, # Number of trials (> = 0)
#    prob) # The probability of success on each trial


#If you want to obtain, for instance, 15 random observations
#from a binomial distribution if the number of trials is 30 and 
#the probability of success on each trial is 0.1 you can type:

rbinom(n = 15, size = 30, prob = 0.1)


#####################################################################################################
#POISSON

# The Poisson probability function with mean lambda can be calculated with the R  with the dpois function
#for any value of x. Belwo are the key inputs of the function:

#dpois(x,           # X-axis values (x = 0, 1, 2, ...)
#lambda,      # Mean number of events that occur on the interval
#log = FALSE) # If TRUE, probabilities are given as log

#example, if you want to calculate the Poisson mass probability function for x∈{0,1,…,10} with mean 5,
#you can type:

dpois(0:10, lambda = 5)

#The Poisson probability mass function can be plotted in R making use of the plot function, as in the following example:

## Grid of X-axis values
x <- 0:50

#-----------
# lambda: 5
#-----------
lambda <- 5
plot(dpois(x, lambda), type = "h", lwd = 2,
     main = "Poisson probability function",
     ylab = "P(X = x)", xlab = "Number of events")

#-----------
# lambda: 10
#-----------
lambda <- 10
lines(dpois(x, lambda), type = "h", lwd = 2, col = rgb(1,0,0, 0.7))

#-----------
# lambda: 20
#-----------
lambda <- 20
lines(dpois(x, lambda), type = "h", lwd = 2, col = rgb(0, 1, 0, 0.7))

# Legend
legend("topright", legend = c("5", "10", "20"),
       title = expression(lambda), title.adj = 0.75,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)

# The cummulative probability function of a Poisson rv. So X follows a Poisson distribution taking values equal or lower than 
#x can be calculated with the ppois funtion. Its inputs are:

#ppois(q,     # Quantile or vector of quantiles
#lambda,       # Mean or vector of means
#lower.tail = TRUE, # If TRUE, probabilities are P(X <= x), or P(X > x) otherwise
#log.p = FALSE)     # If TRUE, probabilities are given as log

# If you want to calculate, for instance, the probability of observing 5 or less events
#if the mean of events occurring on a specific interval is 10 you can type:

ppois(5, lambda = 10) 

# example: Consider that the number of visits on a web page is known to follow a Poisson distribution 
#with mean 15 visits per hour. Find the probability of getting 10 or less visits per hour, 

ppois(10, lambda = 15) # or alternatively use 

1 - ppois(10, lambda = 15, lower.tail = FALSE) # or alternatively use 

sum(dpois(0:10, lambda = 15)) # all three will give you the same answer.

# Find the The probability of getting more than 20 visits per hour, 

ppois(20, lambda = 15, lower.tail = FALSE) # or alternatively use

1 - ppois(20, lambda = 15)        # or alternatively use

1 - sum(dpois(0:20, lambda = 15))  # all give the same answer

# The cumulative distribution of the Poisson distribution can be represented for different values of lambda

# Grid of X-axis values
x <- 0:50

#-----------
# lambda: 5
#-----------
lambda <- 5
plot(ppois(x, lambda), type = "s", lwd = 2,
     main = "Poisson distribution function",
     xlab = "Number of events", ylab = "F(x)")

#-----------
# lambda: 10
#-----------
lambda <- 10
lines(ppois(x, lambda), type = "s", lwd = 2, col = 2)

#-----------
# lambda: 20
#-----------
lambda <- 20
lines(ppois(x, lambda), type = "s", lwd = 2, col = 3)

# Legend
legend("bottomright", legend = c("5", "10", "20"),
       title = expression(lambda), title.adj = 0.75,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)

#The qpois function allows obtaining the corresponding Poisson quantiles for a set of probabilities. Its inputs are:
#qpois(p,                 # Probability or vector of probabilities
#lambda,            # Mean or vector of means
#lower.tail = TRUE, # If TRUE, probabilities are P(X <= x), or P(X > x) otherwise
#log.p = FALSE)     # If TRUE, probabilities are given as log

#the quantile 0.5 of a Poisson distribution is equal to the mean:

qpois(0.5, lambda = 10) 

# The Poisson quantile function can be plotted in R for a set of probabilities for different lambdas.
##-----------
# lambda: 20
#-----------
plot(qpois(seq(0, 1, 0.001), lambda = 20),
     main = "Poisson quantile functions",
     ylab = "Q(p)", xlab = "p",
     type = "s", col = 3, xaxt = "n")

axis(1, labels = seq(0, 1, 0.1), at = 0:10 * 100)

#-----------
# lambda: 10
#-----------
lines(qpois(seq(0, 1, 0.001), lambda = 10), type = "s", col = 2)

#-----------
# lambda: 5
#-----------
lines(qpois(seq(0, 1, 0.001), lambda = 5), type = "s")

# Legend
legend("topleft", legend = c("5", "10", "20"),
       title = expression(lambda), title.adj = 0.75,
       lty = 1, col = 1:3, lwd = 2, box.lty = 0)

#f you want to randomly draw n observations from a Poisson distribution you can make use of the rpois function. 
#Its key inputs are:

#rpois(n,      # Number of random observations to be generated
#lambda) # Mean or vector of lambdas

# If you want to obtain 10 random observations from a Poisson distribution with mean 4 in R you can type:

rpois(10, lambda = 4)

#####################################
# Discrete uniform distribution
###################################################################################
# Manual implementation of the probability function
dupf <- function(x,n) ifelse(x>=1 & x<=n & round(x)==x,1/n,0) 

# Manual implementation of the distribution function
ducdf <- function(x,n) ifelse(x<1,0,ifelse(x<=n,floor(x)/n,1))

# n = 6
probs <- dupf(x = 1:6, n = 6)
print(probs)
# probability function plot
plot(1:6, probs, xlab = "x", ylab = "Probability", type = "h", lwd = 3)
# distribution function plot
cdf <- ducdf(x = 0:6, n = 6)
plot(0:6, cdf, xlab = "x", ylab = "CDF", type = "s", lwd = 2)# Discrete uniform distribution
###################################################################################

# Problem 1
# Suppose there are twelve multiple choice questions in an English class quiz. 
#Each question has five possible answers, and only one of them is correct.
#Find the probability of having four or less correct answers if a student attempts
#to answer every question at random.

#Since only one out of five possible answers is correct, the probability of answering a question correctly 
#by random is 1/5=0.2. 
#We can find the probability of having exactly 4 correct answers by random attempts as follows.

dbinom(4, size=12, prob=0.2) 

#To find the probability of having four or less correct answers by random attempts,
#we apply the function dbinom with x = 0,…,4.

dbinom(0, size=12, prob=0.2) + 
  + dbinom(1, size=12, prob=0.2) + 
  + dbinom(2, size=12, prob=0.2) + 
  + dbinom(3, size=12, prob=0.2) + 
  + dbinom(4, size=12, prob=0.2) 

#Alternatively, we can use the cumulative probability function for binomial distribution pbinom.
pbinom(4, size=12, prob=0.2)

#########################################################################

#Problem 2
# If there are twelve cars crossing a bridge per minute on average, 
#find the probability of having seventeen or more cars crossing the bridge in a particular minute.

# The probability of having sixteen or less cars crossing the bridge in a particular minute is given
#by the function ppois.

ppois(16, lambda=12)   # lower tail

# Hence the probability of having seventeen or more cars crossing the bridge in a minute 
#is in the upper tail of the probability density function.

ppois(16, lambda=12, lower=FALSE)   # upper tail 