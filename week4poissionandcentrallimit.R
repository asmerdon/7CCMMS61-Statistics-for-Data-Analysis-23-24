##################################################################################
#  Example 1
##################################################################################
rm(list= ls())

# P(T<=1)
pexp(1 , rate = 0.5)

# P(T>3)
1-pexp(3 , rate = 0.5)

# P(1<T<=3)
pexp(3 , rate = 0.5) - pexp(1 , rate = 0.5)

##################################################################################
#  Example 2
##################################################################################
rm(list= ls())

qcauchy(p = 0.99)


##################################################################################
#  Example 3
##################################################################################
rm(list= ls())
# P(X>=3)
1 - dpois(x = 0, lambda = 3.2) - dpois(x = 1, lambda = 3.2)- dpois(x = 2, lambda = 3.2)

# P(B)
p.b <- dpois(x = 1, lambda = 0.8)

# P(C)
p.c <-  1 - dpois(x = 0, lambda = 2.4) - dpois(x = 1, lambda = 2.4)

# P(A) = P(B) P(C)

p.a = p.b*p.c

p.a


##################################################################################
#  Central Limit Theorem
##################################################################################
rm(list= ls())


#####################################################
# Central Limit Theorem
#####################################################
#--------------------
# One variable
#--------------------
# simulated data
set.seed(1234)
sim <- runif(10000, min = -0.5, max = 0.5)
# standardized simulated data
sim <- scale(sim)
hist(sim, probability = T, xlim = c(-3,3), ylim = c(0,0.5), breaks = 30)
curve(dnorm, -3,3, lwd = 2, add = T)
box()
#--------------------
# Three variables
#--------------------
# simulated data
set.seed(1234)
# The three original variables
sim0 <- matrix(0, ncol = 2, nrow = 10000)
for(i in 1:2) sim0[,i] <- runif(10000, min = -0.5, max = 0.5)
head(sim0)
# produce the sum of the 2 variables
sim <- rowMeans(sim0)
# standardized simulated data
sim <- scale(sim)
hist(sim, probability = T, xlim = c(-3,3), ylim = c(0,0.5), breaks = 30)
curve(dnorm, -3,3, lwd = 2, add = T)
box()
#--------------------
# Ten variables
#--------------------
# simulated data
set.seed(1234)
# The ten original variables
sim0 <- matrix(0, ncol = 10, nrow = 10000)

for(i in 1:10) sim0[,i] <- runif(10000, min = -0.5, max = 0.5)
head(sim0)
# produce the sum of the 3 variables
sim <- rowMeans(sim0)
# standardized simulated data
sim <- scale(sim)
hist(sim, probability = T, xlim = c(-3,3), ylim = c(0,0.5), breaks = 30)
curve(dnorm, -3,3, lwd = 2, add = T)
box()