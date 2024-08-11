x<-seq(-4,4,len=100) 
y<-dnorm(x,mean=0,sd=1)
plot(x,y,type='l')

#q1
meet_specs <- function(){
  p<- pnorm(0.14,mean=0.13,sd=0.005)-pnorm(0,12,mean=0.13, sd=0.005)
  cat("The prob")
}

meet_specs()

sym_int <- function(){
}

#q2

#q3
density_function_p <- function(){
  x <- 0:20
  fy <- dpois(x,10)
  plot(x, fy, type="h")
}
density_function_p()

caught_fishes_4 <- function(){
  p <- dpois(4,10)
  cat("The prob that exactly 4 fishes are caught in an hour is", p)
}

caught_fishes_4()

caught_fishes_at_4 <- function(){
  p <- 1-ppois(3,10) #P(x>=4)
  cat("The prob that atleast  4 fishes are caught in an hour is", p)
}

caught_fishes_at_4()

min_time <- function(){
  mn <- 1:60
  
  pb <-rep(NA, length(mn))
  
  for (k in 1:length(mn)){
    pb[k] <- 1-ppois(3,10*(mn[k]/60))
  }
  
  
  plot(mn, pb, type="l")
  
  th <-which(pb>=0.9)[1]
  aline(v=mn[th])
  
}