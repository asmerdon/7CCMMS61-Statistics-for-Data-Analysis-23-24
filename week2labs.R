data("iris")
head(iris)
attach(iris)
Sepal.Length
mean(Sepal.Length)
IQR(Sepal.Length)
quantile(Sepal.Length,0.75)-quantile(Sepal.Length,0.25)
mean(Petal.Length)
IQR(Petal.Length)


#apply function
apply(iris[,1:4],2,mean)
apply(iris[,1:4],2,sd)
apply(iris[,1:4],2,median)
apply(iris[,1:4],2,quantile, probs=0.75)-apply(iris[,1:4],2,quantile, probs=0.25)
apply(iris[,1:4],2,IQR)

table(Species)/length(Species)

#categorical variable to number:
species_code<-rep(NA, length(Species))
species_code[which(Species=="setosa")]<-1
species_code[which(Species=="versicolor")]<-2
species_code[which(Species=="virginica")]<-3
#plots (histogram)
par(mfrow=c(4,4)) # set more than 1 plot per figure (4x4)

for (i in 1:4){
  hist(iris[,i],main ="",xlab=names(iris)[i],probability=FALSE)
  for (j in which((1:4)!=i)){
    plot(iris[,i],iris[,j],col=species_code,
      pch=species_code,xlab=names(iris)[i],ylab=names(iris)[j])
  }
}

#boxplots
par(mfrow=c(2,2))
boxplot(Sepal.Length~Species,main="Sepal.Length")
boxplot(Sepal.Width~Species,main="Sepal.Width")
boxplot(Petal.Length~Species,main="Petal.Length")
boxplot(Petal.Width~Species,main="Petal.width")

cor(iris[,1:4])

