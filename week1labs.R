country<-c('Belgium','Denmark','France','GB','Ireland','Italy','Luxembourg')
inflation_rate<-c(2.8,1.2,2.1,1.6,1.5,4.6,3.6)
unemployment_rate<-c(9.4,10.4,10.8,10.5,18.4,11.1,2.6)
Data<-data.frame(country,inflation_rate,unemployment_rate)
head(Data)

#q2inflation
max(inflation_rate)
country[inflation_rate==max(inflation_rate)]
min(inflation_rate)
country[inflation_rate==min(inflation_rate)]
range<-c(max(inflation_rate)-min(inflation_rate))
median(inflation_rate)
IQR(inflation_rate)
mean(inflation_rate)
var(inflation_rate)
sd(inflation_rate)

#q2unemployment
max(unemployment_rate)
country[unemployment_rate==max(unemployment_rate)]
min(unemployment_rate)
country[unemployment_rate==min(unemployment_rate)]
range<-c(max(unemployment_rate)-min(unemployment_rate))
median(unemployment_rate)
IQR(unemployment_rate)
mean(unemployment_rate)
var(unemployment_rate)
sd(unemployment_rate)

data<-read.csv("C:/Users/k23031306/OneDrive - King's College London/Documents/R/week1/mussels.csv",header=TRUE)
Mass<-data[,1]
hist(Mass)
boxplot(data[,3])
