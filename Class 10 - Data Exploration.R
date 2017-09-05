install.packages("Deducer")
install.packages("Rcmdr")

library("Rcmdr")
install.packages('rJava')

setwd('C:/YYYYYY/AMMA 2017/Data/data_2017/Session 2')

#R imports and stores the data in a data frame called "dt"
dt<-read.csv("train.csv")
#structure  of the data file imported
str(dt)

#Event Rate in the data
sum(dt$Target)/nrow(dt)

summary(dt)

# subset into continous
cont<-subset(dt,select=-c(id))

#more detailed exploration
z<-cont
z<-cont
for (i in 1:ncol(z)) 
{
  z1<-t(as.data.frame(quantile(z[,i],prob=c(0,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.98,0.99,0.999,1),na.rm=T)))
  row.names(z1)[1]<-colnames(z)[i]
  total<-nrow(z)
  total_miss<-sum(is.na(z[,i]))
  z1<-cbind(z1,total,total_miss)
  if (i==1) y<-z1 else y<-rbind(y,z1)
}

write.csv(y,"univ_cont.csv")



library(psych)
#library(help=psych)

data(galton,package = "psych")
head(galton)
plot(galton$parent, galton$child)
plot(galton$parent,
     galton$child,
     xlab = "Height of Parent",
     ylab= "Height of Children",
     main=" Relationship between Parent and Children Heights",
     pch=17,
     col="red")



# ------------------- Time Series Plot or Line Chart -------------------------

# Scenario - How Average Month Temprature is changing across years
# nottem         Average Monthly Temperatures at Nottingham,1920-1939

#library(help = "datasets")
data(nottem,package = "datasets")
head(nottem)
plot(nottem)
nottem

# Add elements
plot(nottem,
     xlab="Years",
     ylab="Avg Monthly Temp",
     main="Temp across years",
     col="blue",
     type="l",
     pch=20)

# ------------------------  Frequency Distributions: Histogram -------------------

# Distribution of Customer Age: How many customers are available across different age groups

# Generate Age data
## Generate a numeric vector for Age
Age <- as.integer(rnorm(10000,m=55, sd=15))
# histogram
hist(Age)
#?hist 
hist(Age, breaks=50)

# Add elements or beautify Histogram
hist(Age, 
     breaks=30,
     col="green",
     border="white", 
     xlab="Age", 
     ylab="Counts", 
     main="Histogram:Age")

# Scenario: Distribution of Mortality Rates

#http://www.stats4stem.org/r-usmelanoma-data.html
install.packages("HSAUR2")
library(HSAUR2)
data("USmelanoma")
names(USmelanoma)

xr <- range(USmelanoma$mortality) * c(0.9, 1.1)
# Histogram
hist(USmelanoma$mortality,
     xlim = xr, 
     xlab = "Mortality", 
     main = "Histogram:Mortality", 
     ylab = "Counts",
     col="red",
     border="yellow")

# --------------  Box Plot : Distribution of a quantative/numeric Variable ----------------
?boxplot

# Box Plot
boxplot(USmelanoma$mortality, 
        ylim = xr, 
        horizontal = TRUE, 
        xlab = "Mortality")
quantile(USmelanoma$mortality, probs = c(0, 0.25,0.5,0.75,1))

table(USmelanoma$ocean)

boxplot(mortality ~ ocean, 
        data = USmelanoma, 
        xlab = "Contiguity to an ocean", 
        ylab = "Mortality")

