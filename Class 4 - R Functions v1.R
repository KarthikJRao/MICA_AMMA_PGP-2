###-------------  R - Commonly used functions -----------------

# ----------------------Numeric Functions -----------
# -  digit manipulations
a <-19.753
# Truncate 
trunc(a)
as.integer(a)
# Ceiling
ceiling(a)
# flloor
floor(a)
# round
round(a,2)  # round(a,0.01)
round(a,1)
round(a,0)

 norm.vect <- rnorm(100000)
 
 mean(norm.vect)
 sd(norm.vect)
 
 # % of number between +/- 1 sigma or standard deviation
 
 norm.vect.1 <-  norm.vect<=1 &  norm.vect >=-1
 
 sum(norm.vect.1 )/length(norm.vect.1)
 
 # % of number between +/- 2 sigma or standard deviation
 
 norm.vect.2 <-  norm.vect<=2 &  norm.vect >=-2
 
 sum(norm.vect.2)/length(norm.vect.2)
 
 # % of number between +/- 3 sigma or standard deviation
 
 norm.vect.3 <-  norm.vect<=3 &  norm.vect >=-3
 
 sum(norm.vect.3)/length(norm.vect.3)
 
 # what is probability that a standard normal random number be between 0 and 1?
 rnorm(100)
 hist(rnorm(100),
      breaks=50,
      col="blue",
      border="white")
 
 # 100 points normal 
 
  #norm <- sigma*stanNormal+mean
 
 norm <- rnorm(10000,50,15)
 mean(norm)
 sd(norm)
 
 hist(norm,
      breaks=50,
      col="blue",
      border="white")
 
 norm.vect.int <- as.integer(norm.vect)
 norm.vect.rnd <- round(norm.vect,2)
 head(norm.vect.rnd,10)

num <- c(25,20,15,30,40)
num1 <- num-26
num2 <-num1*num1
mean(num)
sd(num)
var(num)

min(norm.vect.int )





# Generate numbers which follow a normal distribution
normal <- rnorm(100000) # defualt mean 0 and standard deviation 1
head(normal)

mean(normal)
sd(normal)

hist(normal, breaks=50, col='red')
a <- rnorm(100000,mean = 50,sd=20)
mean(a)
sd(a)

hist(a)

a <- rnorm(1000,40,20)
mean(a)
sd(a)

a.round <- round(a,2)
# print a few initial observations, default 6 
head(a.round)

bino <- rbinom(100,5,0.5)

table(bino)

hist(bino)

# H1B Visa Visa Process
app <- 60000
applyFolks <- 300000

# Chances of Indian be in a sample is 0.2

indians <- rbinom(100,300000,0.2)

hist(indians)

app/applyFolks


# Check if negative values in a vector
head(a)

v1 <- a<0



class(v1)
table(v1)

a.nega <- a[ v1]
head(a.nega)

a1 <- a[!a<0]
head(a1)



v2 <- a1<0

table(v2)


# - Transformation
a.sqrt <- sqrt(a)

table(is.na(a.sqrt))

a1.sqrt <- sqrt(a1)

tail(a.sqrt,20)

df.a <- data.frame(a.sqrt )

length(a.sqrt)

miss.vect <- is.na(a.sqrt)


head(miss.vect,150)
table(miss.vect)

# fining null or missing values
a.missing <- a[miss.vect]
head(a.missing)
a.missing <- a.sqrt[ is.na(a.sqrt)]

a.non.missing <- a.sqrt[ !is.na(a.sqrt)]

length(a.non.missing)

min(a.sqrt)
min(a.non.missing)

min.value <- min(a.sqrt,na.rm = T)

# Managing negative
min.value <- min(a)



vect <- a+abs(min(a))

min(vect)

a1 <- a+abs(min(a))+1

min(a1)

table(a1<0)

a1.nega <- a1[ a1<0]

a1.sqrt <-sqrt(a1)

hist(a1.sqrt)

min(a1.sqrt)

min(a1)

# logarithmic
a1.log <- log(a1)

max(a1.log)

min(a1.log)

a2<-a1+1

a2.log <- log(a2)

min(a2.log)

hist(a1)
hist(a2.log)
a.exp <- exp(a)

hist(a.exp)

log(10)
exp(log(10))

exp(1000)

#Scenario: You have a vector of mean annual temperature in degrees Fahrenheit
# Convert to ceilciius

library(datasets)
library(help=datasets)
#Loads specified data sets
data(nhtemp,package ="datasets" )

help(nhtemp)

head(nhtemp)

cel <- (5/9) * (nhtemp - 32)

temp.df <- data.frame(nhtemp,cel)


par(mfrow=c(1,1))# split plot window in 1 row and 2 column

?plot

windows() # Open new window
par(mfrow=c(2,1))# split plot window in 1 row and 1 column
plot(temp.df$cel,
     main="Celsius",
     col="red")
plot(nhtemp,main="Fahrenheit",col="blue")
dev.off() # close the plotting window
# ----------------------------Statistical Functions -----------
a <- rnorm(1000,mean = 123,sd=20)
# total
sum(a)
length(a)
sum(a)/length(a)
# Mean, and Median
mean(a)

median(a)

fff <- c(23,32,56,76,84,65,65)
fff <- sort(fff)
median(fff)

fff <- c(23,32,56,76,43,66,76,84,65,65)
fff <- sort(fff)

quantile(fff,0)
quantile(fff,1)
quantile(fff,0.5)

quantile(a,1)

quantile(a,0.75)

quantile(fff,prob=c(0.1,0.2,0.3,0.4))

# Max, Min and Range
max(a)
min(a)
range(a)

# Standard deviation
sd(a)
# variance
var(a)

sqrt(var(a))
# Quantile
hist(a, breaks=40, col = "blue",
     border = "white")
quantile(a,0.5)
quantile(a,0.25)
quantile(a,0.75)

quantile(a,0) # Min
quantile(a,1) # Max

# skewness and kurtosis
install.packages("moments")
library(moments)
skewness(a) # Skewness if negative then negatively skewed higher the value, higher is skewness
kurtosis(a) # NOrmal distribution has value around 3 

# ---- Distribution
# normal
norm <- rnorm(1000,50,20)

hist(norm,
     col="red")
# Binomial
bino <-rbinom(1000,5,0.85)
hist(bino,
     col="blue")
table(bino)
# Poisson
pois <-rpois(1000,10)
hist(pois,
     col="orange")
skewness(pois)
kurtosis(pois)

## ----------------  Character Functions -----------

a <- "look for repeated words or sentences, only take the top 50 of"
class(a)
# Count characters
nchar(a)

dt <- "12 Jan2012"
len <- nchar(dt)
yr <- substr(dt,len+1-4, len)

# numeric
x <- c(12,334,454,1)
length(x)
nchar(x)

str <- "Piyush, Prakash"
6+1+1+3
nchar(str)

str1 <- c("Piyush", "Prakash")
nchar(str1)
# Change case of the string
a.cap <- toupper(a)
a.low <- tolower(a.cap)
# no proper case function



# combine strings : paste()
firstName <-"Ram"
lastName <- "Prajapat"
Name <- paste(firstName,lastName,sep="^^^^^^")
Name

paste0(firstName,lastName)

folder="C:\\Ram\\R for Data Science\\data"
file="binary.csv"
filename <- paste(folder,
                  file,
                  sep="\\")
dt <- read.csv(file=filename,header=T)

View(dt)

# split sting: strsplit

st <- strsplit(a, split = " ")

as.vector(st[[1]])
length(st[[1]])

st[[1]][3]

st[1]


#Scenario - count words

post.details <- "28k views Aggregate (count) rows that match a condition"
str1 <- strsplit(post.details," ")
length(str1[[1]])

# Counting characters in each element of a columns
str1.df <- data.frame(vars =str1)
names(str1.df) <-"words"
str1.df$chars <- nchar(as.character(str1.df$words))

class(str1.df$words)


# sub string/part of an sting : substr
# Scenario: find day from date string
date <- "13January2015"
day <- substr(date,1,2)
day
year <- substr(date,10,14)

class(year)

#Get std code from phone numbers
phone_number <- c('03561-220885','06641-226813', '03192-244661')
std_code <- substr(phone_number,1,5)


## ------------------- Date Functions----------------------------

#get system date
Sys.Date()

# Current date and time
date()

# Convert object to a date object
date1 <-"2015-01-12"
class(date1)
date2 <- as.Date(date1)
class(date2)

date3 <- "12-Jan-2014"
date4 <- as.Date(date3,format="%d-%b-%Y")
class(date4)
date5 <- "12-11-2014"
date6 <- as.Date(date5,format="%d-%m-%Y")
date6
class(date6)

# strptime concert character into POXIt format representing calendar dates and times.

dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
times <- c("23:03:20", "22:29:56", "01:03:30", "18:21:03", "16:56:26")
x <- paste(dates, times)
strptime(x, "%m/%d/%y %H:%M:%S")

# Convert character vectors into Dates vector
dates.format <- as.Date(dates,format="%m/%d/%y")
# month, year and week from a date
dates.format.df <- data.frame(dates.format,
                           month=months(dates.format),
                           weekday=weekdays(dates.format),
                           year=format(dates.format,"%Y"),
                           month.num =format(dates.format,"%m"))

# difftime Time intervals creation, printing, and some arithmetic.
date1 <-as.Date("1978-01-09")
current <- Sys.Date()

age.month <-as.integer(difftime(current,
                     date1,                     
                     unit="days")/30)

##----------------------- User Defined ---------------------------

pow <- function(base, pow){
  a =pow*log(base)
  exp(a)
}

pow(2,3.3)

pow(5,5)

?pow

library(help="datasets")
rm(pow)


# ---------------- Assignment 3 -------------------------

# Create a dataset of Custs
#                    ID as distinct numerical values
#                    Income is normal distribution M: 250,000 and SD:75000
#                    Gender 60% are Male in the sample (Hint runif and ifelse)
                     


