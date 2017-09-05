
##################  Reading Data  #########################


## Reading data from program editor

## Create a matrix
input.matrix <- matrix(1:100, ncol = 5)
## See the data
input.matrix         # we see that a matrix with 5 columns and 20 rows has been formed

# Uniform Random Numbers between 1 to 1000

rr <-1000*runif(2)

class(rr)

rr<-as.integer(1000*runif(2))

class(rr)
mode(rr)

unif <- as.integer(1000*runif(200000))

plot(unif)

head(unif)

hist(unif)

# Scenario 

# Application: 300,000
# Selection: 60,000

Prob <- 60000/300000

Appl.num <- seq(1,300000,1)

head(Appl.num)

selected <- sample(Appl.num,60000,replace = F)

selected.df <- data.frame(selected)


LETTERS[1:4]

sample(letters[1:4], 10, replace = T)

## Create a Data Frame
input.df <- data.frame(ID = 1:10, 
                       Class = sample(letters[1:4], 10, replace = TRUE),
                       Value = seq(1:5))
## View the data frame
View(input.df)
# we see that the data frame has 3 columns named ID, Class, and Value
# The data frame also has 10 rows of data

## See the column names
colnames(input.df)
names(input.df)

row.names(input.df)
## See the dimensions
dim(input.df)

## Set the Working Directory
getwd()

## Reading data from Comma separated file (csv)
input_csv.df <- read.csv(file="C:\\YYYYYY\\AMMA 2017\\Data\\data_2017\\binary.csv")
View(input_csv.df)
# reset directory and read file
setwd("C:\\YYYYYY\\AMMA 2017\\Data\\data_2017")
input_csv.df <- read.csv("binary.csv",header = T)

## See the structure of the data
str(input_csv.df)

sum(input_csv.df$gre)
?sum

## Reading Date Values
input_wthdt.df <- read.csv("binary_withdate.csv")
str(input_wthdt.df)

tab <- data.frame(table(input_wthdt.df$application_date))
sum(tab$Freq)
View(tab)
# we see the date field has been read as factor
summary(input_wthdt.df$gre)
# read Date as character
input_wthdt.df <- read.csv("binary_withdate.csv", stringsAsFactors = F)
str(input_wthdt.df)
View(input_wthdt.df)
mode(input_wthdt.df$application_date)
## working with dates
d <-"2004-12-03"

d
class(d)
d1 <- as.Date(d)
class(d1)
mode(d1)
## find system date
s <-Sys.Date()
s
## Current date and time
c <-date()
c

# reading dates with other than defaul format
# d - Day e.g 1, 2 etc
# m - month
# b - month /Jan, Feb
# B - Month January
# y - 2 digit year
# Y - 4 Digit year

d2 <-as.Date("12-January-2012",format="%d-%B-%Y")

d2
class(d2)

format(d2,"%B")


d3 <-as.Date("12-February-2012",format="%d-%B-%Y")
d3 <-as.Date("12-February-12",format="%d-%B-%y")

d4 <-as.Date("12-12-12",format="%d-%m-%y")

d4
d2

dd <- format(d2,"%d/%B/%Y")


## Calculate age 
dob <-as.Date("12-Jan-1983",format="%d-%b-%Y")
dob
age <- difftime(Sys.Date(),dob,units="days")
as.integer(as.numeric(age)/365)

## Correct the date format
input_wthdt.df$application_date1 <- as.Date(input_wthdt.df$application_date, format="%m/%d/%Y")

str(input_wthdt.df)

## Read the data specifying the Class of the data
input_wthdt.df1 <- read.csv("binary_withdate.csv", colClasses=c(application_date = 'myDate'))
str(input_wthdt.df1)


## Read data from the web
## You can directly read a file directly from the internet by specifying the URL
input_webdata.df <- read.table("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")
str(input_webdata.df)

input_webdata.df$

## Read the first few records of a dataset
head(input_webdata.df)
## The first 6 rows of data have been displayed

## Ques 1 : What is we need to display the first 10 rows instead?
## Ques 2 : What if we want to display the last few rows?

csv_file <- read.table(file="binary.csv",
                                header = TRUE, 
                                sep = ',')
str(csv_file)
## Read a Tab Delimited file
input_tabdlmtd.df <- read.table(file="tab_delimited_data.txt",
                                header = TRUE, 
                                sep = '\t')

head(input_tabdlmtd.df)

## We can use the same functions as with a csv file 
## to read dates and modify formats with a tab-delimited file as well


input_dollar.df <- read.table(file="dollar_delimited_data.txt",
                                header = TRUE, 
                                sep = '$')

View(input_dollar.df)
## Read SAS dataset Data
install.packages("sas7bdat")

library(sas7bdat)

library(help=sas7bdat)

bank_ins = read.sas7bdat("bank_additional_full.sas7bdat")
names(bank_ins)
str(bank_ins)

library(foreign)
library(help=foreign)

# Read SPSS file
spss <- data.frame(read.spss("p004.sav"))

names(spss)
table(spss$PROTEIN)

library(help=sqldf)

sum.tb <- sqldf("select PROTEIN,count(*) 
                        from spss 
                 where PROTEIN>1
                group by PROTEIN")

## Read Data From Facebook

install.packages(c("Rfacebook","RCurl","rjson"))
install.packages("twitteR")
library(Rfacebook)
library(RCurl)
library(rjson)
library(twitteR)

library(help=Rfacebook)
# connecting to Facebook

#https://developers.facebook.com/tools/explorer

accessToken <-"EAACEdEose0cBAM4A6BcQfQjLzaH6pxd47BOPsZBBrRgqhtX2B1fE5W2yoKKCuU0VXgUkQ8YqaLHyD61ZBfp7hH1yYeESEmkH4qL6Bnv2zLv9Nmm7EwtWil0RgXCg9nfOKq6xypZB3FAb8vU6VDihplakJEVJZBiTxL5MZCyl27tQW3qn6AFOwjqBiVypGHhsZD"

# Get data from a company page
flipkartPage <-getPage(page="flipkart",
                       token=accessToken,
                       n=10)

flipkartPage <-getPage(page="flipkart",
                       token=accessToken,
                       n=150)


View(flipkartPage)

teslaPage <-getPage(page="tesla",
                       token=accessToken,
                       n=10)


View(teslaPage)


##Twitter on R

api_key <- "c6MfJko2KyiV1P9W55u2jXmkd"

api_secret <- "nCVQrEU3sx74AGlmCBHIJWZJacyEEam4AJwhRt22IUdMS2v7Yp"

access_token <- "707591447726305280-Y28HU8d677t2zIPSGSPoWf3BjTBXRaN"

access_token_secret <- "5cek9he8B5RcQEgmKe5KOJ74kfhcU5lQxrcuJdcFcl0Wd"

setup_twitter_oauth(api_key,api_secret)
vkohliTwitterpage <- getUser("imVkohli","707591447726305280-Y28HU8d677t2zIPSGSPoWf3BjTBXRaN")

View(vkohliTwitterpage)
Searchresults<-searchTwitter("#", n=10)
head(Searchresults)
##################  Writing Data  #########################

one_row <-bank_ins[,3]

bank_ins.smpl <- bank_ins[1:1000,]
## Save the R object
save(bank_ins,file="bank_ins.smpl.Rda")
# remove
rm(bank_ins.smpl)
rm(bank_ins)
## Load the data back

names(bank_ins)

load("bank_ins.smpl.Rda")

## install and load datasets package
install.packages("datasets")
require(datasets)

library(help="datasets")
## Save the data as a csv file

tt <- mtcars

names(tt)

write.csv(mtcars, "mtcars.csv")

tt$carmodel <- row.names(mtcars)
row.names(tt) <- NULL

write.csv(tt, "C:\\Ram\\R for Data Science\\data\\mtcars.csv",
          row.names=F
)


## Save the data as a Tab Delimited file
write.table(mtcars, "mtcars.txt", sep = '%', quote = FALSE, row.names=F)

## write to file without column names

write.table(mtcars, 
            "mtcars_noheader.txt", 
            sep = '%', 
            quote = FALSE, 
            row.names=F,
            col.names = F)

write.csv(mtcars, 
            "mtcars_noheader1.txt", 
            quote = FALSE, 
            row.names=F,
            col.names = F)

write.table(mtcars, 
            "mtcars_noheadercsv.csv", 
            sep = ',', 
            quote = FALSE, 
            row.names=F,
            col.names = F)


attach()

names(male)

summary(Age)

rm(Age)

Age

mean(male$Age)

attach(male)

mean(Age)

myfile <- read.table(text="MyName Age
                           Ram 38
                           Shyam 25", header=T)

write.csv(myfile,file="myfile.csv")

write.table(myfile, 
            "myfile.txt", 
            sep = '$', 
            row.names=F,
            col.names = F)

students <- data.frame(Name= c("Ram","Dinesh","Mona","Sona"),
                      Gender=c("Male","Male","Female","Female"),
                      Age =rnorm(4,50,20))
View(students)

write.csv(students,
          file="students.csv",
          row.names = F)
getwd()
# ----------------- Reference --------------------------
# http://dni-institute.in/blogs/read-large-files-into-r/


# Assignment 2

# Q(1):  Down a  csv file from https://data.oecd.org/healthstat/infant-mortality-rates.htm and read the csv file
# And remove the last variable 


# Q(2): Read 2 table from html page https://en.wikipedia.org/wiki/India%E2%80%93Pakistan_cricket_rivalry
# Find number of ODI Matches won by India

