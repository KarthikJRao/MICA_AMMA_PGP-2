#Exercise 1
# 1. Use "women" dataframe in package 'datasets'. Find the number of women with height more than average, but weight less than average.
#
# 2. Create a data frame of 15 Indian cities and their Create a data frame of 15 Indian cities and their population size. You could refer cities and population from this page: http://worldatlas.com/articles/the-biggest-cities-in-india.html
#

getwd()

setwd("C:\\YYYYYY\\AMMA 2017\\Data\\data_2017")

install.packages("data.table")
install.packages("datasets")
View(women)
htavg = mean(women$height)
wtavg = mean(women$weight)
n = 0
for(i in 1:length(women$height))
  if(women$height[i]>htavg & women$weight[i]<wtavg) 
    n=n+1
n

####### or ...#######
result=women[(women$height>htavg & women$weight<wtavg),]
View(result)



getwd()

setwd("C:\\YYYYYY\\AMMA 2017\\Data\\data_2017")

input_popcity.df<-read.csv("Population city data.csv",header=T)
View(input_popcity.df)



######### or..... ##########
install.packages("rvest")
install.packages("xml2")
library("xml2")
library("rvest")
city_link = "http://www.worldatlas.com/articles/the-biggest-cities-in-india.html"
city_file=read_html(city_link)
city_table=html_nodes(city_file,"table")
city_table_final<-html_table(city_table[1],fill=TRUE)
View(city_table_final)