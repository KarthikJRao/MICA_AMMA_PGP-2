#Exercise 2
# 1. Download a CSV file from https://data.oecd.org/healthstat/infant-mortality-rates.htm and read the CSV file and remove the last variable
#
# 2. Read 2 table from html page https://en.wikipedia.org/wiki/India–Pakistan_cricket_rivalry and find the number of ODI matches won by India
#

library("xml2")
library("rvest")
getwd()
setwd("C:\\YYYYYY\\AMMA 2017\\Data\\data_2017")
input_csv.df<-read.csv("DP_LIVE_02072017055613917.csv",header=T)
class(input_csv.df)

input_csv.df<-input_csv.df[,-8]
View(input_csv.df)





###### Number of ODI matches won by INDIA ####


cricket_link = "https://en.wikipedia.org/wiki/India%E2%80%93Pakistan_cricket_rivalry"
cricket_file=read_html(cricket_link)
cricket_table=html_nodes(cricket_file,"table")
cricket_table_final<-html_table(cricket_table[2],fill=TRUE)
View(cricket_table_final)
class(cricket_table_final)
cricket_table.df <- as.data.frame(cricket_table_final)
View(cricket_table.df)
NumberODI = cricket_table.df$ODI[2]

NumberODI

