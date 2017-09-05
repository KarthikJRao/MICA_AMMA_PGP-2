#Exercise 6
#Get data from http://stats.espncricinfo.com/ci/content/records/284248.html and work on the questions
#
#Plot 5 highest runs across the year
#Find highest number of times a player had become the highest run scorer in a calendar year
#Plot contribution of player country in becoming highest run getters EX: Australian players 20%
#

library("xml2")
library("rvest")
cricket_link = "http://stats.espncricinfo.com/ci/content/records/284248.html"
cricket_file=read_html(cricket_link)
cricket_table=html_nodes(cricket_file,"table")
cricket_table_final<-html_table(cricket_table[1],fill=TRUE)
View(cricket_table_final)
class(cricket_table_final)
cricket_table.df <- as.data.frame(cricket_table_final)

cricket_table.df[with(cricket_table.df, order(Runs, Player)), ]

View(cricket_table.df)

Top_Five_Scorers<-c(cricket_table.df$Player[1], cricket_table.df$Player[2], cricket_table.df$Player[3], cricket_table.df$Player[4], cricket_table.df$Player[5])
Top_Five_Scorers

## Top Five Scorers End

PAK=0
AUS=0
IND=0
SL=0
ENG=0
WI=0
SA=0

for(i in 1:length(cricket_table.df$Player))
{
  if(grepl("PAK",cricket_table.df$Player[i]))
    PAK=PAK+1
}

PAK

for(i in 1:length(cricket_table.df$Player))
{
  if(grepl("AUS",cricket_table.df$Player[i]))
    AUS=AUS+1
}

AUS
for(i in 1:length(cricket_table.df$Player))
{
  if(grepl("INDIA",cricket_table.df$Player[i]))
    IND=IND+1
}

IND
for(i in 1:length(cricket_table.df$Player))
{
  if(grepl("SL",cricket_table.df$Player[i]))
    SL=SL+1
}

SL
for(i in 1:length(cricket_table.df$Player))
{
  if(grepl("ENG",cricket_table.df$Player[i]))
    ENG=ENG+1
}

ENG
for(i in 1:length(cricket_table.df$Player))
{
  if(grepl("WI",cricket_table.df$Player[i]))
    WI=WI+1
}

WI
for(i in 1:length(cricket_table.df$Player))
{
  if(grepl("SA",cricket_table.df$Player[i]))
    SA=SA+1
}

SA

Total_country_appearances = PAK + AUS + IND + WI + ENG + SA + SL

PAK_ratio = 100*PAK/Total_country_appearances
AUS_ratio = 100*AUS/Total_country_appearances
IND_ratio = 100*IND/Total_country_appearances
SL_ratio = 100*SL/Total_country_appearances
WI_ratio = 100*WI/Total_country_appearances
ENG_ratio = 100*ENG/Total_country_appearances
SA_ratio = 100*SA/Total_country_appearances


