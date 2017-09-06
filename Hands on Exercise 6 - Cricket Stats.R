#Exercise 6
# 1. Get data from http://stats.espncricinfo.com/ci/content/records/284248.html and work on the questions
#
# Plot 5 highest runs across the year
# 2. Find highest number of times a player had become the highest run scorer in a calendar year
# 3. Plot contribution of player country in becoming highest run getters EX: Australian players 20%
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

help(plot)
Top.Five.df <- NULL
Top.Five.df$Runs<- cricket_table.df$Runs[1:5]
Top.Five.df$Year<- cricket_table.df$Year[1:5]

plot(Top.Five.df$Year, Top.Five.df$Runs)

## Top Five Scorers End

## Number of times a player had become highest run getter in a calendar year

Year_store <- unique(cricket_table.df$Year)
countOfYears <- rep(0,length(Year_store))
countOfRuns <- NULL
Name_array <- NULL

cricket_table_yearsort.df<- cricket_table.df[with(cricket_table.df, order(Year, Player)), ]
View(cricket_table_yearsort.df)

for (i3 in 1:length(Year_store))
{
  if (!(is.na(Year_store[i3])))
  {
    for (i4 in 1:nrow(cricket_table_yearsort.df))
    {
      if (!(is.na(cricket_table_yearsort.df$Runs[i4])))
      {
        if (cricket_table_yearsort.df$Year[i4]==Year_store[i3])
        {
          countOfRuns[i3] <- cricket_table_yearsort.df$Runs[i4]
          Name_array[i3] <- cricket_table_yearsort.df$Player[i4]
        }  
        
      }  
      
    }
  }  
}

countOfRuns
Name_array

## End of Number of times a player had become highest run getter in a calendar year

##Start of Contribution ofplayer country in becoming highest run getters

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


## End of Contribution ofplayer country in becoming highest run getters