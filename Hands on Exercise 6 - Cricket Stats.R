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

cricket_table_yearsort.df<- cricket_table.df[with(cricket_table.df, order(Year, Player)), ]
View(cricket_table_yearsort.df)

yearsOcc <- as.data.frame(table(cricket_table.df$Year))
yearsOcc$MaxRuns <- rep(0,nrow(yearsOcc))
yearsOcc$MaxRunsPlayer <- rep("",nrow(yearsOcc))
for(i3 in 1:nrow(yearsOcc))
{
  
  maxRunsSubset <- subset(cricket_table.df,cricket_table.df$Year==yearsOcc$Var1[i3])
  yearsOcc$MaxRuns[i3] <- max(maxRunsSubset$Runs)
}

for (i4 in 1:nrow(yearsOcc))
{
  for (i5 in 1:nrow(cricket_table.df))
  {
    if(cricket_table.df$Year[i5]==yearsOcc$Var1[i4] && cricket_table.df$Runs[i5]==yearsOcc$MaxRuns[i4])
      yearsOcc$MaxRunsPlayer[i4] <- cricket_table.df$Player[i5]
  }  
  
}

playerMaxRunsTable <- as.data.frame(table(yearsOcc$MaxRunsPlayer))
max(playerMaxRunsTable$Freq)

print(playerMaxRunsTable)
print("Maximum number of times a player has got the highest number of runs in a year")
print(max(playerMaxRunsTable$Freq))

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

PAK_ratio = round(100*PAK/Total_country_appearances,2)
AUS_ratio = round(100*AUS/Total_country_appearances,2)
IND_ratio = round(100*IND/Total_country_appearances,2)
SL_ratio = round(100*SL/Total_country_appearances,2)
WI_ratio = round(100*WI/Total_country_appearances,2)
ENG_ratio = round(100*ENG/Total_country_appearances,2)
SA_ratio = round(100*SA/Total_country_appearances,2)

Country_Percent.df <- NULL
Country_Percent.df$Country <- as.factor(c("PAK","AUS","IND","SL","WI","ENG","SA"))
Country_Percent.df$Percentage <- c(PAK_ratio,AUS_ratio,IND_ratio,SL_ratio,WI_ratio,ENG_ratio,SA_ratio)

plot(Country_Percent.df$Country,Country_Percent.df$Percentage)
## End of Contribution ofplayer country in becoming highest run getters