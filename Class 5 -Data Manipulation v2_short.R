
# -----------------------  Read data

# set up library
setwd("C:\\Ram\\R for Data Science\\data")
# read data
prd_spend <-read.csv(file="prod_spend.csv",
                     head=T)
names(prd_spend)
str(prd_spend)
summary(prd_spend)


# ------------------------  Add column

# Scenario 1: Prod Group: Reward and Non-Reward
table(prd_spend$Prod_Code)


####------------------------------ Rename Column Name

# get existing column names
names(prd_spend)

colnames(prd_spend)
rownames(prd_spend)
# Change Name of a column using index
colnames(prd_spend)[5] <-"product.grouping"
colnames(prd_spend)


#### ------------------------------------ KEEP and DROP a column

# create a duplicate data frame
prd_spend1 <-prd_spend
names(prd_spend1)

# keep a list of columns


prd_spend1 <- prd_spend1[,c(1,2)]
prd_spend1 <- prd_spend1[,c("Balance","Spend_Value")]
names(prd_spend1)

# drop one column using index
prd_spend1 <-prd_spend
names(prd_spend1)
prd_spend1 <- prd_spend1[,-2] # we have to sure of the index
names(prd_spend1)
# drop column using variable name
prd_spend1 <- prd_spend1[,!names(prd_spend1) %in% c("Spend.Volume","Profit")]
names(prd_spend1)
# drop a list of columns
prd_spend1 <-prd_spend
names(prd_spend1)
prd_spend1 <- prd_spend1[,!names(prd_spend1) %in% c("sqr_spend","sqroot_spend",     "exp_spend" )]
names(prd_spend1)


#### -------------------------- Change Type of  a column

# Cut and Chanding Factor to Character
prd_spend$spend_level <-cut(prd_spend$Spend_Value,
                            breaks=c(min(prd_spend$Spend_Value),500,1000,2000,max(prd_spend$Spend_Value)),
                            labels =c("0-500","500-1000","1000-2000","2000-High"))

class(prd_spend$spend_level)
mode(prd_spend$spend_level)
table(prd_spend$spend_level)


prd_spend$prd_grp <- ifelse( prd_spend$Prod_Code =="FB" | prd_spend$Prod_Code == "QFF","Reward","Non Reward" )

table(prd_spend$prd_grp)

# using %in%

prd_spend$prd_grp <- ifelse( prd_spend$Prod_Code %in% c("FB","QFF"),"Reward","Non Reward" )
table(prd_spend$prd_grp)


##  -------------------Row or Observation Changes ------------


# create index
hundreth <- seq(from=2,to=nrow(prd_spend),by=100)
head(hundreth)
# Scenario 1
prd_spend.even <-prd_spend[hundreth,]

nrow(prd_spend.even)
## ---
names(prd_spend)
summary(prd_spend$Balance)# scenario 2
names(prd_spend)
# Select customers which have over median spend
spend_over_median <- prd_spend[prd_spend$Balance>4940,]
# Two condition - & element wise comparison and && overall comparison
prd_select <-prd_spend[prd_spend$Balance <100 & prd_spend$Spend_Value>200,]
# Use which function 
prd_select <-prd_spend[which(prd_spend$Balance <100 & prd_spend$Spend_Value>200),]

# Scenario 3: randomly select 1000 rows
sample_index <-sample(1:nrow(prd_spend ), 1000,
                      replace=FALSE)
prd_spend.sample <- prd_spend[sample_index,]
prd_random <- prd_spend [sample(1:nrow(prd_spend ), 1000,
                                replace=FALSE),]

summary(prd_spend)
summary(prd_random)

both.row.col <-prd_spend[1:10,c(2,5)]

## Select both observations and variables
prd_var_obs <- subset(prd_spend,
                      prd_spend$Balance <100 & prd_spend$Spend_Value>200,
                      select=c("Balance","Spend_Value"))

# Find - way to drop variables using subset function

### Sort or order

order <-order(prd_spend$Spend_Value)

prd_spend[875,]

prd_spend$Spend_Value[875]

max(prd_spend$Spend_Value)
min(prd_spend$Spend_Value)

prd_spend.ordered <- prd_spend[order(prd_spend$Spend_Value, decreasing =T),]

### Descening order
prd_spend.ordered <- prd_spend[order(prd_spend$Spend_Value,decreasing =T),]
prd_spend.ordered <- prd_spend[order(prd_spend$Spend_Value,decreasing =F),]
### Descening order with multiple columns
prd_spend.ordered <- prd_spend[order(prd_spend$Prod_Code,prd_spend$Spend_Value,decreasing =T),]
### Multiple Columns with different ordar
prd_spend.ordered1 <- prd_spend[order(-prd_spend$Spend_Value,prd_spend$Prod_Code),]

### arrange function from plyr

library(plyr)
names(prd_spend)
prd_spend.ordered2 <- arrange(prd_spend,
                              desc(Prod_Code),
                              Spend_Value)

### Aggregation



install.packages("sqldf")
library(sqldf)
names(prd_spend)
#Average spend by product
avg_prd <- sqldf('select Prod_Code,
                 avg(Spend_Value) as avg_spend_val,
                 sum(Spend_Value) as sm_spend_val
                 from prd_spend
                 group by Prod_Code')


#aggregate function
?aggregate
avg_prd1 <- aggregate(prd_spend$Spend_Value,
                      by=list(prd_spend$Prod_Code), 
                      data = prd_spend, 
                      mean)
# r average two columns
prd_spend$bal_grp <- cut(prd_spend$Balance, breaks=c(-15000,0,2000,5000,6000,8000,85000))

table(prd_spend$bal_grp)

avg_prd2 <- aggregate(prd_spend$Spend_Value,
                      by =list(prd_spend$Prod_Code,prd_spend$bal_grp), 
                      data = prd_spend, 
                      mean)

avg_prd2 <- aggregate(Spend_Value~Prod_Code+bal_grp, 
                      data = prd_spend, 
                      mean)

#Average spend by product
avg_prd3 <- sqldf('select Prod_Code,
                  bal_grp,
                  avg(Spend_Value) as avg_spend_val,
                  sum(Spend_Value) as sm_spend_val
                  from prd_spend
                  group by Prod_Code, bal_grp')

# Month and Year Sales
sales <-read.csv(file="weekly.sales.csv",
                 stringsAsFactors =F,
                 head=T)
names(sales)
class(sales$Date)
head(sales$Date)
sales$Date <- as.Date(sales$Date, format="%m/%d/%Y")
sales$month <-format(sales$Date,"%b")
sales$year <- format(sales$Date,"%Y")

#Sales by month and year
sales.month.year <- sqldf('select year,
                          month,
                          sum(Sales) as sales
                          from sales
                          group by year,
                          month')
table(sales.month.year$year)

# Reference

#http://dni-institute.in/blogs/r-quiz-data-frame-manipulations/
#http://dni-institute.in/blogs/data-frame-manipulations-in-r/


# -----------  Assignment 4---------------------------------

# Analysis of  student performance in Math by downloading https://archive.ics.uci.edu/ml/datasets/student+performance datasets
#  Q(1) Answer following questions - 
#         a) What is average Grades for Male and Female students
#         b) Which combination of Guardian and Sudent Gender has highest Grades for G1, G2 and G3

#  Q(2) Students Absences (variable:absences) can be broken into 4/5 groups each group has same % students
# Find average Grades for different level of absences


