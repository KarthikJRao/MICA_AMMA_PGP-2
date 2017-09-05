#--------------------------------------------------------------------#
#             Visaulization using R                                  #
#                                                                    #
#--------------------------------------------------------------------#

# Our first plot
par(mfrow=c(2,2))
x <- c (1, 2, 3, 4, 5)
y <- c (1, 5, 3, 2, 0)
windows()
par(mfrow=c(2,1))
plot (x, y, main="Top Left")
plot (x, y, 
      main="Top Right",
      type="b")
par(mfrow=c(1,1))

## -------------- Explore relationship between two variables----------------------------
#  How Height of Children is linked to Height of Parents
#  A scatter plot  - a plot to show the relationship between two quantitative/numeric variables

install.packages("psych")
library(psych)
library(help=psych)

data(galton,package = "psych")

head(galton)

plot(galton$parent,
     galton$child)

# Add elements to the graph

plot(galton$parent,
     galton$child,
     xlab = "Height of Parent",
     ylab= "Height of Children",
     main=" Relationship between Parent and Children Heights")
# Changes in Plotting Characters
plot(galton$parent,
     galton$child,
     xlab = "Height of Parent",
     ylab= "Height of Children",
     main=" Relationship between Parent and Children Heights",
     pch=17,
     col="red")

# Fit a line between X and Y  or Height of Parent and Children

abline(lm(galton$parent~galton$child),
       col = "blue",
       lwd=4,
       lty=5)

# ------------------- Time Series Plot or Line Chart -------------------------

# Scenario - How Average Month Temprature is changing across years
# nottem                  Average Monthly Temperatures at Nottingham,1920-1939

library(help = "datasets")
data(nottem,package = "datasets")

head(nottem)
class(nottem)

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
?hist 
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


# Barplot: Plot Numeric Values for each of categorical values

# Read data
setwd("C:\\YYYYYY\\AMMA 2017\\Data\\data_2017")

prd_spend <- read.csv(file = "prod_spend.csv")


names(prd_spend)
# Avg Balance by Product
avg.spend.prd <- aggregate(prd_spend$Spend_Value,
                           by=list(prd_spend$Prod_Code),
                           mean)
names(avg.spend.prd)
names(avg.spend.prd) <-c("Product","Avg.Spend")

barplot(height=avg.spend.prd$Avg.Spend,
        names.arg = avg.spend.prd$Product,        
        xlab="$ Spend",
        ylab="Product",
        main="Spend by Product",
        col="blue",
        border="white",
        horiz=T)
box()

?barplot

movies_data <- read.table("movies.tab", sep="\t", header=TRUE, quote="", comment="")

library(sqldf)
yearwise_anime<-sqldf("select year, count(*) as Num_movies 
                      from movies_data where Animation=1
                      group by year")
sum(movies_data$Animation)

sum(yearwise_anime$Num_movies)
barplot(height=yearwise_anime$Num_movies,
        col='deeppink4',
        names.arg=yearwise_anime$year,
        xlab="year",
        ylab="Count of Movies",
        main="Count of Movies across years")

# Stacked and Group Column Charts
library(help=datasets)
head(mtcars)
# Stacked
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, 
        main="Car Distribution by Gears and V/S",
        xlab="Number of Gears", 
        col=c("darkblue","red"),
        legend = rownames(counts))
# Goruped
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and V/S",
        xlab="Number of Gears",
        col=c("darkblue","red"),
        legend = rownames(counts),
        beside=TRUE)


# ---------------- Pie Chart : Plotting Proportion of categories -------------------------

# data
Label <- c("0-20","20-30","30-40","40-50","50-60")
Count <- c(16,395,315,161,68)
age <- data.frame(Label,Count)

# Pie Chart
pie(age$Count)

# Pie Chart
pie(age$Count,
    label=age$Label,
    radius=1,
    main="Customers by Age Group",
    col=c("red","blue","orange","green","black"),
    border="white",
    clockwise=T
)

Count.pct <- c(16,395,315,161,68)/sum(Count)
Count.pct.label = as.integer(Count.pct*100)
pie(Count.pct,
    label=Count.pct.label,
    radius=1,
    main="Customers by Age Group",
    col=c("red","blue","orange","green","black"),
    border="white",
    clockwise=T
)

#The pie3D( ) function in the plotrix package provides 3D exploded pie charts
install.packages("plotrix")
library(plotrix)
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")
pie3D(slices,
      labels=lbls,
      explode=0.1,
      main="Pie Chart of Countries ")


##################       ggplot  #################################################

housingUS <- read.csv(file="housingUS.csv")
names(housingUS)

head(housingUS)

housingUS$Home.Value <- gsub('$', '', housingUS$Home.Value, fixed = TRUE)
housingUS$Home.Value <- as.numeric(gsub(',', '', housingUS$Home.Value, fixed = TRUE))

housingUS$Structure.Cost <- gsub('$', '', housingUS$Structure.Cost, fixed = TRUE)
housingUS$Structure.Cost <- as.numeric(gsub(',', '', housingUS$Structure.Cost, fixed = TRUE))

# Histogram
ggplot(housingUS, aes(x = Home.Value)) +
  geom_histogram()

# Scatter plot
ggplot(subset(housingUS, STATE %in% c("MA", "TX")),
       aes(x=Date,
           y=Home.Value,
           color=STATE))+
  geom_point()



# reference:http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html



# ---------------------------------  Assignment  6 ----------------

# Get data from http://stats.espncricinfo.com/ci/content/records/284248.html and wok on below questions
# Q(1): Plot 5 Highest Run across Years

# Q(2): Find higest number of a times a player has become highest run scorer in a calendar year

# Q(3): Plot contribution of player country in becoming highest run getters. e.g. 20% Australian players
