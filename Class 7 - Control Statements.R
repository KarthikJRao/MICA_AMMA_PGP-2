## Control Statements in R

# IF
# IFELSE
# FOR
# WHILE
# Lapply, sapply, and apply 
# Functions

#           ------- Operators --------------

#         && Logical AND as binary operator
#         || Logical OR
#          ! Logical NOT

a <- T
b <- F

a && b

a & b

a<- c(12,3,15,3)
b <-c(2,12,22,4)

a>10 & b <15

a>10 && b <15


a>10 || b >10
a>10 | b >10

a1<- a> 5
a1
!a1

a <-c(T,F,T)
b <- c(T,F,F)

a&b

a&&b
# ---------------- IF ELSE Statement 

a = 5
if(a>3){
  #print(paste(a,"is greater than 3"))
  print(a,"is greater than 3")
}else{
  print(paste(a,"is less than 3"))
}

# Odd even
EvenOdd<-function(numVal){
  if(numVal%%2 == 0){
    print(paste(numVal,"is even.",sep=" "));
    }else{
      print(paste(numVal,"is odd.", sep=" "));
      }
}

EvenOdd(200)
#  ---------------- IFELSE -------------------------
#   ifelse(test, yes, no)

a <- c(rbinom(10,4,0.5))



ifelse(a>2,"Yes","No")
# replace negative spend value with 0
# set up library
setwd("C:\\Ram\\R for Data Science\\data")
# read data
prd_spend <-read.csv(file="prod_spend.csv",
                     head=T)
str(prd_spend)
summary(prd_spend$Spend_Value)
prd_spend$Spend_Value <- ifelse(prd_spend$Spend_Value <0,0,prd_spend$Spend_Value)
summary(prd_spend$Spend_Value)

## -------------------- FOR ---------------------------
#  Perform set of activities for all elements of a vector or an index

a <- c(5,7,13,15)

for (item in a) {
  # Calculate square
  b <-item*item
  print(b)
}


a[3]
for (j in 1:length(a)){
    print(a[j])
  print(a[j]*a[j])
}



for(i in 1:100){
  b<-i**2
  if(i==1){
    df <- data.frame()
  }
  df <-rbind(df,b)
  names(df) <-"Square"
  df
}
View(df)

## --------------- WHILE ---------------------------
# do something while a logical statement is true.
# while (statement) {
#   list of actions..
#   }

a =5
while(a>0){
  print(a)
  a=a-1
}

a =5
while(a <10){
  print(a)
    a=a+1
}

a=5
while(a <10){
  if(a <7){
    b =a**2
    print(b)
  }else{
    print(a*2)
  }
  a=a+1
}

# -------------- Inbuilt Loop based functions: Lapply, sapply, and apply

?apply

df <- data.frame(a=seq(11,20,1),
                 b=rnorm(10,mean=50,sd=15),
                 c=rbinom(10,5,0.15)
)
# Finding mean of all columns

summary(df)
# Get mean of each columns of a data frame
mean.row <- apply(df,2,mean)

mean.row <- apply(df,2,function(x) quantile(x,0.95))

quantile(df$a,probs = 0.95)
# Get mean of each row of a data frame
df$mean.col <- apply(df[,c(1,2,3)],1,mean)
df$max.col <- apply(df[,c(1,2,3)],1,max)


max.row <- apply(df,2,max)
max.col <- apply(df,1,max)

#
?sapply

mean.s <- sapply(df,mean)
mean.l <- lapply(df,mean)

# find missing values for each of the variables
df1 <- data.frame(var1 = c(12,13,465,676,323,546,NA,12,32,45),
                  var2 = c(15,6,23,46,NA,124,32,46,35,NA) )

mean(df1$var1, na.rm=T)

miss.cnt <- sapply(df1,function(x) paste(100*sum(is.na(x))/length(x),"%"))
View(miss.cnt)

avgVal <- function(Vect){
  len <- length(Vect) - sum(is.na(Vect))
  sum <- sum(Vect,na.rm =T)
  sum/len
}
v1 <- c(15,6,23,46,NA,124,32,46,35,NA)

avgVal(v1)
mean(v1,,na.rm =T)
m1 <- sapply(df1, avgVal)

# ------------ Function --------------------------------------

# Find mean of each of the data frame columns
mean.all <- function(df){
  # initialize data frame
  mean.df <- data.frame(VarName=character(),
                        meanValue=numeric())
  for(c in names(df)){
    # Check if column is numeric
    if(is.numeric(df[,c])){
      m <- mean(df[,c], na.rm=T)
      temp.df <- data.frame(VarName=c,
                            meanValue=m)
      mean.df <-rbind(mean.df,temp.df)
    }
  }
  mean.df
}

mm <- mean.all(prd_spend)

# Validate with sapply
sapply(prd_spend, mean)

barplot(german.sumaary$VarMean)


hist(german$V4)



