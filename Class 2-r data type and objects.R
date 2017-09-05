
library(arules)

library(lme4)

library(mtk)
# R Data Type
##### R objects
# If you overwrite something, there is no warning or error!
# Be careful of what you have named your objects, 
# as it might be overwritten inadvertently

#-------------------- Numeric--------------------
a<-7^3

class(a)
# Should get the answer as 'numeric'
mode(a)
# same answer! 

?mode
?class
## R basic data types
# 1. Numeric 
# is the basic data type in R

# Let us 'c' 
# c() or concatenate is a very powerful operator in R

num_var <-c(1,2,3,4,3.5,7.82)

class(num_var)
mode(num_var)

n1 <-10
n2 <-22
n3 <- c(n1,n2)
n3
n4 <- ls()
n4
mode(num_var)
class(num_var)

# ----------------- 2. Integer -----------------

1:5

intvar <-c(1:100)

mode(intvar)
class(intvar)

intar <- seq(1,100,by=3)
help(seq)

head(intar,10)

tail(intar)

?head
help(head)

# ---------------- 3. Character ---------------------
# 3. Character: character is the third data type
mystring<-"Logistic"

mystring<-'Logistic'
newstring<-'Stuff'
mystring <-c(mystring,newstring)

# extending the same array
mystring1<-c(mystring, "Multinomial","MOnday")

?rep

rep(2,20)

mystring<-rep(mystring,3)

num.rep <- rep(33,5)

num.rep <- rep(seq(2,10, by=3),5)

mode(mystring)
class(mystring)

d <- 1:10

# Categorical variables
cities<-c('Bangalore')

class(cities)

cities<-c(cities,'Mumbai')
cities<-c(cities,'Delhi')
cities <- c("Bangalore","Mumbai","Delhi","Chennai")
cities<-rep(cities, 2)

?rep
factor_cities<-as.factor(x=cities)

mode(factor_cities)

levels(factor_cities)

as.numeric(factor_cities)

class(factor_cities)
mode(factor_cities)
mode(cities)

gender <- c("F","M","M","F","F","M","F")
class(gender)
mode(gender)

gender1 <- as.factor(gender)


gender2 <- as.numeric(gender1)

gender3 <- as.numeric(gender)

# Just change it to numeric
x<-as.numeric(factor_cities)
x
class(x)
x1 <- as.character(factor_cities)
x1

class(x1)
## and for the sake of completion

#---------------------- 4.logical -----------------------------
# 4. Logical
y <- 5>6
y
y1 <-5>2
y1

logic_var <- as.logical(c(1, 0, 2, 5, 0, 0))
logic_var2 <- as.logical(c(1>0, 0>-8, 2>2, 5<=5, 0>=-3, 0<3))
## try it yourself
class(logic_var)
mode(logic_var)

a <- c("Ram",1,3,1)
a
class(a)
mode(a)

a1 <- as.numeric(a)

is.na(a)

is.na(a1)

d1 <- c("High",3,5)

d2 <- as.numeric(d1)

a[2]

a2 <- a1[!is.na(a1)]

#------------------------- 5.complex ---------------
# 5. Complex
cmplx_var <- 2 + 6i

class(cmplx_var)

sqrt(???1+0i)


## ------------------------- R Objects --------------------------------------
## ----- R objects -Vector, matrix, array,list, and data frame
# ---------------------  vector ---
v.num <- 50:80 # create
length(v.num)
v.num  #print

v.num[5] #access

ind <-10:15
v.num[ind]# access
v.num[10:15]

v.num1 <- v.num *2

v.num[10]
v.num1[10]

v.num1[2] <-100000


class(v.num1)

char <- LETTERS
char1 <- letters

letters[c(12,13)]

v.char <- LETTERS # create character vectors using existing vectors of english alphabets
v.char1 <- letters[2:7] # small alphabets with selection
c1 <- c("TT","MEL","Sd")

print(v.char1) #print

logi <- c(T,T,F,F)

logi[3]

# ------------------- matrix ---------------------------
## define a matrix
v <-seq(1,100,by=5) # vector

v[10]

length(v)

m1<-matrix(v,nrow=10)

m1[,1]

m1[,2]

m1[5,]

m1 #print
m1[6,1]# access elements
m1[4,2]

m11<-matrix(v,nrow=10,byrow = TRUE)

m1<-matrix(v,nrow=5,byrow = TRUE)

m2<-matrix(v,ncol =5)

m2<-matrix(ncol =5,v)

m2
class(m2)
mode(m2)
#character column

letters
LETTERS

letters <- c("a","b")

let = letters

letters <- let

letters[1:20]

m.char <- matrix(letters[1:20],5,byrow = TRUE)

m.char[,3]

m.char[5,]

# logical matrix
v.logic <- c(T,T,T,F,F,T,F,F,T,T)

v2 <- matrix(v.logic,nrow = 2,byrow = T)

m2<-matrix(v,nrow=5,byrow=T) # assign value first by rows
m2[,1] # first column
m2[5,] # select full rows

# Matrix Operations
m2 <-m2*100
m2
m2.tran <- t(m2)

matrix(v.logic,byrow = T)

class(x)

dim(m2)

?dim
v
# convert a vector into Matrix using dim function
dim(v) <- c(10,2)
v

### ------------------- Array --------------------
## Array: multi dimensional object

my.array <- array(1:48, dim=c(4,4,3))

my.array[2,,]

my.array[,3,]

my.array[,,3]

my.array[,,2] # access second matrix of 3,4 dimension
my.array[2,,] # second row of both the matrices
my.array[,2,] # second column of both the matrices
my.array[,2,]

### ------------------List-----------------------------
## define a list
list_var <- list(name="Fred", mynumbers=v, my=my.array, age=5.3)
## access member of a list
list_var[2] # copy of a member
list_var[[2]][1,]
list_var[[1]] # directly the elements
list_var[[2]]
list_var$my # access member

list_var[1]
list_var[[1]]

list_var$age

list_var$age

mm <-list_var[[2]][,2]

list_var$name

list_var$

vv <-list_var$my

list_var$mynumbers[,2]

list_var$my[,2,]

class(list_var)
mode(list_var)

###----------------- Data Frame -----------------------------
## define a data frame
# Data frame is equivalent to SAS dataset.
n = c(2, 3, 5) 
s = c("aa", "bb", "cc") 
b = c(TRUE, FALSE, TRUE) 
# create data frame from 3 vectors
df = data.frame(n, s, b)       # df is a data frame
View(df)

df$s
df[[2]]
df[,c(2,3)]
class(df$b)
class(s)

gender<- c("Male", "Female")
gender[1]
class(gender)
mode(gender)
gender[2]
s[1]
mode(df$s)


  
df1 = data.frame(n, s, b,stringsAsFactors =F)
class(df1$s)
class(df$b)

df1[3,]


df[,2]

df.mat <-as.matrix(df)
df.mat.2 <- data.frame(df.mat)

class(df.mat.2$b)
class(df$b)

class(df.mat.2$b)
# Convert Factor to numeric
df.mat.2$b1 <- as.numeric(df.mat.2$b)

"
- Interger
- numeric
- Character
- Logical 
- Complex

"


View(df) # view data frame
names(df) # column names

nrow(df) # number of rows
ncol(df) # number of columns

names(df) <- c("Num","Char","logical")
names(df)
View(df) # view data frame

class(df$Char)
mode(df$Char)

df$logical

df$Char

d <- as.Date("2013-01-10")
class(d)
mode(d)

install.packages("datasets")
library(datasets)
require(datasets)

library(help=datasets)

mdf <-mtcars

?head
head(mdf)

head(mdf,3)

nrow(mtcars)

mtcars[2,c(3,4)]

mtcars[c(21,31,11),c(2,3)]

mtcars[c(21,31,11),c("disp","hp")]


# built in data frame
summary(mtcars)
str(mtcars)
# print of data frame
head(mtcars) # defalut first 6 rows
tail(mtcars) # last 6 rows

head(mtcars,12) # custom number of rows

m.df <- mtcars[c(10,15),c(1,3)]

mtcars[12,c(2,5,7)] # by index
# Excluding column 3
mat.car<- mtcars[, -c(3)]

View(mtcars)

var1 <-mtcars$mpg

mtcars[10:15,c("wt","am")]

## -------------------Functions---------------------------


