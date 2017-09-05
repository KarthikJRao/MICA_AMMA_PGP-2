
rm(spend.jan14)
month <- rep("Jan",5)
spend.jan14 <- data.frame(custID =c(11,12,13,14,15),Spend=rnorm(5,250,30),month)
spend.feb14 <- data.frame(custID =c(11,12,14,16,17),Spend=rnorm(5,260,30),month=rep("Feb",nrow(spend.jan14)))

spend.jan14 
spend.feb14 
View(spend.feb14)
View(spend.jan14)

# Append multiple data frames
spend.jan.feb14 <- rbind(spend.jan14,
                         spend.feb14)

View(spend.jan.feb14)
# Names of columns
names(spend.jan.feb14)
## [1] "custID" "Spend"
# Count of rows and columns
ncol(spend.jan.feb14)
nrow(spend.jan.feb14)

# different number of columns

month <- rep("Jan",nrow(spend.jan14))
spend.jan14 <- data.frame(custID =c(11,12,13,14,15),Spend=rnorm(5,250,30))
spend.feb14 <- data.frame(custID =c(11,12,14,16,17),Spend=rnorm(5,260,30),month=rep("Feb",nrow(spend.jan14)))

spend.jan14 
spend.feb14 

# Append multiple data frames
spend.jan.feb14 <- rbind(spend.feb14,
                         spend.jan14
)


### ------------ Merge Data Frames------------------------

cust <- read.table(header=T, text='ID Age Sex Marital_Status
1001 56 F Married
                   1002 45 M Single
                   1003 34 F Single
                   1004 26 M Married
                   1005 47 M Single
                   ')

spend <-read.table(header=T, text='ID        Spend_Amt       Spend_Count 
                   1001    123     3
                   1002    345     5
                   1003    133     4
                   1004    3242    12
                   1006    536     6
                   ')

cust_spend <-merge(cust,
                   spend,
                   by="ID") # Intersection

View(cust_spend)
# Left
cust_spend_left <-merge(cust,
                        spend,
                        by="ID",
                        all.x=TRUE) ##x refers to the first object - cust, not spend
View(cust_spend_left)

# Right
cust_spend_right <-merge(spend,
                         cust,
                         by="ID",
                         all.x=T)
View(cust_spend_right)
cust_spend_right <-merge(cust,
                         spend,
                         by="ID",
                         all.y=T) ##y refers to the second object - spend

cust_spend_full <-merge(cust,
                        spend,
                        by="ID",
                        all.x=T,
                        all.y=T) ##x,y refer to the first and second objects, cust and spend

View(cust_spend_full)
cust_spend_full2 <- cust_spend_full

Missing.df<-cust_spend_full
Missing.df$age_missing<-is.na(Missing.df$Age)
Missing.df$Spend_Amt_missing<-is.na(Missing.df$Spend_Amt)

Non_missing.df<-Missing.df[!(Missing.df$age_missing | Missing.df$Spend_Amt_missing)]
View(Non_missing.df)

# SQL type joins in R



library(sqldf)

## inner join
sql.cust.spend <- sqldf("SELECT a.*,
                        b.*
                        FROM cust a
                        JOIN spend b USING(ID)
                        ")

sql.cust.spend [,"ID"]
names(sql.cust.spend )

## left join 
sql.cust.spend.left <- sqldf("
                             SELECT a.*,
                             b.*
                             FROM cust a
                             LEFT JOIN spend b USING(ID)
                             ")
## Right join
sql.cust.spend.right <- sqldf("
                              SELECT b.*,
                              a.*
                              FROM spend b
                              left JOIN cust a  USING(ID)
                              ")


# -------------------------   Assignment 5



#Emp Data

"
ID	ENAME	JOB	MGR	HIREDATE	SAL	DEPTNO
7839	BUSH	PRESIDENT	NA	17/11/1981	5000	10
7934	ELISON	CLERK	7782	23/01/1982	1300	10
7782	MERKEL	MANAGER	7839	9/6/1981	2450	10
7566	PUTIN	MANAGER	7839	2/4/1981	2975	
7369	THATCHER	CLERK	7902	17/12/1980	800	20
7788	CARNEGIE	ANALYST	7566	9/12/1982	3000	20
7876	FORD	CLERK	7788	12/1/1983	1100	20
7902	TOOSK	ANALYST	7566	3/12/1981	3000	20
7900	BUFFETT	CLERK	7698	3/12/1981	950	
7654	CHIRACK	SALESMAN	7698	28/09/1981	1250	30
7698	BLAIR	MANAGER	7839	1/5/1981	2850	30
7844	GATES	SALESMAN	7698	8/9/1981	1500	30
7521	WALTON	SALESMAN	7698	22/02/1981	1250	30
7499	BAROSSO	SALESMAN	7698	20/02/1981	1600	30

"

# Department data

"
DEPTNO	DNAME	LOC
10	ACCOUNTING	NEW YORK
20	RESEARCH	LONDON
30	SALES	PARIS
5	QUALITY	TOKYO

"
# Q(1): Combine teh above datasets and create a data which has departmnt name & location along with employee information

# Q(2): Find the average salary by location

# Q(3): Create a dataslet which has name of employee along with its manager name and find the manager which has the most direct reportees

# Q(4):  If salary increment across levels are as follow, what would be salary after the increment
"
Role	Increment
ANALYST	15%
CLERK	12.50%
MANAGER	10.20%
PRESIDENT	5.70%
SALESMAN	13.30%
"
