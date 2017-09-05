# Exercise 4
# Analysis of Student Performance in Math by downloading: https://archive.ics.uci.edu/ml/datasets/student+performance datasets, unzip to a folder on your system and read the data.
#Q(1) Answer following questions
#What is average Grades for Male and Female students
#b) Which combination of Guardian and StudentGender has highest Grades for G1,G2 and G3
#Q(2) Students Absences (variable: absences) can be broken into 4 or 5 groups each group has almost same % students (hint cut and quantile)
#And then find average Grades for different level of absences
#

getwd()
setwd("C:\\YYYYYY\\AMMA 2017\\Data\\data_2017")

d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students
View(d1)
View(d3)
class(d3)

female_mean_grade=0
male_mean_grade=0

for(i in 1:length(d3$G3.x))
{
  if(d3$sex[i]=="F")
  {  female_mean_grade = female_mean_grade + d3$G3.x[i] + d3$G3.y[i]
  }
else
{
male_mean_grade = male_mean_grade + d3$G3.x[i] + d3$G3.y[i]
}
}
  male_mean_grade = male_mean_grade/length(d3$G3.x)
  female_mean_grade = female_mean_grade/length(d3$G3.x)
  
  male_mean_grade
  female_mean_grade  ####Part 1 ends here
  

## PARt 2
  
  max_G1 = max(d3$G1.x + d3$G1.y)
  max_G2 = max(d3$G2.x + d3$G2.y)
  max_G3 = max(d3$G3.x + d3$G3.y)
  
  for(j in 1:length(d3$G3.x))
  {  
    if(d3$G1.x[j]+d3$G1.y[j] == max_G1)
      {Guardian_G1 = d3$guardian.x[j]
      Gender_G1 = d3$sex[j]
    }
  }  
  Guardian_G1
  Gender_G1
  
  for(j in 1:length(d3$G3.x))
  {  
    if(d3$G2.x[j]+d3$G2.y[j] == max_G1)
    {Guardian_G2 = d3$guardian.x[j]
    Gender_G2 = d3$sex[j]
    }
  }  
  Guardian_G2
  Gender_G2
  
  for(j in 1:length(d3$G3.x))
  {  
    if(d3$G3.x[j]+d3$G3.y[j] == max_G1)
    {Guardian_G3 = d3$guardian.x[j]
    Gender_G3 = d3$sex[j]
    }
  }  
  Guardian_G3
  Gender_G3
  
  ## Q1 ends here ##
