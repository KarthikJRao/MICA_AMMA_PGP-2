## Exercise 5
# Q1. Combine the datasets Emp and Dept and create a dataframe which has department name & loocation along
# with employee information
# Q2. Find the average salary by location
# Q3. Create a dataset which has name of employee along with their manager name and find the manager who
# has the most direct reportees
# Q4. Salary increments are as follows - ANALYST - 15%, CLERK  -12.5%, MANAGER - 10.20%, PRESIDENT - 5.70%, SALESMAN - 13.30%
# Calculate the salaries after the increment

## STart of Q1

getwd()
setwd("C:/YYYYYY/AMMA/2017/Data/data_2017")
emp_CSV <- read.csv("Emp.csv")
dept_CSV <- read.csv("Dept.csv")
#,header= TRUE, sep= ";")

# Merge Employee and Dept by DeptNo #
emp_dept_merged <- merge(emp_CSV,dept_CSV,by="DEPTNO",all = TRUE)

## End of Q1

## Start of Q2

# Average salary by location #
N_EmpLoc <- c(rep(0,nrow(dept)))
salaryLoc <- c(rep(0,nrow(dept)))

for (i1 in 1:nrow(dept))
{
  for (i2 in 1:nrow(emp_dept_merged))
  {
    if (!is.na(emp_dept_merged$LOC[i2]))
    {  
      if (emp_dept_merged$LOC[i2]==dept$LOC[i1])
      {
        N_EmpLoc[i1] <- N_EmpLoc[i1] +1
        salaryLoc[i1] <- salaryLoc[i1] + emp_dept_merged$SAL[i2]
      }
    }    
  }  
}    

mean_salary_loc <- c(rep(0,nrow(dept)))
for (i3 in 1:nrow(dept))
{
  mean_salary_loc[i3] <- salaryLoc[i3]/N_EmpLoc[i3]
}

## End of Q2

## STart of Q3

Employee_IDS <- unique(emp_dept_merged$ID)
countOfEmpUnder <- rep(0,length(Employee_IDS))

for (i3 in 1:length(Employee_IDS))
{
  if (!(is.na(Employee_IDS[i3])))
  {
    for (i4 in 1:nrow(emp_dept_merged))
    {
      if (!(is.na(emp_dept_merged$MGR[i4])))
      {
        if (emp_dept_merged$MGR[i4]==Employee_IDS[i3])
        {
          countOfEmpUnder[i3] <- countOfEmpUnder[i3] + 1
        }  
        
      }  
      
    }
  }  
  
}

for (i5 in 1:length(Employee_IDS))
{
  if (countOfEmpUnder[i5]==max(countOfEmpUnder))
  {
    position <- i5
  }
} 

print("Manager with the highest number of employees")
print(emp_dept_merged$ENAME[position])

## End of Q3

## Start of Q4

Emp_csv.df<-read.csv("Emp.csv",header=T)
Dept_csv.df<-read.csv("Dept.csv",header=T)

Emp_Dept_merged.df=merge(Emp_csv.df,Dept_csv.df,by=c("DEPTNO"),all=TRUE)
Emp_Dept_merged_hike.df <- Emp_Dept_merged.df

for(i in 1:nrow(Emp_Dept_merged.df))
{
  if(!is.na(Emp_Dept_merged.df$JOB[i]))
  {
  if(Emp_Dept_merged.df$JOB[i]=="ANALYST")
  {
    Emp_Dept_merged_hike.df$SAL[i] = Emp_Dept_merged.df$SAL[i] + (0.15 * Emp_Dept_merged.df$SAL[i])
  }
  }
}

for(i in 1:nrow(Emp_Dept_merged.df))
{
  if(!is.na(Emp_Dept_merged.df$JOB[i]))
     {
  if(Emp_Dept_merged.df$JOB[i]=="CLERK")
  {
    Emp_Dept_merged_hike.df$SAL[i] = Emp_Dept_merged.df$SAL[i] + (0.125 * Emp_Dept_merged.df$SAL[i])
  }
  }
}

for(i in 1:nrow(Emp_Dept_merged.df))
{
  if(!is.na(Emp_Dept_merged.df$JOB[i]))
     {
  if(Emp_Dept_merged.df$JOB[i]=="MANAGER")
  {
    Emp_Dept_merged_hike.df$SAL[i] = Emp_Dept_merged.df$SAL[i] + (0.102 * Emp_Dept_merged.df$SAL[i])
  }
  }
}

for(i in 1:nrow(Emp_Dept_merged.df))
{
  if(!is.na(Emp_Dept_merged.df$JOB[i]))
  {
  if(Emp_Dept_merged.df$JOB[i]=="PRESIDENT")
  {
    Emp_Dept_merged_hike.df$SAL[i] = Emp_Dept_merged.df$SAL[i] + (0.057 * Emp_Dept_merged.df$SAL[i])
  }
  }
}

for(i in 1:nrow(Emp_Dept_merged.df))
{
  if(!is.na(Emp_Dept_merged.df$JOB[i]))
{
  if(Emp_Dept_merged.df$JOB[i]=="SALESMAN")
  {
    Emp_Dept_merged_hike.df$SAL[i] = Emp_Dept_merged.df$SAL[i] + (0.133 * Emp_Dept_merged.df$SAL[i])
  }
  }
}

#Increased Salaries Dataframe
View(Emp_Dept_merged_hike.df)