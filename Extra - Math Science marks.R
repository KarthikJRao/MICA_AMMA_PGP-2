student.df <- data.frame(Name <- c(head(LETTERS,5)), 
                       Age <- c(23,22,21,25,20),
                       Math_marks <- c(87,86,89,88,79),
                       Science_marks <- c(65,87,78,55,93))

student.df$Total_marks<-student.df$Math_marks....c.87..86..89..88..79.+student.df$Science_marks....c.65..87..78..55..93.

student.df$Percent_Math <-(student.df$Math_marks....c.87..86..89..88..79./student.df$Total_marks....student.df.Math_marks....c.87..86..89..88..79....)*100

student.df$Percent_Science <-(student.df$Science_marks....c.65..87..78..55..93./student.df$Total_marks....student.df.Math_marks....c.87..86..89..88..79....)*100


View(student.df)

student2 <- student.df[c(2:7)] 
student2 <- student.df[,-1]
## adding some transformed columns to student.df
student.df$log_age<-log(student.df$Age....c.23..22..21..25..20.)
student.df$exp_age<-exp(student.df$Age....c.23..22..21..25..20./mean(student.df$Age....c.23..22..21..25..20.))
student.df$inv_age<-1/(student.df$Age....c.23..22..21..25..20.)
student.df$sqr_age<-(student.df$Age....c.23..22..21..25..20.)*(student.df$Age....c.23..22..21..25..20.)
student.df$sqrt_age<-sqrt(student.df$Age....c.23..22..21..25..20.)

##changing from numeric to character
student.df$log_age<-as.character(student.df$log_age)
class(student.df$log_age)

View(student.df)

s1<-student.df[student.df$Age>=23,]
View(s1)

?sample

sample_index<-sample(1:nrow(student.df),3,replace=F)
View(student2)