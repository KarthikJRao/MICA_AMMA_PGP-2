#installXLSXsupport(perl="C:/Program Files/bin/perl.exe")
install.packages("randomForest")
install.packages("gdata")

#______________________________Code for the Random Forest generation_________________________________________
  
#Determine the working directory
getwd()

#Change the working directory to point to the location where the data files are stored
setwd('C:/YYYYYY/AMMA 2017/Data/data_2017/Session 2')


# relevant packages for random forest functions and input/output orerations respectively
# Stored the file in an excel sheet named data

library(randomForest)
library(gdata)
#Invoking the relevant libraries installed above

dt<-read.csv("train.csv")
#R imports and stores the data in a data frame called "dt"
# Now we begin the process of data cleaning and preparation for applying Random Forests


#*******************************************Code for Data Exploration *****************************************************
#structure  of the data file imported
str(dt)

#Event Rate in the data
sum(dt$Target)/nrow(dt)

# subset into continous
cont<-subset(dt,select=-c(id))

#more detailed exploration
z<-cont
z<-cont
for (i in 1:ncol(z)) 
{
  z1<-t(as.data.frame(quantile(z[,i],prob=c(0,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.98,0.99,0.999,1),na.rm=T)))
  row.names(z1)[1]<-colnames(z)[i]
  total<-nrow(z)
  total_miss<-sum(is.na(z[,i]))
  z1<-cbind(z1,total,total_miss)
  if (i==1) y<-z1 else y<-rbind(y,z1)
}

write.csv(y,"univ_cont.csv")


#missing value treatment
sum(is.na(dt$x1))
dt$x1 <- ifelse(is.na(dt$x1),dt$x5-dt$x3,dt$x1)
sum(is.na(dt$x1))

dt$x29 <- ifelse(is.na(dt$x29),0,dt$x29)
dt$x30 <- ifelse(is.na(dt$x30),0,dt$x30)
dt$x31 <- ifelse(is.na(dt$x31),0,dt$x31)
dt$x33 <- ifelse(is.na(dt$x33),0,dt$x33)
dt$x35 <- ifelse(is.na(dt$x35),0,dt$x35)
dt$x38 <- ifelse(is.na(dt$x38),0,dt$x38)

#recheck 

z<-dt
for (i in 1:ncol(z)) 
{
  total<-nrow(z)
  total_miss<-sum(is.na(z[,i]))
  z1<-cbind(colnames(z)[i],total,total_miss)
  if (i==1) y<-z1 else y<-rbind(y,z1)
}

write.csv(y,"missing_check.csv")


#**************************************************Modeling Process******************************************************
  
dt<-read.csv("train.csv")

#structure 
dt$x39<-as.factor(dt$x39)
dt$Target<-as.factor(dt$Target)
dt$x1 <- ifelse(is.na(dt$x1),dt$x5-dt$x3,dt$x1)
dt$x29 <- ifelse(is.na(dt$x29),0,dt$x29)
dt$x30 <- ifelse(is.na(dt$x30),0,dt$x30)
dt$x31 <- ifelse(is.na(dt$x31),0,dt$x31)
dt$x33 <- ifelse(is.na(dt$x33),0,dt$x33)
dt$x35 <- ifelse(is.na(dt$x35),0,dt$x35)
dt$x38 <- ifelse(is.na(dt$x38),0,dt$x38)
dt_tree<-subset(dt,select=-c(x39,id))


#split the dataset into development and validation
set.seed(100)
tot<-nrow(dt_tree)
devrec<-as.integer(0.7*tot)
devr<-sample(tot,devrec)
dev<-dt_tree[devr,]
val<-dt_tree[-devr,]

#random forest setup
x<-subset(dev,select=-c(Target))
y<-dev$Target

#**************************************Random Forest Model Build*************************************************
library(randomForest)
rf<-randomForest(x,y,mtry=6,ntry=500,importance=TRUE)

bestmtry<- tuneRF(x,y,ntreeTry=500,stepFactor=1.5, improve=0.05,trace=TRUE, plot=TRUE,dobest=FALSE)

rf<-randomForest(x,y,mtry=4,ntry=500,importance=TRUE, probability=TRUE)

importance(rf)
varImpPlot(rf)

# Estimates for importance of individual attributes in the analysis

#******************************************Model Assessment - Confusion Matrix ************************************
  
  predict <- predict(rf, val, type='response')
# Computes predicted values of income for all input vectors in the validation dataset

table(predict,val$Target)
# Computes confusion matrix for the predicted values in the test dataset

#oob rate

#Three types of prediction 
dev_oob<-predict(rf,type="prob")
dev_prob<-predict(rf,dev,type="prob")
val_prob<-predict(rf,val,type="prob")

#**************************************Model Assessment - Area Under Curve****************************
  
#auc 
install.packages("ROCR")
dev_class<-as.data.frame(dev_oob)
prob_dt<-cbind(as.numeric(levels(dev$Target))[dev$Target],dev_class)
colnames(prob_dt)[1]<-"Target"
colnames(prob_dt)[2]<-"prob_0"
colnames(prob_dt)[3]<-"prob_1"
library(ROCR)

pred<-prediction(prob_dt$prob_1,prob_dt$Target)
auc <- performance(pred,"auc")
as.numeric(auc@y.values)

#auc - dev model
dev_class<-as.data.frame(dev_prob)
prob_dt<-cbind(as.numeric(levels(dev$Target))[dev$Target],dev_class)
colnames(prob_dt)[1]<-"Target"
colnames(prob_dt)[2]<-"prob_0"
colnames(prob_dt)[3]<-"prob_1"
library(ROCR)

pred<-prediction(prob_dt$prob_1,prob_dt$Target)
auc <- performance(pred,"auc")
as.numeric(auc@y.values)

#auc - val
val_class<-as.data.frame(val_prob)
prob_dt<-cbind(as.numeric(levels(val$Target))[val$Target],val_class)
colnames(prob_dt)[1]<-"Target"
colnames(prob_dt)[2]<-"prob_0"
colnames(prob_dt)[3]<-"prob_1"
library(ROCR)

pred<-prediction(prob_dt$prob_1,prob_dt$Target)
auc <- performance(pred,"auc")
as.numeric(auc@y.values)

#************************Model Assessment - Concordance**************************************************
  fastConc<-function(model){
    # Get all actual observations and their fitted values into a frame
    fitted<-data.frame(cbind(prob_dt$Target,prob_dt$prob_1))
    colnames(fitted)<-c('respvar','score')
    # Subset only ones
    ones<-fitted[fitted[,1]==1,]
    # Subset only zeros
    zeros<-fitted[fitted[,1]==0,]
    
    # Initialise all the values
    pairs_tested<-nrow(ones)*nrow(zeros)
    conc<-0
    disc<-0
    
    # Get the values in a for-loop
    for(i in 1:nrow(ones))
    {
      conc<-conc + sum(ones[i,"score"]>zeros[,"score"])
      disc<-disc + sum(ones[i,"score"]<zeros[,"score"])
    }
    # Calculate concordance, discordance and ties
    concordance<-conc/pairs_tested
    discordance<-disc/pairs_tested
    ties_perc<-(1-concordance-discordance)
    return(list("Concordance"=concordance,
                "Discordance"=discordance,
                "Tied"=ties_perc,
                "Pairs"=pairs_tested))
  }

fastConc() 

#********************Model Assessment - KS Statistics / Rank Ordering *************************************
  
prob_dt<-prob_dt[order(-rank(prob_dt$prob_1)),]
breaks<-unique(quantile(prob_dt$prob_1, probs=c(0:10/10)))
cnt<-length(breaks)-1
c<-cut(prob_dt$prob_1, breaks=breaks, labels=1:cnt,include.lowest=TRUE)
prob_dt<-cbind(prob_dt,c)
install.packages("sqldf")
library(sqldf)
summ<-sqldf('select c, count(*) as total, sum(Target) as events,
            sum(prob_1) as sum_prob
            from prob_dt group by c')

write.csv(summ,"mod_out.csv")

View(summ)