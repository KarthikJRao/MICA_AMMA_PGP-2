# Set up working directory

setwd("C:\\YYYYYY\\AMMA 2017\\Data\\data_2017")

# Read data
card_balance<- read.csv(file = "card_balance.csv", stringsAsFactors = F)


# --------------------- Multiple Regression -----------------------------

sm_f <- sample(1:nrow(card_balance),0.6*nrow(card_balance), replace = F)
sm_card_balance <- card_balance[sm_f,]


# Obj: Estimate billed amount for next 3 months

names(sm_card_balance)

summary(sm_card_balance)

# We can remove the customers who do not have any billed amount in the first 3 months
sm_card_balance$Pre_billed_amt_3m <-apply(sm_card_balance[,c(7,8,9)],1, sum)

summary(sm_card_balance)
quantile(sm_card_balance$Pre_billed_amt_3m, probs = seq(0,1,0.05))


# Exclude with negative billed amount
sm_card_balance <- sm_card_balance[sm_card_balance$Pre_billed_amt_3m>30,]

# Target Variable: Average Balance in next 3 month
sm_card_balance$billed_3m <-apply(sm_card_balance[,c(10,11,12)],1, mean)


quantile(sm_card_balance$billed_3m, probs = seq(0,1,0.05))

hist(sm_card_balance$billed_3m,breaks = 20)

quantile(sm_card_balance$billed_3m, probs = seq(0,1,0.05))

# Outlier Treatment of Target Variables
sm_card_balance$billed_3m <- ifelse(sm_card_balance$billed_3m>165000,165000,
                                 ifelse(sm_card_balance$billed_3m<0,0,sm_card_balance$billed_3m))

names(sm_card_balance)

# Regression MOdel fitting
card_billed_amt_reg <- lm(billed_3m~AGE+Pre_billed_amt_3m+Billed_amount_1+Billed_amount_2,
                data=sm_card_balance)

summary(card_billed_amt_reg)

sm_card_balance$pred_bill_amt <- predict(card_billed_amt_reg,sm_card_balance)

plot(sm_card_balance$billed_3m,
     sm_card_balance$pred_bill_amt,
     pch=20,
     col="red")
abline(lm(sm_card_balance$billed_3m ~ sm_card_balance$pred_bill_amt),col="blue",lwd=4)

# --------------------- Logistic Regression -----------------------------

# Target Variable: If Billed Amount reduces below 20%

names(sm_card_balance)

sm_card_balance$Pred_bill_drop <- ifelse (sm_card_balance$billed_3m < 0.2*sm_card_balance$Pre_billed_amt_3m,1,0)

table(sm_card_balance$Pred_bill_drop)

# Demographic Variables

Billed_amt_drop_logit <- glm(formula = Pred_bill_drop~
                               AGE+Pre_billed_amt_3m+Billed_amount_1+Payment_1+Payment_2+Payment_3+
                               Billed_amount_2
                             ,
                      family=binomial,
                      data=sm_card_balance)
summary(Billed_amt_drop_logit)

# Predict
sm_card_balance$Pred_Amt_drop_prob <- predict(Billed_amt_drop_logit,
                                              sm_card_balance,
                              type = c("response"))

table(sm_card_balance$Pred_bill_drop)/nrow(sm_card_balance)
quantile(sm_card_balance$Pred_Amt_drop_prob, 
         probs = seq(0,1,0.05))

sm_card_balance$Pred_Amt_drop_Class <- ifelse(sm_card_balance$Pred_Amt_drop_prob >0.288,1,0)

table(sm_card_balance$Pred_Amt_drop_Class,sm_card_balance$Pred_bill_drop)

library(caret)

# Create Confusion Matrix
confusionMatrix(data=factor(sm_card_balance$Pred_Amt_drop_Class),
                reference=sm_card_balance$Pred_bill_drop,
                positive='1')

library("ROCR")

perf.obj <- prediction(predictions=sm_card_balance$Pred_Amt_drop_Class,
                       labels=sm_card_balance$Pred_bill_drop)
# Get data for ROC curve
roc.obj <- performance(perf.obj, measure="tpr", x.measure="fpr")
plot(roc.obj,
     main="Balance Amt Drop - ROC Curves",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")
abline(0,1,col="grey")

#--------------------------  Decision Tree: CART -----------------------------------

install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
dt1 <- rpart(Pred_bill_drop~AGE+Pre_billed_amt_3m+Billed_amount_1+Payment_1+Payment_2+Payment_3+
               Billed_amount_2,
             data=sm_card_balance)

library(rpart.plot)
rpart.plot(dt1)

install.packages("libcoin")
install.packages("partykit")
install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID)
library(help=CHAID)

sm_card_balance1 <- sm_card_balance
sm_card_balance1$AGE_cat <- cut(sm_card_balance1$AGE,
                                quantile(sm_card_balance1$AGE, probs = seq(0,1,0.05)))
sm_card_balance1$Pre_billed_amt_3m_cat <- cut(sm_card_balance1$Pre_billed_amt_3m,
                                quantile(sm_card_balance1$Pre_billed_amt_3m, probs = seq(0,1,0.05)))
sm_card_balance1$Payment_1_cat <- cut(sm_card_balance1$Payment_1,
                                      unique(quantile(sm_card_balance1$Payment_1, probs = seq(0,1,0.05))))

dt.chaid <- chaid(as.factor(Pred_bill_drop)~AGE_cat+Pre_billed_amt_3m_cat+Payment_1_cat,
             data=sm_card_balance1)


dt.chaid

plot(dt.chaid) 
     