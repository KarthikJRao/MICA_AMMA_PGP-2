
# Author: Amritendu Roy
# Logistic Regression for Bank X-Sell Product
######################################################

# getting some important packages for the analysis
install.packages("gmodels")
install.packages("Hmisc")
install.packages("pROC")
install.packages("ResourceSelection")
install.packages("car")
install.packages("caret")
install.packages("dplyr")
library(gmodels)
library(Hmisc)
library(pROC)
library(ResourceSelection)
library(car)
library(caret)
library(dplyr)

cat("\014") # Clearing the screen

# Setting the working directory
setwd("C:/mica/data")
getwd()

# reading client datasets
df.client <- read.csv('bank_client.csv')
str(df.client)

# reading other attributes
df.attr <- read.csv('bank_other_attributes.csv')
str(df.attr)

# reading campaign data
df.campaign <- read.csv('latest_campaign.csv')
str(df.campaign)

# reading campaign outcome
df.campOutcome <- read.csv('campaign_outcome.csv')
str(df.campOutcome)

# Create campaign data by joining all tables together
df.temp1 <- merge(df.client, df.campaign, by = 'Cust_id', all.x = TRUE)
df.temp2 <- merge(df.temp1, df.attr, by = 'Cust_id', all.x = TRUE)
df.data <- merge(df.temp2, df.campOutcome, by = 'Cust_id', all.x = TRUE)
length(unique(df.data$Cust_id)) == nrow(df.data) #checking for any duplicate customer ID

# clearing out temporary tables
rm(df.temp1,df.temp2)

# see few observations of merged dataset
head(df.data)

# see a quick summary view of the dataset
summary(df.data)

# see the tables structure
str(df.data)

# check the response rate
CrossTable(df.data$y)

# split the data into training and test
set.seed(1234) # for reproducibility
df.data$rand <- runif(nrow(df.data))
df.train <- df.data[df.data$rand <= 0.7,]
df.test <- df.data[df.data$rand > 0.7,]
nrow(df.train)
nrow(df.test)

# how the categorical variables are distributed and are related with target outcome
CrossTable(df.train$job, df.train$y)
CrossTable(df.train$marital, df.train$y)
CrossTable(df.train$education, df.train$y)
CrossTable(df.train$default, df.train$y)
CrossTable(df.train$housing, df.train$y)
CrossTable(df.train$loan, df.train$y)
CrossTable(df.train$poutcome, df.train$y)

# DO IT YOURSELF - Improve your code -------------------------------------------------
# Can you write a function which takes dataframe, and factor variable names, 
# target variable name as argument and show output as we have seen above for each 
# of the variable ?
#-------------------------------------------------------------------------------------


# let see how the numerical variables are distributed
hist(df.train$age)
hist(df.train$balance)
hist(df.train$duration)
hist(df.train$campaign)
hist(df.train$pdays)
hist(df.train$previous)
describe(df.train[c("age", "balance", "duration", "campaign", "pdays", "previous")])


# DO IT YOURSELF Improve your code ---------------------------------------------------
# Can you write a function which takes dataframe, and numerical variable names
# as argument and show output as we have seen above for each of the variable ?
#-------------------------------------------------------------------------------------

# running a full model  
df.train$yact = ifelse(df.train$y == 'yes',1,0)
full.model <- glm(formula = yact ~ age + balance + duration + campaign + pdays + previous +
                    job + marital + education + default + housing + loan + poutcome, 
                  data=df.train, family = binomial)
summary(full.model)

# check for vif
fit <- lm(formula <- yact ~ age + balance + duration + campaign + pdays + previous +
                    job + marital + education + default + housing + loan + poutcome, 
                  data=df.train)
vif(fit)

# automated variable selection - Backward
backward <- step(full.model, direction = 'backward')
summary(backward)

# training probabilities and roc
df.train$prob = predict(full.model, type=c("response"))
q <- roc(y ~ prob, data = df.train)
plot(q)
auc(q)

# variable importance
varImp(full.model, scale = FALSE)

# Perform HL Test
hl <- hoslem.test(df.train$y, df.train$prob, g=10)
hl # we should not be able to reject null hypothesis - not a required test always

# confusion matrix
df.train$ypred = ifelse(df.train$prob>=.5,'pred_yes','pred_no')
table(df.train$ypred,df.train$y)

# decile chart and decision making
df.train$decile <- with(df.train, cut(prob, 
                                breaks=quantile(prob, probs=seq(0,1, by=0.1), na.rm=TRUE), 
                                include.lowest=TRUE))
df.train$decile <- as.numeric(df.train$decile)
df.train <- df.train[order(-df.train$decile),]
train.group <- df.train %>% group_by(decile)
train.group %>% summarise(total_yes = sum(yact), total = count(yact))

decile <- aggregate(df.train$yact, by=list(df.train$decile), FUN = sum)
decile <- decile[order(-decile$Group.1),]
colnames(decile) <- c('decile', 'total_yes')
decile$random_yes <- round(sum(decile$total_yes)/10)
decile$pcnt_yes <- decile$total_yes/sum(decile$total_yes)
decile$cum_pcnt_yes <- cumsum(decile$pcnt_yes)
print(decile)

# DO IT YOURSELF ------------------------------------------------------------------
# Improve your model by removing insignificant variables
# Use automated variable selection to improve models
# check performance of test data
#-------------------------------------------------------------------------------------

