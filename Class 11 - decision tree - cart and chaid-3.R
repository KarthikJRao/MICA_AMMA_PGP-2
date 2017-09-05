install.packages("rpart.plot")
# -------------------- Decision Tree: CART ----------------------------------#
# read data
cart_data1 <- read.csv("http://dni-institute.in/blogs/wp-content/uploads/2017/07/dt_data.csv")
View(cart_data1)
# Build a Decision Tree
cart_data <- subset (cart_data1,select=-X)
View(cart_data)
library(rpart)
library(rpart.plot)
table(cart_data$Spend_Drop_over50pct)/nrow(cart_data)

names(cart_data)

cart_dt <- rpart(Spend_Drop_over50pct~ Gender+Education_level+Last_Month_spend+
               Last_3m_avg_spend,
             data=cart_data)


rpart.plot(cart_dt)

# Reference: http://dni-institute.in/blogs/cart-algorithm-for-decision-tree/

# -------------------- Decision Tree: CHAID ----------------------------------#
install.packages("libcoin")
install.packages("partykit")
install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID)
library(help=CHAID)
dt_chaid_data <- read.csv("http://dni-institute.in/blogs/wp-content/uploads/2017/07/dt_data.csv")

# find unique values
uniq_last_month <- sort(unique(dt_chaid_data$Last_Month_spend))
uniq_last3_month <- sort(unique(dt_chaid_data$Last_3m_avg_spend))
# Split points
split_point_lm <- vector()
for (i in 1:(length(uniq_last_month)-1)){
  split_point_lm[i] <- (uniq_last_month[i]+uniq_last_month[i+1])/2
}
split_point_l3m <- vector()
for (i in 1:(length(uniq_last3_month)-1)){
  split_point_l3m[i] <- (uniq_last3_month[i]+uniq_last3_month[i+1])/2
}

# Factor Variables
dt_chaid_data$Last_Month_spend_cut <- cut(dt_chaid_data$Last_Month_spend,
                                          breaks = c(0,100,350,750,850,950,2000),
                         include.lowest=T)
dt_chaid_data$Last_3m_avg_spend_cut <- cut(dt_chaid_data$Last_3m_avg_spend,
                                          breaks = c(0,100,350,750,850,950,2000),
                                          include.lowest=T)


chaid <- chaid(as.factor(Spend_Drop_over50pct)~Gender+Education_level+
                 Last_Month_spend_cut+
                 Last_3m_avg_spend_cut,
               data=dt_chaid_data)
plot(chaid) 

table(dt_chaid_data$Last_Month_spend_cut)
