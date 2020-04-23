library(leaps)
library(glmnet)
library(dplyr)
library(corrplot)
library(caret)
library(tidyverse)

spotify<-read.csv("D:/Year2_Sem2/Statistical_Learning_with_R/Project/dataset-of-10s.csv")
summary(spotify)
dim(spotify)

#Removing categorical variables (track,artist,uri) and removing duplicates
#Remove duplicates
library(dplyr)
spotify1 <- spotify %>% select(-track, - artist, -uri) %>% unique() #this code only works when there is dplyr
#Removing discrete and target variable
pairs(spotify1%>%select(-time_signature, -sections, -key, -mode, -target))


#Running correlation analysis
#Needs to get rid of target variable only from the dataset
correlation <- cor(as.matrix(spotify1[-16]))
round(correlation,3)

#visualisation
#install.packages("corrplot")
library(corrplot)
corrplot(correlation, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#Remove energy + sections (to avoid problem of multicollinearity)
library(dplyr)
spotify2<- spotify1%>%select(-energy, -sections, -key) %>% mutate(time_signature_dummy = ifelse(time_signature == 4,1,0)) %>% select(-time_signature)

#Conducting a logistic regression with all the variables
spotifyALL<-glm(target~., data=spotify2,family =binomial )
summary(spotifyALL)

library(dplyr)
spotify2<- spotify1%>%select(-energy, -sections, -key) %>% mutate(time_signature_dummy = ifelse(time_signature == 4,1,0)) %>% select(-time_signature)


#Lasso Regression
trainp <- sample(1:nrow(spotify2),nrow(spotify2)/2)
testp <- -trainp
#Get half of the data set as training set
spotify2.train <- spotify2[trainp,]
#The rest is training set
spotify2.test <- spotify2[testp,]
train.x <- model.matrix(target~., data=spotify2.train)[,-1]
train.y <-spotify2.train$target
test.x <- model.matrix(target~., data=spotify2.test)[,-1]
test.y <- spotify2.test$target
grid <- 10^seq(10, -2, length=100)
#Getting the lambda with smallest CV error
lasso.mod <- cv.glmnet(train.x, train.y, alpha=1,family="binomial", lambda=grid)
lambda.lasso <- lasso.mod$lambda.min 
lambda.lasso
#Getting the prediction from the best lambda
lasso.pred <- predict(lasso.mod, newx=test.x, s=lambda.lasso)
#Getting the mse/test error
mean((test.y-lasso.pred)^2)


x <- model.matrix(target~., data=spotify2)[,-1]
y <- spotify2$target
#Using all data to refit model
out.lasso <- glmnet(x,y, alpha=1, lambda = grid, family="binomial")
#Get final model with y-intercepts by using the best lambda
lasso.coef <- predict(out.lasso, type="coefficients", s=lambda.lasso)[1:13,]
lasso.coef[lasso.coef !=0]

#creating a function to calculate log_odds for each data point
library(tidyverse)
log_odds <- function(spotify){
  log = lasso.coef[1] + (spotify$danceability *lasso.coef[2]) + (spotify$loudness * lasso.coef[3]) + (spotify$mode*lasso.coef[4]) + (spotify$instrumentalness*lasso.coef[7]) + (spotify$liveness*lasso.coef[8]) + (spotify$valence *lasso.coef[9]) + (spotify$tempo*lasso.coef[10]) + (spotify$duration_ms *lasso.coef[11]) + (spotify$time_signature*lasso.coef[13])
}
#function to convert log_odds to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)}

logodds <- log_odds(spotify2)
prob <- logit2prob(logodds)
prob_df <- data.frame(p= prob)
#add column of probability from lasso regression
spotify4<- add_column(spotify2, p =prob_df$p)
#new column where u = 1 if p>= 0.5, and u = 0 if p < 0.5)
spotify5 <- add_column(spotify4, u = ifelse(spotify4$p >=0.5,1,0))


pred_1_actual_1 <- nrow(spotify5 %>% filter(target == 1 & u ==1))
pred_0_actual_0 <- nrow(spotify5 %>% filter(target == 0 & u ==0))

pred_1_actual_0 <- nrow(spotify5 %>% filter(target == 0 & u ==1))
pred_0_actual_1 <- nrow(spotify5 %>% filter(target == 1 & u ==0))


sensitivity <- pred_1_actual_1 / (pred_1_actual_1 + pred_0_actual_1)
specificity <- pred_0_actual_0 / (pred_0_actual_0 + pred_1_actual_0)

overall_err = (pred_1_actual_0 + pred_0_actual_1) / nrow(spotify2)
Prediction_Accuracy = 1-overall_err

false_negative_rate <- pred_0_actual_1 / (pred_1_actual_1 + pred_0_actual_1)
false_positive_rate <- pred_1_actual_0 / (pred_0_actual_0 + pred_1_actual_0)

sensitivity
specificity
overall_err
Prediction_Accuracy
false_negative_rate
false_positive_rate

pred_1_actual_1
pred_0_actual_0
pred_1_actual_0
pred_0_actual_1

