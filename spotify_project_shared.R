spotify<-read.csv("D:/Year2_Sem2/Statistical_Learning_with_R/Project/dataset-of-10s.csv")
summary(spotify)
dim(spotify)

#Removing track,artist,uri
#Remove duplicates
library(dplyr)
spotify1 <- spotify %>% select(-track, - artist, -uri) %>% unique() #this code only works when there is dplyr
summary(spotify1)
View(spotify1)
dim(spotify1)

#Removing discrete and target variable
#pairs(spotify1%>%select(-time_signature, -sections, -key, -mode, -target))


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
spotify2<- spotify1%>%select(-energy, -sections)
dim(spotify2)
View(spotify2)

##VALIDATION WORKINGS##
## 50% of the sample size
#sample_size <- floor(0.50 * nrow(spotify2))

## set the seed to make your partition reproducible
# RNGkind(sample.kind="Rounding")
# set.seed(123)
# train_index <- sample(seq_len(nrow(spotify2)), size = sample_size)

# train <- spotify2[train_index, ]
# test <- spotify2[-train_index, ]
# train<-sample(c(TRUE,FALSE),nrow(spotify2), rep=TRUE)
# test<- (!train)

##METHOD 1: LASSO 
trainp <- sample(1:nrow(spotify2),nrow(spotify2)/2)
testp <- -trainp

spotify2.train <- spotify2[trainp,]
spotify2.test <- spotify2[testp,]

train.x <- model.matrix(target~., data=spotify2.train)[,-1]
train.y <-spotify2.train$target
test.x <- model.matrix(target~., data=spotify2.test)[,-1]
test.y <- spotify2.test$target

library(leaps)
library(glmnet)

grid <- 10^seq(10, -2, length=100)

lasso.mod <- cv.glmnet(train.x, train.y, alpha=1,family="binomial", lambda=grid)
lambda.lasso <- lasso.mod$lambda.min 
lambda.lasso
lasso.pred <- predict(lasso.mod, newx=test.x, s=lambda.lasso)
mean((test.y-lasso.pred)^2)

x <- model.matrix(target~., data=spotify2)[,-1]
y <- spotify2$target

x
y

out.lasso <- glmnet(x,y, alpha=1, lambda = grid, family="binomial")
lasso.coef <- predict(out.lasso, type="coefficients", s=lambda.lasso)[1:14,]
lasso.coef[lasso.coef !=0]

ridge.mod <- cv.glmnet(train.x, train.y, alpha=0, lambda=grid,family="binomial")
lambda.rr <- ridge.mod$lambda.min
lambda.rr
ridge.pred <- predict(ridge.mod, newx=test.x, s=lambda.rr)
mean((test.y-ridge.pred)^2)

out.rr <- glmnet(x,y, alpha=0, lambda=grid, family="binomial")
rr.coef <- predict(out.rr, type="coefficients", s=lambda.rr)[1:14,]
rr.coef

#METHOD 2: RUNNING BEST SUBSET LOGISTIC REGRESSION ON TRAIN DATASET
library(bestglm)
spotifyglm1 <- bestglm(Xy = train, family = binomial, IC = "CV", CVArgs = list(Method = "DH", K =10,REP = 1))

#METHOD 3: RUNNING BEST SUBSET LOGISTIC REGRESSION ON WHOLE DATA SET TO DETERMINE BEST INDEPENDENT VARIABLES
#To run logistic regression with subset selection
library(bestglm)
## Perform
spotifyglm <-
  bestglm(Xy = spotify2,
          family = binomial,          # binomial family for logistic
          IC = "BIC",                 # Information criteria for
          method = "exhaustive",nvmax=13)

spotifyglm$BestModels
summary(spotifyglm$BestModel)

#WITH THAT, USE THE BEST INDEPENDENT VARIABLE TO BUILD A NORMAL LOGISTIC REGRESSION MODEL
#Perform normal logistic regression with selected independent variables

spotify3<-spotify2%>%select(danceability, loudness, instrumentalness, liveness, valence, duration_ms,time_signature, target)
View(spotify3)
#data<-data.frame(spotify3)
spotifyglm2<-glm(target~.,
             data=spotify3 ,family =binomial )
summary(spotifyglm2)

#this codes to help in splitting data
install.packages('caTools')
library(caTools)

set.seed(88)
split <- sample.split(spotify3$target, SplitRatio = 0.50)

#get training and test data
spotify3train <- subset(spotify3, split == TRUE)
spotify3test <- subset(spotify3, split == FALSE)

predict <- predict(spotifyglm2, type = 'response')

#confusion matrix
table(spotify3$target, predict > 0.5)

#Source for Method 3 :https://www.analyticsvidhya.com/blog/2015/11/beginners-guide-on-logistic-regression-in-r/





#IGNORE BELOW##
#Run k-fold #edit here
set.seed(123)
library(boot)
k <- 10
kfCV <- cv.glm(data=data, glmfit=spotifyglm, K=k)
kfCV$delta
kfCV


folds <- sample(1:k, nrow(spotify3), replace=TRUE)
cv.errors <- matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))
for (j in 1:k) {
  best.fit <- regsubsets(Target~., data=Hit[folds!=j,], nvmax=19)
  for (i in 1:19){
    pred <- predict.regsubsets(best.fit, Hit[folds==j,], id=i)
    cv.errors[j,i] <- mean((Hit$Salary[folds==j]-pred)^2)
  }
}
mean.cv <- apply(cv.errors, 2, mean)#to average over the columns of matrix to error for each model
mean.cv
bb <- which.min(mean.cv)
bb
# we should use the full data as training set to get the full model
coef(regfit2.all, bb)



###ignore below###
RNGkind(sample.kind="Rounding")
set.seed(123)
N  <- 100
X1 <- rnorm(N, 175, 7)
X2 <- rnorm(N,  30, 8)
X3 <- abs(rnorm(N, 60, 30))
Y  <- 0.5*X1 - 0.3*X2 - 0.4*X3 + 10 + rnorm(N, 0, 3)
dfRegr <- data.frame(X1, X2, X3, Y)
Crossvalidation
glmFit <- glm(Y ~ X1 + X2 + X3, data=dfRegr,
              family=gaussian(link="identity"))
library(boot)
k    <- 3
kfCV <- cv.glm(data=dfRegr, glmfit=glmFit, K=k)
kfCV$delta
########MY OWN RUBBISH #####
#Creation of confusion matrix
spotifyglm1.prob<-predict(spotifyglm1,type ="response")
spotifyglm1.prob

spotifyglm1.pred<-rep(0,6259)
spotifyglm1.pred[spotifyglm1.prob >.5]<-1

table(spotifyglm1.pred, target)

##START HERE##
kfold<-bestglm(Xy=spotify2, IC="CV", CVArgs=list(Method="HTF", K=10, REP=20))
kfold
#-------------------------------------------------------------------------------
# Cross validation (customized)

library(plyr)   # progress bar
library(caret)  # confusion matrix

# False positive rate
fpr <- NULL

# False negative rate
fnr <- NULL

# Number of iterations
k <- 50

# Initialize progress bar
pbar <- create_progress_bar('text')
pbar$init(k)

# Accuracy
acc <- NULL


for(i in 1:k) {
  # Train-test splitting
  # 50% of samples -> fitting
  # 50% of samples -> testing
  smp_size <- floor(0.50 * nrow(spotify2))
  index <- sample(seq_len(nrow(spotify2)),size=smp_size)
  train <- spotify2[index, ]
  test <- spotify2[-index, ]
  
  # Fitting
  model <- glm(target ~ ., data = spotify2, family = "binomial")
  
  
  # Predict results
  results_prob <- predict(model,subset(test,type='response'))
  
  # If prob > 0.5 then 1, else 0
  results <- ifelse(results_prob > 0.5,1,0)
  
  # Actual answers
  answers <- test$Creditability
  
  # Accuracy calculation
  misClasificError <- mean(answers != results)
  
  # Collecting results
  acc[i] <- 1-misClasificError
  
  # Confusion matrix
  cm <- confusionMatrix(data=results, reference=answers)
  fpr[i] <- cm$table[2]/(nrow(spotify2)-smp_size)
  fnr[i] <- cm$table[3]/(nrow(spotify2)-smp_size)
  
  pbar$step()}

# Average accuracy of the model
mean(acc)

par(mfcol=c(1,2))

# Histogram of accuracy
hist(acc,xlab='Accuracy',ylab='Freq',
     col='cyan',border='blue',density=30)

# Boxplot of accuracy
boxplot(acc,col='cyan',border='blue',horizontal=T,xlab='Accuracy',
        main='Accuracy CV')

# Confusion matrix and plots of fpr and fnr
mean(fpr)
mean(fnr)
hist(fpr,xlab='% of fnr',ylab='Freq',main='FPR',
     col='cyan',border='blue',density=30)
hist(fnr,xlab='% of fnr',ylab='Freq',main='FNR',
     col='cyan',border='blue',density=30)