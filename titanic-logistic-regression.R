library('ggplot2')
library('ggthemes')
library('dplyr')
library('mice') # imputation 
library('randomForest') 
library('data.table')
library('pscl') # for McFadden R^2, for logistic regressions
library('ROCR') # for plotting ROC curve & calculating Area Under Curve (AUC)

setwd("/Users/hannahsmythe/Downloads/")
getwd()
titanic <- fread("titanic-train.csv", stringsAsFactors = TRUE)
str(titanic)
# count NAs per category
sapply(titanic, function(x) sum(is.na(x)))  
# impute missing age using mean
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm = T)

##### -------------------------------------------------------------------------------------------------------
##### ---------------------------- PART I: DATA EXPLORATION ----------------------------------------------------
##### -------------------------------------------------------------------------------------------------------

### Exploring Survival ---------------------------------------------------------------------------------------
#summarize Survived column as table
table(titanic$Survived)
# proportion of survived vs not survived
prop.table(table(titanic$Survived)) # 38% survived

### Exploring Passenger Gender --------------------------------------------------------------------------------
summary(titanic$Sex)
is.factor(titanic$Sex) # check if gender is a factor
contrasts(titanic$Sex) # check how R has dummified the factors 

## Overall proportions of deaths by gender
prop.table(table(titanic$Sex, titanic$Survived))

## Overall proportions of deaths per gender
prop.table(table(titanic$Sex, titanic$Survived), 1) # 1 means get proportions by rows; 2 by col

### Explore Passenger Age ---------------------------------------------------------------------------------------
summary(train$Age)
titanic$Child <- 0 # add Child y/n column
titanic$Child[titanic$Age < 18] <- 1 
#proportions of children/not children by sex who survived 
aggregate(Survived ~ Child + Sex, data = titanic, FUN = function(x) {sum(x)/length(x)})

### Explore class ---------------------------------------------------------------------------------------
## beyond 1/2/3: is there a difference within class by ticket price? ------------------------------------
titanic$FareLevel <- '30+'
titanic$FareLevel[titanic$Fare < 30 & titanic$Fare >= 20] <- '20-30'
titanic$FareLevel[titanic$Fare < 20 & titanic$Fare >= 10] <- '10-20'
titanic$FareLevel[titanic$Fare < 10] <- '<10'
aggregate(Survived ~ FareLevel + Pclass + Sex, data=titanic, FUN=function(x) {sum(x)/length(x)})

##### -------------------------------------------------------------------------------------------------------
##### ---------------------------- PART II: LOGISTIC REGRESSION ---------------------------------------------
##### -------------------------------------------------------------------------------------------------------

titanic$Pclass = as.factor(titanic$Pclass) # make pClass a factor

## remove columns with many missing values
str(titanic) # subset to Sex, age, pclass, sibsp, parch, fare, embarked, survived 
titanic <- subset(titanic, select = c(2,3,5,6,7,8,10,12))

### split into test and training sets ----------------------------------------------------------------------
sample_size <- floor(.90 * nrow(titanic))
set.seed(534) # set seed to make reproducible partition
trainer <- sample(seq_len(nrow(titanic)), size = sample_size)
train <- titanic[trainer, ]
test <- titanic[-trainer]
nrow(train)
nrow(test)

### Build logistic model ---------------------------------------------------------------------------------------
logistic_model <- glm(Survived ~., family = binomial(link = 'logit'), maxit = 100, data = train)

summary(logistic_model)

confint(logistic_model) # get 95% confdence interval for each predictor's coeff

### Run ANOVA to analyze deviance table ------------------------------------------------------------------------
anova(logistic_model, test = "Chisq")

### use McFadden R^2 to assess goodness of fit -----------------------------------------------------------------
pR2(logistic_model)

### Plot ROC curve of True Positive Rate against False Positive Rate -------------------------------------------
p <- predict(logistic_model,              
             newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

### measure AUC ---------------------------------------------------------------------------------------
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
