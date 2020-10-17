#set packages
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(ipred)
library(glmnet)

#part 1
#import the MalwaresamplesCSV
malware <- read.csv("MalwareSamples10000.csv", stringsAsFactors=TRUE);

#set seed
set.seed(10506441)
#Partition the data with a 80/20 split
trainRow <- createDataPartition(malware$isMalware,
                                p=0.8,
                                list=FALSE);

trainingData <- malware[trainRow,-1] #Creation of a training set. Excluding the first column specimenId
testingData <- malware[-trainRow,-1] #Creation of a test set. Excluding the first column specimenId

#Part 2

library(dplyr)
set.seed(10506441)
models.list1 <- c("Logistic Ridge Regression",
                  "Logistic LASSO Regression",
                  "Logistic Elastic-Net Regression")
models.list2 <- c("Classification Tree",
                  "Bagging Tree",
                  "Random Forest")
mymodels <- c(sample(models.list1, size=1),
              sample(models.list2, size=1))
mymodels %>% data.frame
#Resulted with Logistic Elastic-Net Regression & Bagging 

#Are all categorical variables stored as factors? 
str(malware)
#All categorical features are now factors. 

#LOGISTIC ELASTIC-NET REGRESSION 
#Lambdas 
lambdas <- 10^seq(-3,3,length=100) 
alphas <- seq(0.1,0.9, by=0.1); alphas
set.seed(10506441)
mod.malware.elastic <- train(isMalware ~.,
                             data=trainingData,
                             method="glmnet",
                             preProcess=NULL,
                             trControl=trainControl("repeatedcv",
                                                    number = 10,
                                                    repeats = 5),
                             tuneGrid = expand.grid(alpha = alphas,
                                                    lambda = lambdas)
)
mod.malware.elastic$bestTune #identifying the optimal hyperparameters 

#Model Coefficients 
coef(mod.malware.elastic$finalModel, mod.malware.elastic$bestTune$lambda)

#Predicting the probability of Malware in the test set. 
prediction.class.elastic <- predict(mod.malware.elastic,new=testingData)

#Confusion Matrix
confusion.elastic <- table(prediction.class.elastic %>% as.factor %>% relevel(ref="Yes"),
                           testingData$isMalware %>% as.factor %>% relevel(ref="Yes"));

prop <- prop.table(confusion.elastic,2); prop %>% round(digit=3)

confusionMatrix(confusion.elastic)

#BAGGING
#Working on the training set
set.seed(10506441)
baggingtree.malware <- bagging(isMalware~.,
                               data=trainingData,
                               nbagg=100, #bootstrap samples 
                               coob=TRUE);
baggingtree.malware

#Now comparing to the test set 

test.predicition.bagging <- predict(baggingtree.malware,newdata = testingData, type= "class");

confusion.bagging <- confusionMatrix(test.predicition.bagging %>% relevel(ref="Yes"),
                                     testingData$isMalware %>% relevel(ref="Yes"))

confusion.bagging

#Now to tune the hyperparameters 

grid.malware <- expand.grid(nbagg=seq(25,150,25), #Tuning the number of bootstrap samples
                            cp=seq(0,0.5,0.1), #Tuning the complexity parameter 
                            minsplit=seq(5,20,5), #Tuning the minimum split 
                            OOB.miscalss=NA, #Out of bag missclassification error 
                            test.sensitivity=NA,
                            test.specificity=NA,
                            test.accuracy=NA)

for (I in 1:nrow(grid.malware))
{
  set.seed(10506441)
  
  baggingtree.malware <- bagging(isMalware~.,
                                 data=trainingData,
                                 nbagg=grid.malware$nbagg[I], #looping though bootstrap samples 
                                 coob = TRUE,
                                 control=rpart.control(cp=grid.malware$cp[I],
                                                       minsplit = grid.malware$minsplit[I]));
  
  grid.malware$OOB.miscalss[I] <- baggingtree.malware$err*100 #Out of Bag error Expressed as a percentage. 
  
  test.predicition.bagging <- predict(baggingtree.malware,newdata=testingData, type= "class"); #looping through the test set. 
  
  confusion.bagging <- confusionMatrix(test.predicition.bagging %>% relevel(ref="Yes"), 
                                       testingData$isMalware %>% relevel(ref="Yes")) #Looping through the confusion matrices 
  
  prop.confusion.bagging <- confusion.bagging$table %>% prop.table(2)
  grid.malware$test.sensitivity[I] <- prop.confusion.bagging[1,1]*100
  grid.malware$test.specificity[I] <- prop.confusion.bagging[2,2]*100
  grid.malware$test.accuracy[I] <- confusion.bagging$overall[1]*100
}

grid.malware[order(grid.malware$OOB.miscalss,decreasing = FALSE)[1:10],] %>% round(2)

#Bagging with the hyperparametres tuned. 
set.seed(10506441)
baggingtree.malware.tuned <- bagging(isMalware~.,
                               data=trainingData,
                               nbagg=50, 
                               coob = TRUE,
                               control=rpart.control(cp=0,
                                                     minsplit =20));
test.predicition.bagging.tuned <- predict(baggingtree.malware.tuned,newdata=testingData, type= "class");
confusion.bagging.tuned <- confusionMatrix(test.predicition.bagging.tuned %>% relevel(ref="Yes"), 
                                     testingData$isMalware %>% relevel(ref="Yes")) #Looping through the confusion matrices 
confusion.bagging.tuned

#PART 3

#Reading in the Test Dataset 

emailTest <- read.csv("EmailSamples50000.csv", stringsAsFactors=TRUE);

#LOGISTIC ELASTIC-NET REGRESSION TUNED
#Predicting the probability of Malware in the test set. 
set.seed(10506441)
prediction.emailTest.elastic <- predict(mod.malware.elastic,new=emailTest)

#Confusion Matrix
confusion.emailTest.elastic <- table(prediction.emailTest.elastic %>% as.factor %>% relevel(ref="Yes"),
                           emailTest$isMalware %>% as.factor %>% relevel(ref="Yes"));

prop.emailTest <- prop.table(confusion.emailTest.elastic,2); prop %>% round(digit=3)

confusionMatrix(confusion.emailTest.elastic)

#Bagging the EmailSamples5000.csv witht he Tuned Model 
set.seed(10506441)
email.predicition.bagging.tuned <- predict(baggingtree.malware.tuned,newdata=emailTest, type= "class");
email.confusion.bagging.tuned <- confusionMatrix(email.predicition.bagging.tuned %>% relevel(ref="Yes"), 
                                           emailTest$isMalware %>% relevel(ref="Yes")) #Looping through the confusion matrices 
email.confusion.bagging.tuned

