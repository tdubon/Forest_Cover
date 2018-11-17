# Predict 454 Project
# Date created 10/23/2018
# Eric Smith
# Prasanna Venkata Rao
# Tannia Dubon
# Lucas Lu
# Kanaka Venkata Hema Geddam 

###data partitioned at 5, 10, and 15%
###balanced sampling using createDataPartition
####currently testing new approaches to improve model

require(moments)
library(corrplot)
library(plotly)
library(GGally)



####################################################################################################################################
######################### Part 1: Read in data  ############################################

setwd("~/Documents/Northwestern/MSDS_454/Final_Project/Forest_Cover")

forest.df <- read.csv(gzfile(file.choose()),header = FALSE, sep =",")

colnames(forest.df) <- c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology",
                         "Horizontal_Distance_To_Roadways","Hillshade9","Hillshade12","Hillshade3",
                         "Horizontal_Distance_To_Fire_Points","Rawah_Wild_Area","Neota_Wild_Area",
                         "Comanche_Peak_Wild_Area","Cache_la_Poudre_Wild_Area","st1","st2","st3"
                         ,"st4","st5","st6","st7","st8","st9","st10","st11"
                         ,"st12","st13","st14","st15","st16","st17","st18","st19"
                         ,"st20","st21","st22","st23","st24","st25","st26","st27"
                         ,"st28","st29","st30","st31","st32","st33","st34","st35"
                         ,"st36","st37","st38","st39","st40","Cover_Type")
dim(forest.df)
forest.df[1:6, 1:55]

forest.df=forest.df %>% 
  mutate(Cover_Type = ifelse(Cover_Type == 1 ,'SpruceFir',
                             ifelse(Cover_Type == 2 ,'LodgepolePine',
                                    ifelse(Cover_Type == 3 ,'PonderosaPine',
                                           ifelse(Cover_Type == 4 ,'CottonwoodWillow',
                                                  ifelse(Cover_Type == 5 ,'Aspen',
                                                         ifelse(Cover_Type == 6 ,'DouglasFir',
                                                                ifelse(Cover_Type == 7 ,'Krummholz','na'))))))))



forest.df<-setNames(forest.df, tolower(names(forest.df)))


### Need to make sure our data is understood correctly by R, since we have a mix of numerical and categorical
forest.df[11:55]<-lapply(forest.df[11:55], factor)


str(forest.df)



library(caret)
library(partykit)
library(party)


############################################data adjustments: cover_type

forest.df2 <- forest.df
# Make some adjustments to accomodate glmtree();
forest.df2 <- transform(forest.df, treatment=factor(cover_type=='LodgepolePine',
                                                     levels=c(TRUE, FALSE), labels=c('LodgepolePine','Other')))

#forest.df2 <- transform(forest.df, treatment=factor(cover_type=='LodgepolePine' | cover_type=='SpruceFir' | cover_type=='PonderosaPine' | cover_type=='Krummholz' | cover_type=='DouglasFir',
                                                   #levels=c(FALSE, TRUE), labels=c('Other', forest.df$cover_type)))

forest.df2$treatment <- forest.df$cover_type

levels(forest.df2$treatment) <- list(Other = c("Aspen", "CottonwoodWillow"),
                                     LodgepolePine="LodgepolePine", 
                                     SpruceFir="SpruceFir",
                                     PonderosaPine="PonderosaPine", 
                                     Krummholz="Krummholz", 
                                     DouglasFir="DouglasFir")

table(forest.df2$treatment)
forest.df2[["cover_type"]] <- NULL
remove(forest.df)


#############remove st15 - only 3 values and is causing issues in svm
forest.df2[["st15"]] <- NULL

#############################################PARTITIONS FOR FIRST SAMPLE - 5%
## First, split the training set off 
set.seed(156) 
split1 <- createDataPartition(forest.df2$treatment, p = .05)[[1]] 
other1 <- forest.df2[-split1,] 
training1 <- forest.df2[ split1,]

## Now create the evaluation and test sets 
set.seed(934) 
split2 <- createDataPartition(other1$treatment, p = .05)[[1]] 
testing1 <- other1[ split2,] 

## Determine the predictor names 
predictors1 <- names(training1)[names(training1)!= "treatment"]



head(testing1)
head(training1)  

data.train1 <- data.frame(model.matrix(treatment~., data=training1))[,-1]
head(data.train1)
#evaluationIND <- data.frame(model.matrix(price_transf~., data=evaluation))[,-1]
data.test1 <- data.frame(model.matrix(treatment~., data=testing1))[,-1]
head(data.test1)

#add outcome variable back into the dataset
data.train1$treatment <- training1$treatment
#evaluationIND <- evaluation$price_transf
data.test1$treatment <- testing1$treatment


#############################################PARTITIONS FOR SECOND SAMPLE - 10%
## First, split the training set off 
set.seed(1567) 
split1 <- createDataPartition(forest.df2$treatment, p = .10)[[1]] 
other2 <- forest.df2[-split1,] 
training2 <- forest.df2[ split1,]

## Now create the evaluation and test sets 
set.seed(9345) 
split2 <- createDataPartition(other2$treatment, p = .10)[[1]] 
testing2 <- other2[ split2,] 

## Determine the predictor names 
predictors2 <- names(training2)[names(training2)!= "treatment"]



head(testing2)
head(training2)  

data.train2 <- data.frame(model.matrix(treatment~., data=training2))[,-1]
head(data.train2)
#evaluationIND <- data.frame(model.matrix(price_transf~., data=evaluation))[,-1]
data.test2 <- data.frame(model.matrix(treatment~., data=testing2))[,-1]
head(data.test2)

#add outcome variable back into the dataset
data.train2$treatment <- training2$treatment
#evaluationIND <- evaluation$price_transf
data.test2$treatment <- testing2$treatment


#############################################PARTITIONS FOR SECOND SAMPLE - 15%
## First, split the training set off 
set.seed(158) 
split1 <- createDataPartition(forest.df2$treatment, p = .15)[[1]] 
other3 <- forest.df2[-split1,] 
training3 <- forest.df2[ split1,]

## Now create the evaluation and test sets 
set.seed(93456) 
split2 <- createDataPartition(other3$treatment, p = .15)[[1]] 
testing3 <- other3[ split2,] 

## Determine the predictor names 
predictors3 <- names(training3)[names(training3)!= "treatment"]



head(testing3)
head(training3)  

data.train3 <- data.frame(model.matrix(treatment~., data=training3))[,-1]
head(data.train3)
#evaluationIND <- data.frame(model.matrix(price_transf~., data=evaluation))[,-1]
data.test3 <- data.frame(model.matrix(treatment~., data=testing3))[,-1]
head(data.test3)

#add outcome variable back into the dataset
data.train3$treatment <- training3$treatment
#evaluationIND <- evaluation$price_transf
data.test3$treatment <- testing3$treatment





##############################


#library(e1071)

#tune.out=tune(svm, cover_type~., data=data.train1, kernel="radial", ranges=list(cost=c(0.1, 1, 10, 100, 1000),gamma=c(0.5, 1, 2, 3, 4)))

#svm = svm(cover_type~., data=data.train1, kernel="radial", decision.values=TRUE)$decision.values
#fitted <- predict(svm, data.train1)

#par(mfrow=c(1,2))
#rocplot(fitted, data.train1$cover_type, add=T, col="red" )


##############################
#Training Model 1 
##############################
table(data.train1$treatment)


library(caret)
trctrl <- trainControl(method="cv",
                       number=5,
                       classProbs=TRUE,
                       summaryFunction=multiClassSummary)
set.seed(123)

svm.c <- train(treatment~., data.train1,
               method='svmRadial',
               preProcess= c("center", "scale"),
               trControl=trctrl,
               tuneLength = 10)

#svm.c2 <- train(treatment~., data.train1,
               #method='svmRadial',
               #trControl=trctrl,
               #preProcess = c("center", "scale"),
               #tuneLength = 10)

#trained SVM model result
svm.c

#test set prediction
test_pred <- predict(svm.c, newdata=data.test1)
test_pred

#how accurate is the model?
confusionMatrix(test_pred, data.test1$treatment)


##############################
#Training Model 2 
##############################

library(caret)
trctrl.d <- trainControl(method="cv",
                       number=5,
                       classProbs=TRUE,
                       summaryFunction=multiClassSummary)
set.seed(1234)

svm.d <- train(treatment~., data.train2,
               method='svmRadial',
               preProcess= c("center", "scale"),
               trControl=trctrl.d,
               tuneLength = 10)

#trained SVM model result
svm.d

#test set prediction
test.pred.d <- predict(svm.d, newdata=data.test2)
test.pred.d

#how accurate is the model?
confusionMatrix(test.pred.d, data.test2$treatment)


##############################
#Training Model 3 
##############################

library(caret)
trctrl.e <- trainControl(method="cv",
                         number=5,
                         classProbs=TRUE,
                         summaryFunction=multiClassSummary)
set.seed(12345)

svm.e <- train(treatment~., data.train3,
               method='svmRadial',
               preProcess= c("center", "scale"),
               trControl=trctrl.e,
               tuneLength = 10)

#svm.c2 <- train(treatment~., data.train1,
#method='svmRadial',
#trControl=trctrl,
#preProcess = c("center", "scale"),
#tuneLength = 10)

#trained SVM model result
svm.e

#test set prediction
test.pred.e <- predict(svm.e, newdata=data.test3)
test.pred.e

#how accurate is the model?
confusionMatrix(test.pred.e, data.test3$treatment)








############## checking more cost parameters
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(123)



set.seed(3527)
sub <- sample(forest.df2, size=30000, replace=TRUE)
folds <- groupKFold(forest.df2)

trctrl <- trainControl(method="cv",
                       number=5,
                       classProbs=TRUE,
                       summaryFunction=multiClassSummary,
                       index=folds)

svm.grid <- train(treatment~., data.train1,
               method='svmRadial',
               tuneGrid = grid,
               trControl=trctrl,
               tuneLength = 10)

plot(svm.grid)

test_pred_grid <- predict(svm.grid, newdata=data.test1)
test_pred_grid

confusionMatrix(test_pred_grid, data.test1$treatment)

##############################code for elastic net - need to update




pkgs <- list("glmnet", "doParallel", "foreach", "pROC")
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)

a <- seq(0.1, 0.9, 0.05)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(mdlX, mdlY, family = "binomial", nfold = 10, type.measure = "deviance", paralle = TRUE, alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}
cv3 <- search[search$cvm == min(search$cvm), ]
md3 <- glmnet(mdlX, mdlY, family = "binomial", lambda = cv3$lambda.1se, alpha = cv3$alpha)
coef(md3)

roc(newY, as.numeric(predict(md2, newX, type = "response")))
