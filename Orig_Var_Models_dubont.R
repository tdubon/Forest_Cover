
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

forest.df<-setNames(forest.df, tolower(names(forest.df)))

forest.df=forest.df %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'SpruceFir',
                             ifelse(cover_type == 2 ,'LodgepolePine',
                                    ifelse(cover_type == 3 ,'PonderosaPine',
                                           ifelse(cover_type == 4 ,'CottonwoodWillow',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'DouglasFir',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))


#forest.df=forest.df %>% 
#mutate(cover_type2 = ifelse(cover_type == 'SpruceFir' ,'1',
#ifelse(cover_type == 'LodgepolePine' ,'2',
#ifelse(cover_type == 'PonderosaPine' ,'3',
#ifelse(cover_type == 'CottonwoodWillow' ,'4',
#ifelse(cover_type == 'Aspen' ,'5',
#ifelse(cover_type == 'Aspen' ,'6',
#ifelse(cover_type == 'Krummholz' ,'7','na'))))))))



table(forest.df$cover_type)


### Need to make sure our data is understood correctly by R, since we have a mix of numerical and categorical
forest.df[11:55]<-lapply(forest.df[11:55], factor)


str(forest.df)

############forest.df contains full data set and variables that have not been transformed. 
#############s.forest.df contains a subset of the variables to be modeled


########create data frame excluding other categories, leaving only LodgepolePine and SpruceFir
library(dplyr)

s.forest.df <- dplyr::filter(forest.df, cover_type == "LodgepolePine" | cover_type == "SpruceFir")

library(ggplot2)

#s.forest.df <- subset(forest.df, cover_type == "LodgepolePine" | cover_type == "SpruceFir")
remove_missing(s.forest.df, na.rm=FALSE, vars = names(s.forest.df$cover_type))
s.forest.df$cover_type <- droplevels(s.forest.df$cover_type)
table(s.forest.df$cover_type)




#############################################PARTITIONS FOR FIRST SAMPLE FULL DATA SET - 5%
## First, split the training set off 
set.seed(156) 
split1 <- createDataPartition(forest.df$cover_type, p = .05)[[1]] 
other1 <- forest.df[-split1,] 
training1 <- forest.df[ split1,]

## Now create the evaluation and test sets 
set.seed(934) 
split2 <- createDataPartition(other1$cover_type, p = .30)[[1]] 
testing1 <- other1[ split2,] 

## Determine the predictor names 
predictors1 <- names(training1)[names(training1)!= "treatment"]



head(testing1)
head(training1)  

data.train1 <- data.frame(model.matrix(cover_type~., data=training1))[,-1]
head(data.train1)
data.test1 <- data.frame(model.matrix(cover_type~., data=testing1))[,-1]
head(data.test1)

#add outcome variable back into the dataset
data.train1$cover_type <- training1$cover_type
data.test1$cover_type <- testing1$cover_type


########################################################################
#Training Model 1 
########################################################################

table(data.train1$st)
#data.train1$st81 <- NULL
#data.test1$st81 <- NULL

#data.train1$st71 <- NULL
#data.test1$st71 <- NULL

#data.train1$st361 <- NULL
#data.test1$st361 <- NULL

data.train1$st151 <- NULL
data.test1$st151 <- NULL

library(caret)
trctrl <- trainControl(method="cv",
                       number=5,
                       classProbs=TRUE,
                       summaryFunction=multiClassSummary)

set.seed(123)

svm.c <- train(cover_type~., data.train1,
               method='svmRadial',
               preProcess= c("center", "scale"),
               trControl=trctrl,
               tuneLength = 10)


#trained SVM model result
names(svm.c)
svm.c$coefnames
svm.c$modelInfo
svm.c$results
#test set prediction
test_pred <- predict(svm.c, newdata=data.test1)


#how accurate is the model?
confusionMatrix(test_pred, data.test1$cover_type)




#############################################PARTITIONS FOR FIRST SAMPLE SUBSET OF DATA - 5%
## First, split the training set off 
set.seed(156) 
split1 <- createDataPartition(s.forest.df$cover_type, p = .05)[[1]] 
other1 <- s.forest.df[-split1,] 
s.training <- s.forest.df[ split1,]

## Now create the evaluation and test sets 
set.seed(934) 
split2 <- createDataPartition(other1$cover_type, p = .30)[[1]] 
s.testing <- other1[ split2,] 

## Determine the predictor names 
predictors1 <- names(s.training)[names(s.training)!= "treatment"]


head(s.testing)
head(s.training)  

s.data.train <- data.frame(model.matrix(cover_type~., data=s.training))[,-1]
head(s.data.train)

s.data.test <- data.frame(model.matrix(cover_type~., data=s.testing))[,-1]
head(s.data.test)

#add outcome variable back into the dataset
s.data.train$cover_type <- s.training$cover_type
s.data.test$cover_type <- s.testing$cover_type

######################model using subset of data


########################################################################
#Training Model 1 
########################################################################

s.data.train$st11 <- NULL
s.data.test$st11 <- NULL

s.data.train$st51 <- NULL
s.data.test$st51 <- NULL

s.data.train$st141 <- NULL
s.data.test$st141 <- NULL

s.data.train$st151 <- NULL
s.data.test$st151 <- NULL

s.data.train$st371 <- NULL
s.data.test$st371 <- NULL


library(caret)
trctrl <- trainControl(method="cv",
                       number=5,
                       classProbs=TRUE,
                       summaryFunction=multiClassSummary)

set.seed(123)

svm.c <- train(cover_type~., s.data.train,
               method='svmRadial',
               preProcess= c("center", "scale"),
               trControl=trctrl,
               tuneLength = 10)


#trained SVM model result
names(svm.c)
svm.c$coefnames
svm.c$modelInfo
svm.c$results
#test set prediction
test_pred <- predict(svm.c, newdata=s.data.test)


#how accurate is the model?
confusionMatrix(test_pred, s.data.test$cover_type)
