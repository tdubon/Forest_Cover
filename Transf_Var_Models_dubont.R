

require(multiROC)
library(pROC)
library(randomForest)
require(moments)
library(corrplot)
library(plotly)
library(GGally)
library(caret)


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





table(forest.df$cover_type)


### Need to make sure our data is understood correctly by R, since we have a mix of numerical and categorical
forest.df[11:55]<-lapply(forest.df[11:55], factor)


str(forest.df)



cat("\n","------------------ elevation --------------","\n")

aggregate(forest.df$elevation,by=list(cover_type=forest.df$cover_type),FUN=range)

round(skewness(forest.df$elevation),2)
round(kurtosis(forest.df$elevation),2)

forest.df2 <- forest.df %>% mutate(log_elevation=log(elevation))

par(mfrow=c(1,2))
hist(forest.df[,1], xlab = '',  main=names(forest.df[1]), col="blue")
hist(forest.df2[,56], xlab = '',  main=names(forest.df2[56]), col="red")

boxplot(forest.df[,1],xlab = '',main=names(forest.df[1]),col="blue")
boxplot(forest.df2[,56],xlab = '',main=names(forest.df2[56]),col="red") 


cat("\n","------------------ horizontal distance to hydrology --------------","\n")

aggregate(forest.df$horizontal_distance_to_hydrology,by=list(cover_type=forest.df$cover_type),FUN=mean)


#forest.df <- forest.df %>% mutate(log_hor_dist_hydr=log(horizontal_distance_to_hydrology))
forest.df2 <- forest.df2 %>% mutate(sqrt_hor_dist_hydr=sqrt(horizontal_distance_to_hydrology))


par(mfrow=c(1,2))
hist(forest.df[,4], xlab = '',  main=names(forest.df[4]), col="blue")
hist(forest.df2[,57], xlab = '',  main=names(forest.df2[57]), col="red")
#hist(forest.df[,58], xlab = '',  main=names(forest.df[58]), col="yellow")


boxplot(forest.df[,4],xlab = '',main=names(forest.df[4]),col="blue")
boxplot(forest.df[,57],xlab = '',main=names(forest.df[57]),col="red") 

cat("\n","------------------ horizontal distance to roadways --------------","\n")

aggregate(forest.df$horizontal_distance_to_hydrology,by=list(cover_type=forest.df$cover_type),FUN=mean)
aggregate(forest.df$horizontal_distance_to_hydrology,by=list(cover_type=forest.df$cover_type),FUN=range)


#forest.df <- forest.df %>% mutate(log_hor_dist_rdways=log(horizontal_distance_to_roadways))
forest.df2 <- forest.df2 %>% mutate(sqrt_hor_dist_rdways=sqrt(horizontal_distance_to_roadways))

par(mfrow=c(1,2))
hist(forest.df[,6], xlab = '',  main=names(forest.df[6]), col="blue")
hist(forest.df2[,58], xlab = '',  main=names(forest.df2[58]), col="red")
#hist(forest.df[,59], xlab = '',  main=names(forest.df[59]), col="red")

boxplot(forest.df[,6],xlab = '',main=names(forest.df[6]),col="blue")
boxplot(forest.df2[,58],xlab = '',main=names(forest.df2[58]),col="red") 


cat("\n","------------------ horizontal distance to firepoints --------------","\n")

aggregate(forest.df$horizontal_distance_to_fire_points,by=list(cover_type=forest.df$cover_type),FUN=mean)
aggregate(forest.df$horizontal_distance_to_fire_points,by=list(cover_type=forest.df$cover_type),FUN=range)


#forest.df <- forest.df %>% mutate(log_hor_dist_fire=log(horizontal_distance_to_fire_points))
forest.df2 <- forest.df2 %>% mutate(sqrt_hor_dist_fire=sqrt(horizontal_distance_to_fire_points))

par(mfrow=c(1,2))
hist(forest.df[,10], xlab = '',  main=names(forest.df[10]), col="blue")
hist(forest.df2[,59], xlab = '',  main=names(forest.df2[59]), col="red")
#hist(forest.df[,60], xlab = '',  main=names(forest.df[60]), col="yellow")

boxplot(forest.df[,6],xlab = '',main=names(forest.df[6]),col="blue")
boxplot(forest.df2[,58],xlab = '',main=names(forest.df2[58]),col="red") 





cat("\n","------------------ slope bin --------------","\n")

#aggregate(forest.df$slope,by=list(cover_type=forest.df$cover_type),FUN=mean)
#aggregate(forest.df$slope,by=list(cover_type=forest.df$cover_type),FUN=mean)


# Custom cutpoints using percentiles (20% each) 
#cbs1cuts=as.vector(quantile(forest.df$slope, probs=seq(0,1,0.2), na.rm=TRUE)) # Quantiles 
#cbs1cuts=cbs1cuts[2:(length(cbs1cuts)-1)] # Remove first (min) and last (max) values

# Example: Customized binning 
#result=smbinning.custom(df=forest.df,y="cover_type2",x="slope",cuts=cbs1cuts) 

#table(forest.df$slope) 
#library(dplyr)

#library(binr)
#bins.quantiles(forest.df2$slope, 6, 10, verbose = FALSE)
#forest.df2$bin_slope <- cut(forest.df2$slope, breaks=c(0, 7, 10, 13, 16, 21, 66), 
#labels = c("0-7", "8-10", "11-13", "14-16", "17-21", "22-66"))

#forest.df2$bin_slope[is.na(forest.df2$bin_slope)] <- "0-7"



#table(forest.df2$bin_slope)
#table(is.na(forest.df2$bin_slope))

#ggplot(forest.df2, aes(bin_slope , fill=cover_type )) + geom_bar() +theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
#labs(title="     bin_slope by forest cover type  ")  



cat("\n","------------------ remove original transformed variables --------------","\n")

forest.df2$elevation <- NULL
forest.df2$horizontal_distance_to_hydrology <- NULL
forest.df2$horizontal_distance_to_roadways<- NULL
forest.df2$horizontal_distance_to_fire_points<- NULL
#forest.df2$slope <- NULL



cat("\n","------------------ correlations --------------","\n")

#### Correlation Matrix ###

corr= cor(forest.df2[, c(1:6, 52:54)])
corrplot(corr,method="color", outline=T, cl.pos="n", rect.col="black", tl.col="indianred4",
         addCoef.col="black", addshade = c("negative", "positive", "all"),
         number.digits=2, number.cex=0.60, tl.cex=0.9, cl.cex=1, col=colorRampPalette(c("darksalmon", "white", "deepskyblue"))(100))
#corr

#produce list of correlations by highest value
corr[lower.tri(corr,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
corr=as.data.frame(as.table(corr))  #Turn into a 3-column table
corr=na.omit(corr)  
corr=corr[order(-abs(corr$Freq)),]    #Sort by highest correlation (whether +ve or -ve)
corr

cat("\n","------------------ interaction terms --------------","\n")

forest.df2$i_hillshade9_hillshade3 <- forest.df2$hillshade9*forest.df2$hillshade3 
forest.df2$i_aspect_hillshade3 <- forest.df2$aspect*forest.df2$hillshade3 
forest.df2$i_hillshade12_hillshade3 <- forest.df2$hillshade12*forest.df2$hillshade3
forest.df2$i_vert_dist_to_hydr_sqrt_hor_dist_hydr <- forest.df2$vertical_distance_to_hydrology*forest.df2$sqrt_hor_dist_hydr
forest.df2$i_aspect_hillshade9 <- forest.df2$aspect*forest.df2$hillshade9



#############################################PARTITIONS FOR FIRST SAMPLE - 5%

## First, split the training set off 
set.seed(156) 
split1 <- createDataPartition(forest.df2$cover_type, p = .05)[[1]] 
other1 <- forest.df2[-split1,] 
training1 <- forest.df2[ split1,]

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
#evaluationIND <- data.frame(model.matrix(price_transf~., data=evaluation))[,-1]
data.test1 <- data.frame(model.matrix(cover_type~., data=testing1))[,-1]
head(data.test1)

#add outcome variable back into the dataset
data.train1$cover_type <- training1$cover_type
#evaluationIND <- evaluation$price_transf
data.test1$cover_type <- testing1$cover_type

########################################################################
#Training Model 1 
########################################################################

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


#############################################PARTITIONS FOR FIRST SAMPLE SUBSET OF DATA - 1%

############################################data adjustments: cover_type

s.forest.df <- forest.df

s.forest.df$treatment <- forest.df$cover_type

levels(s.forest.df$treatment) <- list(Other = c("Aspen", "CottonwoodWillow", "PonderosaPine","Krummholz", "DouglasFir"),
                                     LodgepolePine="LodgepolePine", 
                                     SpruceFir="SpruceFir")

table(s.forest.df$treatment)

cover.forest.df <- s.forest.df
s.forest.df[["cover_type"]] <- NULL


######################## TRAINING & TEST DATA FOR REGROUPED CLASS

## First, split the training set off 
set.seed(156) 
split1 <- createDataPartition(s.forest.df$treatment, p = .10)[[1]] 
other1 <- s.forest.df[-split1,] 
s.training <- s.forest.df[ split1,]

## Now create the evaluation and test sets 
set.seed(934) 
split2 <- createDataPartition(other1$treatment, p = .30)[[1]] 
s.testing <- other1[ split2,] 

## Determine the predictor names 
predictors1 <- names(s.training)[names(s.training)!= "treatment"]


head(s.testing)
head(s.training)  

s.data.train <- data.frame(model.matrix(treatment~., data=s.training))[,-1]
head(s.data.train)

s.data.test <- data.frame(model.matrix(treatment~., data=s.testing))[,-1]
head(s.data.test)

#add outcome variable back into the dataset
s.data.train$treatment <- s.training$treatment
s.data.test$treatment <- s.testing$treatment


#######################


s.data.train$st151 <- NULL
s.data.test$st151 <- NULL

s.data.train$st81 <- NULL
s.data.test$st81 <- NULL

s.data.train$st371 <- NULL
s.data.test$st371 <- NULL

s.data.train$st71 <- NULL
s.data.test$st71 <- NULL

s.data.train$st361 <- NULL
s.data.test$st361 <- NULL


library(caret)
trctrl <- trainControl(method="cv",
                       number=5,
                       classProbs=TRUE,
                       summaryFunction=multiClassSummary)

set.seed(1234)

svm.c <- train(treatment~., s.data.train,
               method='svmRadial',
               preProcess= c("center", "scale"),
               trControl=trctrl,
               tuneLength = 10)

plot(svm.c, s.data.train)

#trained SVM model result
names(svm.c)
svm.c$coefnames
svm.c$modelInfo
svm.c$results
#test set prediction
test_pred <- predict(svm.c, newdata=s.data.test, decision.values=TRUE)


#how accurate is the model?
confusionMatrix(test_pred, s.data.test$treatment)



#################################ROC

#training data
library(pROC)
rf_roc <- multiclass.roc(s.data.train$treatment,as.numeric(predict(svm.c)), 
                         percent = TRUE, main="SVM - Performance In-Sample Data",
                         identity=FALSE, print.auc=TRUE, auc.polygon=TRUE,
                         max.auc.polygon=TRUE, auc.polygon.col="gray",
                         max.auc.polygon.col="blue", print.thres=TRUE)
rf_roc #90.76

rs<-rf_roc[['rocs']]
plot.roc(rf_roc[['rocs']][[1]]) 
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))

plot(svm.c)


#test data
rf_roc2 <- multiclass.roc(s.data.test$treatment,as.numeric(predict(svm.c, newdata=s.data.test)), 
                          percent = TRUE, main="SVM - Performance on Out-of-Sample Data",
                          identity=FALSE, print.auc=TRUE, auc.polygon=TRUE,
                          max.auc.polygon=TRUE, auc.polygon.col="gray",
                          max.auc.polygon.col="blue", print.thres=TRUE)
rf_roc2 #89.25


rs2<-rf_roc2[['rocs']]
plot.roc(rf_roc2[['rocs']][[1]]) 
sapply(2:length(rs2),function(i) lines.roc(rs2[[i]],col=i))
plot.roc(rf_roc2[['rocs']][[1]]) 

plot(svm.c)


par(mfrow=c(1,2))
plot.roc(rf_roc[['rocs']][[1]]) 
plot.roc(rf_roc2[['rocs']][[1]]) 





################################SVM Plot

func = predict(svm.c, xgrid, decision.values = TRUE)
func = attributes(func)$decision

xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(svm.c, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)

contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
contour(px1, px2, matrix(func, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)
