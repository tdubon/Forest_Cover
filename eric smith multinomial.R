library(dplyr)
library(glmnet)
library(caret)

forest_cover <- read.table("C:/Users/ems97/OneDrive/Documents/MSPA/454/Forest_Cover/covtype.data.gz",
                           header = FALSE, sep =",")
dim(forest_cover)
names(forest_cover) <- c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology",
                         "Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm",
                         "Horizontal_Distance_To_Fire_Points","Wilderness_Area1",
                         "Wilderness_Area2","Wilderness_Area3","Wilderness_Area4",
                         "soil_Type1","soil_Type2","soil_Type3"
                         ,"soil_Type4","soil_Type5","soil_Type6","soil_Type7","soil_Type8","soil_Type9","soil_Type10","soil_Type11"
                         ,"soil_Type12","soil_Type13","soil_Type14","soil_Type15","soil_Type16","soil_Type17","soil_Type18","soil_Type19"
                         ,"soil_Type20","soil_Type21","soil_Type22","soil_Type23","soil_Type24","soil_Type25","soil_Type26","soil_Type27"
                         ,"soil_Type28","soil_Type29","soil_Type30","soil_Type31","soil_Type32","soil_Type33","soil_Type34","soil_Type35"
                         ,"soil_Type36","soil_Type37","soil_Type38","soil_Type39","soil_Type40","Cover_Type") 


str(forest_cover)


########################Replace wilderness areas with  actual names############




forest_cover<-dplyr::rename(forest_cover, Rawah_Wild_Area = Wilderness_Area1,
                            Neota_Wild_Area = Wilderness_Area2,
                            Comanche_Peak_Wild_Area = Wilderness_Area3,
                            Cache_la_Poudre_Wild_Area = Wilderness_Area4)

forest_cover<-setNames(forest_cover, tolower(names(forest_cover)))

## Assign df=forest_cover

df=forest_cover

## Derive binned variables

df$soil_type1_6 <- 0
df$soil_type38_39_40 <- 0
df$soil_type23_30_12_32_33_29 <- 0
for (i in 1:length(df)) {
  if (df$soil_type1[i]==1 | df$soil_type2[i]==1 | df$soil_type3[i]==1 | df$soil_type4[i]==1 | df$soil_type5[i]==1 | df$soil_type6[i]==1) {
    df$soil_type1_6[i] <- 1
  }
  if (df$soil_type38[i]==1 | df$soil_type39[i]==1 | df$soil_type40[i]==1) {
    df$soil_type38_39_40[i] <- 1
  }
  if (df$soil_type23[i]==1 | df$soil_type30[i]==1 | df$soil_type12[i]==1 | df$soil_type32[i]==1 | 
      df$soil_type33[i]==1 | df$soil_type29[i]==1) {
    df$soil_type23_30_12_32_33_29 <- 1
  }
}



## make copy of cover_type. This is needed for minority class classifier

df$cover_type1=df$cover_type

## reset any class apart from spruce and lodgepole to others.Thus
## we will have three classes namely spruce, lodgepole and others
df=df %>% mutate(cover_type = ifelse(cover_type == 1 ,'Spruce/Fir',
                 ifelse(cover_type == 2 ,'Lodgepole Pine',ifelse(cover_type == 3 ,'Ponderosa Pine',
                                                                 ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                                                        ifelse(cover_type == 5 ,'Aspen',
                                                                               ifelse(cover_type == 6 ,'Douglas-fir',
                                                                                      ifelse(cover_type == 7 ,'Krummholz',NA))))))))





set.seed(1234)

library(glmnet)
trainIndex <- createDataPartition(df$cover_type, p = .7, 
                                  list = FALSE, 
                                  times = 1)
df_train <- df[ trainIndex,]
df_test  <- df[-trainIndex,]


## create df_train_others and df_test others to create a classifier only for 
## Aspen ,Cottonwood/Willow, Douglas-fir, Krummholz &   Ponderosa Pine 

df_train_others <-df[ trainIndex,]
df_test_others  <-df[-trainIndex,]

df_test_all <-df[-trainIndex,]


## so we two classifiers 
## one on df_train containing Lodgepole Pine,Spruce/Fir and others
## second classifier on df_train_others classifying among Aspen ,Cottonwood/Willow, Douglas-fir, Krummholz &   Ponderosa Pine 



##remove cover_type1
df_train <-subset(df_train, select=-c(cover_type1))
df_test <-subset(df_test, select=-c(cover_type1))

####others

df_train_others$cover_type=df_train_others$cover_type1
df_test_others$cover_type=df_test_others$cover_type1


#df_train_others ==> only Aspen ,Cottonwood/Willow, Douglas-fir, Krummholz &   Ponderosa Pine 
df_train_others=df_train_others %>% filter(cover_type !=1  & cover_type != 2 )
dim(df_train_others)

dim(df_test_others)

table(df_train_others$cover_type)
table(df_test_others$cover_type)

df_train_others= df_train_others %>% 
  mutate(cover_type =   ifelse(cover_type == 3 ,'Ponderosa Pine',
                               ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                      ifelse(cover_type == 5 ,'Aspen',
                                             ifelse(cover_type == 6 ,'Douglas-fir',
                                                    ifelse(cover_type == 7 ,'Krummholz','na'))))))

df_test_others= df_test_others %>% 
  mutate(cover_type =   ifelse(cover_type == 3 ,'Ponderosa Pine',
                               ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                      ifelse(cover_type == 5 ,'Aspen',
                                             ifelse(cover_type == 6 ,'Douglas-fir',
                                                    ifelse(cover_type == 7 ,'Krummholz','na'))))))

table(df_train_others$cover_type)
table(df_test_others$cover_type)



df_train_others <-df_train_others[-56]
df_test_others <-df_test_others[-56]


df_test_all$cover_type=df_test_all$cover_type1
df_test_all = df_test_all %>% mutate(cover_type = ifelse(cover_type == 1 ,'Spruce/Fir',
                                                         ifelse(cover_type == 2 ,'Lodgepole Pine',ifelse(cover_type == 3 ,'Ponderosa Pine',
                                                                                                         ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                                                                                                ifelse(cover_type == 5 ,'Aspen',
                                                                                                                       ifelse(cover_type == 6 ,'Douglas-fir',
                                                                                                                              ifelse(cover_type == 7 ,'Krummholz',NA))))))))
table(df_test_all$cover_type)

df_test_all <-df_test_all[-56]


df_train$cover_type = as.factor(df_train$cover_type)
df_train_others$cover_type = as.factor(df_train_others$cover_type)
df_test$cover_type = as.factor(df_test$cover_type)
df_test_others$cover_type = as.factor(df_test_others$cover_type)
df_test_all$cover_type = as.factor(df_test_all$cover_type)

x.train_others <- model.matrix(~.*.-1, df_train_others[,-55])
y.train_others<-df_train_others$cover_type


x.test_others <- model.matrix(~.*.-1, df_test_others[,-55]) 
y.test_others<-df_test_others$cover_type


## random forest model

library(randomForest)
set.seed(1234)
model.rf <- randomForest(cover_type ~.,
                         df_train, importance=T ,ntree = 100, do.trace=T)
confusionMatrix(predict(model.rf), df_train$cover_type)

plot(model.rf)

summary(model.rf)

importance(model.rf)
varImpPlot(model.rf,sort = T,main="Variable Importance",n.var=50)


######Multinomial Logistic Regression

library(nnet)

#Main Types
set.seed(1234)
mlr <- multinom(cover_type ~ hillshade_3pm + log(hillshade_9am+1) + elevation + horizontal_distance_to_fire_points
                + horizontal_distance_to_roadways + log(vertical_distance_to_hydrology+174) + hillshade_noon
                + soil_type33 + sqrt(horizontal_distance_to_hydrology+1) + soil_type20 + aspect
                + soil_type31 + soil_type22 + hillshade_9am:hillshade_noon, data=df_train)

set.seed(1234)
mlr <- multinom(cover_type ~ hillshade_3pm + log(hillshade_9am+1) + elevation + horizontal_distance_to_fire_points
                + horizontal_distance_to_roadways + log(vertical_distance_to_hydrology+174) + hillshade_noon
                + sqrt(horizontal_distance_to_hydrology+1) + soil_type1_6 + aspect + soil_type38_39_40
                + soil_type23_30_12_32_33_29 + soil_type10 + comanche_peak_wild_area, data=df_train)


mlr_predict <- predict(mlr,newdata=df_train,type='class')
confusionMatrix(as.factor(mlr_predict),as.factor(df_train$cover_type))

library(pROC)
mr <- multiclass.roc(as.numeric(df_test$cover_type),as.numeric(mlr_predict), percent = TRUE)

mlr_predict <- predict(mlr,newdata=df_test,type='class')
confusionMatrix(as.factor(mlr_predict),as.factor(df_test$cover_type))


library(pROC)
mr <- multiclass.roc(as.numeric(df_train$cover_type),as.numeric(mlr_predict), percent = TRUE)

mlr_prediction <- predict(mlr,newdata=test.df,type="class")