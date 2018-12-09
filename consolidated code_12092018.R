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
df_gbm=forest_cover
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

#############code for gbm,lasso,ridge,elasticnet and ann using keras
## code for gbm, lasso,ridge ,elastic net and ann

###########################gbm#####gbm#####gbm######################################
mapping_df <- read.csv("D:/Data/Project_Folders/mtg_coverdata/mapping.csv",
                       sep =",")   

forest_cover<-setNames(forest_cover, tolower(names(forest_cover)))

df=forest_cover
df$cover_type1=df$cover_type
df_mod<-df

df_mod$wld_area <- names(df_mod[11:14])[max.col(df_mod[11:14])]
table(df_mod$wld_area)

df_mod$soil_type <- names(df_mod[15:54])[max.col(df_mod[15:54])]
table(df_mod$soil_type)


df_mod$wld_area<-as.factor(df_mod$wld_area)
df_mod$soil_type<-as.factor(df_mod$soil_type)

library(sqldf)
#sqldf(" select  climate,
#   case
#      when climate like '%1%' then   'lower montane dry'
#      when climate like '%2%' then   'lower montane'
#      when climate like '%3%' then   'montane dry'
#      when climate like '%4%' then   'montane'
#      when climate like '%5%' then   'montane dry and montane'
#      when climate like '%6%' then   'montane and subalpine'
#      when climate like '%7%' then   'subalpine'
#      when climate like '%8%' then   'alpine'
#      else 'unknown'
##      end exd
#      from df_mod"
#)

##new features

df_final<-sqldf('select df_mod.*, mapping_df.soil_desc,
                mapping_df.climate,
                mapping_df.geology
                from df_mod left outer join mapping_df   on 
                df_mod.soil_type = mapping_df.soil_type')




#drop column 59 which is soil_type from outer join

#euclidian distance 
#Euclidean_Distance_To_Hydrology	Square root of the sum of the squared horizontal & vertical distances to water


df_final$distance_eucld<-sqrt(df_final$horizontal_distance_to_hydrology**2+df_final$vertical_distance_to_hydrology**2) 


df_final<-df_final[-(56:57)]


##transformation to sqrt and log 
transform <- function(x,method) {
  
  #Square-root:
  if(method == "sqrt") {print("Applying Square-root transformation:"); y=sqrt(x+1)}
  
  #Log:
  if(method == "log") {print("Applying Log transformation:"); y=log(x+ 1 - min(x))}
  
  return(y)
  
}

df_final$slope=transform(df_final$slope,method="sqrt")
df_final[,4]=transform(df_final[,4],method="sqrt")
df_final[,6]=transform(df_final[,6],method="sqrt")
df_final[,10]=transform(df_final[,10],method="sqrt")
df_final[,60]=transform(df_final[,60],method="sqrt")
df_final[,5]=transform(df_final[,5],method="log")


##delete the redudant columns
#3  ==>   slope                              sqrt  
#4   ==>  horizontal_distance_to_hydrology   sqrt
#5   ==>  vertical_distance_to_hydrology     log
#6   ==>  horizontal_distance_to_roadways    sqrt
#10  ==>  horizontal_distance_to_fire_points sqrt
#60  ==>  distance_eucld                     sqrt 






#### Interactions

df_final$aspectelevation =  df_final$aspect *df_final$elevation
df_final$aspecthillshade_3pm =  df_final$aspect *df_final$hillshade_3pm
df_final$aspecthillshade_9am =  df_final$aspect *df_final$hillshade_9am
df_final$aspecthillshade_noon =  df_final$aspect *df_final$hillshade_noon
df_final$aspecthorizontal_distance_to_fire_points =  df_final$aspect *df_final$horizontal_distance_to_fire_points
df_final$aspecthorizontal_distance_to_hydrology =  df_final$aspect *df_final$horizontal_distance_to_hydrology
df_final$aspecthorizontal_distance_to_roadways =  df_final$aspect *df_final$horizontal_distance_to_roadways
df_final$aspectslope =  df_final$aspect *df_final$slope

df_final$aspectvertical_distance_to_hydrology =  df_final$aspect *df_final$vertical_distance_to_hydrology
df_final$elevationaspect =  df_final$elevation *df_final$aspect

df_final$elevationhillshade_3pm =  df_final$elevation *df_final$hillshade_3pm
df_final$elevationhillshade_9am =  df_final$elevation *df_final$hillshade_9am
df_final$elevationhillshade_noon =  df_final$elevation *df_final$hillshade_noon
df_final$elevationhorizontal_distance_to_fire_points =  df_final$elevation *df_final$horizontal_distance_to_fire_points
df_final$elevationhorizontal_distance_to_hydrology =  df_final$elevation *df_final$horizontal_distance_to_hydrology
df_final$elevationhorizontal_distance_to_roadways =  df_final$elevation *df_final$horizontal_distance_to_roadways

df_final$elevationslope =  df_final$elevation *df_final$slope
df_final$elevationvertical_distance_to_hydrology =  df_final$elevation *df_final$vertical_distance_to_hydrology
df_final$slopeaspect =  df_final$slope *df_final$aspect

df_final$slopeelevation =  df_final$slope *df_final$elevation
df_final$slopehillshade_3pm =  df_final$slope *df_final$hillshade_3pm
df_final$slopehillshade_9am =  df_final$slope *df_final$hillshade_9am
df_final$slopehillshade_noon =  df_final$slope *df_final$hillshade_noon
df_final$slopehorizontal_distance_to_fire_points =  df_final$slope *df_final$horizontal_distance_to_fire_points
df_final$slopehorizontal_distance_to_hydrology =  df_final$slope *df_final$horizontal_distance_to_hydrology
df_final$slopehorizontal_distance_to_roadways =  df_final$slope *df_final$horizontal_distance_to_roadways

df_final$slopevertical_distance_to_hydrology =  df_final$slope *df_final$vertical_distance_to_hydrology

set.seed(1234)





df_final=df_final %>% mutate(cover_type = ifelse(cover_type == 1 ,'Spruce/Fir',
                                                 ifelse(cover_type == 2 ,'Lodgepole Pine','others')))
df_final<-df_final[,-which(names(df_final) %in% c("soil_type15"))]
library(glmnet)
trainIndex <- createDataPartition(df_final$cover_type, p = .7, 
                                  list = FALSE, 
                                  times = 1)
df_train <- df_final[ trainIndex,]
df_test  <- df_final[-trainIndex,]

df_plot_train<-df_final[ trainIndex,]
df_plot_train$cover_type=df_train$cover_type1
df_plot_train <-df_plot_train[-55]

df_plot_test<-df_final[ -trainIndex,]
df_plot_test$cover_type=df_test$cover_type1
df_plot_test <-df_plot_test[-55]



df_plot_train=df_plot_train %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'Spruce',
                             ifelse(cover_type == 2 ,'Lodgepole',
                                    ifelse(cover_type == 3 ,'Ponderosa',
                                           ifelse(cover_type == 4 ,'Cottonwood',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'Douglas',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))



df_plot_test=df_plot_test %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'Spruce',
                             ifelse(cover_type == 2 ,'Lodgepole',
                                    ifelse(cover_type == 3 ,'Ponderosa',
                                           ifelse(cover_type == 4 ,'Cottonwood',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'Douglas',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))





true_label_train <- dummies::dummy(df_plot_train$cover_type)
true_label_train <- data.frame(true_label_train)

true_label_test <- dummies::dummy(df_plot_test$cover_type)
true_label_test <- data.frame(true_label_test)


####################################################
df_train_others <-df_final[ trainIndex,]
df_test_others  <-df_final[-trainIndex,]


df_train_all <-df_final[ trainIndex,]
df_test_all  <-df_final[-trainIndex,]



##remove cover_type1
df_train <-df_train[-55]
df_test <-df_test[-55]
####others

df_train_others$cover_type=df_train_others$cover_type1
df_test_others$cover_type=df_test_others$cover_type1

df_train_others <-df_train_others[-55]
df_test_others <-df_test_others[-55]


df_train_all$cover_type=df_train_all$cover_type1
df_test_all$cover_type=df_test_all$cover_type1

df_train_all <-df_train_all[-55]
df_test_all <-df_test_all[-55]


df_train_others=df_train_others %>% filter(cover_type !=1  & cover_type != 2 )
dim(df_train_others)


df_test_others=df_test_others %>% filter(cover_type !=1  & cover_type != 2)
dim(df_test_others)

df_train_others$cover_type=droplevels(as.factor(df_train_others$cover_type))
df_test_others$cover_type=droplevels(as.factor(df_test_others$cover_type))


df_test_all=df_test_all %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'Spruce/Fir',
                             ifelse(cover_type == 2 ,'Lodgepole Pine',
                                    ifelse(cover_type == 3 ,'Ponderosa Pine',
                                           ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'Douglas-fir',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))

df_train_all=df_train_all %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'Spruce/Fir',
                             ifelse(cover_type == 2 ,'Lodgepole Pine',
                                    ifelse(cover_type == 3 ,'Ponderosa Pine',
                                           ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'Douglas-fir',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))



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

dim(df_train)
dim(df_test)
dim(df_train_others)
dim(df_test_others)
dim(df_train_all)
dim(df_test_all)


system.time(gbmModel <- gbm(cover_type ~ .,
                            data= df_train ,
                            interaction.depth=3,
                            n.trees = 750 ,
                            n.minobsinnode=400,
                            cv.folds=3,
                            shrinkage=.03,
                            verbose=TRUE,
                            n.cores = 6 , 
                            distribution = "multinomial"))

bestreee = gbm.perf(gbmModel)


gbm_pred<-predict(gbmModel,
                  newdata = df_test,
                  n.trees = bestreee,
                  type = "response")
gbm_pred<-predict(gbmModel,
                  newdata = df_train,
                  n.trees = gbm.perf(gbmModel),
                  type = "response")
predict_gbm <- apply(gbm_pred, 1, which.max)
predict_gbm<-as.factor(colnames(gbm_pred)[predict_gbm])
#confusionMatrix(as.factor(predict_gbm),as.factor(df_test$cover_type))

bestreee = gbm.perf(gbmModel)




#save(gbmModel , file="D:/Data/Project_Folders/mtg_coverdata/gbmModel_final.Rdata")
gbmModel <- get(load('D:/Data/Project_Folders/mtg_coverdata/gbmModel_final.Rdata'))
gbmModel1 <- get(load('D:/Data/Project_Folders/mtg_coverdata/gbmModel1_final.Rdata'))


varimp_cols<-c(as.character(droplevels(summary(gbmModel)$var[1:30])),'cover_type') 

##78.76

system.time(gbmModel1 <- gbm(cover_type ~ .,
                             data= df_train[varimp_cols] ,
                             interaction.depth=3,
                             n.trees = 750 ,
                             n.minobsinnode=400,
                             cv.folds=3,
                             shrinkage=.03,
                             verbose=TRUE,
                             n.cores = 6 , 
                             distribution = "multinomial"))


plot(gbmModel)

bestreee1 = gbm.perf(gbmModel1)
gbm_pred1<-predict(gbmModel1,
                   newdata = df_test[varimp_cols],
                   n.trees = bestreee1,
                   type = "response")
predict_gbm1 <- apply(gbm_pred1, 1, which.max)
predict_gbm1<-as.factor(colnames(gbm_pred1)[predict_gbm1])
confusionMatrix(as.factor(predict_gbm1),as.factor(df_test$cover_type))
save(gbmModel1 , file="D:/Data/Project_Folders/mtg_coverdata/gbmModel1_final.Rdata")
#gbmModel1 <- get(load('D:/forestcover/Forest_Cover/gbm_model1.Rdata')




##

df_predict_others=df_test[which(as.character(predict_gbm1)=='others'),]

df_predict_others_train=df_train[which(as.character(predict_gbm)=='others'),]


#############3others

system.time(gbmModel_others <- gbm(cover_type ~ .,
                                   data= df_train_others ,
                                   interaction.depth=3,
                                   n.trees = 750 ,
                                   n.minobsinnode=400,
                                   cv.folds=3,
                                   shrinkage=.03,
                                   verbose=TRUE,
                                   n.cores = 6 , 
                                   distribution = "multinomial"))

bestreee = gbm.perf(gbmModel_others)
gbm_pred_others<-predict(gbmModel_others,
                         newdata = df_train_others,
                         n.trees = gbm.perf(gbmModel_others),
                         type = "response")

predict_gbm_others <- apply(gbm_pred_others, 1, which.max)
predict_gbm_others <-as.factor(colnames(gbm_pred_others)[predict_gbm_others])
confusionMatrix(as.factor(predict_gbm_others),as.factor(df_test_others$cover_type))



#save(gbmModel_others , file="D:/Data/Project_Folders/mtg_coverdata/gbmModel_others.Rdata")

#gbmModel_others <- get(load('D:/forestcover/Forest_Cover/gbmmodel_others.Rdata')






varimp_cols_others<-c(as.character(droplevels(summary(gbmModel_others)$var[1:30])),'cover_type') 





system.time(gbmModel1_others <- gbm(cover_type ~ .,
                                    data= df_train_others[varimp_cols_others] ,
                                    interaction.depth=3,
                                    n.trees = 750 ,
                                    n.minobsinnode=400,
                                    cv.folds=3,
                                    shrinkage=.03,
                                    verbose=TRUE,
                                    n.cores = 6 , 
                                    distribution = "multinomial"))

bestreee1 = gbm.perf(gbmModel1_others)
gbm_pred1_others<-predict(gbmModel1_others,
                          newdata = df_test_others[varimp_cols_others],
                          n.trees = bestreee1,
                          type = "response")
predict_gbm1_others <- apply(gbm_pred1_others, 1, which.max)
predict_gbm1_others<-as.factor(colnames(gbm_pred1_others)[predict_gbm1_others])
confusionMatrix(as.factor(predict_gbm1_others),as.factor(df_test_others$cover_type))
save(gbmModel1_others , file="D:/Data/Project_Folders/mtg_coverdata/gbmModel1_others.Rdata")
#gbmModel1_others <- get(load('D:/forestcover/Forest_Cover/gbmmodel1_others.Rdata')



#87.74
table(as.factor(predict_gbm1_others),as.factor(df_test_others$cover_type))

##final prediction on minority classes ..predict all others absed on model from others

gbm_pred2_others<-predict(gbmModel_others,
                          newdata = df_predict_others_train,
                          n.trees = gbm.perf(gbmModel_others),
                          type = "response")

gbm_pred2_others<-predict(gbmModel1_others,
                          newdata = df_predict_others[varimp_cols_others],
                          n.trees = bestreee1,
                          type = "response")
predict_gbm2_others <- apply(gbm_pred2_others, 1, which.max)
predict_gbm2_others<-as.factor(colnames(gbm_pred2_others)[predict_gbm2_others])
#confusionMatrix(as.factor(predict_gbm2_others),as.factor(df_predict_others$cover_type))

df1= as.character(predict_gbm)
df2=as.character(predict_gbm2_others)
idx=which(as.character(predict_gbm)=='others')
df1[idx]=df2[1:length(df2)]
table(df1)

confusionMatrix(as.factor(df1),as.factor(df_train_all$cover_type))

##77.38

###probability for auc
#gbm_pred1_prob


df_nb_train<-df_train[varimp_cols]
df_nb_test<-df_test[varimp_cols]

y.train<-as.factor(df_nb_train$cover_type)
y.test<-as.factor(df_nb_test$cover_type)

nb1<-naiveBayes(y.train~.,data=df_nb_train[,-31])
pred_nb1<-predict(nb1,df_nb_test[,-31],type="raw")
pred_nb2<-(apply(pred_nb1,1,which.max))
pred_nb2<-colnames(pred_nb1)[pred_nb2]
confusionMatrix(as.factor(pred_nb2),as.factor(y.test))


pred_nb1<-predict(nb1,df_nb_test[,-54])
#pred_nb2<-(apply(pred_nb1,1,which.max))
#pred_nb2<-colnames(pred_nb1)[pred_nb2]
confusionMatrix(as.factor(pred_nb1),as.factor(y.test))   



gbmModel<-get(load('D:/Data/Project_Folders/mtg_coverdata/gbmModel_final.Rdata'))
gbmModel_others<-get(load('D:/Data/Project_Folders/mtg_coverdata/gbmModel_others.Rdata'))


bestreee = gbm.perf(gbmModel)
gbm_pred<-predict(gbmModel,
                  newdata = df_test,
                  n.trees = bestreee,
                  type = "response")
#predict_gbm <- apply(gbm_pred, 1, which.max)
#predict_gbm<-as.factor(colnames(gbm_pred)[predict_gbm])

gbm_pred <- data.frame(gbm_pred)
colnames(gbm_pred) <- paste(colnames(gbm_pred), "_gbm")


bestreee = gbm.perf(gbmModel_others)
gbm_pred1<-predict(gbmModel_others,
                   newdata = df_test,
                   n.trees = gbm.perf(gbmModel_others),
                   type = "response")
#predict_gbm1 <- apply(gbm_pred1, 1, which.max)
#predict_gbm1<-as.factor(colnames(gbm_pred)[predict_gbm1])

gbm_pred1 <- data.frame(gbm_pred1)
colnames(gbm_pred1) <- paste(colnames(gbm_pred1), "_gbm")



gbm_pred<-cbind(gbm_pred[,-2],gbm_pred1)
colnames(gbm_pred)

true_label<-true_label_test
pred<-gbm_pred
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")
colnames(true_label) <- gsub("cover_type", "", colnames(true_label))
colnames(true_label) <- gsub(" ", "", colnames(true_label))
final_df <- cbind(true_label, pred)
colnames(final_df)


cols<-c("Lodgepole_pred_gbm",
        "Spruce_pred_gbm",
        "Aspen_pred_gbm",
        "Cottonwood_pred_gbm",
        "Douglas_pred_gbm",
        "Krummholz_pred_gbm",
        "Ponderosa_pred_gbm")
names(final_df)[8:14] <-cols  
colnames(final_df)

final_df<-final_df[,c(1,2,3,4,5,6,7,10,11,12,13,8,14,9)]

colnames(final_df)

require(multiROC)

roc_res <- multi_roc(final_df,force_diag=T)
pr_res <- multi_pr(final_df, force_diag=T)


plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)

unlist(roc_res$AUC)

write.csv(unlist(roc_res$AUC), file = "D:/Data/Project_Folders/mtg_coverdata/gbm_test_auc.csv")
#write.csv(unlist(roc_res$AUC), file = "D:/Data/Project_Folders/mtg_coverdata/gbm_train_auc.csv")


require(ggplot2)
gbm_roc<-ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))


png(filename="D:/Data/Project_Folders/mtg_coverdata/gbm_roc.png")
plot(gbm_roc)
dev.off()
############################end of gbm #####################################3



#########lasso,ridge,elastic net#########lasso , ridge,elasticnet ###########

df_train <- df_final[ trainIndex,]
df_test  <- df_final[-trainIndex,]

df_train_others <-df_final[ trainIndex,]
df_test_others  <-df_final[-trainIndex,]

#####################
#########################################
df_plot_train<-df_final[ trainIndex,]
df_plot_train$cover_type=df_train$cover_type1
df_plot_train <-df_plot_train[-55]

df_plot_test<-df_final[ -trainIndex,]
df_plot_test$cover_type=df_test$cover_type1
df_plot_test <-df_plot_test[-55]



df_plot_train=df_plot_train %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'Spruce',
                             ifelse(cover_type == 2 ,'Lodgepole',
                                    ifelse(cover_type == 3 ,'Ponderosa',
                                           ifelse(cover_type == 4 ,'Cottonwood',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'Douglas',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))



df_plot_test=df_plot_test %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'Spruce',
                             ifelse(cover_type == 2 ,'Lodgepole',
                                    ifelse(cover_type == 3 ,'Ponderosa',
                                           ifelse(cover_type == 4 ,'Cottonwood',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'Douglas',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))





true_label_train <- dummies::dummy(df_plot_train$cover_type)
true_label_train <- data.frame(true_label_train)

true_label_test <- dummies::dummy(df_plot_test$cover_type)
true_label_test <- data.frame(true_label_test)

##################################################


#######################

##remove cover_type1
df_train <-df_train[-55]
df_test <-df_test[-55]



dim(df_train)
dim(df_test)



df_train_others$cover_type=df_train_others$cover_type1
df_test_others$cover_type=df_test_others$cover_type1
df_train_others <-df_train_others[-55]
df_test_others <-df_test_others[-55]

dim(df_train_others)
dim(df_test_others)

x.train <- model.matrix(~.-1, df_train[,-54])
y.train<-df_train$cover_type


x.test <- model.matrix(~.-1, df_test[,-54]) 
y.test<-df_test$cover_type

dim(x.train)
dim(x.test)



####others


df_train_others=df_train_others %>% filter(cover_type !=1  & cover_type != 2 )
dim(df_train_others)

df_test_others=df_test_others %>% filter(cover_type !=1  & cover_type != 2)
dim(df_test_others)

table(df_train_others$cover_type)
table(df_test_others$cover_type)

df_train_others= df_train_others %>% 
  mutate(cover_type =   ifelse(cover_type == 3 ,'Ponderosa Pine',
                               ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                      ifelse(cover_type == 5 ,'Aspen',
                                             ifelse(cover_type == 6 ,'Douglas-fir',
                                                    ifelse(cover_type == 7 ,'Krummholz','na'))))))
dim(df_train_others)

df_test_others= df_test_others %>% 
  mutate(cover_type =   ifelse(cover_type == 3 ,'Ponderosa Pine',
                               ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                      ifelse(cover_type == 5 ,'Aspen',
                                             ifelse(cover_type == 6 ,'Douglas-fir',
                                                    ifelse(cover_type == 7 ,'Krummholz','na'))))))
dim(df_test_others)




x.train_others <- model.matrix(~.-1, df_train_others[,-54])
y.train_others<-df_train_others$cover_type


x.test_others <- model.matrix(~.-1, df_test_others[,-54]) 
y.test_others<-df_test_others$cover_type

dim(x.train_others)
dim(x.test_others)

library(doParallel)
library(doSNOW)
library(foreach)

t2 = Sys.time()
cl = makeCluster(9)
registerDoSNOW(cl)
set.seed(seed)

fit_lasso<-cv.glmnet(x.train,
                     y.train,
                     family="multinomial",
                     type.measure = "class",
                     alpha=1,
                     parallel = TRUE)

save(fit_lasso , file="D:/Data/Project_Folders/mtg_coverdata/fit_lasso1.Rdata")

predict_lasso<-predict(fit_lasso,newx=x.test,s=fit_lasso$lambda.min, type = "class")
confusionMatrix(as.factor(predict_lasso),as.factor(y.test))
#tmp_coeffs_lasso <- coef(fit_lasso, s = "lambda.min")
stopCluster(cl)

predict_lasso<-predict(fit_lasso,newx=x.train,s=fit_lasso$lambda.min, type = "class")
confusionMatrix(as.factor(predict_lasso),as.factor(y.test))

message("...Start to train glmnet...")
print((Sys.time() - t2))

coef(fit_lasso, s = "lambda.min")
plot(fit_lasso, xvar = "lambda", label = TRUE)



t2 = Sys.time()
cl = makeCluster(9)
registerDoSNOW(cl)
set.seed(seed)

fit_lasso_others<-cv.glmnet(x.train_others,
                            y.train_others,
                            family="multinomial",
                            type.measure = "class",
                            alpha=1,
                            parallel = TRUE)

save(fit_lasso_others , file="D:/Data/Project_Folders/mtg_coverdata/fit_lasso_others1.Rdata")

predict_lasso_others<-predict(fit_lasso_others,newx=x.test_others,s=fit_lasso_others$lambda.min, type = "class")
predict_lasso_others<-predict(fit_lasso_others,newx=x.train_others,s=fit_lasso_others$lambda.min, type = "class")


confusionMatrix(as.factor(predict_lasso_others),as.factor(y.test_others))
#tmp_coeffs_lasso_others <- coef(fit_lasso_others, s = "lambda.min")
stopCluster(cl)


message("...Start to train glmnet...")
print((Sys.time() - t2))

coef(fit_lasso_others, s = "lambda.min")
plot(fit_lasso_others, xvar = "lambda", label = TRUE)


df_predict_others=df_test[which(as.character(predict_lasso)=='others'),]
df_predict_others=df_train[which(as.character(predict_lasso)=='others'),]

##for these columns need to be modified
dim(df_predict_others)




df_train_all <- df_final[ trainIndex,]
df_test_all  <- df_final[-trainIndex,]

df_train_all$cover_type=df_train_all$cover_type1
df_test_all$cover_type=df_test_all$cover_type1


df_test_all=df_test_all %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'Spruce/Fir',
                             ifelse(cover_type == 2 ,'Lodgepole Pine',
                                    ifelse(cover_type == 3 ,'Ponderosa Pine',
                                           ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'Douglas-fir',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))



df_train_all=df_train_all %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'Spruce/Fir',
                             ifelse(cover_type == 2 ,'Lodgepole Pine',
                                    ifelse(cover_type == 3 ,'Ponderosa Pine',
                                           ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'Douglas-fir',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))




x.lasso_others <- model.matrix(~.-1, df_predict_others[,-54]) 
y.lasso_others<-df_predict_others$cover_type

predict_lasso_others<-predict(fit_lasso_others,newx=x.lasso_others ,s=fit_lasso_others$lambda.min, type = "class")

df1= as.character(predict_lasso)
df2=as.character(predict_lasso_others)
idx=which(as.character(predict_lasso)=='others')
df1[idx]=df2[1:length(df2)]
table(df1)

confusionMatrix(as.factor(df1),as.factor(df_train_all$cover_type))





t2 = Sys.time()
cl = makeCluster(9)
registerDoSNOW(cl)
set.seed(seed)

fit_ridge<-cv.glmnet(x.train,
                     y.train,
                     family="multinomial",
                     type.measure = "class",
                     alpha=0,
                     parallel = TRUE)

save(fit_ridge , file="D:/Data/Project_Folders/mtg_coverdata/fit_ridge1.Rdata")

predict_ridge<-predict(fit_ridge,newx=x.test,s=fit_ridge$lambda.min, type = "class")
predict_ridge<-predict(fit_ridge,newx=x.train,s=fit_ridge$lambda.min, type = "class")

confusionMatrix(as.factor(predict_ridge),as.factor(y.test))
#tmp_coeffs_ridge <- coef(fit_ridge, s = "lambda.min")
stopCluster(cl)


message("...Start to train glmnet...")
print((Sys.time() - t2))

coef(fit_ridge, s = "lambda.min")
plot(fit_ridge, xvar = "lambda", label = TRUE)



t2 = Sys.time()
cl = makeCluster(9)
registerDoSNOW(cl)
set.seed(seed)

fit_ridge_others<-cv.glmnet(x.train_others,
                            y.train_others,
                            family="multinomial",
                            type.measure = "class",
                            alpha=0,
                            parallel = TRUE)

save(fit_ridge_others , file="D:/Data/Project_Folders/mtg_coverdata/fit_ridge_others1.Rdata")

predict_ridge_others<-predict(fit_ridge_others,newx=x.test_others,s=fit_ridge_others$lambda.min, type = "class")
predict_ridge_others<-predict(fit_ridge_others,newx=x.train_others,s=fit_ridge_others$lambda.min, type = "class")



confusionMatrix(as.factor(predict_ridge_others),as.factor(y.test_others))
#tmp_coeffs_ridge_others <- coef(fit_ridge_others, s = "lambda.min")
stopCluster(cl)


message("...Start to train glmnet...")
print((Sys.time() - t2))

coef(fit_ridge_others, s = "lambda.min")
plot(fit_ridge_others, xvar = "lambda", label = TRUE)


df_predict_others=df_test[which(as.character(predict_ridge)=='others'),]
df_predict_others=df_train[which(as.character(predict_ridge)=='others'),]


##for these columns need to be modified
dim(df_predict_others)




df_train_all <- df_final[ trainIndex,]
df_test_all  <- df_final[-trainIndex,]

df_train_all$cover_type=df_train_all$cover_type1
df_test_all$cover_type=df_test_all$cover_type1


df_test_all=df_test_all %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'Spruce/Fir',
                             ifelse(cover_type == 2 ,'Lodgepole Pine',
                                    ifelse(cover_type == 3 ,'Ponderosa Pine',
                                           ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'Douglas-fir',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))



x.ridge_others <- model.matrix(~.-1, df_predict_others[,-54]) 
y.ridge_others<-df_predict_others$cover_type

predict_ridge_others<-predict(fit_ridge_others,newx=x.ridge_others ,s=fit_ridge_others$lambda.min, type = "class")

df1= as.character(predict_ridge)
df2=as.character(predict_ridge_others)
idx=which(as.character(predict_ridge)=='others')
df1[idx]=df2[1:length(df2)]
table(df1)

confusionMatrix(as.factor(df1),as.factor(df_train_all$cover_type))


t2 = Sys.time()
cl = makeCluster(9)
registerDoSNOW(cl)
set.seed(seed)

fit_elastic<-cv.glmnet(x.train,
                       y.train,
                       family="multinomial",
                       type.measure = "class",
                       alpha=.5,
                       parallel = TRUE)

save(fit_elastic , file="D:/Data/Project_Folders/mtg_coverdata/fit_elastic1.Rdata")

predict_elastic<-predict(fit_elastic,newx=x.test,s=fit_elastic$lambda.min, type = "class")
predict_elastic<-predict(fit_elastic,newx=x.train,s=fit_elastic$lambda.min, type = "class")

confusionMatrix(as.factor(predict_elastic),as.factor(y.test))
#tmp_coeffs_elastic <- coef(fit_elastic, s = "lambda.min")
stopCluster(cl)


message("...Start to train glmnet...")
print((Sys.time() - t2))

coef(fit_elastic, s = "lambda.min")
plot(fit_elastic, xvar = "lambda", label = TRUE)



t2 = Sys.time()
cl = makeCluster(9)
registerDoSNOW(cl)
set.seed(seed)

fit_elastic_others<-cv.glmnet(x.train_others,
                              y.train_others,
                              family="multinomial",
                              type.measure = "class",
                              alpha=.5,
                              parallel = TRUE)

save(fit_elastic_others , file="D:/Data/Project_Folders/mtg_coverdata/fit_elastic_others1.Rdata")

predict_elastic_others<-predict(fit_elastic_others,newx=x.test_others,s=fit_elastic_others$lambda.min, type = "class")
predict_elastic_others<-predict(fit_elastic_others,newx=x.train_others,s=fit_elastic_others$lambda.min, type = "class")


confusionMatrix(as.factor(predict_elastic_others),as.factor(y.test_others))
#tmp_coeffs_elastic_others <- coef(fit_elastic_others, s = "lambda.min")
stopCluster(cl)


message("...Start to train glmnet...")
print((Sys.time() - t2))

coef(fit_elastic_others, s = "lambda.min")
plot(fit_elastic_others, xvar = "lambda", label = TRUE)


df_predict_others=df_test[which(as.character(predict_elastic)=='others'),]
df_predict_others=df_train[which(as.character(predict_elastic)=='others'),]
##for these columns need to be modified
dim(df_predict_others)




df_train_all <- df_final[ trainIndex,]
df_test_all  <- df_final[-trainIndex,]

df_train_all$cover_type=df_train_all$cover_type1
df_test_all$cover_type=df_test_all$cover_type1


df_test_all=df_test_all %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'Spruce/Fir',
                             ifelse(cover_type == 2 ,'Lodgepole Pine',
                                    ifelse(cover_type == 3 ,'Ponderosa Pine',
                                           ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'Douglas-fir',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))



x.elastic_others <- model.matrix(~.-1, df_predict_others[,-54]) 
y.elastic_others<-df_predict_others$cover_type

predict_elastic_others<-predict(fit_elastic_others,newx=x.elastic_others ,s=fit_elastic_others$lambda.min, type = "class")

df1= as.character(predict_elastic)
df2=as.character(predict_elastic_others)
idx=which(as.character(predict_elastic)=='others')
df1[idx]=df2[1:length(df2)]
table(df1)

confusionMatrix(as.factor(df1),as.factor(df_train_all$cover_type))



#########################################AUC
fit_lasso<-get(load('D:/Data/Project_Folders/mtg_coverdata/fit_lasso1.Rdata'))
fit_lasso_others<-get(load('D:/Data/Project_Folders/mtg_coverdata/fit_lasso_others1.Rdata'))

#lasso_pred<-predict(fit_lasso,newx=x.test,s=fit_lasso$lambda.min,type="response")
lasso_pred<-predict(fit_lasso,newx=x.train,s=fit_lasso$lambda.min,type="response")
lasso_pred <- data.frame(lasso_pred)
colnames(lasso_pred) <- paste(colnames(lasso_pred), "_lasso")


#lasso_pred1<-predict(fit_lasso_others,newx=x.test,s=fit_lasso_others$lambda.min,type="response")
lasso_pred1<-predict(fit_lasso_others,newx=x.train,s=fit_lasso_others$lambda.min,type="response")
lasso_pred1 <- data.frame(lasso_pred1)
colnames(lasso_pred1) <- paste(colnames(lasso_pred1), "_lasso")
lasso_pred<-cbind(lasso_pred[,-2],lasso_pred1)
colnames(lasso_pred)

#true_label<-true_label_test
true_label<-true_label_train

pred<-lasso_pred
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")
colnames(true_label) <- gsub("cover_type", "", colnames(true_label))
colnames(true_label) <- gsub(" ", "", colnames(true_label))
final_df <- cbind(true_label, pred)
colnames(final_df)


cols<-c("Lodgepole_pred_lasso",
        "Spruce_pred_lasso",
        "Aspen_pred_lasso",
        "Cottonwood_pred_lasso",
        "Douglas_pred_lasso",
        "Krummholz_pred_lasso",
        "Ponderosa_pred_lasso")
names(final_df)[8:14] <-cols  
colnames(final_df)

final_df<-final_df[,c(1,2,3,4,5,6,7,10,11,12,13,8,14,9)]

colnames(final_df)

require(multiROC)

roc_res <- multi_roc(final_df,force_diag=T)
pr_res <- multi_pr(final_df, force_diag=T)

unlist(roc_res$AUC)
#write.csv(unlist(roc_res$AUC), file = "D:/Data/Project_Folders/mtg_coverdata/lasso_test_auc.csv")
#write.csv(unlist(roc_res$AUC), file = "D:/Data/Project_Folders/mtg_coverdata/lasso_train_auc.csv")


plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)


require(ggplot2)

lasso_roc<-ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

png(filename="D:/Data/Project_Folders/mtg_coverdata/lasso_roc_train.png")
plot(lasso_roc)
dev.off()


rm(lasso_roc)
gc


fit_ridge<-get(load('D:/Data/Project_Folders/mtg_coverdata/fit_ridge1.Rdata'))
fit_ridge_others<-get(load('D:/Data/Project_Folders/mtg_coverdata/fit_ridge_others1.Rdata'))

ridge_pred<-predict(fit_ridge,newx=x.train,s=fit_ridge$lambda.min,type="response")
#ridge_pred<-predict(fit_ridge,newx=x.test,s=fit_ridge$lambda.min,type="response")
ridge_pred <- data.frame(ridge_pred)
colnames(ridge_pred) <- paste(colnames(ridge_pred), "_ridge")


ridge_pred1<-predict(fit_ridge_others,newx=x.train,s=fit_ridge_others$lambda.min,type="response")
#ridge_pred1<-predict(fit_ridge_others,newx=x.test,s=fit_ridge_others$lambda.min,type="response")

ridge_pred1 <- data.frame(ridge_pred1)
colnames(ridge_pred1) <- paste(colnames(ridge_pred1), "_ridge")
ridge_pred<-cbind(ridge_pred[,-2],ridge_pred1)
colnames(ridge_pred)

true_label<-true_label_train
#true_label<-true_label_test

pred<-ridge_pred
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")
colnames(true_label) <- gsub("cover_type", "", colnames(true_label))
colnames(true_label) <- gsub(" ", "", colnames(true_label))
final_df <- cbind(true_label, pred)
colnames(final_df)


cols<-c("Lodgepole_pred_ridge",
        "Spruce_pred_ridge",
        "Aspen_pred_ridge",
        "Cottonwood_pred_ridge",
        "Douglas_pred_ridge",
        "Krummholz_pred_ridge",
        "Ponderosa_pred_ridge")
names(final_df)[8:14] <-cols  
colnames(final_df)

final_df<-final_df[,c(1,2,3,4,5,6,7,10,11,12,13,8,14,9)]

colnames(final_df)

require(multiROC)

roc_res <- multi_roc(final_df,force_diag=T)
pr_res <- multi_pr(final_df, force_diag=T)

unlist(roc_res$AUC)

#write.csv(unlist(roc_res$AUC), file = "D:/Data/Project_Folders/mtg_coverdata/ridge_test_auc.csv")
#write.csv(unlist(roc_res$AUC), file = "D:/Data/Project_Folders/mtg_coverdata/ridge_train_auc.csv")

plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)


require(ggplot2)
ridge_roc<-ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

png(filename="D:/Data/Project_Folders/mtg_coverdata/ridge_roc_train.png")
plot(ridge_roc)
dev.off()

rm(ridge_roc)
gc

fit_elastic<-get(load('D:/Data/Project_Folders/mtg_coverdata/fit_elastic1.Rdata'))
fit_elastic_others<-get(load('D:/Data/Project_Folders/mtg_coverdata/fit_elastic_others1.Rdata'))


elastic_pred<-predict(fit_elastic,newx=x.train,s=fit_elastic$lambda.min,type="response")
#elastic_pred<-predict(fit_elastic,newx=x.test,s=fit_elastic$lambda.min,type="response")

elastic_pred <- data.frame(elastic_pred)
colnames(elastic_pred) <- paste(colnames(elastic_pred), "_elastic")


elastic_pred1<-predict(fit_elastic_others,newx=x.train,s=fit_elastic_others$lambda.min,type="response")
#elastic_pred1<-predict(fit_elastic_others,newx=x.test,s=fit_elastic_others$lambda.min,type="response")

elastic_pred1 <- data.frame(elastic_pred1)
colnames(elastic_pred1) <- paste(colnames(elastic_pred1), "_elastic")
elastic_pred<-cbind(elastic_pred[,-2],elastic_pred1)
colnames(elastic_pred)

true_label<-true_label_train
#true_label<-true_label_test

pred<-elastic_pred
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")
colnames(true_label) <- gsub("cover_type", "", colnames(true_label))
colnames(true_label) <- gsub(" ", "", colnames(true_label))
final_df <- cbind(true_label, pred)
colnames(final_df)


cols<-c("Lodgepole_pred_elastic",
        "Spruce_pred_elastic",
        "Aspen_pred_elastic",
        "Cottonwood_pred_elastic",
        "Douglas_pred_elastic",
        "Krummholz_pred_elastic",
        "Ponderosa_pred_elastic")
names(final_df)[8:14] <-cols  
colnames(final_df)

final_df<-final_df[,c(1,2,3,4,5,6,7,10,11,12,13,8,14,9)]

colnames(final_df)

require(multiROC)

roc_res <- multi_roc(final_df,force_diag=T)
pr_res <- multi_pr(final_df, force_diag=T)

unlist(roc_res$AUC)

#write.csv(unlist(roc_res$AUC), file = "D:/Data/Project_Folders/mtg_coverdata/elasticnet_test_auc.csv")
#write.csv(unlist(roc_res$AUC), file = "D:/Data/Project_Folders/mtg_coverdata/elasticnet_train_auc.csv")


plot_roc_df <- plot_roc_data(roc_res)
plot_pr_df <- plot_pr_data(pr_res)


require(ggplot2)
elastic_roc<-ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))

png(filename="D:/Data/Project_Folders/mtg_coverdata/elastic_roc_train.png")
plot(elastic_roc)
dev.off()

rm(elastic_roc)
gc
#####################end of regularization techniques ########################

#####################ANN#######################################################




df_train <- df_final[ trainIndex,]
df_test  <- df_final[-trainIndex,]

df_plot_train<-df_final[ trainIndex,]
df_plot_train$cover_type=df_train$cover_type1
df_plot_train <-df_plot_train[-55]

df_plot_test<-df_final[ -trainIndex,]
df_plot_test$cover_type=df_test$cover_type1
df_plot_test <-df_plot_test[-55]



df_plot_train=df_plot_train %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'Spruce',
                             ifelse(cover_type == 2 ,'Lodgepole',
                                    ifelse(cover_type == 3 ,'Ponderosa',
                                           ifelse(cover_type == 4 ,'Cottonwood',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'Douglas',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))



df_plot_test=df_plot_test %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'Spruce',
                             ifelse(cover_type == 2 ,'Lodgepole',
                                    ifelse(cover_type == 3 ,'Ponderosa',
                                           ifelse(cover_type == 4 ,'Cottonwood',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'Douglas',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))





true_label_train <- dummies::dummy(df_plot_train$cover_type)
true_label_train <- data.frame(true_label_train)

true_label_test <- dummies::dummy(df_plot_test$cover_type)
true_label_test <- data.frame(true_label_test)


#####################################################

df_train_others <-df_final[ trainIndex,]
df_test_others  <-df_final[-trainIndex,]

df_train_others_nn <-df_final[ trainIndex,]
df_test_others_nn  <-df_final[-trainIndex,]


##remove cover_type1
df_train <-df_train[-55]
df_test <-df_test[-55]



dim(df_train)
dim(df_test)



df_train_others$cover_type=df_train_others$cover_type1
df_test_others$cover_type=df_test_others$cover_type1

df_train_others_nn$cover_type=df_train_others_nn$cover_type1
df_test_others_nn$cover_type=df_test_others_nn$cover_type1

df_train_others <-df_train_others[-55]
df_test_others <-df_test_others[-55]

df_train_others_nn <-df_train_others_nn[-55]
df_test_others_nn <-df_test_others_nn[-55]

dim(df_train_others)
dim(df_test_others)
dim(df_test_others_nn)

which.num <- which(sapply(df_train,class)=="numeric" | sapply(df_train,class)=="integer")

library(stringr)
cols<-names(df_train[,which.num])
cols=cols[!str_detect(cols,pattern="_type")]

df_train[cols]= as.matrix(apply(df_train[cols], 2, function(x) (x-min(x))/(max(x) - min(x))))
df_test[cols]= as.matrix(apply(df_test[cols], 2, function(x) (x-min(x))/(max(x) - min(x))))

df_train_others_nn[cols]= as.matrix(apply(df_train_others_nn[cols], 2, function(x) (x-min(x))/(max(x) - min(x))))
df_test_others_nn[cols]= as.matrix(apply(df_test_others_nn[cols], 2, function(x) (x-min(x))/(max(x) - min(x))))


x.train <- model.matrix(~.-1, df_train[,-54])
y.train<-df_train$cover_type


x.test <- model.matrix(~.-1, df_test[,-54]) 
y.test<-df_test$cover_type

dim(x.train)
dim(x.test)



####others


df_train_others=df_train_others %>% filter(cover_type !=1  & cover_type != 2 )
dim(df_train_others)

df_test_others=df_test_others %>% filter(cover_type !=1  & cover_type != 2)
dim(df_test_others)

table(df_train_others$cover_type)
table(df_test_others$cover_type)

df_train_others_nn=df_train_others_nn %>% filter(cover_type !=1  & cover_type != 2 )
dim(df_train_others_nn)

df_test_others_nn=df_test_others_nn %>% filter(cover_type !=1  & cover_type != 2)
dim(df_test_others_nn)

df_train_others= df_train_others %>% 
  mutate(cover_type =   ifelse(cover_type == 3 ,'Ponderosa Pine',
                               ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                      ifelse(cover_type == 5 ,'Aspen',
                                             ifelse(cover_type == 6 ,'Douglas-fir',
                                                    ifelse(cover_type == 7 ,'Krummholz','na'))))))
dim(df_train_others)

df_test_others= df_test_others %>% 
  mutate(cover_type =   ifelse(cover_type == 3 ,'Ponderosa Pine',
                               ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                      ifelse(cover_type == 5 ,'Aspen',
                                             ifelse(cover_type == 6 ,'Douglas-fir',
                                                    ifelse(cover_type == 7 ,'Krummholz','na'))))))
dim(df_test_others)



x.train_others <- model.matrix(~.-1, df_train_others[,-54])
y.train_others<-df_train_others$cover_type


x.test_others <- model.matrix(~.-1, df_test_others[,-54]) 
y.test_others<-df_test_others$cover_type

x.train_others_nn <- model.matrix(~.-1, df_train_others_nn[,-54]) 
y.train_others_nn<-df_train_others_nn$cover_type


x.test_others_nn <- model.matrix(~.-1, df_test_others_nn[,-54]) 
y.test_others_nn<-df_test_others_nn$cover_type

dim(x.train_others)
dim(x.test_others)


dim(x.train_others_nn)
dim(x.test_others_nn)

dim(x.train)
dim(x.test)


library(keras)


y.train[y.train=="Lodgepole Pine"]<-2
y.train[y.train=="Spruce/Fir"]<-1
y.train[y.train=="others"]<-3
y_train<-to_categorical(y.train)[,2:4]


y.test[y.test=="Lodgepole Pine"]<-2
y.test[y.test=="Spruce/Fir"]<-1
y.test[y.test=="others"]<-3
y_test<-to_categorical(y.test)
y_test<-y_test[,2:4]

model <- keras_model_sequential()

model %>% 
  layer_dense(units = 128, activation = "relu", input_shape = c(96)) %>% 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 3, activation = "softmax")

summary(model)

#k_set_value(model$optimizer$lr, .00001)



model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = 'adam',
  metrics = 'accuracy'
)

history <- model %>% fit(
  x.train, y_train, 
  epochs = 150, batch_size = 258, 
  validation_split = 0.2
)


pred_nn <- model %>% predict(x.test, batch_size = 128)
pred_nn=apply(pred_nn, 1,which.max)
# Confusion matrix
confusionMatrix(as.factor(pred_nn),as.factor(y.test))
model %>% evaluate(x.test,y_test,verbose = 1)


###################################################################

pred_nn <- model %>% predict_proba(x.train, batch_size = 128)
pred_nn=apply(pred_nn, 1,which.max)
# Confusion matrix
confusionMatrix(as.factor(pred_nn),as.factor(y.train))
model %>% evaluate(x.train,y_train,verbose = 1)

#######################################################################3


y_train_others_nn<-to_categorical(y.train_others_nn)[,4:8]
y_test_others_nn<-to_categorical(y.test_others_nn)[,4:8]

model1 <- keras_model_sequential()

model1 %>% 
  layer_dense(units = 128, activation = "relu", input_shape = c(96)) %>% 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 5, activation = "softmax")

summary(model1)

#k_set_value(model$optimizer$lr, .00001)



model1 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = 'adam',
  metrics = 'accuracy'
)

history <- model1 %>% fit(
  x.train_others_nn, y_train_others_nn, 
  epochs = 150, batch_size = 258, 
  validation_split = 0.2
)

pred_nn_others <- model1 %>% predict(x.test_others_nn, batch_size = 128)

pred_nn_others <- model1 %>% predict(x.train_others_nn, batch_size = 128)



pred_nn_others=apply(pred_nn_others, 1,which.max)
# Confusion matrix
#confusionMatrix(as.factor(pred_train),as.factor(y.test_others_nn))
model1 %>% evaluate(x.test_others_nn,y_test_others_nn,verbose = 1)
table(as.factor(pred_nn_others),as.factor(y.test_others_nn))


pred_nn_others <- model1 %>% predict(x.train_others_nn, batch_size = 128)
pred_nn_others=apply(pred_nn_others, 1,which.max)
# Confusion matrix
#confusionMatrix(as.factor(pred_train),as.factor(y.test_others_nn))
model1 %>% evaluate(x.train_others_nn,y_train_others_nn,verbose = 1)
table(as.factor(pred_nn_others),as.factor(y.train_others_nn))

pred_prob_nn <- model1 %>% predict_proba(x.train_others_nn, batch_size = 128)
#pred_nn_others=apply(pred_nn_others, 1,which.max)
# Confusion matrix
#confusionMatrix(as.factor(pred_train),as.factor(y.test_others_nn))
#model1 %>% evaluate(x.train_others_nn,y_train_others_nn,verbose = 1)
table(as.factor(pred_nn_others),as.factor(y.train_others_nn))



table(as.factor(pred_prob_nn),as.factor(y.train_others_nn))
df_predict_others=df_train[which(as.character(pred_nn)=='3'),]
x.train_others_nn_min <- model.matrix(~.-1, df_predict_others[,-54]) 
pred_nn_others_min<-model1 %>% predict(x.train_others_nn_min, batch_size = 128)
pred_nn_others_min=apply(pred_nn_others_min, 1,which.max)


df1=as.character(pred_nn)



df_predict_others=df_test[which(as.character(pred_nn)=='3'),]
df_predict_others=df_train[which(as.character(pred_nn)=='3'),]
##for these columns need to be modified
dim(df_predict_others)
str(df_predict_others)

x.test_others_nn_min <- model.matrix(~.-1, df_predict_others[,-54]) 
x.train_others_nn_min <- model.matrix(~.-1, df_predict_others[,-54]) 
#y.test_others_nn<-df_test_others_nn$cover_type



pred_nn_others_min<-model1 %>% predict(x.train_others_nn_min, batch_size = 128)
pred_nn_others_min=apply(pred_nn_others_min, 1,which.max)


df1=as.character(pred_nn)

df1[which(df1==1)]<-'Spruce/Fir' 
df1[which(df1==2)]<-'Lodgepole Pine'
df1[which(df1==3)]<-'others' 

df2=as.character(pred_nn_others_min)


df2[which(df2==1)]<-'Ponderosa Pine' 
df2[which(df2==2)]<-'Cottonwood/Willow'
df2[which(df2==3)]<-'Aspen' 
df2[which(df2==4)]<-'Douglas-fir'
df2[which(df2==5)]<-'Krummholz' 




idx=which(as.character(pred_nn)==3)
length(df1[which(df1=='Lodgepole Pine')])

length(df1[which(df1=='Spruce/Fir')])
df1[idx]=df2[1:length(df2)]
length(df1[which(df1=='Lodgepole Pine')])
length(df1[which(df1=='Spruce/Fir')])
table(df1)

df_train_all <- df_final[ trainIndex,]
df_test_all  <- df_final[-trainIndex,]

df_train_all$cover_type=df_train_all$cover_type1
df_test_all$cover_type=df_test_all$cover_type1

df_test_all=df_test_all %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'Spruce/Fir',
                             ifelse(cover_type == 2 ,'Lodgepole Pine',
                                    ifelse(cover_type == 3 ,'Ponderosa Pine',
                                           ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'Douglas-fir',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))

df_train_all=df_train_all %>% 
  mutate(cover_type = ifelse(cover_type == 1 ,'Spruce/Fir',
                             ifelse(cover_type == 2 ,'Lodgepole Pine',
                                    ifelse(cover_type == 3 ,'Ponderosa Pine',
                                           ifelse(cover_type == 4 ,'Cottonwood/Willow',
                                                  ifelse(cover_type == 5 ,'Aspen',
                                                         ifelse(cover_type == 6 ,'Douglas-fir',
                                                                ifelse(cover_type == 7 ,'Krummholz','na'))))))))





confusionMatrix(as.factor(df1),as.factor(df_train_all$cover_type))
