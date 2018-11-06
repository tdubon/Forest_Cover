# Predict 454 Project
# Date created 10/23/2018
# Eric Smith
# Prasanna Venkata Rao
# Tannia Dubon
# Lucas Lu
# Kanaka Venkata Hema Geddam 


#install.packages("plotly")
#install.packages("GGally")

library(randomForest)
require(moments)
library(corrplot)
library(plotly)
library(GGally)
library(caret)

####################################################################################################################################
######################### Part 1: Read in data  ############################################
gz.file <- read.csv(gzfile(file.choose()),header = FALSE, sep =",")
forest.df <-  gz.file
dim(forest.df)

# Use the structure function str();
cat("\n","----- Initial Structure of data frame -----","\n")
str(forest.df)
#Changing the names of the columns in the dataframe
names(forest.df) <- c("Elevation","Aspect","Slope","Horizontal_Distance_To_Hydrology","Vertical_Distance_To_Hydrology",
                      "Horizontal_Distance_To_Roadways","Hillshade_9am","Hillshade_Noon","Hillshade_3pm",
                      "Horizontal_Distance_To_Fire_Points","Rawah_Wild_Area","Neota_Wild_Area",
                      "Comanche_Peak_Wild_Area","Cache_la_Poudre_Wild_Area","soil_Type1","soil_Type2","soil_Type3"
                      ,"soil_Type4","soil_Type5","soil_Type6","soil_Type7","soil_Type8","soil_Type9","soil_Type10","soil_Type11"
                      ,"soil_Type12","soil_Type13","soil_Type14","soil_Type15","soil_Type16","soil_Type17","soil_Type18","soil_Type19"
                      ,"soil_Type20","soil_Type21","soil_Type22","soil_Type23","soil_Type24","soil_Type25","soil_Type26","soil_Type27"
                      ,"soil_Type28","soil_Type29","soil_Type30","soil_Type31","soil_Type32","soil_Type33","soil_Type34","soil_Type35"
                      ,"soil_Type36","soil_Type37","soil_Type38","soil_Type39","soil_Type40","Cover_Type")

forest.df=forest.df %>% 
  mutate(Cover_Type = ifelse(Cover_Type == 1 ,'Spruce/Fir',
                             ifelse(Cover_Type == 2 ,'Lodgepole Pine',
                                    ifelse(Cover_Type == 3 ,'Ponderosa Pine',
                                           ifelse(Cover_Type == 4 ,'Cottonwood/Willow',
                                                  ifelse(Cover_Type == 5 ,'Aspen',
                                                         ifelse(Cover_Type == 6 ,'Douglas-fir',
                                                                ifelse(Cover_Type == 7 ,'Krummholz','na'))))))))

forest.df<-setNames(forest.df, tolower(names(forest.df)))

# Use the structure function str();
cat("\n","----- Initial Structure of data frame -----","\n")
str(forest.df)
#To get the descriptive statistics of the dataset:
summary(forest.df)
# Show the header of the data frame;
head(forest.df)

### Need to make sure our data is understood correctly by R, since we have a mix of numerical and categorical
forest.df[11:55]<-lapply(forest.df[11:55], factor)

str(forest.df)

cat("\n","----- CONSOLIDATING DUMMY VARIABLES INTO CATEGORICL VARIABLES-----","\n")

forest_mod<-forest.df

forest_mod$wld_area <- names(forest_mod[11:14])[max.col(forest_mod[11:14])]
table(forest_mod$wld_area)

forest_mod$soil_type <- names(forest_mod[15:54])[max.col(forest_mod[15:54])]
table(forest_mod$soil_type)

cat("\n","----- Structure of modified data frame -----","\n")
str(forest_mod)
summary(forest_mod)

####################################################################################################################################
######################### Part 2: Data Preparation ########################################
# Data Quality Check
#Check for missing values
sapply(forest_mod, function(x) sum(is.na(x)))
#Check missing data percentage
pcentNA <- function(x){sum(is.na(x))/length(x)*100}
apply(forest_mod,2,pcentNA)

#outlier detection
# boxplot.stats(forest_mod$Elevation, coef = 1.5, do.conf = TRUE, do.out = TRUE)
# boxplot.stats(forest_mod$Aspect, coef = 1.5, do.conf = TRUE, do.out = TRUE)
# boxplot.stats(forest_mod$Slope, coef = 1.5, do.conf = TRUE, do.out = TRUE)
# boxplot.stats(forest_mod$Horizontal_Distance_To_Hydrology, coef = 1.5, do.conf = TRUE, do.out = TRUE)
# boxplot.stats(forest_mod$Vertical_Distance_To_Hydrology, coef = 1.5, do.conf = TRUE, do.out = TRUE)
# boxplot.stats(forest_mod$Horizontal_Distance_To_Roadways, coef = 1.5, do.conf = TRUE, do.out = TRUE)
# boxplot.stats(forest_mod$Hillshade_9am, coef = 1.5, do.conf = TRUE, do.out = TRUE)
# boxplot.stats(forest_mod$Hillshade_Noon, coef = 1.5, do.conf = TRUE, do.out = TRUE)
# boxplot.stats(forest_mod$Hillshade_3pm, coef = 1.5, do.conf = TRUE, do.out = TRUE)
# boxplot.stats(forest_mod$Horizontal_Distance_To_Fire_Points, coef = 1.5,do.conf = TRUE, do.out = TRUE)

####################################################################################################################################
############## Part 3: Data Exploration ###################################################
##### 1 - Traditional EDA #######

# High level summary of numerical data
elv <- summary(forest_mod$elevation)
asp <- summary(forest_mod$aspect)
slp <- summary(forest_mod$slope)
hdtohyd <- summary(forest_mod$horizontal_distance_to_hydrology)
vdtohyd <- summary(forest_mod$vertical_distance_to_hydrology)
chdtord <- summary(forest_mod$horizontal_distance_to_roadways)
hs9am <- summary(forest_mod$hillshade_9am)
hsnoon <- summary(forest_mod$hillshade_noon)
hs3pm <- summary(forest_mod$hillshade_3pm)
hdtofp <- summary(forest_mod$horizontal_distance_to_fire_points)

All <- rbind(elv,asp,slp,hdtohyd,vdtohyd,chdtord,hs9am,hsnoon,hs3pm,hdtofp)
round(All,2)

round(skewness(forest_mod$elevation),2)
round(kurtosis(forest_mod$elevation),2)
round(skewness(forest_mod$aspect),2)
round(kurtosis(forest_mod$aspect),2)
round(skewness(forest_mod$slope),2)
round(kurtosis(forest_mod$slope),2)
round(skewness(forest_mod$horizontal_distance_to_hydrology),2)
round(kurtosis(forest_mod$horizontal_distance_to_hydrology),2)
round(skewness(forest_mod$vertical_distance_to_hydrology),2)
round(kurtosis(forest_mod$vertical_distance_to_hydrology),2)
round(skewness(forest_mod$horizontal_distance_to_roadways),2)
round(kurtosis(forest_mod$horizontal_distance_to_roadways),2)
round(skewness(forest_mod$hillshade_9am),2)
round(kurtosis(forest_mod$hillshade_9am),2)
round(skewness(forest_mod$hillshade_noon),2)
round(kurtosis(forest_mod$hillshade_noon),2)
round(skewness(forest_mod$hillshade_3pm),2)
round(kurtosis(forest_mod$hillshade_3pm),2)
round(skewness(forest_mod$horizontal_distance_to_fire_points),2)
round(kurtosis(forest_mod$horizontal_distance_to_fire_points),2)

quantile(forest_mod$elevation, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 
quantile(forest_mod$aspect, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1))
quantile(forest_mod$slope, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 
quantile(forest_mod$horizontal_distance_to_hydrology, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 
quantile(forest_mod$vertical_distance_to_hydrology, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 
quantile(forest_mod$horizontal_distance_to_roadways, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 
quantile(forest_mod$hillshade_9am, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 
quantile(forest_mod$hillshade_noon, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 
quantile(forest_mod$hillshade_3pm, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 
quantile(forest_mod$horizontal_distance_to_fire_points, c(0, 0.01, 0.03, 0.05, 0.25, 0.5, 0.75, 0.95, 0.97, 0.99, 1)) 

## cover_type distribution
table(forest_mod$cover_type)/nrow(forest_mod)

ggplot(forest_mod, aes(cover_type , fill=cover_type )) + geom_bar() +theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="     Forest cover type  ")  

##
colours<- c  ("seagreen", "yellow", "green", "violet", "orange", 
              "steelblue", "pink", "cyan","purple","magenta") 
##

## Numeric Variables - Histograms, Density Plots and Box Plots 
par(mfrow=c(2,5))
for(i in 1:10){
  hist(forest_mod[,i], xlab = '', col=colours[i],  main=names(forest_mod[i]))
}

par(mfrow=c(2,5))
for(i in 1:10){
  plot(density(forest_mod[,i]), xlab = '',main=names(forest_mod[i]))
  polygon(density(forest_mod[,i]),col=colours[i],border="black")
}

par(mfrow=c(2,5))
for(i in 1:10){
  boxplot(forest_mod[,i],xlab = '',main=names(forest_mod[i]),col=colours[i])
}

par(mfrow=c(1,1))

## Categorical features bar plots
forest_mod$wld_area<-as.factor(forest_mod$wld_area)
table(forest_mod$wld_area)/nrow(forest_mod)
forest_mod$soil_type<-as.factor(forest_mod$soil_type)
table(forest_mod$soil_type)/nrow(forest_mod)

ggplot(forest_mod, aes(wld_area))+
  geom_bar(aes(fill=wld_area), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.1)) +
  labs(title="                Wild Area  plot   ")   

ggplot(forest_mod, aes(soil_type))+
  geom_bar(aes(fill=soil_type), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="                                      Soil Type Bar plot   ") 

## Distribution of the cover type by 

#1)  Elevation
#2)  aspect
#3)  slope
#4)  horizontal_distance_to_hydrology
#5)  vertical_distance_to_hydrology
#6)  horizontal_distance_to_roadways
#7)  hillshade_9am
#8)  hillshade_noon
#9)  hillshade_3pm
#10) horizontal_distance_to_fire_points
#11) Wilderness Area
#12) Soil type
#13) cover type as hill shade varies
#14) cover type as a function of elevation, slope and aspect

#1) Elevation  vs cover_type: #Obseravtion: Distinct separation seen between classes and elevation
ggplot(forest_mod, aes(x=elevation)) + geom_density()
ggplot(forest_mod, aes(x=elevation)) + geom_density(aes(group=cover_type, color=cover_type, fill=cover_type), alpha=.1)

#2) Aspect  vs cover_type: #Observation : No clear separation as such seen
ggplot(forest_mod, aes(x=aspect)) + geom_density()
ggplot(forest_mod, aes(x=aspect)) + geom_density(aes(group=cover_type, color=cover_type, fill=cover_type), alpha=.1)

#3)Slope vs cover_type: #Observation : No clear sepration as such seen, Cover is dense at certain regions
ggplot(forest_mod, aes(x=slope)) + geom_density()
ggplot(forest_mod, aes(x=slope)) + geom_density(aes(group=cover_type, color=cover_type, fill=cover_type), alpha=.1)

#4)Horizontal distance hydrology vs cover_type: #Observation : No clear sepration as such seen, Cover is dense at certain regions
ggplot(forest_mod, aes(x=horizontal_distance_to_hydrology)) + geom_density()
ggplot(forest_mod, aes(x=horizontal_distance_to_hydrology)) + geom_density(aes(group=cover_type, color=cover_type, fill=cover_type), alpha=.1)

#5)Vertical distance hydrology vs cover_type: #Observation : No clear separation as such seen, Cover is dense at certain regions
ggplot(forest_mod, aes(x=vertical_distance_to_hydrology)) + geom_density()
ggplot(forest_mod, aes(x=vertical_distance_to_hydrology)) + geom_density(aes(group=cover_type, color=cover_type, fill=cover_type), alpha=.1)

#6)horizontal_distance_to_roadways vs cover_type: #Observation : Class separation  seen.Not distinct, Cover is dense at certain regions
ggplot(forest_mod, aes(x=horizontal_distance_to_roadways)) + geom_density()
ggplot(forest_mod, aes(x=horizontal_distance_to_roadways)) + geom_density(aes(group=cover_type, color=cover_type, fill=cover_type), alpha=.1)

#7)9:00am hill shadevs cover_type: #Observation : No class sepration, Cover is dense at certain regions
ggplot(forest_mod, aes(x=hillshade_9am)) + geom_density()
ggplot(forest_mod, aes(x=hillshade_9am)) + geom_density(aes(group=cover_type, color=cover_type, fill=cover_type), alpha=.1)

#8)hillshade_noon vs cover_type: #Observation : No class sepration, Cover is dense at certain regions
ggplot(forest_mod, aes(x=hillshade_noon)) + geom_density()
ggplot(forest_mod, aes(x=hillshade_noon)) + geom_density(aes(group=cover_type, color=cover_type, fill=cover_type), alpha=.1)

#9)hillshade_3pm vs cover_type: #Observation : No class sepration, Looks like Normal distribution
ggplot(forest_mod, aes(x=hillshade_3pm)) + geom_density()
ggplot(forest_mod, aes(x=hillshade_3pm)) + geom_density(aes(group=cover_type, color=cover_type, fill=cover_type), alpha=.1)

#10)horizontal_distance_to_fire_points  vs cover_type: #Observation : class sepration likely
ggplot(forest_mod, aes(x=horizontal_distance_to_fire_points)) + geom_density()
ggplot(forest_mod, aes(x=horizontal_distance_to_fire_points)) + geom_density(aes(group=cover_type, color=cover_type, fill=cover_type), alpha=.1)

#11)Wilderness Area vs cover_type
ggplot(forest_mod, aes(x=wld_area)) + 
  geom_bar(mapping=aes(group=cover_type, colour=cover_type, fill=cover_type), alpha=0.5)

#12)soil type vs cover_type
ggplot(forest_mod, aes(x= soil_type)) + 
  geom_bar(mapping=aes(group=cover_type, colour=cover_type, fill=cover_type), alpha=0.5)+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#13)cover type as hill shade varies
plot_ly(forest_mod, x = ~hillshade_9am , y = ~hillshade_noon , z = ~hillshade_3pm, type = "scatter3d",
        mode = "markers",color =~cover_type)

#14)cover type as a function of elevation,slope and aspect
plot_ly(forest_mod, x = ~elevation , y = ~slope, z = ~aspect, type = "scatter3d",
        mode = "markers",color =~cover_type)

#### Correlation Matrix ###

corr= cor(forest_mod[, c(1:10)])
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

##### 2 -  Model Based EDA #######
# help(randomForest)
set.seed(1234)
model_eda.rf <- randomForest(cover_type ~.-soil_type40 -cache_la_poudre_wild_area -wld_area -soil_type, 
                             forest_mod, importance=T ,ntree = 100)
plot(model_eda.rf)

summary(model_eda.rf)
varImpPlot(model_eda.rf,sort = T,main="Variable Importance",n.var=15)

# Variable Importance Table
var.imp <- data.frame(importance(model_eda.rf, type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]
dim(var.imp)

####################################################################################################################################
############## Part 3: Model Buidling ############################################################
######## Step 1 - Add a train/test flag to split the sample ##############
set.seed(1234)
trainIndex <- createDataPartition(forest_mod$cover_type, p = .7, 
                                  list = FALSE, 
                                  times = 1)
forest_mod_train <- forest_mod[ trainIndex,]
forest_mod_test  <- forest_mod[-trainIndex,]

# Check the counts on the train/test split
dim(forest_mod)
dim(forest_mod_train)
dim(forest_mod_test)

# Check the train/test split as a percentage of whole
(dim(forest_mod_train)[1]/dim(forest_mod)[1])*100
(dim(forest_mod_test)[1]/dim(forest_mod)[1])*100

##distribution in main dataset
table(forest_mod$cover_type)/nrow(forest_mod)

##distribution in train and test
table(forest_mod_train$cover_type)/nrow(forest_mod_train)
table(forest_mod_test$cover_type)/nrow(forest_mod_test)
table(forest_mod_train$wld_area)/nrow(forest_mod_train)
table(forest_mod_test$wld_area)/nrow(forest_mod_test)
table(forest_mod_train$soil_type)/nrow(forest_mod_train)
table(forest_mod_test$soil_type)/nrow(forest_mod_test)

######## Step 2 - Modeling #####################
######### MODEL 1 - RANDOM FOREST #######

set.seed(1234)
model.rf <- randomForest(cover_type ~.-soil_type40 -cache_la_poudre_wild_area -wld_area -soil_type,
                         forest_mod_train, importance=T ,ntree = 403, do.trace=T)
confusionMatrix(predict(model.rf), forest_mod_train$cover_type)

plot(model.rf)

summary(model.rf)

importance(model.rf)
varImpPlot(model.rf,sort = T,main="Variable Importance",n.var=15)

# Variable Importance Table
var.imp <- data.frame(importance(model.rf, type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]
dim(var.imp)


############################################################################################################################
############## Part 4: Predictive Accuracy ############################################################
set.seed(1234)
model.rf.test <- predict(model.rf, forest_mod_test)
confusionMatrix(model.rf.test, forest_mod_test$cover_type)

