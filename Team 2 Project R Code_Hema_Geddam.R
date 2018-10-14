# Predict 454 Project
#Kanaka Venkata Hema Geddam - Date created 10/09/2018

library(randomForest)
require(moments)
library(corrplot)
################################################################################################################################################
#Read in data
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
# Use the structure function str();
cat("\n","----- Initial Structure of data frame -----","\n")
str(forest.df)
#To get the descriptive statistics of the dataset:
summary(forest.df)
# Show the header of the data frame;
head(forest.df)
### Need to make sure our data is understood correctly by R, since we have a mix of numerical and categorical

forest.df$Cover_Type = as.factor(forest.df$Cover_Type)
cat("\n","----- ReStructure of data frame -----","\n")
str(forest.df)
class(forest.df$Cover_Type)
######################### Part 2: Data Preparation #####################
# Data Quality Check
#Check for missing values
sapply(forest.df, function(x) sum(is.na(x)))
#Check missing data percentage
pcentNA <- function(x){sum(is.na(x))/length(x)*100}
apply(forest.df,2,pcentNA)

#outlier detection
boxplot.stats(forest.df$Elevation, coef = 1.5, do.conf = TRUE, do.out = TRUE)
boxplot.stats(forest.df$Aspect, coef = 1.5, do.conf = TRUE, do.out = TRUE)
boxplot.stats(forest.df$Slope, coef = 1.5, do.conf = TRUE, do.out = TRUE)
boxplot.stats(forest.df$Horizontal_Distance_To_Hydrology, coef = 1.5, do.conf = TRUE, do.out = TRUE)
boxplot.stats(forest.df$Vertical_Distance_To_Hydrology, coef = 1.5, do.conf = TRUE, do.out = TRUE)
boxplot.stats(forest.df$Horizontal_Distance_To_Roadways, coef = 1.5, do.conf = TRUE, do.out = TRUE)
boxplot.stats(forest.df$Hillshade_9am, coef = 1.5, do.conf = TRUE, do.out = TRUE)
boxplot.stats(forest.df$Hillshade_Noon, coef = 1.5, do.conf = TRUE, do.out = TRUE)
boxplot.stats(forest.df$Hillshade_3pm, coef = 1.5, do.conf = TRUE, do.out = TRUE)
boxplot.stats(forest.df$Horizontal_Distance_To_Fire_Points, coef = 1.5,do.conf = TRUE, do.out = TRUE)


############## Part 1: Data Exploration ##########################################################################
# High level summary of data
elv <- summary(forest.df$Elevation)
asp <- summary(forest.df$Aspect)
slp <- summary(forest.df$Slope)
hdtohyd <- summary(forest.df$Horizontal_Distance_To_Hydrology)
vdtohyd <- summary(forest.df$Vertical_Distance_To_Hydrology)
chdtord <- summary(forest.df$Horizontal_Distance_To_Roadways)
hs9am <- summary(forest.df$Hillshade_9am)
hsnoon <- summary(forest.df$Hillshade_Noon)
hs3pm <- summary(forest.df$Hillshade_3pm)
hdtofp <- summary(forest.df$Horizontal_Distance_To_Fire_Points)

All <- rbind(elv,asp,slp,hdtohyd,vdtohyd,chdtord,hs9am,hsnoon,hs3pm,hdtofp)
round(All,2)

quantile(forest.df$Elevation, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)) 
quantile(forest.df$Aspect, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1))
quantile(forest.df$Slope, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)) 
quantile(forest.df$Horizontal_Distance_To_Hydrology, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)) 
quantile(forest.df$Vertical_Distance_To_Hydrology, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)) 
quantile(forest.df$Horizontal_Distance_To_Roadways, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)) 
quantile(forest.df$Hillshade_9am, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)) 
quantile(forest.df$Hillshade_Noon, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)) 
quantile(forest.df$Hillshade_3pm, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)) 
quantile(forest.df$Horizontal_Distance_To_Fire_Points, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)) 

table(forest.df$Cover_Type)/nrow(forest.df)

# Histograms And Box Plots for Numeric Variables
par(mfrow=c(2,3))
hist(forest.df$Elevation, col = "deepskyblue", xlab = "Elevation", main = "Elevation Histogram")
hist(forest.df$Aspect, col = "lightcoral", xlab = "Aspect", main = "Aspect Histogram")
hist(forest.df$Slope, col = "yellowgreen", xlab = "Slope", main = "Slope Histogram")
boxplot(forest.df$Elevation, col = "deepskyblue", main = "Elevation BoxPlot")
boxplot(forest.df$Aspect, col = "lightcoral", main = "Aspect BoxPlot")
boxplot(forest.df$Slope, col = "yellowgreen", main = "Slope BoxPlot")
par(mfrow=c(1,1))

round(skewness(forest.df$Elevation),2)
round(kurtosis(forest.df$Elevation),2)
round(skewness(forest.df$Aspect),2)
round(kurtosis(forest.df$Aspect),2)
round(skewness(forest.df$Slope),2)
round(kurtosis(forest.df$Slope),2)

par(mfrow=c(2,3))
hist(forest.df$Horizontal_Distance_To_Hydrology, col = "lightslateblue", xlab = "Horizontal_Distance_To_Hydrology", main = "Horizontal_Distance_To_Hydrology Histogram")
hist(forest.df$Vertical_Distance_To_Hydrology, col = "plum2", xlab = "Vertical_Distance_To_Hydrology", main = "Vertical_Distance_To_Hydrology Histogram")
hist(forest.df$Horizontal_Distance_To_Roadways, col = "lightseagreen", xlab = "Horizontal_Distance_To_Roadways", main = "Horizontal_Distance_To_Roadways Histogram")
boxplot(forest.df$Horizontal_Distance_To_Hydrology, col = "lightslateblue", main = "Horizontal_Distance_To_Hydrology BoxPlot")
boxplot(forest.df$Vertical_Distance_To_Hydrology, col = "plum2", main = "Vertical_Distance_To_Hydrology BoxPlot")
boxplot(forest.df$Horizontal_Distance_To_Roadways, col = "lightseagreen", main = "Horizontal_Distance_To_Roadways BoxPlot")
par(mfrow=c(1,1))

round(skewness(forest.df$Horizontal_Distance_To_Hydrology),2)
round(kurtosis(forest.df$Horizontal_Distance_To_Hydrology),2)
round(skewness(forest.df$Vertical_Distance_To_Hydrology),2)
round(kurtosis(forest.df$Vertical_Distance_To_Hydrology),2)
round(skewness(forest.df$Horizontal_Distance_To_Roadways),2)
round(kurtosis(forest.df$Horizontal_Distance_To_Roadways),2)

par(mfrow=c(2,4))
hist(forest.df$Hillshade_9am, col = "steelblue", xlab = "Hillshade_9am", main = "Hillshade_9am Histogram")
hist(forest.df$Hillshade_Noon, col = "palevioletred2", xlab = "Hillshade_Noon", main = "Hillshade_Noon Histogram")
hist(forest.df$Hillshade_3pm, col = "palegreen3", xlab = "Hillshade_3pm", main = "Hillshade_3pm Histogram")
hist(forest.df$Horizontal_Distance_To_Fire_Points, col = "darkorange", xlab = "Horizontal_Distance_To_Fire_Points", main = "Horizontal_Distance_To_Fire_Points Histogram")
boxplot(forest.df$Hillshade_9am, col = "steelblue", main = "Hillshade_9am BoxPlot")
boxplot(forest.df$Hillshade_Noon, col = "palevioletred2", main = "Hillshade_Noon BoxPlot")
boxplot(forest.df$Hillshade_3pm, col = "palegreen3", main = "Hillshade_3pm BoxPlot")
boxplot(forest.df$Horizontal_Distance_To_Fire_Points, col = "darkorange", main = "Horizontal_Distance_To_Fire_Points BoxPlot")
par(mfrow=c(1,1))

round(skewness(forest.df$Hillshade_9am),2)
round(kurtosis(forest.df$Hillshade_9am),2)
round(skewness(forest.df$Hillshade_Noon),2)
round(kurtosis(forest.df$Hillshade_Noon),2)
round(skewness(forest.df$Hillshade_3pm),2)
round(kurtosis(forest.df$Hillshade_3pm),2)
round(skewness(forest.df$Horizontal_Distance_To_Fire_Points),2)
round(kurtosis(forest.df$Horizontal_Distance_To_Fire_Points),2)

######## Correlation Matrix ##########

numeric <- subset(forest.df , select = c(1,2,3,4,5,6,7,8,9,10), na.rm = TRUE)
c <- cor(numeric)
corrplot(c, method = "square",na.label = "NA")

##########################################################################################
# Add a train/test flag to split the sample 
##########################################################################################
set.seed(123)
forest.df$u <- runif(n=dim(forest.df)[1],min=0,max=1);
forest.df$train <- ifelse(forest.df$u<0.70,1,0);

# Check the counts on the train/test split
table(forest.df$train)

# Check the train/test split as a percentage of whole
table(forest.df$train)/dim(forest.df)[1]

train.df <- subset(forest.df,train==1);
test.df <- subset(forest.df,train!=1);

# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up?
dim(forest.df)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)[1]

train.df$u <- NULL
train.df$train <- NULL

str(train.df)

table(train.df$Cover_Type)/nrow(train.df)
table(test.df$Cover_Type)/nrow(test.df)

###############################################################################################################################
# Help on ramdonForest package and function
help(randomForest)
set.seed(123)
model.rf <- randomForest(Cover_Type ~.-soil_Type40 -Cache_la_Poudre_Wild_Area,train.df,importance=T)
plot(model.rf)

summary(model.rf)
varImpPlot(model.rf,sort = T,main="Variable Importance",n.var=30)

# Variable Importance Table
var.imp <- data.frame(importance(model.rf, type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]
dim(var.imp)

############################################################################################################################
