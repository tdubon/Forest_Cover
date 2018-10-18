library(DataExplorer)
library(tidyr)
library(dplyr)
library(dlookr)
library(ggplot2)
library(e1071)
library(ggpubr)
library(plotly)
library(GGally)
library(rgl)
library(factoextra)
library(NbClust)


 
forest_cover <- read.table("D:/Prasanna/Prasanna/MS454/Forest Cover/covtype.data.gz",
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

 

forest_cover=forest_cover %>% 
  mutate(Cover_Type = ifelse(Cover_Type == 1 ,'Spruce/Fir',
                      ifelse(Cover_Type == 2 ,'Lodgepole Pine',
                      ifelse(Cover_Type == 3 ,'Ponderosa Pine',
                      ifelse(Cover_Type == 4 ,'Cottonwood/Willow',
                      ifelse(Cover_Type == 5 ,'Aspen',
                      ifelse(Cover_Type == 6 ,'Douglas-fir',
                      ifelse(Cover_Type == 7 ,'Krummholz','na'))))))))

forest_cover<-setNames(forest_cover, tolower(names(forest_cover)))

##converting binary variables to factors for EDA

forest_cover[11:55]<-lapply(forest_cover[11:55], factor)

## percentage distribution of various cover type :Highly imbalanced
prop.table(table(forest_cover$cover_type))


forest_cover_mod<-forest_cover
 
forest_cover_mod$wld_area <- names(forest_cover_mod[11:14])[max.col(forest_cover_mod[11:14])]
table(forest_cover_mod$wld_area)

forest_cover_mod$soil_type <- names(forest_cover_mod[15:54])[max.col(forest_cover_mod[15:54])]
table(forest_cover_mod$soil_type)

###########################Basic stats results & observations ##################################3

#
# There are 10 numerical variables  and  45 categorical variables .
# There are neither missing values nor any coulmn having only zeros among numerical features
# All numerical columns do have sd > 0

# Categorical : There are no missing values among teh categorical features

# Among the categorical features , the following soil types were less than .10 % of the
# distribution

#soil_type14,soil_type15,soil_type25,soil_type36,soil_type37,soil_type7 & soil_type8
#Soil_type29 was most widely spread accounting for almost 20% of the distribution

#Regarding wilderness area , cache_la_poudre_wild_area & neota_wild_area accounted for
# less than 5 % while comanche_peak_wild_area & rawah_wild_area accounted for
# 44 % of the distribution respectively

#regarding cover type , 
## Lodgepole Pine had maximum cover accounting to 48.6 % followed by Spruce accounting for 36 %
# and followed by the rest. Cottonwood/willow  type forest cover accounted only for .47  %
# of the forest cover.
#Based on these stats it looks like the classes are imbalanced.This c


 
#Univariate analysis
-----------------------
#1) forest cover
  

#Aspen           	 9493
#Cottonwood/Willow	 2747
#Douglas-fir	       17367
#Krummholz	         20510
#Lodgepole Pine	   283301
#Ponderosa Pine   	 35754
#Spruce/Fir	       211840


ggplot(forest_cover, aes(cover_type , fill=cover_type ))
   + geom_bar() +theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="     Forest cover type  ")  

 
#numerical values:

#elevation
#aspect
#slope
#horizontal_distance_to_hydrology
#vertical_distance_to_hydrology
#horizontal_distance_to_roadways
#hillshade_9am
#hillshade_noon
#hillshade_3pm
#horizontal_distance_to_fire_points

colours<- c  ("seagreen", "yellow", "green", "violet", "orange", 
             "steelblue", "pink", "cyan","purple","magenta") 

 
for(i in 1:10){
  hist(forest_cover[,i], xlab = '', col=colours[i],  main=names(forest_cover[i]))
  
}


#Density plots 

par(mfrow=c(2,5))
for(i in 1:10){
  plot(density(forest_cover[,i]), xlab = '',main=names(forest_cover[i]))
  polygon(density(forest_cover[,i]),col=colours[i],border="black")
  
}

 

par(mfrow=c(2,5))
for(i in 1:10){
  boxplot(forest_cover[,i],xlab = '',main=names(forest_cover[i]),col=colours[i])
 
}

##Numerical feature  observations

## Categorical features bar plots
forest_cover_mod$wld_area<-as.factor(forest_cover_mod$wld_area)
forest_cover_mod$soil_type<-as.factor(forest_cover_mod$soil_type)
  
ggplot(forest_cover_mod, aes(wld_area))+
             geom_bar(aes(fill=wld_area), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.1)) +
  labs(title="                Wild Area  plot   ")   

ggplot(forest_cover_mod, aes(soil_type))+
  geom_bar(aes(fill=soil_type), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="                                      Soil Type Bar plot   ")   
 

## Cat Features observations


#############

# Distribution of the cover type by 

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


#1) Elevation  vs cover_type:

ggplot(forest_cover, aes(x=elevation)) + geom_density()
ggplot(forest_cover, aes(x=elevation)) + geom_density(aes(group=cover_type, color=cover_type,
                                                          fill=cover_type), alpha=.1)
# Obseravtion: Distinct separation seen between classes and elevation



#2) Aspect  vs cover_type:

ggplot(forest_cover, aes(x=aspect)) + geom_density()
ggplot(forest_cover, aes(x=aspect)) + geom_density(aes(group=cover_type, color=cover_type,
                                                          fill=cover_type), alpha=.1)
#Observation : No clear separation as such seen

 


#3)Slope vs cover_type
ggplot(forest_cover, aes(x=slope)) + geom_density()
ggplot(forest_cover, aes(x=slope)) + geom_density(aes(group=cover_type, color=cover_type,
                                                       fill=cover_type), alpha=.1)
#Observation : No clear sepration as such seen
#              Cover is dense at certain regions

#4)Horizontal distance hydrology vs cover_type
ggplot(forest_cover, aes(x=horizontal_distance_to_hydrology)) + geom_density()
ggplot(forest_cover, aes(x=horizontal_distance_to_hydrology)) + geom_density(aes(group=cover_type, color=cover_type,
                                                      fill=cover_type), alpha=.1)
#Observation : No clear sepration as such seen
#              Cover is dense at certain regions

#4)Vertical distance hydrology vs cover_type
ggplot(forest_cover, aes(x=vertical_distance_to_hydrology)) + geom_density()
ggplot(forest_cover, aes(x=vertical_distance_to_hydrology)) + geom_density(aes(group=cover_type, color=cover_type,
                                                                                 fill=cover_type), alpha=.1)
#Observation : No clear separation as such seen
#              Cover is dense at certain regions

#5)Vertical distance hydrology vs cover_type
ggplot(forest_cover, aes(x=vertical_distance_to_hydrology)) + geom_density()
ggplot(forest_cover, aes(x=vertical_distance_to_hydrology)) + geom_density(aes(group=cover_type, color=cover_type,
                                                                               fill=cover_type), alpha=.1)
#Observation : No clear separation as such seen
#              Cover is dense at certain regions


#6)horizontal_distance_to_roadways vs cover_type
ggplot(forest_cover, aes(x=horizontal_distance_to_roadways)) + geom_density()
ggplot(forest_cover, aes(x=horizontal_distance_to_roadways)) + geom_density(aes(group=cover_type, color=cover_type,
                                                                               fill=cover_type), alpha=.1)
#Observation : Class separation  seen.Not distinct
#              Cover is dense at certain regions


#7)9:00am hill shade  vs cover_type
ggplot(forest_cover, aes(x=hillshade_9am)) + geom_density()
ggplot(forest_cover, aes(x=hillshade_9am)) + geom_density(aes(group=cover_type, color=cover_type,
                                                                                fill=cover_type), alpha=.1)
#Observation : No class sepration
#              Cover is dense at certain regions


#8)hillshade_noon   vs cover_type
ggplot(forest_cover, aes(x=hillshade_noon)) + geom_density()
ggplot(forest_cover, aes(x=hillshade_noon)) + geom_density(aes(group=cover_type, color=cover_type,
                                                              fill=cover_type), alpha=.1)
#Observation : No class sepration
#              Cover is dense at certain regions

#9)hillshade_3pm   vs cover_type
ggplot(forest_cover, aes(x=hillshade_3pm)) + geom_density()
ggplot(forest_cover, aes(x=hillshade_3pm)) + geom_density(aes(group=cover_type, color=cover_type,
                                                               fill=cover_type), alpha=.1)
#Observation : No class sepration
#              Looks like Normal distribution


#10)horizontal_distance_to_fire_points  vs cover_type
ggplot(forest_cover, aes(x=horizontal_distance_to_fire_points)) + geom_density()
ggplot(forest_cover, aes(x=horizontal_distance_to_fire_points)) + geom_density(aes(group=cover_type, color=cover_type,
                                                              fill=cover_type), alpha=.1)
#Observation : class sepration likely
#               


###categorical features vs cover type

#1) Wilderness Area vs cover_type
 
ggplot(forest_cover_mod, aes(x=wld_area)) + 
  geom_bar(mapping=aes(group=cover_type, colour=cover_type, fill=cover_type), alpha=0.5)

#Observations


#2) Soil type  vs cover_type

ggplot(forest_cover_mod, aes(x= soil_type)) + 
  geom_bar(mapping=aes(group=cover_type, colour=cover_type, fill=cover_type), alpha=0.5)+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#observations


################ Cover type as hill shade varies
 

plot_ly(forest_cover_mod, x = ~hillshade_9am , y = ~hillshade_noon , z = ~hillshade_3pm, type = "scatter3d",
        mode = "markers",color =~cover_type)

 
##observations

#################cover type as a function of elevation,slope and aspect

plot_ly(forest_cover_mod, x = ~elevation , y = ~slope, z = ~aspect, type = "scatter3d",
        mode = "markers",color =~cover_type)


##observations



#####correlation plot ################################################
 
ggpairs(data = forest_cover, columns = 1:10, title = "Correlations among numerical features",
        mapping = aes(colour = cover_type, alpha = .3))




correlations<-as.data.frame(cor(forest_cover[1:10]))

tbl_df(correlations)

## There seems to be correlation of 78 % between hill_shade_9pm and hill_shade_3pm

###########Explore if there are any clusters######################### 
 
 
pc <- princomp(forest_cover[1:10], cor=TRUE, scores=TRUE)
summary(pc)
 
 