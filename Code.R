setwd("~/Documents/Northwestern/WorkingFiles/MSDS_454/Forest_Cover")

library(R.utils)
gunzip("covtype.data.gz")


cat("\n","----- FORMAT DATA -----","\n")
forest <- read.table("covtype.data", sep =",", dec = ".", header = FALSE)

col_headings <- c('elevation', 
                  'aspect', 
                  'slope', 
                  'horiz_dist_hydr',
                  'vert_dist_hydr',
                  'horiz_dist_road',
                  'hillshade_9h', 
                  'hillshade_12h',
                  'hillshade_15h',
                  'horiz_dist_fire',
                  'wld_area_rawah',
                  'wld_area_neota',
                  'wld_area_comanche',
                  'wld_area_cache',
                  'soil_type_1',
                  'soil_type_2',
                  'soil_type_3',
                  'soil_type_4',
                  'soil_type_5',
                  'soil_type_6',
                  'soil_type_7',
                  'soil_type_8',
                  'soil_type_9',
                  'soil_type_10',
                  'soil_type_11',
                  'soil_type_12',
                  'soil_type_13',
                  'soil_type_14',
                  'soil_type_15',
                  'soil_type_16',
                  'soil_type_17',
                  'soil_type_18',
                  'soil_type_19',
                  'soil_type_20',
                  'soil_type_21',
                  'soil_type_22',
                  'soil_type_23',
                  'soil_type_24',
                  'soil_type_25',
                  'soil_type_26',
                  'soil_type_27',
                  'soil_type_28',
                  'soil_type_29',
                  'soil_type_30',
                  'soil_type_31',
                  'soil_type_32',
                  'soil_type_33',
                  'soil_type_34',
                  'soil_type_35',
                  'soil_type_36',
                  'soil_type_37',
                  'soil_type_38',
                  'soil_type_39',
                  'soil_type_40',
                  'cover_types'
                  )
       
colnames(forest) <- col_headings

is.data.frame(forest)


cat("\n","----- SUMMARY STATISTICS -----","\n")
print(str(forest))
dim(forest)
sum(is.na(forest))
summary(forest)


cat("\n","----- VARIABLE TRANSFORMATIONS -----","\n")

forest.t <- forest
forest.t$wld_area_rawah <- as.factor(forest$wld_area_rawah)
forest.t$wld_area_neota <- as.factor(forest$wld_area_neota)
forest.t$wld_area_comanche <- as.factor(forest$wld_area_comanche)
forest.t$wld_area_cache <- as.factor(forest$wld_area_cache)
forest.t$soil_type_1 <- as.factor(forest$soil_type_1)
forest.t$soil_type_2 <- as.factor(forest$soil_type_2)
forest.t$soil_type_3 <- as.factor(forest$soil_type_3)
forest.t$soil_type_4 <- as.factor(forest$soil_type_4)
forest.t$soil_type_5 <- as.factor(forest$soil_type_5)
forest.t$soil_type_6 <- as.factor(forest$soil_type_6)
forest.t$soil_type_7 <- as.factor(forest$soil_type_7)
forest.t$soil_type_8 <- as.factor(forest$soil_type_8)
forest.t$soil_type_9 <- as.factor(forest$soil_type_9)
forest.t$soil_type_10 <- as.factor(forest$soil_type_10)
forest.t$soil_type_11 <- as.factor(forest$soil_type_11)
forest.t$soil_type_12 <- as.factor(forest$soil_type_12)
forest.t$soil_type_13 <- as.factor(forest$soil_type_13)
forest.t$soil_type_14 <- as.factor(forest$soil_type_14)
forest.t$soil_type_15 <- as.factor(forest$soil_type_15)
forest.t$soil_type_16 <- as.factor(forest$soil_type_16)
forest.t$soil_type_17 <- as.factor(forest$soil_type_17)
forest.t$soil_type_18 <- as.factor(forest$soil_type_18)
forest.t$soil_type_19 <- as.factor(forest$soil_type_19)
forest.t$soil_type_20 <- as.factor(forest$soil_type_20)
forest.t$soil_type_21 <- as.factor(forest$soil_type_21)
forest.t$soil_type_22 <- as.factor(forest$soil_type_22)
forest.t$soil_type_23 <- as.factor(forest$soil_type_23)
forest.t$soil_type_24 <- as.factor(forest$soil_type_24)
forest.t$soil_type_25 <- as.factor(forest$soil_type_25)
forest.t$soil_type_26 <- as.factor(forest$soil_type_26)
forest.t$soil_type_27 <- as.factor(forest$soil_type_27)
forest.t$soil_type_28 <- as.factor(forest$soil_type_28)
forest.t$soil_type_29 <- as.factor(forest$soil_type_29)
forest.t$soil_type_30 <- as.factor(forest$soil_type_30)
forest.t$soil_type_31 <- as.factor(forest$soil_type_31)
forest.t$soil_type_32 <- as.factor(forest$soil_type_32)
forest.t$soil_type_33 <- as.factor(forest$soil_type_33)
forest.t$soil_type_34 <- as.factor(forest$soil_type_34)
forest.t$soil_type_35 <- as.factor(forest$soil_type_35)
forest.t$soil_type_36 <- as.factor(forest$soil_type_36)
forest.t$soil_type_37 <- as.factor(forest$soil_type_37)
forest.t$soil_type_38 <- as.factor(forest$soil_type_38)
forest.t$soil_type_39 <- as.factor(forest$soil_type_39)
forest.t$soil_type_40 <- as.factor(forest$soil_type_40)
forest.t$cover_types <- as.factor(forest$cover_types)


cat("\n","----- RESTRUCTURING OF DUMMY VARIABLES -----","\n")
wld_areas <- cbind.data.frame(forest.t$wld_area_rawah, forest$wld_area_neota, forest$wld_area_comanche, forest$wld_area_cache)
soil_types <- cbind.data.frame(forest.t$soil_type_1,
                               forest.t$soil_type_2,
                               forest.t$soil_type_3,
                               forest.t$soil_type_4,
                               forest.t$soil_type_5,
                               forest.t$soil_type_6,
                               forest.t$soil_type_7,
                               forest.t$soil_type_8,
                               forest.t$soil_type_9,
                               forest.t$soil_type_10,
                               forest.t$soil_type_11,
                               forest.t$soil_type_12,
                               forest.t$soil_type_13,
                               forest.t$soil_type_14,
                               forest.t$soil_type_15,
                               forest.t$soil_type_16,
                               forest.t$soil_type_17,
                               forest.t$soil_type_18,
                               forest.t$soil_type_19,
                               forest.t$soil_type_20,
                               forest.t$soil_type_21,
                               forest.t$soil_type_22,
                               forest.t$soil_type_23,
                               forest.t$soil_type_24,
                               forest.t$soil_type_25,
                               forest.t$soil_type_26,
                               forest.t$soil_type_27,
                               forest.t$soil_type_28,
                               forest.t$soil_type_29,
                               forest.t$soil_type_30,
                               forest.t$soil_type_31,
                               forest.t$soil_type_32,
                               forest.t$soil_type_33,
                               forest.t$soil_type_34,
                               forest.t$soil_type_35,
                               forest.t$soil_type_36,
                               forest.t$soil_type_37,
                               forest.t$soil_type_38,
                               forest.t$soil_type_39,
                               forest.t$soil_type_40)

#forest.t$wld_area <- ifelse((forest.t$wld_area_rawah == "1"),1,0)
#forest.t$wld_area <- if((forest.t$wld_area_neota == "1"), 2)
#forest.t$wld_area <- forest.t$wld_area, levels=c(1,0), labels=c()
#forest.t$wld_area <- ifelse((forest.t$wld_area_rawah == "1"),1)
#diamonds_transformed$internet <- factor(diamonds_transformed$internet,levels=c(1,2),labels=c("NO","YES"))


cat("\n","----- ANALYSIS OF VARIABLES - TABLES & CORRELATION -----","\n")
aggregate(diamonds_transformed$price_transf,by=list(saleType=diamonds_transformed$channel),FUN=sum)

aggregate(wld_areas, by=1, FUN=sum)

library(corrplot)
corr= cor(forest[, c(1:10,55)])
corrplot(corr,method="color", outline=T, cl.pos="n", rect.col="black", tl.col="indianred4", addCoef.col="black", number.digits=2, number.cex=0.60, tl.cex=0.7, cl.cex=1, col=colorRampPalette(c("green4", "white", "yellow"))(100))
corr

#produce list of correlations by highest value
corr[lower.tri(corr,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
corr=as.data.frame(as.table(corr))  #Turn into a 3-column table
corr=na.omit(corr)  #Get rid of the junk we flagged above
corr=corr[order(-abs(corr$Freq)),]    #Sort by highest correlation (whether +ve or -ve)
corr
#highest corr 


cat("\n","----- OUTLIER DETECTION -----","\n")
# Multi-variate outlier detection
for(i in 2:length(forest.t)) {
  if(is.numeric(forest.t[, i])) {
    formula <- paste0(names(forest.t)[i], "~.-ID")
    outlier_check <- lm(formula=formula, data=forest.t)
    cooksd <- cooks.distance(outlier_check)
    pct_outliers <- round(100*sum(ifelse(cooksd>4*mean(cooksd, na.rm=T),1,0))/length(forest.t[,i]), digits=0)
    png(paste0("./imgs/cooksd_",names(forest.t)[i], ".png"))
    plot(cooksd, pch="*", cex=2, 
         main=paste0("Influential Obs for ", names(forest.t)[i], " by Cooks distance: ", pct_outliers, "%"))  # plot cook's distance
    abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
    text(x=1:length(cooksd)+1, y=cooksd, 
         labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
    dev.off()
  }
}


cat("\n","---------- Identify most important variable using random forests for class variables ----------","\n")
attach(forest.t)
tree.forest.t=tree(cover_types~.)
summary(forest.t)
par(mfrow=c(1,1))
plot(tree.forest.t)
text(tree.forest.t, pretty=0)
tree.forest.t
#most impt variables: 



cat("\n","----- Perform Graphical EDA - Histograms -----","\n")

require(ggplot2)
ggplot(data=forest.t) + geom_histogram(mapping=aes(x=elevation))
ggplot(data=forest.t) + geom_histogram(mapping=aes(x=aspect))
ggplot(data=forest.t) + geom_histogram(mapping=aes(x=slope))
ggplot(data=forest.t) + geom_histogram(mapping=aes(x=horiz_dist_hydr))
ggplot(data=forest.t) + geom_histogram(mapping=aes(x=vert_dist_hydr))
ggplot(data=forest.t) + geom_histogram(mapping=aes(x=horiz_dist_road))
ggplot(data=forest.t) + geom_histogram(mapping=aes(x=hillshade_9h))
ggplot(data=forest.t) + geom_histogram(mapping=aes(x=hillshade_12h))
ggplot(data=forest.t) + geom_histogram(mapping=aes(x=hillshade_15h))
ggplot(data=forest.t) + geom_histogram(mapping=aes(x=horiz_dist_fire))


quit()

aggregate(forest$wld_area_rawah + forest$wld_area_neota + forest$wld_area_comanche + forest$wld_area_cache,  by , FUN=mean)

pairs(forest)
pairs(forest$wld_area_rawah + wld_area_neota + wld_area_comanche + wld_area_cache)

cat("\n","----- Perform Graphical EDA - Histograms -----","\n") 






