#IMPORTING DATASETS AND CLEANING THEM

#Importing dataset cab_rides

cab_rides <- read.csv("C:/Users/AJAY/Downloads/Multivariate/project/cab_rides.csv")
summary(cab_rides)
cab_data<-cab_rides

cab_data$date_time<-as.POSIXct((cab_data$time_stamp/1000),origin = "1970-01-01 00:53:20", tz="GMT")

weather <- read.csv("C:/Users/AJAY/Downloads/Multivariate/project/weather.xls")
summary(weather)
str(weather)
weather_data<-weather

#creating a date_time column in weather_data

weather_data$date_time<-as.POSIXct(weather_data$time_stamp,origin = "1970-01-01 00:53:20", tz="GMT")
str(weather_data)

#merge the datasets to reflect the same time for a location
cab_data$merge_date<-paste(cab_data$source,"-",as.Date(cab_data$date_time),"-",format(cab_data$date_time,"%H:%M:%S"))
weather_data$merge_date<-paste(weather_data$location,"-",as.Date(weather_data$date_time),"-",format(weather_data$date_time,"%H:%M:%S"))

#making those values as characters
weather_data$merge_date<-as.character(weather_data$merge_date)
cab_data$merge_date<-as.character(cab_data$merge_date)


#verify that merge_date has unique values.

weather_data<-subset(weather_data,!duplicated(weather_data$merge_date))
isTRUE(duplicated(weather_data$merge_date))

#Merging both the dataframes.
merge_data<-merge(x=weather_data, y=cab_data,by='merge_date', all.x=TRUE)
str(merge_data)

#Handling Missing values
#Filling NA values in price
merge_data$rain[is.na(merge_data$rain)]<-0

#Extracting the numerical columns in a new dataframe "df"
merge_data$temp<-merge_data[,c(2)] #renaming a column
df<-merge_data[,c(4,5,6,8,9,10,11,17,22,16)]

#Data preparation
#Dealing with missing values
summary(merge_data)
summary(df)

merge_data$surge_multiplier = ifelse(is.na(merge_data$surge_multiplier),
                                     ave(merge_data$surge_multiplier , FUN = function(x) mean(x, na.rm = TRUE)),
                                     merge_data$surge_multiplier)

merge_data$price = ifelse(is.na(merge_data$price),
                          ave(merge_data$price , FUN = function(x) mean(x, na.rm = TRUE)),
                          merge_data$price)

df$distance = ifelse(is.na(df$distance),
                     ave(df$distance , FUN = function(x) mean(x, na.rm = TRUE)),
                     df$distance)

df$surge_multiplier = ifelse(is.na(df$surge_multiplier),
                             ave(df$surge_multiplier , FUN = function(x) mean(x, na.rm = TRUE)),
                             df$surge_multiplier)

df$price = ifelse(is.na(df$price),
                  ave(df$price , FUN = function(x) mean(x, na.rm = TRUE)),
                  df$price)

#Checking for null values
any(is.na(df))

#Adding date and time column in the df data set
df$day<-weekdays(df$date_time)
df$time<-format(df$date_time.x,"%H:%M:%S")
df$date_time<-as.Date(df$date_time.x)
merge_data$day=weekdays(merge_data$date_time.x)

#Creating a Numeric dataframe
x<-df[,c(1,2,3,4,5,7,9)]
str(x)

#BOXPLOT

boxplot(x$temp, main="Temperature Box plot",yaxt="n", xlab="Temperature", horizontal=TRUE)
boxplot(x$pressure, main="Pressure Box plot",yaxt="n", xlab="Pressure", horizontal=TRUE)
boxplot(x$humidity, main="Humidity Box plot",yaxt="n", xlab="Humidity", horizontal=TRUE)
boxplot(x$wind, main="Wind Box plot",yaxt="n", xlab="Wind", horizontal=TRUE)
boxplot(x$distance, main="Wind Box plot",yaxt="n", xlab="Wind", horizontal=TRUE)

#Q-Q Plot to check normality..
library(rcompanion)
plotNormalHistogram(x$pressure)
qqnorm(df$pressure)
qqline(df$pressure, col="red")

plotNormalHistogram(x$humidity)
qqnorm(df$humidity)
qqline(df$humidity, col="red")

plotNormalHistogram(x$wind)
qqnorm(df$wind)
qqline(df$wind, col="red")

plotNormalHistogram(x$distance)
qqnorm(df$distance)
qqline(df$distance)

plotNormalHistogram(x$temp)
qqnorm(df$temp)
qqline(df$temp, col="red")

#Deviation from normality can be observed in our variables. Let's check for multivariate analysis using chi-squre plot

## CORRELATION, COVARIANCE AND DISTANCE
#We are calculating for: clouds, pressure, rain, humidity, wind, distance, surge_multiplier, temp, price
covariance<-cov(x) #variamce-covariance matrix created
correlation<-cor(x) #standardized
#colmeans
cm<-colMeans(x)
distance<-dist(scale(x,center=FALSE))
#Calculating di(generalized distance for all observations of our data)
d <- apply(x, MARGIN = 1, function(x) + t(x - cm) %*% solve(covariance) %*% (x - cm))


##The sorted distance are now plotted against the appropriate quantiles of the chi-distribution
plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 5), sd <- sort(d),xlab = expression(paste(chi[5]^2, " Quantile")),ylab = "Ordered distances")
oups <- which(rank(abs(qc - sd), ties = "random") > nrow(x) - 5)
text(qc[oups], sd[oups] - 1.5,oups)
abline(a=0,b=1,col="red") 
#Our observations seems to deviate from linearity after a certain point 


#There is a complete deviation from Normality. We will aplly the log transformation on our dataset.

#x_new<-x+1
#x_new=log(x - (min(x) - 1))
x_new<-log(x[,c(2,4,5,6,7)])


covariance<-cov(x_new) #variamce-covariance matrix created
correlation<-cor(x_new) #standardized
#colmeans
cm<-colMeans(x_new)
distance<-dist(scale(x_new,center=FALSE))
#Calculating di(generalized distance for all observations of our data)
d <- apply(x_new, MARGIN = 1, function(x_new) + t(x_new - cm) %*% solve(covariance) %*% (x_new - cm))


plot(qc <- qchisq((1:nrow(x_new) - 1/2) / nrow(x_new), df = 6), sd <- sort(d),xlab = expression(paste(chi[6]^2, " Quantile")),ylab = "Ordered distances")
oups <- which(rank(abs(qc - sd), ties = "random") > nrow(x) - 6)
text(qc[oups], sd[oups] - 1.5,oups)
abline(a=0,b=1,col="red") 


#We have normalized the data..

##Pca || T-test || F-test

#Get the Correlations between the measurements

cor(x_new)
sapply(x_new, sd, na.rm = TRUE)
#There are not considerable differences between these standard deviations.. Still let's see the PCAs.

# Using prcomp to compute the principal components (eigenvalues and eigenvectors). 
#With scale=TRUE, variable means are set to zero, and variances set to one
x_pca <- prcomp(x_new,scale=TRUE)
x_pca
summary(x_pca)
#x_pca$rotation

#We see that the first four components account for nearly 80% of the total variance.
# sample scores stored in x_pca$x
# singular values (square roots of eigenvalues) stored in x_pca$sdev
# loadings (eigenvectors) are stored in x_pca$rotation
# variable means stored in x_pca$center
# variable standard deviations stored in x_pca$scale
# A table containing eigenvalues and %'s accounted, follows
# Eigenvalues are sdev^2

(eigen_x <- x_pca$sdev^2)
names(eigen_x) <- paste("PC",1:5,sep="")
eigen_x
sumlambdas <- sum(eigen_x)
sumlambdas #total sample variance
propvar <- eigen_x/sumlambdas
propvar
cumvar_x <- cumsum(propvar)
cumvar_x
matlambdas <- rbind(eigen_x,propvar,cumvar_x)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)


# Sample scores stored in x_pca$x
#We need to calculate the scores on each of these components for each individual in our sample. 

#x_pca$x
xtyp_pca <- cbind(data.frame(df$price),x_pca$x)
str(xtyp_pca)
#xtyp_pca

#Merging price column
colnames(xtyp_pca)[colnames(xtyp_pca)=="df.price"] <- "price"
str(xtyp_pca)

#Sample scores stoted. x_pca$x

#T-Test-- We see that true difference in all the means is different from zero.
t.test(xtyp_pca$PC1,xtyp_pca$price,var.equal = TRUE)
t.test(xtyp_pca$PC2,xtyp_pca$price,var.equal = TRUE)
t.test(xtyp_pca$PC3,xtyp_pca$price,var.equal = TRUE)
t.test(xtyp_pca$PC4,xtyp_pca$price,var.equal = TRUE)
t.test(xtyp_pca$PC5,xtyp_pca$price,var.equal = TRUE)
#F-Test #Testing Variation


#Variance Test- Test for variance
var.test(xtyp_pca$PC1,xtyp_pca$price)
var.test(xtyp_pca$PC2,xtyp_pca$price)
var.test(xtyp_pca$PC3,xtyp_pca$price)
var.test(xtyp_pca$PC4,xtyp_pca$price)
var.test(xtyp_pca$PC5,xtyp_pca$price)

#Plotting the scores of Pricipal Component 1 and Principal component 2
plot(xtyp_pca$PC1, xtyp_pca$PC2,xlab="PC1:", ylab="PC2")
abline(h=0)
abline(v=0)


#Plotting the Variance of Principal Components
plot(eigen_x, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

#Plotting the Log variance of COmponents

plot(log(eigen_x), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")

#Variance of the principal components

#View(x_pca)
diag(cov(x_pca$x))
#x_pca$x[,1]
#x_pca$x

#Plotting the scores

xlim <- range(x_pca$x[,1])
plot(x_pca$x,xlim=xlim,ylim=xlim)
#x_pca$rotation[,1]
#x_pca$rotation

#Scatter plot matrix of the actual data

plot(x_new)


#Variance plot for each component. We can see that all components play a dominant role.

plot(x_pca)


#get the original value of the data based on PCA
center <- x_pca$center
scale <- x_pca$scale
new_x <- as.matrix(x_new)
#drop(scale(new_x,center=center, scale=scale)%*%x_pca$rotation[,1])
#predict(x_pca)[,1]
#The aboved two gives us the same thing. predict is a good function to know.

x_new$price<-df$price
out <- sapply(1:5, function(i){plot(x_new$price,x_pca$x[,i],xlab=paste("PC",i,sep=""),
                                    ylab="Price")})
pairs(x_pca$x[,1:5], ylim = c(-6,4),xlim = c(-6,4),panel=function(x,y,...){text(x,y,x_new$price)})


#CLuster Analysis

#install.packages("cluster", #lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(cluster)

#Pulling the numerical variables in the "Cluster" dataframe. Scaling the values..
cluster <- df[,c(1,2,4,5,7,9)]
matstd.cluster <- scale(cluster)
dim(matstd.cluster)

#Calculating the distance between all observations..
dist.cluster <- dist(matstd.cluster, method="euclidean")
length(dist.cluster)

# Invoking hclust command (cluster analysis by single linkage method)
hclust_cluster <- hclust(dist.cluster, method = "single")
par(mar=c(6, 4, 4, 2) + 0.1)
plot(as.dendrogram(hclust_cluster),ylab="Distance between weather conditions",ylim=c(0,2.5),main="Dendrogram of weather conditions")

#K-means Clustering for k=2 and then computing the percentage variance
#attach(cluster)
matstd.cluster <- scale(cluster)
# Computing the percentage of variation accounted for. Two clusters
kmeans2.cluster <- kmeans(matstd.cluster,2,nstart = 10)
perc.var.2 <- round(100*(1 - kmeans2.cluster$betweenss/kmeans2.cluster$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2

# Computing the percentage of variation accounted for. Three clusters
kmeans3.cluster <- kmeans(matstd.cluster,3,nstart = 10)
perc.var.3 <- round(100*(1 - kmeans3.cluster$betweenss/kmeans3.cluster$totss),1)
names(perc.var.3) <- "Perc. 3 clus"
perc.var.3

# Computing the percentage of variation accounted for. Four clusters
kmeans4.cluster <- kmeans(matstd.cluster,4,nstart = 10)
perc.var.4 <- round(100*(1 - kmeans4.cluster$betweenss/kmeans4.cluster$totss),1)
names(perc.var.4) <- "Perc. 4 clus"
perc.var.4

# Computing the percentage of variation accounted for. Five clusters
kmeans5.cluster <- kmeans(matstd.cluster,5,nstart = 10)
perc.var.5 <- round(100*(1 - kmeans5.cluster$betweenss/kmeans5.cluster$totss),1)
names(perc.var.5) <- "Perc. 5 clus"
perc.var.5


# Computing the percentage of variation accounted for. Six clusters
kmeans6.cluster <- kmeans(matstd.cluster,6,nstart = 10)
perc.var.6 <- round(100*(1 - kmeans6.cluster$betweenss/kmeans6.cluster$totss),1)
names(perc.var.6) <- "Perc. 6 clus"
perc.var.6

# plots to compare
#install.packages("VIM")
library(VIM)
#install.packages("tidyverse")
library(tidyverse)  # data manipulation
#install.packages("cluster")
library(cluster)    # clustering algorithms
#install.packages("factoextra")
library(factoextra)
p1 <- fviz_cluster(kmeans2.cluster, geom = "point", data = cluster) + ggtitle("k = 2")
p2 <- fviz_cluster(kmeans3.cluster, geom = "point",  data = cluster) + ggtitle("k = 3")
p3 <- fviz_cluster(kmeans4.cluster, geom = "point",  data = cluster) + ggtitle("k = 4")
p4 <- fviz_cluster(kmeans5.cluster, geom = "point",  data = cluster) + ggtitle("k = 5")
p5 <- fviz_cluster(kmeans6.cluster, geom = "point",  data = cluster) + ggtitle("k = 6")

#Grid plot
library(gridExtra)
grid.arrange(p1, p2, p3, p4,p5, nrow = 2)

#K-means Clustering for k=2 and then computing the percentage variance
#attach(cluster)
matstd.cluster <- scale(cluster)
# Computing the percentage of variation accounted for. Two clusters
kmeans2.cluster <- kmeans(matstd.cluster,2,nstart = 10)
perc.var.2 <- round(100*(1 - kmeans2.cluster$betweenss/kmeans2.cluster$totss),1)
names(perc.var.2) <- "Perc. 2 clus"
perc.var.2

# Computing the percentage of variation accounted for. Three clusters
kmeans3.cluster <- kmeans(matstd.cluster,3,nstart = 10)
perc.var.3 <- round(100*(1 - kmeans3.cluster$betweenss/kmeans3.cluster$totss),1)
names(perc.var.3) <- "Perc. 3 clus"
perc.var.3

# Computing the percentage of variation accounted for. Four clusters
kmeans4.cluster <- kmeans(matstd.cluster,4,nstart = 10)
perc.var.4 <- round(100*(1 - kmeans4.cluster$betweenss/kmeans4.cluster$totss),1)
names(perc.var.4) <- "Perc. 4 clus"
perc.var.4

# Computing the percentage of variation accounted for. Five clusters
kmeans5.cluster <- kmeans(matstd.cluster,5,nstart = 10)
perc.var.5 <- round(100*(1 - kmeans5.cluster$betweenss/kmeans5.cluster$totss),1)
names(perc.var.5) <- "Perc. 5 clus"
perc.var.5


# Computing the percentage of variation accounted for. Six clusters
kmeans6.cluster <- kmeans(matstd.cluster,6,nstart = 10)
perc.var.6 <- round(100*(1 - kmeans6.cluster$betweenss/kmeans6.cluster$totss),1)
names(perc.var.6) <- "Perc. 6 clus"
perc.var.6

# plots to compare
#install.packages("VIM")
library(VIM)
#install.packages("tidyverse")
library(tidyverse)  # data manipulation
#install.packages("cluster")
library(cluster)    # clustering algorithms
#install.packages("factoextra")
library(factoextra)
p1 <- fviz_cluster(kmeans2.cluster, geom = "point", data = cluster) + ggtitle("k = 2")
p2 <- fviz_cluster(kmeans3.cluster, geom = "point",  data = cluster) + ggtitle("k = 3")
p3 <- fviz_cluster(kmeans4.cluster, geom = "point",  data = cluster) + ggtitle("k = 4")
p4 <- fviz_cluster(kmeans5.cluster, geom = "point",  data = cluster) + ggtitle("k = 5")
p5 <- fviz_cluster(kmeans6.cluster, geom = "point",  data = cluster) + ggtitle("k = 6")

#Grid plot
library(gridExtra)
grid.arrange(p1, p2, p3, p4,p5, nrow = 2)

#K=5 seems optimal number of clusters

fviz_cluster(kmeans5.cluster, data = cluster)

#Adding cluster number to the file for each observation-
clusterFile <- cbind(df, clusterNum = kmeans5.cluster$cluster)
head(clusterFile)

#We found that all our columns are significant and are not so highly correlated. Hence, we would keep them as they are. There were no considerable differences between their standard deviations.. Still we checked with PCA and decided to keep all the variables . We need not to do the Factor ANalysis in our dataset.. still we will verify doing so!

##Factor Analysis

library(psych)
install.packages("psych", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
fit.pc <- principal(x_new, nfactors=5, rotate="varimax")
fit.pc
fa.parallel(x_new) # See factor recommendation

fa.plot(fit.pc) # See Correlations within Factors

# Visualize the relationship
#Here we can see that each variable is assigned to each factor. Hence, we do not proceed with factor analysis on our dataset.
fa.diagram(fit.pc)
# See Factor recommendations for a simple structure
vss(cluster)
