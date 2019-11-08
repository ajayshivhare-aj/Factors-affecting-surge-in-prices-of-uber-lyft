#IMPORTING DATASETS AND CLEANING THEM

#Importing dataset cab_rides


cab_rides <- read.csv("C:/Users/AJAY/Downloads/Multivariate/project/cab_rides.csv")
summary(cab_rides)
cab_data<-cab_rides


# Creating a date_time column

cab_data$date_time<-as.POSIXct((cab_data$time_stamp/1000),origin = "1970-01-01 00:53:20", tz="GMT")


#Importing dataset weather

weather <- read.csv("C:/Users/AJAY/Downloads/Multivariate/project/weather.csv")
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
#str(merge_data)


merge_data$rain<-as.numeric(merge_data$rain)
merge_data$rain[is.na(merge_data$rain)]<-0

for ( i in 1:length(merge_data$rain)){
  if(merge_data$rain[i]>0 & merge_data$rain[i]<=0.30){
    merge_data$rain[i]=1
  }
}

for ( i in 1:length(merge_data$rain)){
  if(merge_data$rain[i]>=0.30 & merge_data$rain[i]!=1){
    merge_data$rain[i]=2
  }
}

merge_data$rain = factor(merge_data$rain,
                         levels = c(0,1,2),
                         labels = c(0,1,2))


#Handling Missing values

#Extracting the numerical columns in a new dataframe "df"
merge_data$temp<-merge_data[,c(2)] #renaming a column
df<-merge_data[,c(4,5,8,9,10,11,22,16)]


#Data preparation
#Dealing with missing values
#summary(merge_data)
#summary(df)

merge_data$distance = ifelse(is.na(merge_data$distance),
                             ave(merge_data$distance , FUN = function(x) mean(x, na.rm = TRUE)),
                             merge_data$surge_multiplier)

merge_data$price = ifelse(is.na(merge_data$price),
                          ave(merge_data$price , FUN = function(x) mean(x, na.rm = TRUE)),
                          merge_data$price)

df$distance = ifelse(is.na(df$distance),
                     ave(df$distance , FUN = function(x) mean(x, na.rm = TRUE)),
                     df$distance)


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
x<-df[,c(1,2,3,4,6,7,8)]
head(x)


#Let's check for multivariate analysis using chi-squre plot

## CORRELATION, COVARIANCE AND DISTANCE

#We are calculating for: clouds, pressure, rain, humidity, wind, distance, temp
covariance<-cov(x) #variance-covariance matrix created
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

#There is a complete deviation from Normality. We will apply the log transformation on our dataset.

#x_new<-x+1
#x_new=log(x - (min(x) - 1))
x_new<-log(x+1)


covariance<-cov(x_new) #variance-covariance matrix created
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
x_new<-x_new[-7]
cor(x_new)
sapply(x_new, sd, na.rm = TRUE)
#There are not considerable differences between these standard deviations.. Still let's see the PCAs.


#Let's Visualize Correlation..
library(corrplot)
corrplot(cor(x_new), method="ellipse")


# Using prcomp to compute the principal components (eigenvalues and eigenvectors). 
#With scale=TRUE, variable means are set to zero, and variances set to one

x_pca <- prcomp(x_new,scale=TRUE)
#x_pca$rotation
summary(x_pca)
#x_pca$rotation

eigen_x <- x_pca$sdev^2
names(eigen_x) <- paste("PC",1:6,sep="")
#eigen_x
sumlambdas <- sum(eigen_x)
sumlambdas #total sample variance
propvar <- eigen_x/sumlambdas
#propvar
cumvar_x <- cumsum(propvar)
#cumvar_x
matlambdas <- rbind(eigen_x,propvar,cumvar_x)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)


# Sample scores stored in x_pca$x

#We need to calculate the scores on each of these components for each individual in our sample. 

#x_pca$rotation
xtyp_pca <- cbind(data.frame(df$price),x_pca$x)
str(xtyp_pca)
#xtyp_pca

#Merging price column
colnames(xtyp_pca)[colnames(xtyp_pca)=="df.price"] <- "price"
str(xtyp_pca)

#Plotting the scores of Pricipal Component 1 and Principal component 2

plot(xtyp_pca$PC1, xtyp_pca$PC2,xlab="PC1:", ylab="PC2")
abline(h=0)
abline(v=0)


#Plotting the Variance of Principal Components
plot(eigen_x, xlab = "Component number", ylab = "Component variance", type = "b", main = "Scree diagram")

#Plotting the Log variance of COmponents
plot(log(eigen_x), xlab = "Component number",ylab = "log(Component variance)", type="o",main = "Log(eigenvalue) diagram")

#Cumulative scree plot
plot(cumsum(propvar), xlab = "Principal Component",
ylab = "Cumulative Proportion of Variance Explained",
type = "b")

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

#Variance plot for each component. We can see that all components play a dominant role.
plot(x_pca)

#Taking first 4 components
xtyp_pca<-xtyp_pca[1:4]

#They are explaing 88% of the total variance.


##Factor Analysis


library(psych)
#install.packages("psych", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
fit.pc <- principal(x_new, nfactors=4, rotate="varimax")
fit.pc


round(fit.pc$values, 3)
fit.pc$loadings
#The first 4 factors have an Eigenvalue >1 and which explains almost 88% of the variance. We can effectively reduce dimensionality from 6 to 4 while only losing about 11% of the variance.



# Communalities

fit.pc$communality

#The variance in clouds accounted by all fators is 0.87, This is the extent to which an item correlates with all other items. All comunalities are high, which means that the extracted components represent the variables well. If they are low , you may need to extract another component.

# Rotated factor scores, Notice the columns ordering: RC1, RC3, RC2 and RC4
fit.pc_scores<-fit.pc$scores
head(fit.pc_scores)

# See factor recommendation

fa.parallel(x_new, fm='minres', fa='fa')
#Blue line shows the eigen values of actual data and the two red lines show simulated and resampled data. Here factors between 2-4 will be a good choice..


fa.plot(fit.pc) # See Correlations within Factors

# Visualize the relationship
fa.diagram(fit.pc)
#Red dotted line means Wind marginally falls under the RC1 bucket.


# See Factor recommendations for a simple structure
vss(x_new) 
#We should continue with 4 factors as it has the max vss complexity.
#Regression analysis using the factors scores as the independent variable:
#Let's combine the dependent variable and the factor scores into a dataset and label them.

cab<-cbind(x[7],fit.pc$scores)
#Labelling the data
names(cab)<-c("Price","Wind_Pressure","Temperature","Humidity","Distance")
head(cab)

#Let's split the dataset into training and testing dataset. (80:20)
set.seed(101)
Atrain<-sample(nrow(cab),nrow(cab)*0.80)
cab_train<-cab[Atrain,]
cab_test<-cab[-Atrain,]
dim(cab_train)
dim(cab_test)

#Performing multiple regression (Taking alpha=0.1)
fit1 <- lm(Price~Wind_Pressure+Temperature+Humidity+Distance, data=cab_train)
#show the results
summary(fit1)

#Wind_Pressure is highly insignificant
fit2 <- lm(Price~Temperature+Humidity+Distance, data=cab_train)
#show the results
summary(fit2)

#Humidity is insignificant
fit3 <- lm(Price~Temperature+Distance, data=cab_train)
#show the results
summary(fit3)
#Now we can see that all the variables are significant now. This model is explaining only 0.1% of variance only.

# Predicted Test

predicted_test <- predict(fit3, newdata= cab_test)
#Residuals Analysis
cab_testresults <- cab_test
cab_testresults$predicted <- predicted_test
cab_testresults$residual <- cab_testresults$Price - cab_testresults$predicted
#View(cab_testresults$residual)
plot(cab_testresults$predicted,cab_testresults$residual,xlab="Predicted",ylab="Residuals",pch=21,bg="red",col="red",main="Predicted Vs Residuals")
abline(0,0)


#Anova Table
anova(fit3)
vcov(fit3)
cov2cor(vcov(fit3))
temp <- influence.measures(fit3)
temp

#diagnostic plots
plot(fit3)



# QQ-Plot
# Normality of Residuals
# qq plot for studentized resid
#install.packages("car")
library(car)
library(qqplotr)
qqPlot(fit3, main="QQ Plot")


# Distribution of studentized residuals

library(MASS)
sresid <- studres(fit3)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)


#Hence we see that residuals are normally distributed

# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(mtcars)-length(fit3$coefficients)-2))
plot(fit3, which=4, cook.levels=cutoff)

# Influence Plot
influencePlot(fit3, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


# non-constant error variance test
ncvTest(fit3)

# plot studentized residuals vs. fitted values
spreadLevelPlot(fit3)
