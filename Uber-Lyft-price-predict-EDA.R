#IMPORTING DATASETS AND CLEANING THEM.....

#Importing dataset cab_rides
#cab_rides <- read.csv("C:/Users/nisht/Desktop/MITA/Fall/MVA/Final Project/cab_rides.csv")
summary(cab_rides)
str(cab_rides)
cab_data<-cab_rides


# Creating a date_time column
cab_data$date_time<-as.POSIXct((cab_data$time_stamp/1000),origin = "1970-01-01 00:53:20", tz="GMT")

#Importing dataset weather
#weather <- read.csv("C:/Users/nisht/Desktop/MITA/Fall/MVA/Final Project/weather.xls")
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

#Filling NA values in price
merge_data$rain[is.na(merge_data$rain)]<-0

#Extracting the numerical columns in a new dataframe "df"
merge_data$temp<-merge_data[,c(2)] #renaming a column
df<-merge_data[,c(4,5,6,8,9,10,11,17,22,16)]
summary(df)

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


#Checking for any null values
any(is.na(df))

#Adding date and time column in the df data set

df$day<-weekdays(df$date_time)
df$time<-format(df$date_time.x,"%H:%M:%S")
df$date_time<-as.Date(df$date_time.x)
merge_data$day=weekdays(merge_data$date_time.x)

#EXPLORING VARIABLES AND THEIR RELATIONSHIPS

tapply(merge_data$price, merge_data$day, FUN=sum)
barplot(tapply(merge_data$price, merge_data$day, FUN=sum),xlab = "Days",ylab="Price",
        main = "Price over Weekdays", col = "red")

#Price over Distance
tapply(merge_data$price, merge_data$distance, FUN=mean)
plot(tapply(merge_data$price, merge_data$distance, FUN=mean),xlab = "Distance",ylab="Price",
     main = "Price over Increasing Distance", col="blue")


#Rain over days
table(merge_data$rain)
tapply(merge_data$rain, merge_data$day, FUN=sum)
barplot(tapply(merge_data$rain, merge_data$day, FUN=sum),xlab = "Days",ylab="Rain",
        main = "Rain over Weekdays", col="sky blue")


#Surge_Multiplier over Days
tapply(merge_data$surge_multiplier, merge_data$day, FUN=mean)

barplot(tapply(merge_data$surge_multiplier, merge_data$day, FUN=sum),xlab = "Days",
        ylab="Surge_Multiplier",main = "Surges over Weekdays", col = "green")


#Cloud vs Price (We can see an increase in price with increase in clouds)
ggplot(data = merge_data, aes(x = merge_data$clouds, fill = merge_data$price))+
  geom_bar(color = "dark blue", size = 2)+labs(y="Count", x= "Clouds",title="Clouds Vs Price")



#EXPLORATORY ANALYSIS

# Normal Plot. Are they in a straight line. 

#BOXPLOT

boxplot(df$clouds, main="Clouds Box plot",yaxt="n", xlab="Clouds", horizontal=TRUE)
boxplot(df$temp, main="Temperature Box plot",yaxt="n", xlab="Temperature", horizontal=TRUE)
boxplot(df$pressure, main="Pressure Box plot",yaxt="n", xlab="Pressure", horizontal=TRUE)
boxplot(df$humidity, main="Humidity Box plot",yaxt="n", xlab="Humidity", horizontal=TRUE)
boxplot(df$wind, main="Wind Box plot",yaxt="n", xlab="Wind", horizontal=TRUE)
boxplot(df$distance, main="Wind Box plot",yaxt="n", xlab="Wind", horizontal=TRUE)


#Normal Probability plots for clouds, price, temp, pressure, humidity, rain, wind, distance
qqnorm(df[,"clouds"], main = "clouds"); qqline(df[,"clouds"])
qqnorm(df[,"price"], main = "price"); qqline(df[,"price"])
qqnorm(df[,"temp"], main = "temp"); qqline(df[,"temp"])
qqnorm(df[,"pressure"], main = "pressure"); qqline(df[,"pressure"])
qqnorm(df[,"humidity"], main = "humidity"); qqline(df[,"humidity"])
qqnorm(df[,"rain"], main = "rain"); qqline(df[,"rain"])
qqnorm(df[,"wind"], main = "wind"); qqline(df[,"wind"])
qqnorm(df[,"distance"], main = "distance"); qqline(df[,"distance"])

#CORRELATION, COVARIANCE AND DISTANCE

#We are calculating for: clouds, pressure, rain, humidity, wind, distance, surge_multiplier, temp, price
covariance<-cov(df[,c(1,2,3,4,5,7,8,9,10)]) #variamce-covariance matrix created
correlation<-cor(df[,c(1,2,3,4,5,7,8,9,10)]) #standardized
#colmeans
cm<-colMeans(df[,c(1,2,3,4,5,7,8,9,10)])
distance<-dist(scale(df[,c(1,2,3,4,5,7,8,9,10)],center=FALSE))
#Calculating di(generalized distance for all observations of our data)
#before that first extract all numeric variable in a dataframe
x<-df[,c(1,2,3,4,5,7,8,9,10)]
d <- apply(x, MARGIN = 1, function(x) + t(x - cm) %*% solve(covariance) %*% (x - cm))

#MVA Stuff
install.packages("MVA")
library(MVA)
install.packages("HSAUR2")
library(HSAUR2)

# Check out the datasets available in these packages
data(package='MVA')
data(package='HSAUR2')

#The sorted distance are now plotted against the appropriate quantiles of the chi-distribution

plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 8), sd <- sort(d),xlab = expression(paste(chi[8]^2, " Quantile")),ylab = "Ordered distances")
oups <- which(rank(abs(qc - sd), ties = "random") > nrow(x) - 3)
text(qc[oups], sd[oups] - 1.5,oups)
abline(a = 0, b = 1) 
#Our observations seems to deviate from linearity after a certain point 

#3d scatterplot
library(scatterplot3d)
s3d <- scatterplot3d(x$clouds,x$humidity,x$price,pch=c(1,16)[x$price],
                     xlab="Clouds", ylab="Humidity", angle=45,zlab="Price", 
                     lty.hide=2,type="h",y.margin.add=0.1,font.axis=2,font.lab=2)


#Scatterplot Matrix
pairs(x, pch = ".", cex = 1.5)
#Correlation matrix to determine the degree of closeness
round(cor(x), 4)
#Another scatter plot matrix to determine the linear fit of each pair of variables
library(SciViews)
pairs(x, 
      panel=function(x,y,...){
        points(x,y,...) 
        abline(lm(y~x),col="grey")
      }, pch=".", cex=1.5)


#Scatter plot along with the boxplot
pairs(x, 
      panel=function(x,y,...){
        points(x,y,...) 
        abline(lm(y~x),col="grey")
      }, diag.panel=panel.boxplot, pch=".",cex=1.5)

#Pca || T-test || F-test

#Keeping only the independent variables
x<-x[,c(-9)]
summary(x)

#Get the Correlations between the measurements
cor(x)
# Using prcomp to compute the principal components (eigenvalues and eigenvectors). 
#With scale=TRUE, variable means are set to zero, and variances set to one
x_pca <- prcomp(x,scale=TRUE)
x_pca
summary(x_pca)
x_pca$rotation
# sample scores stored in sparrows_pca$x
# singular values (square roots of eigenvalues) stored in sparrow_pca$sdev
# loadings (eigenvectors) are stored in sparrows_pca$rotation
# variable means stored in sparrows_pca$center
# variable standard deviations stored in sparrows_pca$scale
# A table containing eigenvalues and %'s accounted, follows
# Eigenvalues are sdev^2
(eigen_x <- x_pca$sdev^2)
names(eigen_x) <- paste("PC",1:8,sep="")
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
x_pca$x
xtyp_pca <- cbind(data.frame(df$price),x_pca$x)
xtyp_pca
colnames(xtyp_pca)[colnames(xtyp_pca)=="df.price"] <- "price"


t.test(xtyp_pca$PC1,xtyp_pca$price,var.equal = TRUE)
t.test(xtyp_pca$PC2,xtyp_pca$price,var.equal = TRUE)
t.test(xtyp_pca$PC3,xtyp_pca$price,var.equal = TRUE)
t.test(xtyp_pca$PC4,xtyp_pca$price,var.equal = TRUE)
t.test(xtyp_pca$PC5,xtyp_pca$price,var.equal = TRUE)
t.test(xtyp_pca$PC6,xtyp_pca$price,var.equal = TRUE)
t.test(xtyp_pca$PC7,xtyp_pca$price,var.equal = TRUE)
t.test(xtyp_pca$PC8,xtyp_pca$price,var.equal = TRUE)


# F ratio tests
var.test(xtyp_pca$PC1,xtyp_pca$price)
var.test(xtyp_pca$PC2,xtyp_pca$price)
var.test(xtyp_pca$PC3,xtyp_pca$price)
var.test(xtyp_pca$PC4,xtyp_pca$price)
var.test(xtyp_pca$PC5,xtyp_pca$price)
var.test(xtyp_pca$PC6,xtyp_pca$price)
var.test(xtyp_pca$PC7,xtyp_pca$price)
var.test(xtyp_pca$PC8,xtyp_pca$price)


