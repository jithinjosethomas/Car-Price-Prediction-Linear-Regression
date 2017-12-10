install.packages("car")
install.packages("lmtest")
install.packages("caret")
install.packages("lattice")
install.packages("MASS")

library("car")
library("lmtest")
library("caret")
library("lattice")
library("MASS")
library("stringr")


#A Chinese automobile company Geely Auto aspires to enter the US market
#by setting up their manufacturing unit there and producing cars locally
#to give competition to their US and European counterparts. 

#They have contracted an automobile consulting company to understand the 
#factors on which the pricing of a car depends.Specifically, they want to 
#understand the factors affecting the pricing of cars in the American marketing.

#OBJECTIVE
#---------#
#Essentially, the company wants to know:
#1)Which variables are significant in predicting the price of a car
#2)How well those variables describe the price of a car

#Setting working directory
#setwd("~/Desktop/PGDDA Course/Regression Case Study")

carPricedata <- read.csv(file="CarPrice_Assignment.csv", header=TRUE, sep=",",stringsAsFactors=FALSE)

#rounding of numerical values to 1,2 or 3 decimal places as deemed fit
round(carPricedata$price,2)
round(carPricedata$wheelbase,1)
round(carPricedata$carlength,1)
round(carPricedata$carwidth,1)
round(carPricedata$carheight,1)
round(carPricedata$boreratio,2)
round(carPricedata$stroke,3)
round(carPricedata$compressionratio,1)


View(carPricedata)
str(carPricedata)

#Dim of carPricedata
dim(carPricedata)
#205 rows
#26 columns

#Column names
names(carPricedata)

#DATA CLEANING AND MANIPULATION
#=============================#

#check for missing values
sapply(carPricedata, function(x) sum(is.na(x)))
# no missing values

#Check for duplicate values
sum(duplicated(carPricedata$car_ID))

na.omit(carPricedata)

#Check for NA values
#there is no NA values
#as dim() gives same number of rows and columns after executing na.omit
na.omit(carPricedata)
#Dim of carPricedata
dim(carPricedata)
#205 rows
#26 columns

#Column names
names(carPricedata)


#There is a variable named CarName which is comprised of two parts 
#the first word is the name of 'car company' and the second is the 'car model'. 
#For example, chevrolet impala has 'chevrolet' as the car company name and 'impala' as the car model name. 
#Consider only company name as the independent variable for the model building.
#carPricedata$CarName <-(str_split_fixed(carPricedata$CarName, " ", 2)
split <-str_split_fixed(carPricedata$CarName, " ", 2)
split
carPricedata$CarName <- split[,1]

#correcting the car spellings
#egs: vw,vokswagen --> volkswagen
#egs: maxda --> mazda
#egs: porcshe --> porsche
#egs: toyouta --> toyota
idx <- agrep(pattern = "maxda", x = carPricedata$CarName, ignore.case = FALSE, value = FALSE, max.distance =0)
# replace the elements that matches 
carPricedata$CarName[idx] <- "mazda"

idx <- agrep(pattern = "porcshce", x = carPricedata$CarName, ignore.case = FALSE, value = FALSE, max.distance =0)
# replace the elements that matches 
carPricedata$CarName[idx] <- "porsche"

idx <- agrep(pattern = "toyouta", x = carPricedata$CarName, ignore.case = FALSE, value = FALSE, max.distance =0)
# replace the elements that matches 
carPricedata$CarName[idx] <- "toyota"

idx <- agrep(pattern = "Nissan", x = carPricedata$CarName, ignore.case = FALSE, value = FALSE, max.distance =0)
# replace the elements that matches 
carPricedata$CarName[idx] <- "nissan"

idx <- agrep(pattern = "vw", x = carPricedata$CarName, ignore.case = FALSE, value = FALSE, max.distance =0)
# replace the elements that matches 
carPricedata$CarName[idx] <- "volkswagen"

idx <- agrep(pattern = "vokswagen", x = carPricedata$CarName, ignore.case = FALSE, value = FALSE, max.distance =0)
# replace the elements that matches 
carPricedata$CarName[idx] <- "volkswagen"

idx <- agrep(pattern = "alfa-romero", x = carPricedata$CarName, ignore.case = FALSE, value = FALSE, max.distance =0)
# replace the elements that matches 
carPricedata$CarName[idx] <- "alfa-romeo"

str(carPricedata$CarName)
carPricedata$CarName <- as.factor(carPricedata$CarName)
summary(carPricedata$CarName)


#fuel type
str(carPricedata$fueltype)
#factor according to fueltype
carPricedata$fueltype <- as.factor(carPricedata$fueltype)
summary(carPricedata$fueltype)
#convert fueltype variable to numeric is to replace the levels- diesel and gas with 1 and 0 
levels(carPricedata$fueltype)<-c(1,0)
# Now store the numeric values in the same variable
carPricedata$fueltype <- as.numeric(levels(carPricedata$fueltype))[carPricedata$fueltype]

#aspiration
str(carPricedata$aspiration)
#factor according to aspiration
carPricedata$aspiration <- as.factor(carPricedata$aspiration)
summary(carPricedata$aspiration)
#convert aspiration variable to numeric is to replace the levels - std and turbo with 1 and 0
levels(carPricedata$aspiration)<-c(1,0)
carPricedata$aspiration <- as.numeric(levels(carPricedata$aspiration))[carPricedata$aspiration]

#doornumber
str(carPricedata$doornumber)
#factor according to doornumber
carPricedata$doornumber <- as.factor(carPricedata$doornumber)
summary(carPricedata$doornumber)
#convert doornumber variable to numeric is to replace the levels - four and two with 1 and 0
levels(carPricedata$doornumber)<-c(1,0)
carPricedata$doornumber <- as.numeric(levels(carPricedata$doornumber))[carPricedata$doornumber]


str(carPricedata$enginelocation)
carPricedata$enginelocation <- as.factor(carPricedata$enginelocation)
summary(carPricedata$enginelocation)
#convert enginelocation variable to numeric is to replace the levels - front and rear with 1 and 0
levels(carPricedata$enginelocation)<-c(1,0)
carPricedata$enginelocation <- as.numeric(levels(carPricedata$enginelocation))[carPricedata$enginelocation]


#Now we come across variables having more than 3 levels.
str(carPricedata$carbody)
#factor according to carbody
carPricedata$carbody <- as.factor(carPricedata$carbody)
summary(carPricedata$carbody)
#Converting "carbody" into dummies . 
dummy_1 <- data.frame(model.matrix( ~carbody, data = carPricedata))
#check the dummy_1 data frame.
#View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "carbody". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
carPricedata_1 <- cbind(carPricedata[,-7], dummy_1)
#View(carPricedata_1)



str(carPricedata_1$drivewheel)
carPricedata_1$drivewheel <- as.factor(carPricedata_1$drivewheel)
summary(carPricedata_1$drivewheel)
#Converting "drivewheel" into dummies . 
dummy_2 <- data.frame(model.matrix( ~drivewheel, data = carPricedata_1))
#check the dummy_2 data frame.
#View(dummy_2)
#This column should be removed from the newly created dummy_2 dataframe containing the dummy values for the variable "drivewheel". 
dummy_2 <- dummy_2[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "drivewheel" column
carPricedata_2 <- cbind(carPricedata_1[,-7], dummy_2)
#View(carPricedata_2)




str(carPricedata_2$enginetype)
carPricedata_2$enginetype <- as.factor(carPricedata_2$enginetype)
summary(factor(carPricedata$enginetype))
#Converting "enginetype" into dummies . 
dummy_3 <- data.frame(model.matrix( ~enginetype, data = carPricedata_2))
#check the dummy_3 data frame.
#View(dummy_3)
#This column should be removed from the newly created dummy_3 dataframe containing the dummy values for the variable "enginetype". 
dummy_3 <- dummy_3[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "enginetype" column
carPricedata_3 <- cbind(carPricedata_2[,-13], dummy_3)
#View(carPricedata_3)


str(carPricedata_3$cylindernumber)
carPricedata_3$cylindernumber <- as.factor(carPricedata_3$cylindernumber)
summary(carPricedata_3$cylindernumber)
#Converting "cylindernumber" into dummies . 
dummy_4 <- data.frame(model.matrix( ~cylindernumber, data = carPricedata_3))
#check the dummy_4 data frame.
#View(dummy_4)
#This column should be removed from the newly created dummy_4 dataframe containing the dummy values for the variable "cylindernumber". 
dummy_4 <- dummy_4[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "cylindernumber" column
carPricedata_4 <- cbind(carPricedata_3[,-13], dummy_4)
#View(carPricedata_4)



str(carPricedata_4$fuelsystem)
carPricedata_4$fuelsystem <- as.factor(carPricedata_4$fuelsystem)
summary(carPricedata_4$fuelsystem)
#Converting "fuelsystem" into dummies . 
dummy_5 <- data.frame(model.matrix( ~fuelsystem, data = carPricedata_4))
#check the dummy_5 data frame.
#View(dummy_5)
#This column should be removed from the newly created dummy_5 dataframe containing the dummy values for the variable "fuelsystem". 
dummy_5 <- dummy_5[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "fuelsystem" column
carPricedata_5 <- cbind(carPricedata_4[,-14], dummy_5) 
#View(carPricedata_5)

str(carPricedata_5$symboling)
carPricedata_5$symboling <- as.factor(carPricedata_5$symboling)
summary(carPricedata_5$symboling)
#Converting "symboling" into dummies . 
dummy_6 <- data.frame(model.matrix( ~symboling, data = carPricedata_5))
#check the dummy_6 data frame.
#View(dummy_6)
#This column should be removed from the newly created dummy_6 dataframe containing the dummy values for the variable "symboling". 
dummy_6 <- dummy_6[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "symboling" column
carPricedata_6 <- cbind(carPricedata_5[,-2], dummy_6) 
#View(carPricedata_6)


str(carPricedata_6$CarName)
carPricedata_6$CarName <- as.factor(carPricedata_6$CarName)
#Converting "CarName" into dummies . 
dummy_7 <- data.frame(model.matrix( ~CarName, data = carPricedata_6))
#check the dummy_7 data frame.
View(dummy_7)
#This column should be removed from the newly created dummy_7 dataframe containing the dummy values for the variable "CarName". 
dummy_7 <- dummy_7[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "CarName" column
carPricedata_7 <- cbind(carPricedata_6[,-2], dummy_7) 
View(carPricedata_7)

#create a copy of final df
carPricedata_copy <- carPricedata_7
#remove Car_ID column
carPricedata_copy$car_ID <- NULL
View(carPricedata_copy)


#Derived metrics
#power to weight ration
#to measure the vehicles performance with engine power output.
carPricedata_copy$powertoweightratio <- round(carPricedata_copy$horsepower/carPricedata_copy$curbweight,3)

#Dim of carPricedata_copy 
dim(carPricedata_copy)
#205 rows
#70 columns


#check for outliers
#wheelbase
outlier_values <- boxplot.stats(carPricedata_copy$wheelbase)$out  # outlier values.
boxplot(carPricedata_copy$wheelbase, main="wheelbase", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#carlength
outlier_values <- boxplot.stats(carPricedata_copy$carlength)$out  # outlier values.
boxplot(carPricedata_copy$carlength, main="carlength", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#carwidth
outlier_values <- boxplot.stats(carPricedata_copy$carwidth)$out  # outlier values.
boxplot(carPricedata_copy$carwidth, main="carwidth", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#carheight
outlier_values <- boxplot.stats(carPricedata_copy$carheight)$out  # outlier values.
boxplot(carPricedata_copy$carheight, main="carheight", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#curbweight
outlier_values <- boxplot.stats(carPricedata_copy$curbweight)$out  # outlier values.
boxplot(carPricedata_copy$curbweight, main="curbweight", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#enginesize
outlier_values <- boxplot.stats(carPricedata_copy$enginesize)$out  # outlier values.
boxplot(carPricedata_copy$enginesize, main="enginesize", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#boreratio
outlier_values <- boxplot.stats(carPricedata_copy$boreratio)$out  # outlier values.
boxplot(carPricedata_copy$boreratio, main="boreratio", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)


#stroke
outlier_values <- boxplot.stats(carPricedata_copy$stroke)$out  # outlier values.
boxplot(carPricedata_copy$stroke, main="stroke", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)


#compressionratio
outlier_values <- boxplot.stats(carPricedata_copy$compressionratio)$out  # outlier values.
boxplot(carPricedata_copy$compressionratio, main="compressionratio", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)


#horsepower
outlier_values <- boxplot.stats(carPricedata_copy$horsepower)$out  # outlier values.
boxplot(carPricedata_copy$horsepower, main="horsepower", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)


#peakrpm
outlier_values <- boxplot.stats(carPricedata_copy$peakrpm)$out  # outlier values.
boxplot(carPricedata_copy$peakrpm, main="peakrpm", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#citympg
outlier_values <- boxplot.stats(carPricedata_copy$citympg)$out  # outlier values.
boxplot(carPricedata_copy$citympg, main="citympg", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)


#highwaympg
outlier_values <- boxplot.stats(carPricedata_copy$highwaympg)$out  # outlier values.
boxplot(carPricedata_copy$highwaympg, main="highwaympg", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)


#price
outlier_values <- boxplot.stats(carPricedata_copy$price)$out  # outlier values.
boxplot(carPricedata_copy$price, main="price", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)


#Outlier treatment
#Capping done for outlier values

#########
#Checking for outliers in wheelbase
quantile(carPricedata_copy$wheelbase,seq(0,1,0.01))
#jump in value in 99% to 100% 115.544 to 120.900
carPricedata_copy$wheelbase[which(carPricedata_copy$wheelbase > 115.544)]<-115.544 #overwrite topmost values

#carlength
quantile(carPricedata_copy$carlength,seq(0,1,0.01))
#99% to 100% 202.480 to 208.100 
carPricedata_copy$carlength[which(carPricedata_copy$carlength > 202.480)]<-202.480
#2% to 3% 150.000 to 155.900
carPricedata_copy$carlength[which(carPricedata_copy$carlength < 155.900)]<-155.900

#carwidth
quantile(carPricedata_copy$carwidth,seq(0,1,0.01))
#0% to 1% 60.300 to 62.536
carPricedata_copy$carwidth[which(carPricedata_copy$carwidth < 62.536)]<-62.536


#curbweight
quantile(carPricedata_copy$curbweight,seq(0,1,0.01))
#0% to 1% 1488.00 to 1819.72 
carPricedata_copy$curbweight[which(carPricedata_copy$curbweight < 1819.72 )]<-1819.72 
#93% to 94% 3376.08 to 3471.80 
carPricedata_copy$curbweight[which(carPricedata_copy$curbweight > 3376.08 )]<-3376.08 


#enginesize
quantile(carPricedata_copy$enginesize,seq(0,1,0.01))
#2% to 3% 79.08 to  90.00
carPricedata_copy$enginesize[which(carPricedata_copy$enginesize < 90.00 )]<-90.00 
#99%  to 100% 302.16 to 326.00
carPricedata_copy$enginesize[which(carPricedata_copy$enginesize > 302.16 )]<-302.16 

#boreratio
quantile(carPricedata_copy$boreratio,seq(0,1,0.01))
# 0%  to  1% 2.5400 to 2.9100
carPricedata_copy$boreratio[which(carPricedata_copy$boreratio < 2.9100 )]<-2.9100

#stroke
quantile(carPricedata_copy$stroke,seq(0,1,0.01))
#1% to 2% 2.1968 to 2.6400
carPricedata_copy$stroke[which(carPricedata_copy$stroke < 2.6400 )]<-2.6400
#95% to 96% 3.6400 to 3.8248
carPricedata_copy$stroke[which(carPricedata_copy$stroke > 3.6400 )]<-3.6400

#compressionratio
quantile(carPricedata_copy$compressionratio,seq(0,1,0.01))
#90% to 91% 10.9400 to 21.0000
carPricedata_copy$compressionratio[which(carPricedata_copy$compressionratio > 10.9400 )]<-10.9400

#horsepower
quantile(carPricedata_copy$horsepower,seq(0,1,0.01))
#99% to 100% 207.00 to 288.00 
carPricedata_copy$horsepower[which(carPricedata_copy$horsepower > 207.00 )]<-207.00

#peakrpm
quantile(carPricedata_copy$peakrpm,seq(0,1,0.01))
#99% to 100% 6000 to 6600 
carPricedata_copy$peakrpm[which(carPricedata_copy$peakrpm > 6000 )]<-6000

#citympg
quantile(carPricedata_copy$citympg,seq(0,1,0.01))
#98% to 99% 38.00 to 44.72  
carPricedata_copy$citympg[which(carPricedata_copy$citympg > 38.00 )]<-38.00

#highwaympg
quantile(carPricedata_copy$highwaympg,seq(0,1,0.01))
#99% to 100%  49.88 to 54.00  
carPricedata_copy$highwaympg[which(carPricedata_copy$highwaympg > 49.88 )]<-49.88


# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(carPricedata_copy), 0.7*nrow(carPricedata_copy))
train = carPricedata_copy[trainindices,]
test = carPricedata_copy[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(price~. ,data=train)
#Multiple R-squared:  0.9838,	Adjusted R-squared:  0.9719 
summary(model_1)

# Check if the correlation matrix gives some insight.
corrs = cor(carPricedata_copy)
View(corrs)



# Now, lets use stepAIC
step <- stepAIC(model_1, direction="both")
step


# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2
# You can notice that stepAIC removed variables
# We'll move forward with whatever is left



# Let's execute this model here, 
model_2 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + boreratio + horsepower + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + symboling3 + CarNamebmw + 
                CarNamebuick + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                powertoweightratio, data = train)
# Let us look at the summary of the model
summary(model_2)
#Multiple R-squared:  0.9826,	Adjusted R-squared:  0.9758

## Let us check for multicollinearity 
# If the VIF is above 2 or 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant
vif(model_2)



#checking VIF and p value and removing
#CarNamejaguar

model_3 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + boreratio + horsepower + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + symboling3 + CarNamebmw + 
                CarNamebuick + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                powertoweightratio, data = train)


summary(model_3)
#Multiple R-squared:  0.9823,	Adjusted R-squared:  0.9757 
vif(model_3)


#checking VIF and p value and removing
#curbweight 


model_4 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + boreratio + horsepower + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + symboling3 + CarNamebmw + 
                CarNamebuick + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                powertoweightratio, data = train)

summary(model_4)
#Multiple R-squared:  0.9821,	Adjusted R-squared:  0.9756  
vif(model_4)


#checking VIF and p value and removing
#cylindernumbersix


model_5 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + boreratio + horsepower + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumberthree + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + symboling3 + CarNamebmw + 
                CarNamebuick + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                powertoweightratio, data = train)

summary(model_5)
#Multiple R-squared:  0.982,	Adjusted R-squared:  0.9757 
vif(model_5)


#checking VIF and p value and removing
#cylindernumberfive

model_6 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + boreratio + horsepower + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + cylindernumberfour + 
                cylindernumberthree + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + symboling3 + CarNamebmw + 
                CarNamebuick + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                powertoweightratio, data = train)



summary(model_6)
#Multiple R-squared:  0.9818,	Adjusted R-squared:  0.9756 
vif(model_6)


#checking VIF and p value and removing
#symboling3


model_7 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + boreratio + horsepower + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + cylindernumberfour + 
                cylindernumberthree + fuelsystem2bbl + 
                fuelsystemmpfi + fuelsystemspdi + CarNamebmw + 
                CarNamebuick + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                powertoweightratio, data = train)

summary(model_7)
#Multiple R-squared:  0.9814,	Adjusted R-squared:  0.9754
vif(model_7)




#checking VIF and p value and removing
#fuelsystemspdi

model_8 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + boreratio + horsepower + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + cylindernumberfour + 
                cylindernumberthree + fuelsystem2bbl + 
                fuelsystemmpfi + CarNamebmw + 
                CarNamebuick + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                powertoweightratio, data = train)

summary(model_8)
#Multiple R-squared:  0.9811,	Adjusted R-squared:  0.9751
vif(model_8)



#checking VIF and p value and removing
#drivewheelrwd

model_9 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                enginesize + boreratio + horsepower + peakrpm + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + cylindernumberfour + 
                cylindernumberthree + fuelsystem2bbl + 
                fuelsystemmpfi + CarNamebmw + 
                CarNamebuick + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                powertoweightratio, data = train)


summary(model_9)
#Multiple R-squared:  0.9804,	Adjusted R-squared:  0.9745
vif(model_9)



#checking VIF and p value and removing
#carbodyhardtop

model_10 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + horsepower + peakrpm + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetyperotor + cylindernumberfour + 
                 cylindernumberthree + fuelsystem2bbl + 
                 fuelsystemmpfi + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + CarNameisuzu + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                 powertoweightratio, data = train)


summary(model_10)
#Multiple R-squared:  0.9793,	Adjusted R-squared:  0.9733
vif(model_10)



#checking VIF and p value and removing
#carbodysedan

model_11 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + horsepower + peakrpm + 
                 carbodyhatchback + carbodywagon + 
                 enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetyperotor + cylindernumberfour + 
                 cylindernumberthree + fuelsystem2bbl + 
                 fuelsystemmpfi + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + CarNameisuzu + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                 powertoweightratio, data = train)


summary(model_11)
#Multiple R-squared:  0.979,	Adjusted R-squared:  0.9731 
vif(model_11)

#checking VIF and p value and removing
#cylindernumberthree

model_12 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + horsepower + peakrpm + 
                 carbodyhatchback + carbodywagon + 
                 enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetyperotor + cylindernumberfour + 
                 fuelsystem2bbl + 
                 fuelsystemmpfi + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + CarNameisuzu + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                 powertoweightratio, data = train)


summary(model_12)
#Multiple R-squared:  0.9788,	Adjusted R-squared:  0.9732 
vif(model_12)



#checking VIF and p value and removing
#carbodyhatchback

model_13 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + horsepower + peakrpm + 
                 carbodywagon + 
                 enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetyperotor + cylindernumberfour + 
                 fuelsystem2bbl + 
                 fuelsystemmpfi + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + CarNameisuzu + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                 powertoweightratio, data = train)


summary(model_13)
#Multiple R-squared:  0.9786,	Adjusted R-squared:  0.9732
vif(model_13)


#checking VIF and p value and removing
#carbodywagon

model_14 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + horsepower + peakrpm + 
                 enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetyperotor + cylindernumberfour + 
                 fuelsystem2bbl + 
                 fuelsystemmpfi + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + CarNameisuzu + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                 powertoweightratio, data = train)


summary(model_14)
#Multiple R-squared:  0.9784,	Adjusted R-squared:  0.9731
vif(model_14)


#checking VIF and p value and removing
#horsepower

model_15 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + peakrpm + 
                 enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetyperotor + cylindernumberfour + 
                 fuelsystem2bbl + 
                 fuelsystemmpfi + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + CarNameisuzu + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                 powertoweightratio, data = train)


summary(model_15)
#Multiple R-squared:  0.9775,	Adjusted R-squared:  0.9722
vif(model_15)



#checking VIF and p value and removing
#CarNameisuzu

model_16 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + peakrpm + 
                 enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetyperotor + cylindernumberfour + 
                 fuelsystem2bbl + 
                 fuelsystemmpfi + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                 powertoweightratio, data = train)


summary(model_16)
#Multiple R-squared:  0.9761,	Adjusted R-squared:  0.9708 
vif(model_16)


#checking VIF and p value and removing
#enginetypeohcf

model_17 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + peakrpm + 
                 enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetyperotor + cylindernumberfour + 
                 fuelsystem2bbl + 
                 fuelsystemmpfi + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                 powertoweightratio, data = train)


summary(model_17)
#Multiple R-squared:  0.9738,	Adjusted R-squared:  0.9682 
vif(model_17)



#checking VIF and p value and removing
#enginetypeohc

model_18 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + peakrpm + 
                 enginetypedohcv + enginetypel + 
                 enginetyperotor + cylindernumberfour + 
                 fuelsystem2bbl + 
                 fuelsystemmpfi + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen + 
                 powertoweightratio, data = train)



summary(model_18)
#Multiple R-squared:  0.9733,	Adjusted R-squared:  0.9678 
vif(model_18)


#checking VIF and p value and removing
#powertoweightratio

model_19 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + peakrpm + 
                 enginetypedohcv + enginetypel + 
                 enginetyperotor + cylindernumberfour + 
                 fuelsystem2bbl + 
                 fuelsystemmpfi + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen, 
                 data = train)

summary(model_19)
#Multiple R-squared:  0.9693,	Adjusted R-squared:  0.9633
vif(model_19)



#checking VIF and p value and removing
#fuelsystemmpfi

model_20 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + peakrpm + 
                 enginetypedohcv + enginetypel + 
                 enginetyperotor + cylindernumberfour + 
                 fuelsystem2bbl + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen, 
                 data = train)

summary(model_20)
#Multiple R-squared:  0.9689,	Adjusted R-squared:  0.9632 
vif(model_20)


#checking VIF and p value and removing
#fuelsystem2bbl

model_21 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + peakrpm + 
                 enginetypedohcv + enginetypel + 
                 enginetyperotor + cylindernumberfour + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen, 
                 data = train)

summary(model_21)
#Multiple R-squared:  0.9678,	Adjusted R-squared:  0.9622
vif(model_21)


#checking VIF and p value and removing
#enginetypel

model_22 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + peakrpm + enginetypedohcv + 
                 enginetyperotor + cylindernumberfour + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen, 
                 data = train)



summary(model_22)
#Multiple R-squared:  0.9657,	Adjusted R-squared:  0.9601
vif(model_22)


#checking VIF and p value and removing
#enginetypedohcv

model_23 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + peakrpm +  
                 enginetyperotor + cylindernumberfour + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen, 
                 data = train)

summary(model_23)
#Multiple R-squared:  0.963,	Adjusted R-squared:  0.9573  
vif(model_23)


#checking VIF and p value and removing
#cylindernumberfour

model_24 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + peakrpm +  
                 enginetyperotor + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen, 
                 data = train)

summary(model_24)
#Multiple R-squared:  0.9592,	Adjusted R-squared:  0.9533
vif(model_24)


#checking VIF and p value and removing
#enginetyperotor

model_25 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + boreratio + peakrpm + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen, 
                 data = train)

summary(model_25)
#Multiple R-squared:  0.9567,	Adjusted R-squared:  0.9508 
vif(model_25)


#checking VIF and p value and removing
#boreratio

model_26 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + peakrpm + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota + CarNamevolkswagen, 
                 data = train)

summary(model_26)
#Multiple R-squared:  0.953,	Adjusted R-squared:  0.9471 
vif(model_26)


#checking VIF and p value and removing
#CarNamevolkswagen

model_27 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + peakrpm + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota, 
                 data = train)

summary(model_27)
#Multiple R-squared:  0.9517,	Adjusted R-squared:  0.946 
vif(model_27)


#checking VIF and p value and removing
#CarNamemazda

model_28 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + peakrpm + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault + CarNametoyota, 
                 data = train)

summary(model_28)
#Multiple R-squared:  0.9507,	Adjusted R-squared:  0.9453
vif(model_28)


#checking VIF and p value and removing
#CarNametoyota

model_29 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + peakrpm + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemitsubishi + CarNamenissan + 
                 CarNameplymouth + CarNamerenault, 
                 data = train)

summary(model_29)
#Multiple R-squared:  0.949,	Adjusted R-squared:  0.9438
vif(model_29)


#checking VIF and p value and removing
#CarNamenissan

model_30 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + peakrpm + CarNamebmw + 
                 CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNamemitsubishi +  
                 CarNameplymouth + CarNamerenault, 
                 data = train)

summary(model_30)
#Multiple R-squared:  0.9469,	Adjusted R-squared:  0.942 
vif(model_30)


#checking VIF and p value and removing
#CarNamehonda

model_31 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + peakrpm + CarNamebmw + 
                 CarNamebuick + CarNamedodge + 
                 CarNamemitsubishi +  
                 CarNameplymouth + CarNamerenault, 
                 data = train)

summary(model_31)
#Multiple R-squared:  0.9447,	Adjusted R-squared:  0.9401
vif(model_31)


#checking VIF and p value and removing
#CarNameplymouth

model_32 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + peakrpm + CarNamebmw + 
                 CarNamebuick + CarNamedodge + 
                 CarNamemitsubishi +  
                 CarNamerenault, 
                 data = train)

summary(model_32)
#Multiple R-squared:  0.9428,	Adjusted R-squared:  0.9385 
vif(model_32)


#checking VIF and p value and removing
#CarNameplymouth

model_33 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + peakrpm + CarNamebmw + 
                 CarNamebuick + 
                 CarNamemitsubishi +  
                 CarNamerenault, 
                 data = train)

summary(model_33)
#Multiple R-squared:  0.941,	Adjusted R-squared:  0.937
vif(model_33)


#checking VIF and p value and removing
#CarNamerenault

model_34 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + peakrpm + CarNamebmw + 
                 CarNamebuick + 
                 CarNamemitsubishi,  
                 data = train)

summary(model_34)

#checking VIF and p value and removing
#CarNamemitsubishi

model_35 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + peakrpm + CarNamebmw + 
                 CarNamebuick,
                data = train)

summary(model_35)

#Multiple R-squared:  0.9335,	Adjusted R-squared:   0.93
vif(model_35)


#checking VIF and p value and removing
#aspiration

model_36 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + peakrpm + CarNamebmw + 
                 CarNamebuick,
                 data = train)

summary(model_36)

#Multiple R-squared:  0.9294,	Adjusted R-squared:  0.9263
vif(model_36)


sink(file = "model36.txt")
cat("====================================================================================\n")
print(model_36)
cat("====================================================================================\n")
sink(NULL)


# predicting the results in test dataset
Predict_1 <- predict(model_36,test[,-1])
test$test_price <- Predict_1
# Now, we need to test the r square between actual and predicted sales. 
# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
# check R-squared
rsquared
#0.8707497


sink(file = "Model36Summary.txt")
cat("====================================================================================\n")
print(model_36)
cat("====================================================================================\n")
cat("# Now, we need to test the r square between actual and predicted sales\n") 
cat("# Accuracy of the predictions\n")
cat("# Calculate correlation\n")
cat("# rsquared value\n")
print(rsquared)
cat("# From rsquared value, its fairly good prediction\n")
cat("====================================================================================\n")
cat("# Takeaways\n")
cat("# Car brand is a deciding factor in the car pricing\n")
cat("  luxury brands like bmw,buick has +ve impact on price\n")
cat("# Car Engine specifications like enginesize, enginelocation, peakrpm\n") 
cat("# Car dimension ie; car width will affect the pricing\n")
sink(NULL)




