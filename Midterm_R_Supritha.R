## MIDTERM- Data Analytics, Winter 2023 ##
##Submitted By- Supritha S Rao

#Set working directory
setwd("C:/Users/supri/OneDrive/Desktop/Data Analytics Class")

#Loadig Libraries
library(Hmisc)
library(pastecs)
library(psych)
library(readxl)
library(pwr)

#Load & View data
historical <- read.csv("C:/Users/supri/Downloads/historical.csv")
View(historical)
future <- read.csv("C:/Users/supri/Downloads/future.csv")
View(future)

summary(historical)
summary(future)
#Q1: Pairwise comparisons of historical climatic variables and gross domestic product;
#Solution 1:

## Solved using TABLEAU ##

#Q2: Geographic representations of historical changes in gross domestic product, temperature, and precipitation over time;;
#Solution 2:

## Solved using TABLEAU ##

#Q3: Estimation of the historical effect of climatic variables on gross domestic product; 
#Solution 3:
#creating a data-frame consisting of  climatic variables
# Climatic variables include temp(temperature) and prec (annual mean precipitation in mm)
ClimaticVariablesDF <- data.frame( GDP = historical$gdp, Temperature = historical$temp, Precipitation = historical$prec, Country= historical$country)
Effect <-lm(GDP~Temperature + Precipitation + Country ,data=ClimaticVariablesDF)
summary(Effect)
index <- which(future$country == "United.States")

future[index, "country"] <-"United States"

remove <- which(future$country == "Iran")

future <- future[-remove,]
#Q4: Prediction of the future impact of climate change on gross domestic product, and 
#Solution 4:
#Fetching only Scenario1 set
Scenario1 =subset(future, scenario %in% c("SSP5-8.5"),)
#Fetching only Scenario2 set
Scenario2 =subset(future, scenario %in% c("SSP1-2.6"),)

##For Scenario1
#creating a data-frame consisting of only the climatic variables

FutureClimaticVariablesDF <- data.frame( Temperature = Scenario1$temp, Precipitation = Scenario1$prec, Country = Scenario1$country)


#Predict for S1
PredictS1 <- predict(Effect, newdata=FutureClimaticVariablesDF, type="response")
PredictS1DF <- data.frame(Predicted_GDP_S1 = PredictS1)
#New data-frame with all the predictions for Scenario1
S1DataFrame = data.frame(Scenario1_Temperature = Scenario1$temp, Scenario1_Precipitation=Scenario1$prec,Scenario1_Country=Scenario1$country, PredictedGDP=PredictS1DF$Predicted_GDP_S1 )
#Plotting for temperature-for Scenario1
#ggplot(S1DataFrame, aes(x = S1DataFrame$Scenario1.temp, y = S1DataFrame$PredictedGDP)) + geom_smooth(model=lm, color="purple")+geom_point()
ggplot(S1DataFrame, aes(x = Scenario1_Temperature, y = PredictedGDP, col=Scenario1_Country)) +geom_point()+ggtitle("Scenario 1: GDP V/S TEMPERATURE") 
#Plotting for precipitation-for Scenario1
#ggplot(S1DataFrame, aes(x = S1DataFrame$Scenario1.prec, y = S1DataFrame$PredictedGDP)) + geom_smooth(model=lm, color="purple")+geom_point()
ggplot(S1DataFrame, aes(x = Scenario1_Precipitation, y = PredictedGDP, col=Scenario1_Country)) + geom_point()+ggtitle("Scenario 1: GDP V/S PRECIPITATION") 


##For Scenario2
#creating a data-frame consisting of only the climatic variables
FutureClimaticVariablesDF2 <- data.frame( Temperature = Scenario2$temp, Precipitation = Scenario2$prec, Country = Scenario2$country)
#Predict for S2
PredictS2 <- predict(Effect, newdata=FutureClimaticVariablesDF2, type="response")
PredictS2DF <- data.frame(Predicted_GDP_S2 = PredictS2)
#New data-frame with all the predictions for Scenario1
S2DataFrame = data.frame(Scenario2_Temperature = Scenario2$temp, Scenario2_Precipitation=Scenario2$prec, PredictedGDP_2=PredictS2DF$Predicted_GDP_S2, Scenario2_Country=Scenario2$country )
#Plotting for temperature-for Scenario2
ggplot(S2DataFrame, aes(x = Scenario2_Temperature, y = PredictedGDP_2, col=Scenario2_Country)) +ggtitle("Scenario 2: GDP V/S TEMPERATURE")  +geom_point()
#Plotting for precipitation-for Scenario2
ggplot(S2DataFrame, aes(x = Scenario2_Precipitation, y = PredictedGDP_2, col=Scenario2_Country)) +geom_point()+ggtitle("Scenario 2: GDP V/S PRECIPITATION") 



#Q5: Suggestions  on  additional  data  or  analyses  that  may  be  useful  for  evaluating  the  impact  of  climate change on gross domestic product
#Solution 5:
#In addition to the analysis that we conducted, there can be usage of further different variables
#and data to analyse the economic growth over the coming years. We can take into account of 
#various production through agriculture as someway or the other agriculture highly is dependent 
#on the climatic conditions, only on favourable climatic conditions we can have the best of yield. 
#We can consider further breakdown of economic data by further division of rural and urban area income 
#patterns, weather conditions in extreme climatic places and jobs that further gets effected due 
#to these factors too. For this purpose, a deep dive into finding the right set of data and huge 
#collection of sample data will be required to arrive at a very precise model that can be in turn
#be used to predict the future changes and lower the loses and declines in the GDP and economics
