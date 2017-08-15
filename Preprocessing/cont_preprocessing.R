### Data Preparation ###

### Data Preparation ####

setwd("C:/Master Thesis")
library(dummies)
library(caret)
library(dplyr)

## Loading Data ##


data = read.csv("final_data_tab_1.csv", as.is=T,header=T,sep=",")

### Removing target variable as it turns to 0 when doing outlier imputation

data_cont_Kauf = as.data.frame(data_cont$Kauf)

## Continous Data ##

# removing varaibles which are not quantitative and making a new data frame

remove_variables = names(data) %in% c("sessionID", "X", "cluster_num", "browser", "operatingSystem","X.1", "Kauf")

# Data set for continous variables

data_cont = data[!remove_variables]



# Replacing -1 with NA
str(data_cont)

# variables: meanRecencyVisit recencyVisit purchaseRecency currentViewCountVsLV currentVisittimeVsLV

data_cont[ data_cont == -1 ] <- NA

#### Handling Outliers 

# Outlier Function

remove_outliers = function(x, na.rm = TRUE, ...) 
{
  VarMean = mean(x, na.rm = na.rm, ...)
  VarSD = sd(x,na.rm = na.rm, ...)
  y = x
  y[abs((x - VarMean)/VarSD) >3] = NA
  y
}

# Applying above function

data_cont= sapply(data_cont,function(x)remove_outliers(x))


##Checking number of NAs
table(is.na(data_cont))


#Imputing NA with mean of the Variables

for(i in 1:ncol(data_cont)){
  data_cont[is.na(data_cont[,i]), i] <- mean(data_cont[,i], na.rm = TRUE)
}

#Checking if any NAs left
table(is.na(data_cont))

# turning back to a df

data_cont = as.data.frame(data_cont)

## merging target variable back into df

data_cont = cbind(data_cont,data_cont_Kauf)


###Scaling of Continuous variables

##MinMaxScaling (-1 to 1)

#min_max_scaling=function(col)((col-min(col))/(max(col)-min(col))*2-1)

##check
#min_max_scaling(c(1,2,3,4))
# [1] -1.0000000 -0.3333333  0.3333333  1.0000000

	
#data_cont = sapply(data_cont,function(col)min_max_scaling(col))

##saving
write.csv(data_cont,"data_cont_tab_1.csv",row.names=F)


