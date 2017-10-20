#### Merging With KAUF DATA #####


setwd("C:/Users/Yousuf/OneDrive/Master Thesis/Data_from_network_drive/Metric_Kauf_Data")

metric_data = read.csv("Graph_Metric_Feature_File.csv")
metric_kauf_data = read.csv("Metric_Kauf_Data.csv")

metric_data_final = merge(metric_data,metric_kauf_data, by = "sessionID")

metric_data_final_2 = metric_data_final[ , !(colnames(metric_data_final) %in% c("X.1", "X.x","X.y","clientID"))] 

write.csv(metric_data_final_2, file = "Graph_Metric_Feat_File_With_Kauf_Read_for_Cleaning.csv")


##### Cleaning Graph Metric File #####

library(dummies)
library(caret)
library(dplyr)

# Step 1: Distinguishing categorical and continous variables

str(metric_data_final_2)

### Removing seesionID and Kauf variables to enable outlier detection and correlation calculation

cat_variables = names(metric_data_final_2) %in% c("sessionID","Kauf")

# Data set for continous variables

merged_data_cont = metric_data_final_2[!cat_variables]

# Step : Detecting Outliers

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

merged_data_cont= sapply(merged_data_cont,function(x)remove_outliers(x))

##Checking number of NAs
table(is.na(merged_data_cont))

# Step : Imputing Variables with MEAN

for(i in 1:ncol(merged_data_cont)){
  merged_data_cont[is.na(merged_data_cont[,i]), i] = mean(merged_data_cont[,i], na.rm = TRUE)
}

#Checking if any NAs left
table(is.na(merged_data_cont))

# turning back to a df

merged_data_cont = as.data.frame(merged_data_cont)

# Step : Removing all zero-variance variables - non present

zv <- apply(merged_data_cont, 2, function(x) length(unique(x)) == 1)
sum(zv)
# [1] 0

# Step: Remove Highly Correlated Variables - Make Graphs for Thesis

### using Pearson as I am interested in linear correlations

corr_matrix = cor(merged_data_cont, method = "pearson", use = "complete.obs")


## rounding off correlation to 2 decimal places

corr_matrix = round(corr_matrix, 2)

# findCorrelation: This function searches through a correlation matrix and returns a vector of integers 
#  corresponding to columns to remove to reduce pair-wise correlations.

# Returns a list of variables to be removed (include in written thesis)

list_high_corr_var = findCorrelation(corr_matrix, cutoff = .90, verbose = TRUE)

# List returned no variables with correlation greater than .90

merged_data = cbind(merged_data_cont,metric_data_final_2$sessionID)
merged_data = cbind(merged_data,metric_data_final_2$Kauf)

# Changing variable names back to original ones:

colnames(merged_data)[which(names(merged_data) == "metric_data_final_2$sessionID")] = "sessionID"
colnames(merged_data)[which(names(merged_data) == "metric_data_final_2$Kauf")] = "Kauf"

write.csv(merged_data, file = "Metric_Cleaned_Combined.csv")

