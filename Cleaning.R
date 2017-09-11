### CLEANING!!!! ###

library(dummies)
library(caret)
library(dplyr)

# Step 1: Distinguishing categorical and continous variables

str(feat_all_merged)

###### Brower and Operating System are Categorical Variables/Rest are continous

# Step 2: Dummy Encoding Them

cat_variables = c("browser", "operatingSystem")

merged_data_cat = feat_all_merged[cat_variables]

merged_data_cat = dummy.data.frame(merged_data_cat, sep = ".")

# Step 3: Replacing Variables which have -1 as value with NA

cont_variables = names(feat_all_merged) %in% c("sessionID", 
                                                 "browser", 
                                                 "operatingSystem",
                                                 "Kauf")

# Also removed KAUF and SessionID

# Data set for continous variables

merged_data_cont = feat_all_merged[!cont_variables]



## Replacing -1 with NA

merged_data_cont[ merged_data_cont == -1 ] <- NA

# Step 4: Detecting Outliers

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

# Step 5: Imputing Variables with MEAN

for(i in 1:ncol(merged_data_cont)){
  merged_data_cont[is.na(merged_data_cont[,i]), i] = mean(merged_data_cont[,i], na.rm = TRUE)
}

#Checking if any NAs left
table(is.na(merged_data_cont))

# turning back to a df

merged_data_cont = as.data.frame(merged_data_cont)

# Step 6: Removing all zero-variance variables

zv <- apply(merged_data_cont, 2, function(x) length(unique(x)) == 1)
sum(zv)
# [1] 12

merged_data_cont = merged_data_cont[, !zv]

###  Variables found with Zero Variance for sample set = 12: important to remove 
###  these before calculating correlation matrix

write.csv(merged_data_cont, file = "cont_variables_for_descriptive_analysis.csv")

# Step 7: Remove Highly Correlated Variables - Make Graphs for Thesis

### using Pearson as I am interested in linear correlations

corr_matrix = cor(merged_data_cont, method = "pearson", use = "complete.obs")

## rounding off correlation to 2 decimal places

corr_matrix = round(corr_matrix, 2)

# findCorrelation: This function searches through a correlation matrix and returns a vector of integers 
#  corresponding to columns to remove to reduce pair-wise correlations.

# Returns a list of variables to be removed (include in written thesis)

list_high_corr_var = findCorrelation(corr_matrix, cutoff = .90, verbose = TRUE)

# Removing Variables in: list_high_corr_var

merged_data_cont = merged_data_cont[,-c(42,43,33,32,25)]

# Step 8: Merging Categorical DF, Continous DF and KAUF and Session ID Variables to give a final dataset

merged_data_cont = cbind(merged_data_cont,feat_all_merged$sessionID)
merged_data_cont = cbind(merged_data_cont,feat_all_merged$Kauf)

merged_data_cat = cbind(merged_data_cat, feat_all_merged$sessionID)

# changing variables names back to original ones
colnames(merged_data_cat)[which(names(merged_data_cat) == "feat_all_merged$sessionID")] = "sessionID"

colnames(merged_data_cont)[which(names(merged_data_cont) == "feat_all_merged$sessionID")] = "sessionID"
colnames(merged_data_cont)[which(names(merged_data_cont) == "feat_all_merged$Kauf")] = "Kauf"

# merging the two DFs
Feat_Cleaned_Comb = merge(merged_data_cat,merged_data_cont, by = "sessionID")

write.csv(Feat_Cleaned_Comb, file = "Feat_Cleaned_Combined.csv")
