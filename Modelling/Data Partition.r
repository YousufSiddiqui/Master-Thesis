rm(list=ls())

library(caret)


setwd("C:/Users/Yousuf/OneDrive/Master Thesis/Data_from_network_drive/Model Ready Data")

data_final = read.csv("Feat_Cleaned_Combined.csv")

data_final = data_final[-c(1)]

data_final$Kauf <- as.factor(data_final$Kauf)

set.seed(123)


devIndex = as.data.frame(createFolds(data_final$Kauf, k= 5,  list = T))

data_div_1= data_final[ devIndex$Fold1, c("Kauf","sessionID")]
data_div_2= data_final[ devIndex$Fold2, c("Kauf","sessionID")]
data_div_3= data_final[ devIndex$Fold3, c("Kauf","sessionID")]
data_div_4= data_final[ devIndex$Fold4, c("Kauf","sessionID")]
data_div_5= data_final[ devIndex$Fold5, c("Kauf","sessionID")]


# creation training set, validation set and test set
md_rows = rbind(data_div_1,data_div_2,data_div_3 ) ## Training dataset 60%
val_rows = data_div_4 							  ## Validation dataset 20%
test_rows = data_div_5 						## Test dataset 20%

# merging to create df with all variables for training, validation and test set
data_md = merge( data_final , md_rows,by=c("Kauf","sessionID"))
data_val = merge( data_final , val_rows,by=c("Kauf","sessionID"))
data_test = merge( data_final , test_rows,by=c("Kauf","sessionID"))


##Checking for the target variable distribution in over all dataset
nrow(data_final[data_final$Kauf==0,])/nrow(data_final)*100
#86.49674: Metric , 95.16723: Feature

nrow(data_test[data_test$Kauf==0,])/nrow(data_test)*100
#86.6883: Metric , 95.18433: Feature

data_md = merge( data_final , md_rows,by=c("Kauf","sessionID"))
data_val = merge( data_final , val_rows,by=c("Kauf","sessionID"))
data_test = merge( data_final , test_rows,by=c("Kauf","sessionID"))

data_md$sample="train"
data_val$sample="val"
data_test$sample='test'


full_dv_data= rbind(data_md[,(colnames(data_md))], data_val[,(colnames(data_val))], data_test[,(colnames(data_test))])
class(full_dv_data$sessionID)


full_dv_data$sessionID=as.numeric(full_dv_data$sessionID)
class(full_dv_data$sessionID)
head(full_dv_data)


full_dv_data=full_dv_data[order(full_dv_data[,'sessionID']),]

dim(full_dv_data)
length(unique(full_dv_data$sessionID))


data_final=full_dv_data

###Separating data sets and removing Sample and dv_binary column

##Checking for the target variable distribution in over all dataset
nrow(data_final[data_final$Kauf==0,])/nrow(data_final)*100
#86.49674: Metric, 95.11643: Feature

data_final_train=data_final[data_final$sample=='train',c(1:ncol(data_final)-1)]  
nrow(data_final_train[data_final_train$Kauf==0,])/nrow(data_final_train)*100
#86.40972: Metric, 95.16675: Feature

data_final_val=data_final[data_final$sample=='val',c(1:ncol(data_final)-1)]
nrow(data_final_val[data_final_val$Kauf==0,])/nrow(data_final_val)*100
#86.56569: Metric, 95.11643: Feature 

data_final_test=data_final[data_final$sample=='test',c(1:ncol(data_final)-1)]
nrow(data_final_test[data_final_test$Kauf==0,])/nrow(data_final_test)*100
#86.68883: Metric, 95.16675: Feature

setwd("C:/Users/Yousuf/Desktop")

### taking output in CSV format
write.csv(data_final_train,file = "data_feat_train.csv",row.names=F)
write.csv(data_final_val,file ="data_feat_val.csv",row.names=F)
write.csv(data_final_test,file ="data_feat_test.csv",row.names=F)



