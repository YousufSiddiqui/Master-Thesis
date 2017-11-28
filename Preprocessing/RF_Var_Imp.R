library(randomForest)
library(caret)
library(caTools)

setwd("C:/Users/Yousuf/OneDrive/Master Thesis/Data_from_network_drive")

data_final = read.csv("Feat_Cleaned_Combined.csv")

data_final = data_final[-c(1)]

data_final$Kauf <- as.factor(data_final_train$Kauf)

set.seed(3456)
data_final = data_final[1:158120,]

devIndex = as.data.frame(createFolds(data_final$Kauf, k=5,  list = TRUE))

data_div_1= data_final[ devIndex$Fold1, c("Kauf","sessionID")]
data_div_2= data_final[ devIndex$Fold2, c("Kauf","sessionID")]
data_div_3= data_final[ devIndex$Fold3, c("Kauf","sessionID")]
data_div_4= data_final[ devIndex$Fold4, c("Kauf","sessionID")]
data_div_5= data_final[ devIndex$Fold5, c("Kauf","sessionID")]


# creation training set, validation set and test set
md_rows = rbind(data_div_1,data_div_2,data_div_3 ) ## Training dataset 60%
val_rows = data_div_4 							  ## Validation dataset 20%
test_rows = data_div_5 						

# merging to create df with all variables for training, validation and test set
data_md = merge( data_final , md_rows,by=c("Kauf","sessionID"))
data_val = merge( data_final , val_rows,by=c("Kauf","sessionID"))
data_test = merge( data_final , test_rows,by=c("Kauf","sessionID"))


##Checking target variable distribution in dataset
nrow(data_final[data_final$Kauf==0,])/nrow(data_final)*100


nrow(data_test[data_test$Kauf==0,])/nrow(data_test)*100

data_test = data_test[-c(2)]

Kauf.rf <- randomForest(Kauf ~ ., data = data_test, ntree = 2000, mtry = 11,
                        importance = TRUE, replace = FALSE, na.action = na.omit)

save(Kauf.rf,file = "Kaufrf2.RData")
varImpPlot(Kauf.rf, sort = TRUE)



