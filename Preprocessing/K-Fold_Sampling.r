setwd("C:/Master Thesis")

library(caret)

data = read.csv("final_data_tab_1.csv", as.is=T,header=T,sep=",")

remove_variables = names(data) %in% c("X","X.1")

data = data[!remove_variables]

##Combining all datasets 

data_cat = read.csv("data_cat_tab_1.csv",as.is=T)

data_cat$X = NULL

data_cont = read.csv("data_cont_tab_1.csv",as.is=T)

data_int=cbind(data_cont,data_cat)

## Removing Variables which have zero variance

var0 <- unlist(lapply(data_int, function(x) 0 == var(if (is.factor(x)) as.integer(x) else x)))

list(var0)

remove_variables = names(data_int) %in% c("device_comp",
                                      "device_tab",
                                      "exitEqualLanding", 
                                      "operatingSystemVersion",
                                      "visitorKnown",
                                      "tabVisible",
                                      "productBasketLV", 
                                      "purchaseLV")

data_int = data_int[!remove_variables]

## Removing variables with zero variance

cor(data_int)

tmp <- cor(data_int)
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0


data_int <- data_int[,!apply(tmp,2,function(x) any(x > 0.99))]
head(data_int)


data_final=cbind(data[,c("sessionID")],data_int)


colnames(data_final)[1] <- "sessionID"
colnames(data_final)[41] <- "Kauf"

data_final = data_final[3:10947,]

table(is.na(data_final))


## Sampling into training, validation and test set

set.seed(3456)

##dividing rows in 5 folds with control over target variable 

devIndex = as.data.frame(createFolds(data_final$Kauf, k=5,  list = TRUE))

data_div_1= data_final[ devIndex$Fold1, c("Kauf","sessionID")]
data_div_2= data_final[ devIndex$Fold2, c("Kauf","sessionID")]
data_div_3= data_final[ devIndex$Fold3, c("Kauf","sessionID")]
data_div_4= data_final[ devIndex$Fold4, c("Kauf","sessionID")]
data_div_5= data_final[ devIndex$Fold5, c("Kauf","sessionID")]

# creation training set, validation set and test set
md_rows = rbind(data_div_1,data_div_2,data_div_3) ## Training dataset 60%
val_rows = data_div_4 							  ## Validation dataset 20%
test_rows = data_div_5 							  ## Test dataset 20%

# merging to create df with all variables for training, validation and test set
data_md = merge( data_final , md_rows,by=c("Kauf","sessionID"))
data_val = merge( data_final , val_rows,by=c("Kauf","sessionID"))
data_test = merge( data_final , test_rows,by=c("Kauf","sessionID"))

# Creating a new column to specify which sample it is
data_md$sample="train"
data_val$sample="val"
data_test$sample='test'

## Combining all three data sets, dv means dependent variable
full_dv_data= rbind(data_md[,(colnames(data_md))], data_val[,(colnames(data_val))], data_test[,(colnames(data_test))])
class(full_dv_data$sessionID)


full_dv_data$sessionID=as.numeric(full_dv_data$sessionID)
class(full_dv_data$sessionID)
head(full_dv_data)

## ask shikar why he did this
full_dv_data=full_dv_data[order(full_dv_data[,'sessionID']),]

dim(full_dv_data)
length(unique(full_dv_data$sessionID))

# writing df as csv - so that it may be used later
write.csv(full_dv_data,"preproc_data_dv.csv",row.names=F)
data_final=full_dv_data


###Separating data sets and removing Sample and dv_binary column

##Checking for the target variable distribution in over all dataset
nrow(data_final[data_final$Kauf==0,])/nrow(data_final)*100
#93.20238

data_final_train=data_final[data_final$sample=='train',c(1:ncol(data_final)-1)]  
nrow(data_final_train[data_final_train$Kauf==0,])/nrow(data_final_train)*100
#92.93437

data_final_val=data_final[data_final$sample=='val',c(1:ncol(data_final)-1)]
nrow(data_final_val[data_final_val$Kauf==0,])/nrow(data_final_val)*100
#93.2846

data_final_test=data_final[data_final$sample=='test',c(1:ncol(data_final)-1)]
nrow(data_final_test[data_final_test$Kauf==0,])/nrow(data_final_test)*100
#93.92417

setwd("C:/Master Thesis/Preprocessed_Data")

### taking output in CSV format
write.csv(data_final_train,"preproc_data_train.csv",row.names=F)
write.csv(data_final_val,"preproc_data_val.csv",row.names=F)
write.csv(data_final_test,"preproc_data_test.csv",row.names=F)
