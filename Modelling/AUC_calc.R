
setwd("C:/Users/Yousuf/OneDrive/Master Thesis/Data_from_network_drive/Model Ready Data/Metric Split Data/Result")

lr_val = read.csv("lr_val.csv", as.is = T)


## Renaming column names and taking average 

colnames(lr_val)=  gsub("[.]","x",colnames(lr_val))

for (model in 1:9){
  lr_val[,paste0("averaged_",model)] = rowMeans(lr_val[,grep(paste0("x",model),colnames(lr_val))])
  lr_val[,grep(paste0("x",model),colnames(lr_val))] = NULL
}

target = read.csv("data_metric_val.csv",as.is=T)


lr_val$target= target$Kauf


library(verification)

###Model performance parameters

## AUC
auc = function (obs, pred)
{
  out = (roc.area(as.numeric(obs), as.numeric(pred))$A)
  
  out
}

auc_values=c()
for (model in 1:9){
  auc_val = auc(lr_val$target,lr_val[,model])
  auc_values = c(auc_val,auc_values)
}

optimal_val = max(auc_values)
optimal_model = which(auc_values==optimal_val)
lambda = lr_parameters[optimal_model,1]
cp = lr_parameters[optimal_model,2]
print(paste0("Optimal Parametric combination : lambda = ",lambda," and cp = ",cp," with AUC :",optimal_val))
