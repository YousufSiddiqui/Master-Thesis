library(randomForest)
library(caret)

setwd("C:/Master Thesis/Preprocessed_Data/Folds")

data_final_train = read.csv("preproc_data_train.csv")


data_final_train$Kauf <- as.factor(data_final_train$Kauf)

Kauf.rf <- randomForest(Kauf ~ ., data = data_final_train, ntree = 2000, mtry = 11,
                         importance = TRUE, replace = FALSE, na.action = na.omit)

save(Kauf.rf,file = "Kaufrf2.RData")
varImpPlot(Kauf.rf, sort = TRUE)

# write the variable importance to a file that can be read into excel
fileOut <- file("rrf2.txt", "w")
imp <- importance(Kauf.rf, type = 1, csle = TRUE);
write.table(imp, fileOut, sep="\t", dec=",")
flush(fileOut)
close(fileOut)

# rm(churn.rf)


