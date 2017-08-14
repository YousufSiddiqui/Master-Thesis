### Data Preparation ####

setwd("C:/Master Thesis")
library(dummies)

## Loading Data ##


data = read.csv("final_data_tab_1.csv", as.is=T,header=T,sep=",")

## Categorical Data ##

names(data)
str(data)

# Operating System and Browser - Categorical Variables

cat_variables = c("browser", "operatingSystem")

data_cat = data[cat_variables]


###Dummy Encoding

data_cat <- dummy.data.frame(data_cat, sep = ".")

write.csv(data_cat, file = "data_cat_tab_1.csv")



