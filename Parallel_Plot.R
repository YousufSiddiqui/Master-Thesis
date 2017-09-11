setwd("C:/Master Thesis/Final")

df = read.csv("Merged_Feat_For_Correlation_Analysis.csv", sep = ",",header = TRUE)

library(GGally)8hjust

parallel_plot = ggparcoord(df, columns = c(19,18,17,21,20,39), scale = 'uniminmax', 
           title = 'Parallel Coordinate Plot:Clickstream Data')


                           
                          
                                                                                               