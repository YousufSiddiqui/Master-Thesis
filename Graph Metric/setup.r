#Notes: This code reads json file,extracts relevant information and converts it into a dataframe containing all the nodes.
#from this node df, the links df is made. furthermore weights are assigned to each link


getwd()
library(jsonlite)
library(magrittr)
library(brew)
library(data.tree)

dat <- fromJSON("File7076_24.json", simplifyDataFrame = FALSE)

dat <- as.Node(dat)

print(dat, "p", "C")

#convert this to a data.frame for only nodes and the category of each

data_nodes <- dat %>% ToDataFrameTable(NodeId = "p", Category= "C", time = "t")
data_nodes = na.omit(data_nodes)

#making a new dataframe for links by extracting column from node df and renaming column
data_links = as.data.frame(data_nodes$NodeId) 
colnames(data_links)[1]= "NodeId_from"


# Extracting the 2nd - Last value of the column = node_from  dataframe, saving it to a 
# new column in the dataframe, inserting an NA value to the missing value row for the 
# new column

data_links$NodeId_to = data_nodes$NodeId[c(2:length(data_nodes$NodeId),NA)]

# deleting last row as it is not required for graph formation
data_links = data_links[-nrow(data_links),]

# Using weight formula to input all values in a separte column in the data_links df
data_links$Weights = diff(data_nodes$time)/max(diff(data_nodes$time))
 