#To see how many unique nodes are present compared to the total no.of rows
nrow(data_nodes); length(unique(data_nodes$NodeId))

#To see how many unique links are present compared to the total no.of rows
nrow(data_links); length(unique(data_links[,c("NodeId_from","NodeId_to")]))

# Removing duplicate nodes
data_nodes = data_nodes[!duplicated(data_nodes[,c('NodeId')]),]

data_links$Number= 1:nrow(data_links)
data_nodes$Number= 1:nrow(data_nodes)


library(igraph)
net = graph_from_data_frame(d=data_links, vertices = data_nodes, directed = T)

## Defining Meta-Parameters:

# Setting link weights

E(net)$weight = data_links$Weights*5 # multiplying by 10 to make it more visual

#Vertex Color acc to Category of Page

V(net)$color=V(net)$Category #assign the "Category" attribute as the vertex color
V(net)$color=gsub("other","yellow",V(net)$color) #Sale will be Yellow
V(net)$color=gsub("overview","blue",V(net)$color) #Overview will be blue
V(net)$color=gsub("product","red",V(net)$color) #Product will be Red
V(net)$color=gsub("cart","green",V(net)$color) #Cart will be Green
V(net)$color=gsub("home","grey",V(net)$color) #home will be grey
V(net)$color=gsub("checkout","pink",V(net)$color)

#Vertex size acc to degree

V(net)$size=degree(net)*10 #multiplying by 10, to make it more visual

#Add Legend to Graph Plot


plot.igraph(net,
            edge.arrow.size= 0.5,
            edge.color = "black",
            edge.width = E(net)$weight,
            vertex.label=data_nodes$Number,
            vertex.shapes="square")