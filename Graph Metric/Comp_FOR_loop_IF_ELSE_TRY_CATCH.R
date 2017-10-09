##Libraries Used##

library(jsonlite)
library(magrittr)
library(brew)
library(data.tree)
library(igraph)

files_dir = ("C:/Split_Data")
##loading the names of file in the directory
setwd(files_dir)
myFiles = list.files(pattern="*json")
myFiles


var.data = NULL

#FOR loop used to iterate over all files in a particular folder
# tryCatch is used to continue the script, in case there are errors in particular files which may not be resolved

for (file in myFiles){
  tryCatch({
    print(file)
  
  
  
  
  dat <- fromJSON(file, simplifyDataFrame = FALSE)
  
  # Extracting SessionID and ClientID from json file
  
  dat2 = fromJSON(file, simplifyDataFrame = TRUE)
  
  sessionID = dat2$S[1]
  clientID = dat2$I[1]

  dat <- as.Node(dat)
  
  #print(dat, "p", "C","t")
  
  #convert this to a data.frame for only nodes and the category of each
  
  data_nodes <- dat %>% ToDataFrameTable(NodeId = "p", Category= "C",time = "t")
  data_nodes = na.omit(data_nodes)
  
  ##if statement used to exclude files with 4 or less nodes
  
  if(nrow(data_nodes) > 4){
    #making a new dataframe for links by extracting column from node df and renaming column
    data_links = as.data.frame(data_nodes$NodeId) 
    colnames(data_links)[1]= "NodeId_from"
    
    
    # Extracting the 2nd - Last value of the column = node_from  dataframe, saving it to a 
    # new column in the dataframe, inserting an NA value to the missing value row for the 
    # new column
    
    data_links$NodeId_to = data_nodes$NodeId[c(2:length(data_nodes$NodeId),NA)]
    
    # deleting last row as it is not required for graph formation
    data_links = data_links[-nrow(data_links),]
    
    #Formulae to caluclate weights: diff(data_nodes$time)/max(diff(data_nodes$time))
    
    # Using weight formula to imput all values in a separte column in the data_links df
    data_links$Weights = diff(data_nodes$time)/max(diff(data_nodes$time))
    
    
    
    
    #To see how many unique nodes are present compared to the total no.of rows
    nrow(data_nodes); length(unique(data_nodes$NodeId))
    
    #To see how many unique links are present compared to the total no.of rows
    nrow(data_links); length(unique(data_links[,c("NodeId_from","NodeId_to")]))
    
    # Removing duplicate nodes
    data_nodes = data_nodes[!duplicated(data_nodes[,c('NodeId')]),]
    
    data_links$Number= 1:nrow(data_links)
    data_nodes$Number= 1:nrow(data_nodes) #}
    
    
    net = graph_from_data_frame(d=data_links, vertices = data_nodes, directed = T)
    
    #plot(net,edge.arrow.size= 0.2,vertex.label=data_nodes$Number,vertex.shape="square")
    
    #Weighted Degree - aggregates link weights with direction "TO" the nodes. writes node 
    # weight vector in a new cloumn to the data_node df
    
    data_nodes$NodeWeight =strength(net, vids = V(net), mode = "in",
                                    loops = TRUE, weights = data_links$Weights)
    
    
    
    #Graph Metrics:
    
   #Graph Metrics:

	# Number of Edges: ecount
	EdgeNumbers=gsize(net)

	# Number of Nodes: gorder
	NodeNumbers=gorder(net)

	# Average eccentricity: eccentricity
	AvgEccentricty= mean(eccentricity(net, vids = V(net), mode = "out"))

	# Max eccentricity
	MaxEccentricty= max(eccentricity(net, vids = V(net), mode = "out"))

	# Min eccentricity
	MinEccentricty= min(eccentricity(net, vids = V(net), mode = "out"))

	# Diameter: diameter (weighted)
	Diameter= diameter(net, directed = TRUE, unconnected = TRUE, weights = data_links$Weights)

	# Radius: radius
	Radius = radius(net, mode = "out")

	#Average In-Degree: degree - checkout the normalized parameter
	AvgInDegree= mean(degree(net, v = V(net), mode ="in",loops = F, normalized = T)) 

	#Max In-Degree: degree - checkout the normalized parameter
	MaxInDegree= max(degree(net, v = V(net), mode ="in",loops = F, normalized = T)) 

	#Min In-Degree: degree - checkout the normalized parameter
	MinInDegree= min(degree(net, v = V(net), mode ="in",loops = F, normalized = T)) 

	#Average Out-Degree: degree - checkout the normalized parameter
	AvgOutDegree= mean(degree(net, v = V(net), mode ="out",loops = F, normalized = T)) 

	#Max Out-Degree: degree - checkout the normalized parameter
	MaxOutDegree= max(degree(net, v = V(net), mode ="out",loops = F, normalized = T)) 

	#Min Out-Degree: degree - checkout the normalized parameter
	MinOutDegree= min(degree(net, v = V(net), mode ="out",loops = F, normalized = T)) 

	#Average Shortest Path Length:distance
	AvgShortPathLength= mean_distance(net, directed = TRUE, unconnected = TRUE)

	#Average Closeness Centrality: closeness (weighted)
	AvgClosenessCentrality =mean(closeness(net, vids = V(net), mode = "out",weights = data_links$Weights, normalized = T))

	#Max Closeness Centrality: closeness
	MaxClosenessCentrality =max(closeness(net, vids = V(net), mode = "out",weights = data_links$Weights, normalized = T))

	#Min Closeness Centrality: closeness
	MinClosenessCentrality =min(closeness(net, vids = V(net), mode = "out",weights = data_links$Weights, normalized = T))


	#Average Clustering Coefficent: Transitivity of a graph(igraph)
	Transitivity = transitivity(net, type = "global", vids = NULL,weights = data_links$Weights, isolates = c("NaN", "zero"))

	#Density: edge_density
	EdgeDensity = edge_density(net, loops = TRUE)

	#Average Node Betweenness Centrality
	AvgNodeCentrality = mean(edge_betweenness(net, e = E(net), directed = TRUE, weights = data_links$Weights))

	#Max Node Betweenness Centrality
	MaxNodeCentrality = max(edge_betweenness(net, e = E(net), directed = TRUE, weights = data_links$Weights))

	#Min Node Betweenness Centrality
	MinNodeCentrality = min(edge_betweenness(net, e = E(net), directed = TRUE, weights = data_links$Weights))

	#Average Edge Betweenness Centrality
	AvgEdgeCentrality = mean(betweenness(net, v = V(net), directed = TRUE, weights = data_links$Weights,
				nobigint = TRUE, normalized = T))

	#Max Edge Betweenness Centrality
	MaxEdgeCentrality = max(betweenness(net, v = V(net), directed = TRUE, weights = data_links$Weights,
										 nobigint = TRUE, normalized = T))

	#Min Edge Betweenness Centrality
	MinEdgeCentrality = min(betweenness(net, v = V(net), directed = TRUE, weights = data_links$Weights,
										 nobigint = TRUE, normalized = T))


	#Eigenvalue Centrality: eigen_centrality 
	EigenValueCentrality = eigen_centrality(net, directed = TRUE, scale = TRUE, weights = data_links$Weights,
					 options = arpack_defaults)

	MaxEigenValueCentrality = max(EigenValueCentrality$vector)
	MinEigenValueCentrality = min(EigenValueCentrality$vector)
	AvgEigenValueCentrality = mean(EigenValueCentrality$vector)


	#Page Rank : page_rank
	PageRank = page_rank(net, algo = "prpack", vids = V(net),directed = TRUE, damping = 0.85, personalized = NULL, 
			  weights = data_links$Weights,
			  options = NULL)

	MaxPageRank = max(PageRank$vector)
	MinPageRank = min(PageRank$vector)
	AvgPageRank = mean(PageRank$vector)

	# Closeness Vitality: from library = centiserve

	#ClosenessVitality = closeness.vitality(net, vids = V(net), mode = "out",weights = NULL)
	#Graphs are not strongly connected, cant have a value

	#library(CINNA)
	##calculate_centralities(net, include = "Closeness Vitality")

	#library(centiserve)
	#closeness.vitality(net, vids = V(net), mode = "out",weights = NULL)

    # Creating a df where all the graph metrics are used as variables
    
    var.data = rbind(var.data, data.frame(sessionID ,
                                          clientID ,
                                          EdgeNumbers ,
                                          NodeNumbers ,
                                          AvgEccentricty ,
                                          MaxEccentricty,
										  MinEccentricty,
										  Diameter,
                                          Radius,
                                          AvgInDegree,
										  MaxInDegree,
										  MinInDegree,
                                          AvgOutDegree,
										  MaxOutDegree,
										  MinOutDegree,
										  AvgShortPathLength,
                                          AvgClosenessCentrality,
										  MaxClosenessCentrality,
										  MinClosenessCentrality,
                                          Transitivity ,
                                          EdgeDensity,
                                          AvgNodeCentrality,
										  MaxNodeCentrality,
										  MinNodeCentrality,
                                          AvgEdgeCentrality,
										  MaxEdgeCentrality,
										  MinEdgeCentrality,
										  AvgEigenValueCentrality,
										  MaxEigenValueCentrality,
										  MinEigenValueCentrality,
										  AvgPageRank,
										  MaxPageRank,
										  MinPageRank
    ))  
    
    
  } else { next
    
  }
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})  
  
}                   

write.csv(var.data,file ="Graph_Metric_Feature_File.csv")
