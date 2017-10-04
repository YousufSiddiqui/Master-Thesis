#Graph Metrics:

library(CINA)

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
