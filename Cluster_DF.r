### Clustering Code ###

setwd("C:/Master Thesis")



### Libraries ###

library(ClusterR) # for differnt clustering algorithum
library(dplyr) # for data cleaning
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

#Reading CSV already created in previous session 

feat_data3 = read.csv("DF_tablet_1.csv")

#feat_data3 = feat_data3[1:2000,] # c(-30) is to remove the clientId from the data set

clustering_variables =  names(feat_data3) %in% c("sessionID",
                                            "percPageCart",
                                            "percPageOverview",
                                            "percPageProduct",
                                            "percPageSale",
                                            "percPageSearch",
                                            "sessionCart",
                                            "sessionOverview",
                                            "sessionProduct",
                                            "sessionSale",
                                            "sessionSearch",
                                            "clickEventCart",
                                            "clickEventOverview",
                                            "clickEventProduct",
                                            "clickEventSale",
                                            "clickEventSearch",
                                            "scrollEventCart",
                                            "scrollEventOverview",
                                            "scrollEventProduct",
                                            "scrollEventSale",
                                            "scrollEventSearch",
                                            "tabSwitchCart",
                                            "tabSwitchOverview",
                                            "tabSwitchProduct",
                                            "tabSwitchSale",
                                            "tabSwitchSearch",
                                            "timeSpentOnCart",
                                            "timeSpentOnProduct",
                                            "totPercPageCart",
                                            "totPercPageOverview",
                                            "totPercPageProduct",
                                            "totPercPageSale",
                                            "totPercPageSearch",
                                            "pageCartLV",
                                            "pageCartrVisit",
                                            "pageOverviewLV",
                                            "pageOverviewrVisit",
                                            "pageProductLV",
                                            "pageProductrVisit",
                                            "pageSaleLV",
                                            "pageSalerVisit",
                                            "pageSearchLV",
                                            "pageSearchrVisit",
                                            "totCart",
                                            "totOverview",
                                            "totProduct",
                                            "totSale"
) 




feat_data3 <- feat_data3[clustering_variables]


set.seed(1680) # for reproducibility

#glimpse(feat_data3)

gower_dist <- daisy(feat_data3[,-1],
                    metric = "gower",
                    type = list(logratio = 3))

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

# Output most similar pair

#feat_data3[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
 #       arr.ind = TRUE)[1, ], ]

# Output most dissimilar pair

feat_data3[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

k_plot = plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)


# k=4 from graph

start.time <- Sys.time()

pam_fit <- pam(gower_dist, diss = TRUE, k = 4)

pam_results <- feat_data3 %>%
  dplyr::select(-sessionID) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

feat_data3[pam_fit$medoids, ]

#Cluster Visualization
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = feat_data3$sessionID)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

## Adding cluster number matching to sessionID to Dataframe ##

cluster_number = pam_fit$clustering
feat_data3$cluster_num = cluster_number


