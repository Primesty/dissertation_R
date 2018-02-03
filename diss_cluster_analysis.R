
# Clustering --------------------------------------------------------------


data("iris")

plot(hclust(dist(iris[,3:4])))


clusters <- hclust(dist(liwc_results[,4:76]), method = "avg")

plot(clusters)

clusterCut <- cutree(clusters, 3)

table(clusterCut)

liwc_results %>% filter(part_id %in% liwc_results$part_id[clusterCut == 2]) 

liwc_results %>% filter(part_id %in% liwc_results$part_id[clusterCut == 2]) %>% summarise_all(mean)

aggregate(liwc_results[,-c(1,2)], list(clusterCut), median) # get results for all clusters

liwc_results2 <- liwc_results

liwc_results2$clusterCut <- as.factor(clusterCut)

liwc_results2[, -c(1,2)] %>% group_by(clusterCut) %>% summarise_all(mean)

library(dendextend)

dend <- as.dendrogram(clusters)

dend <- dend %>% 
        color_branches(k = 4) %>% 
        set("branches_lwd", c(2,1,2)) %>% 
        set("branches_lty", c(1,2,1)) %>% 
        color_branches(k = 4,col = c("darkred", "steelblue", "forestgreen", "tomato"))
        
labels(dend) <- paste0("part_id_",liwc_results$part_id)
        
plot(dend, horiz = TRUE, main = "Clustered according to LIWC results")


library(circlize)

circlize_dendrogram(dend, labels_track_height = NA, dend_track_height = .3, main = "Clustered according to LIWC results")

### Diss data

clusters2 <- hclust(dist(diss_data[,3:33]), method = "ward.D")

clusterCut2 <- cutree(clusters2, 4)

table(clusterCut2)

diss_data$part_id[clusterCut2 == 1] # see which participants are in cluster 1

diss_data %>% filter(part_id %in% diss_data$part_id[clusterCut2 == 1]) # print entries for cluster 1; load tidyverse

#alternative
subset(diss_data, part_id %in% diss_data$part_id[clusterCut2 == 1])


sapply(unique(clusterCut2), function(g)diss_data$part_id[clusterCut2 == g])

dend2 <- as.dendrogram(clusters2)

dend2 <- dend2 %>% 
        color_branches(k = 4) %>% 
        set("branches_lwd", c(2,1,2)) %>% 
        set("branches_lty", c(1,2,1)) %>% 
        color_branches(k = 4,col = c("darkred", "steelblue", "forestgreen", "tomato"))
        

labels(dend2) <- paste0("part_id_", diss_data$part_id)

circlize_dendrogram(dend2, labels_track_height = NA, dend_track_height = .3)

# Summary stats for clusters


diss_clust <- diss_data %>% mutate(clust = case_when(part_id %in% diss_data$part_id[clusterCut2 == 1] ~ 1,
                                       part_id %in% diss_data$part_id[clusterCut2 == 2] ~ 2,
                                       part_id %in% diss_data$part_id[clusterCut2 == 3] ~ 3,
                                       part_id %in% diss_data$part_id[clusterCut2 == 4] ~ 4))

diss_clust %>% group_by(clust) %>% summarise(n = n())

library(skimr)

summary(diss_clust)
