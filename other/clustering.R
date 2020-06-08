##basic PCA.

library(irlba)
library(plotly)

mat <- data.frame(by_zip_ll[,-c(1,2,471:477)])
rownames(mat) <- by_zip_ll$zip
##scale the matrix.

mat <- scale(mat, center = T)
##why center?


##############################
##just plot PCA's
first_pca <- PCA(mat,  graph = FALSE, scale.unit = T)
fviz_screeplot(first_pca,addlabels = TRUE, ylim = c(0, 75), barfill = 'yellow')

##not good.

# Extract the results for variables
var <- get_pca_var(first_pca)
# Contributions of variables to PC1

fviz_contrib(first_pca, choice = "var", axes = 1, top = 15) 
# Contributions of variables to PC2
fviz_contrib(first_pca, choice = "ind", axes = 1, top = 10) + title("ZIPS")



pca.out <- prcomp_irlba(x = mat, center = T, scale = F, retx = T, n = 100)
#summary(pca.out)


pc.scores <- as.data.frame(pca.out$x)


pc.scores$region <- by_zip_ll$state.region


pcaplot <- ggplot(pc.scores) +
  aes(x = PC1, y = PC2, col = region) +
  labs(title = "Result of PCA", x = "PC1", y = "PC2") +
  geom_point(alpha = 0.5)

plot_ly(x= pc.scores$PC1, y= pc.scores$PC2, 
        z= pc.scores$PC3, type="scatter3d", alpha = 0.7,color = pc.scores$region)
 
ggplot(pc.scores) +
  aes(x = PC1, y = PC2, col = zip_region ) +
  geom_point(alpha = 0.5)
##still looks same. PCA is not working here, but we do see region wise difference!



##TNSE: almost same as PCA, except doesn't use a linear function of columns.



tsne <- Rtsne(mat, intial_dims = 100, theta = 0, partial_pca = T)
tsn <- as.data.frame(tsne$Y)

tsne$Y

tsn$region <- temp$state.region

tsneplot <- ggplot(tsn, aes(x = V1, y = V2, col = region)) + 
  geom_point(alpha = 0.5) + 
  labs(title = "Results of t-SNE", x = "Dimension 1", y = "Dimension 2") + 
  scale_fill_manual(values = wes_palette("Zissou1", 4, type = "discrete")) + 
  theme_light() 

tsneplot_zip <- ggplot(tsn, aes(x = V1, y = V2, col = zip_region)) + 
  geom_point(alpha = 0.5) + 
  labs(title = "Results of t-SNE", x = "Dimension 1", y = "Dimension 2") + 
  scale_fill_manual(values = wes_palette("Zissou1", 4, type = "discrete")) +
  theme_light() 



##not sure which method: metric MDS, aslo called principal coordinate analyses not to be confused with PCA
plot_metric_mds <- function(meth){
  
  d <- get_dist(mat, method = meth) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=3, list = T) # k is the number of dim
  mds.out <- as.data.frame(fit$points)
  mds.out$region <- by_zip_ll$state.region
  
  p1 <- ggplot(mds.out, aes(x = V1, y = V2, col = region)) + 
    geom_point(alpha = 0.5) + 
    theme_light() +
    labs(title = paste('MDS with metric', meth), x = 'Dim1', y = 'Dim2')
  
  p1
}
mdsplot <- plot_metric_mds('pearson')

grid.arrange(pcaplot, tsneplot, mdsplot, nrow = 1)
##################################



##we do see some groups: based on zip_region .. 4 groups seems adequate. 

#############################################################################

##other means of clustering

method = c("silhouette", "wss", "gap_stat")


mat <- data.frame(by_zip_ll[,-c(1,2,471:477)])
rownames(mat) <- by_zip_ll$zip



##using package clustree to get clusters


##how to get clusters?
#K-means and hierarchical clustering: silhouette method
##mss
fviz_nbclust(scale(mat, center = T, scale = F), kmeans, method = "wss", k.max = 10, linecolor = 'green') +
        theme_minimal() + ggtitle("the Elbow Method")

##hierarchical
fviz_nbclust(scale(mat, center = T, scale = T), hcut, method = "wss", k.max = 10, linecolour = 'pink') +
  theme_minimal() + ggtitle("the Elbow Method")




##silhouette till clusters = 10
fviz_nbclust(scale(mat, center = T, scale = T), kmeans, method = "silhouette", k.max = 10) + theme_minimal() + ggtitle("The Silhouette Plot")

##correlation distance?

dist.cor <- get_dist(scale(mat, center = T, scale = T), method = "pearson")
fviz_dist(dist.cor, show_labels = F)






##5 lines.

##eulidean works not to well for heierarchial. groups were fucked.
######dont run
hier_clust <- hclust(dist.cor, method = 'complete')
sub_grp <- cutree(hier_clust, k = 5)
table(sub_grp)


hier_clust <- hclust(dist.cor, method = 'average')
sub_grp <- cutree(hier_clust, k = 5)
table(sub_grp)
##all other methods work shit.
###################


####
##simple k-means. ##DO NOT SCALE!!!!!!!!!!!!!!!

get_kmeans <- function(k){
simple <- kmeans(scale(mat, center =T), centers = k)
return(simple$cluster)
#ggplot(by_zip_ll, aes(x = long, y = lat, col = as.factor(simple$cluster))) + geom_point(alpha = 0.5) +
  #scale_color_brewer(palette="Dark2") +
  #geom_polygon(aes(x = long, y = lat, group = group),
               #data = state_df, colour = "black", fill = NA) +
  #scale_fill_discrete(name = "Clusters", labels = 1:k)
  
}


ks <- by_zip_ll
k_clusters <- data.frame(c2 = get_kmeans(2), c3 = get_kmeans(3), c4 = get_kmeans(4), c5 = get_kmeans(5))
ks <- data.frame(by_zip_ll, k_clusters)

shared_k <- SharedData$new(ks)
#####################
pal <- colorFactor("viridis", domain = ks$c4)
bscols(widths= c(0.5,NA),
       ))
#####################

bscols(widths= c(0.5,NA),
       list(filter_checkbox("c3", "Pick Cluster", shared_k, ~c3, inline = TRUE)),
       leaflet(shared_k, width = "100%", height = 300) %>%
         addProviderTiles(providers$Stamen.Toner) %>%
         addCircleMarkers(radius = 5, color = ~pal(c3)))
#####################
pal <- colorFactor("viridis", domain = ks$c5)
bscols(widths= c(0.5,NA),
       list(filter_checkbox("c5", "Pick Cluster", shared_k, ~c5, inline = TRUE)),
       leaflet(shared_k, width = "100%", height = 300) %>%
         addProviderTiles(providers$Stamen.Toner) %>%
         addCircleMarkers(radius = 5, color = ~pal(c5)))
#####################
pal <- colorFactor("viridis", domain = ks$c2)
pal3 <- colorFactor("viridis", domain = ks$c3)
bscols(widths= c(0.5,NA),
       list(filter_checkbox("c2", "Pick Cluster", shared_k, ~c2, inline = TRUE)),
       leaflet(shared_k, width = "100%", height = 300) %>%
         addProviderTiles(providers$Stamen.Toner) %>%
         addCircleMarkers(radius = 5, color = ~pal(c2)),
       filter_checkbox("c4", "Pick Cluster", shared_k, ~c4, inline = TRUE),
       leaflet(shared_k, width = "100%", height = 300) %>%
         addProviderTiles(providers$Stamen.Toner) %>%
         addCircleMarkers(radius = 5, color = ~pal(c4)))

##CONCLUSION::: 4 clusters seem appropriate




