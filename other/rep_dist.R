#######################################################
#hierarchical bipartite spectral graph partitioning
#######################################################


state_df <- map_data("state")
my_map_theme <- theme_void()


##using hierarchical bipartite spectral graphing as described by Dhillon 2003
kk <- 2
create_heatmap <- function(kk) {
need <- as.matrix(just_zip[,-c(1,2, 471:474)])

#step1: D1 AND D2
rowsums <- 1/sqrt(rowSums(need)) ##for d1
colsums <- 1/sqrt(colSums(need)) ##for d2

d1 <- diag(rowsums)
d2 <- diag(colsums)

#step2:find A_n

A_n <- d1 %*% need %*% d2

#step3: SVD of A_n

svd_mat <- svd(A_n)

#step4: calculate Z
k <- kk
l <- ceiling(log(k))
u <- svd_mat$u
v <- svd_mat$v

Z1 <- d1 %*% as.matrix(u[,2:l+1])
Z2 <- d2 %*% v[,2:l+1]
Z <- rbind(Z1,Z2)

#step4: kmeans
k_means <- kmeans(Z, centers = k, iter.max = 100)
clusters <- k_means$cluster

temp <- by_zip_ll
temp$cluster <- as.factor(k_means$cluster[1:862])

#heatmap.2(need, Rowv = clusters[1:862], Colv = clusters[863:1330], dendrogram = 'none', trace = 'none',main = paste("Bicluster with k =",kk) 
          #,xlab = "ZIP Code", ylab = "Reponse Vector", labRow = F, labCol = F)

hc <- ggplot(temp) +
  geom_point(aes(x = long, y = lat, color = cluster), 
             size = 3, alpha = 0.5) +
  scale_color_brewer(palette="Dark2") +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = state_df, colour = "black", fill = NA) +
  labs(title = 'Hierarchical Bipartite Clusters') +
  my_map_theme
hc
 
}

create_heatmap(4)


##representative and distinctiveness
##remove answers with 0 variances?





###analysying for k =4
need <- as.matrix(just_zip[,-c(1,2, 471:474)])

#step1: D1 AND D2
rowsums <- 1/sqrt(rowSums(need)) ##for d1
colsums <- 1/sqrt(colSums(need)) ##for d2

d1 <- diag(rowsums)
d2 <- diag(colsums)

#step2:find A_n

A_n <- (d1 %*% need) %*% d2

#step3: SVD of A_n

svd_mat <- svd(A_n)

#step4: calculate Z
k <- 4
l <- ceiling(log(k))
u <- svd_mat$u
v <- svd_mat$v

Z1 <- d1 %*% as.matrix(u[,2:l+1])
Z2 <- d2 %*% v[,2:l+1]
Z <- rbind(Z1,Z2)

#step4: kmeans
k_means <- kmeans(Z, centers = k, iter.max = 100)
clusters <- k_means$cluster



##determining the 'importance' within each cluster.

##compjting matrices as per the paper
temp <- need
temp$cluster_ques <- k_means$cluster[1:862] 


rep <- temp %>% group_by(cluster_ques) %>% summarise_if(is.numeric, sum)

rep <- rep[,-1]/rowSums(rep[,-1])

rel_occ <- temp %>% group_by(cluster_ques) %>% summarise_if(is.numeric, sum) %>% select(-1)
rel_occ <- rel_occ/ colSums(temp[,-469])

rel_size <- (temp %>% group_by(cluster_ques) %>% summarise_if(is.numeric, sum) %>% select(-cluster_ques) %>% rowSums) / rowsums(colSums(need))

dis <- rel_occ - rel_size / (1 - rel_size)

importance <- (rep + dis)/2
importance$cluster <- c(1,2,3,4)
importance$cluster

##exploring california

temp <- just_zip
temp$cluster_ques <- k_means$cluster[1:862] 


temp %>% filter(cluster_ques == 4) %>% nrow()

##cluster 2, with least number of values.



cluster2 <- importance %>% filter(cluster == 2) %>% select(-cluster)










