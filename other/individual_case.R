individual_heatmap <- function(kk) {
  need <- as.matrix(indi[,-c(469:473)])
  
  #step1: D1 AND D2
  rowsums <- 1/sqrt(rowSums(need)) ##for d1
  colsums <- 1/sqrt(colSums(need)) ##for d2
  
  d1 <- diag(rowsums)
  d2 <- diag(colsums)
  
  #step2:find A_n
  
  A_n <- outer(d1, d2) * need
  
  #step3: SVD of A_n, but dont need all
  k <- kk
  l <- ceiling(log(k))
  
  svd_mat <- irlba(A_n, nv = 3+l, nu = 3+l)
  
  #step4: calculate Z
  
  u <- svd_mat$u
  v <- svd_mat$v
  
  Z1 <- d1 %*% as.matrix(u[,2:l+1])
  Z2 <- d2 %*% v[,2:l+1]
  Z <- rbind(Z1,Z2)
  
  #step4: kmeans
  k_means <- kmeans(Z, centers = k, iter.max = 100)
  clusters <- k_means$cluster
  
  temp <- by_zip_ll
  temp$cluster_ques <- k_means$cluster[1:862] 
  
  heatmap.2(need, Rowv = clusters[1:862], Colv = clusters[863:1330], dendrogram = 'none', trace = 'none',main = paste("Bicluster with k =",kk) 
            ,xlab = "ZIP Code", ylab = "Reponse Vector", labRow = F, labCol = F)
  
  ggplot(temp) +
    geom_point(aes(x = long, y = lat, color = as.factor(cluster_ques)), 
               size = 3, alpha = 0.5) +
    scale_color_brewer(palette="Dark2") +
    geom_polygon(aes(x = long, y = lat, group = group),
                 data = state_df, colour = "black", fill = NA) +
    my_map_theme
  
}

create_heatmap(4)