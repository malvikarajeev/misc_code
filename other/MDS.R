##############

mat <- data.frame(by_zip_ll[,-c(1,2,471:477)])
rownames(mat) <- by_zip_ll$zip

##############

library(factoextra)
library(gridExtra)



##not sure which method: metric MDS, aslo called principal coordinate analyses not to be confused with PCA
plot_metric_mds <- function(meth){

d <- get_dist(mat, method = meth) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=3, list = T) # k is the number of dim
mds.out <- as.data.frame(fit$points)
mds.out$region <- by_zip_ll$state.region

p1 <- ggplot(mds.out, aes(x = V1, y = V2, col = region)) + 
  geom_point(alpha = 0.5) + 
  theme_minimal() +
  labs(title = paste('MDS with metric', meth), x = 'Dim1', y = 'Dim2')

p1
}



#####################################################################

##comparing PCA, tSNE, nonmetric MDS
mat <- data.frame(by_zip_ll[,-c(1,2,471:477)])
rownames(mat) <- by_zip_ll$zip

#####################################################################
##center the matrix.

mat <- scale(mat, center = T, scale = F)

#####################################################################
#pca
pca.out <- prcomp_irlba(x = mat, center = T, scale = F, retx = T, n = 100)
summary(pca.out)

pc.scores <- as.data.frame(pca.out$x)
pc.scores$region <- by_zip_ll$state.region


pcaplot <- ggplot(pc.scores) +
  aes(x = PC1, y = PC2, col = region) +
  labs(title = "Results of PCA", x = "Dimension 1", y = "Dimension 2") +
  geom_point(alpha = 0.5)

###rTNSE
tsne <- Rtsne(mat, intial_dims = 100, theta = 0, partial_pca = T)
tsn <- as.data.frame(tsne$Y)
tsn$region <- by_zip_ll$state.region

tsneplot <- ggplot(tsn, aes(x = V1, y = V2, col = region)) + 
          geom_point(alpha = 0.5) + 
          labs(title = "Results of t-SNE", x = "Dimension 1", y = "Dimension 2") + 
        scale_fill_manual(values = wes_palette("Zissou1", 4, type = "discrete")) + 
          theme_minimal()

##MDS

mdsplot <- plot_metric_mds('pearson')

grid.arrange(pcaplot, tsneplot, mdsplot, nrow = 1)






