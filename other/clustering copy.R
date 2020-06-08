##trying kmeans, SVD.

library(irlba)

pca <- prcomp_irlba(by_pin[,-1])

head(pca$totalvar)

partial_eigen(as.matrix(by_pin[,-1])