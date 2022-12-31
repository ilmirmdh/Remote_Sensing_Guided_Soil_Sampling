# Load Packages
library(sp)
library(fields)
library(ggplot2)

# Set Number of Sample (I used Slovin sample size calculation technique)
n<-23

# Deploy The K-Means Algorithm to Compute Clusters
set.seed(314)
myClusters_br <- kmeans(scale(T28[,1:28]), centers=n, iter.max=100,nstart=10)
T28$clusters <- myClusters_br$cluster

# Distance Algorithm to Select A Sample Closest to Centroids
rdist.out <- rdist(x1=myClusters_br$centers,x2=scale(T28[,1:28]))
ids.mindist <- apply(rdist.out,MARGIN=1,which.min)
mySample_br <- T28[ids.mindist,]

# Export Samples to CSV File
write.csv(mySample_br, file = "G:\\SPECTRAL SAMPLING\\TABULAR DATA\\EXPORT_SAMPLE\\SAMPLE_BARE.csv", col.names = FALSE)
