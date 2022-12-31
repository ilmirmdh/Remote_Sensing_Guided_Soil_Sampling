library(sp)
library(fields)
library(ggplot2)

#Read data with coordinates and other attributes of fine grid (discretization of study area)

summary(T25[,1:25])
cor(T17[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)])

#Set number of sampling locations to be selected

n<-23

#Compute clusters

set.seed(314)
myClusters_br <- kmeans(scale(T28[,1:28]), centers=n, iter.max=100,nstart=10)
T28$clusters <- myClusters_br$cluster

#Select locations closest to the centers of the clusters

rdist.out <- rdist(x1=myClusters_br$centers,x2=scale(T28[,1:28]))
ids.mindist <- apply(rdist.out,MARGIN=1,which.min)
mySample_br <- T28[ids.mindist,]

#Plot clusters and sampling points

pdf(file = "KMSample_Bare_Cikapundung.pdf", width = 7, height = 7)
ggplot(T28) +
  geom_tile(mapping = aes(x = POINT_X, y = POINT_Y, fill = factor(clusters))) +
  scale_fill_discrete(name = "cluster") +
  geom_point(data=mySample_br,mapping=aes(x=POINT_X,y=POINT_Y),size=1) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "") +
  coord_fixed() +
  theme(legend.position="none")
dev.off()

pdf(file = "CARIvsFerrous_KMSample_Bare.pdf", width = 7, height = 7)
ggplot(T28) +
  geom_point(mapping=aes(y=SMI,x=VDEPTH,colour=factor(clusters))) +
  geom_point(data=mySample_br,mapping=aes(y=SMI,x=VDEPTH),size=2) +
  scale_y_continuous(name = "SMI") +
  scale_x_continuous(name = "TRI") +
  theme(legend.position="none")
dev.off()

write.csv(mySample_br, file = "G:\\SPECTRAL SAMPLING\\TABULAR DATA\\EXPORT_SAMPLE\\SAMPLE_BARE.csv", col.names = FALSE)
