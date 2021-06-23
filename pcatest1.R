library(ggrepel)
library(ggplot2)
library(magrittr)
library(dplyr)
xt<- read.csv("xingtai.csv",header = T,row.names = 1)
xt1<-xt[c(2:84),c(53:61,63:80)]
xt1mean<-group_by(xt1, group) %>% summarize_each(funs(mean))
xt1mean<-as.data.frame(xt1mean)
rownames(xt1mean)<-as.vector(as.matrix(xt1mean[,1]))
xt1mean<-xt1mean[,-1]
xt1_dist<-dist(xt1mean)
xt1_dist_single<-hclust(xt1_dist,method = "single")
plot(xt1_dist_single,hang = -1,cex=0.6)
#距离聚类
xt1.pca<-prcomp(xt1[,-1],scale = TRUE)
mydata<-as.data.frame(scale(xt1.pca$x[,1:2]))
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15)
{wss[i] <- sum(kmeans(mydata,centers=i)$withinss)}
###这里的wss(within-cluster sum of squares)是组内平方和
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
#确定分为几组
km<-kmeans(mydata,5)
ggplot( ) + 
  geom_text_repel(data = mydata, aes(PC1, PC2, label=row.names(mydata)), size=3)+
  geom_point(data=mydata,aes(x=PC1,y=PC2, fill =as.factor(xt1$group),shape=as.factor(km$cluster),color=as.factor(xt1$group),size=1))+
  geom_hline(yintercept=0,linetype = 3,size = 1) +
  geom_vline(xintercept=0,linetype = 3,size = 1) +
  labs(x="PCA1",y="PCA2")+
  theme_bw() +
  theme(panel.grid=element_blank())