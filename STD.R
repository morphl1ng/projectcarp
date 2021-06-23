rd<- read.csv("5groups.csv",header = T,row.names = 1)
STD<-as.data.frame(matrix(,ncol = 27,nrow = 100))
row.names(STD) <- rownames(rd)
colnames(STD) <- colnames(rd)[2:28]
for (i in 1:100) {
  for (j in 2:28) {
    STD[i,j-1]<-rd[i,j]/rd[i,1]
  }
}
STD$group<-rd[,29]
write.csv(STD,file = "STD.csv")