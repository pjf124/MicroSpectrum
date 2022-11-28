install.packages("gmodels")  
library(gmodels)
setwd("D:\\")
inname = "xx.txt"   
outname = "xx.pdf"  
expr <- read.table("xx.txt", header=T, sep="\t",stringsAsFactors=F)
mycolors=c(rep("black",4), rep("red",4)) 
dim(expr)
exp = expr[,c(2:9)]
head(exp)
data <- t(exp)# transpose the data  
head(data)
dim(data)
data.pca <- fast.prcomp(data,retx=T,scale=T,center=T)
a <- summary(data.pca)    
tmp <- a[4]$importance    
pro1 <- as.numeric(sprintf("%.4f",tmp[2,1]))*100 
pro2 <- as.numeric(sprintf("%.4f",tmp[2,2]))*100  
xmax <- max(data.pca$x[,1])  
xmin <- min(data.pca$x[,1])
ymax <- max(data.pca$x[,2])
ymin <- min(data.pca$x[,2])
samples =rownames(data.pca$x)
pdf(outname)
plot(
  data.pca$x[,1],                           
  data.pca$x[,2],                            
  xlab=paste("PC1","(",pro1,"%)",sep=""),    
  ylab=paste("PC2","(",pro2,"%)",sep=""),    
  main="PCA",                                
  xlim=c(1.1*xmin,1.1*xmax),                 
  ylim=c(1.1*ymin,1.1*ymax),                
  pch=16,col=mycolors)                       
abline(h=0,col="gray")                     
abline(v=0,col="gray")                     
text(data.pca$x[,1],data.pca$x[,2],labels=samples)  
dev.off()

