setwd("D:\\")
data=read.table("xx.txt",header=T,row.names=1,sep="\t")
dim(data)
head(data)
exp=data[,1:9]
head(exp)
library("pheatmap")
col=colorRampPalette(c("blue","white","red"))(100)
pheatmap(exp,border=NA,scale="row",fontsize=7,color=col,show_colnames=T,show_rownames=T,cluster_rows=T,cluster_cols=T)

