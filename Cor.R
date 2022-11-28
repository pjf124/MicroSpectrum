install.packages("gmodels")  
library(gmodels)
install.packages("flattenCorrMatrix")
installed.packages("Hmisc")
setwd("D:\\")
data =read.table("xx.txt", header=T, sep="\t",stringsAsFactors=F)
head(data)
Cor_matr=cor(data)
Cor_matr
library(Hmisc)
res2<-rcorr(as.matrix(data))
res2
res2$P
write.table(res2$P,"p-value.txt",sep="\t")
library(PerformanceAnalytics)
chart.Correlation(data,histogram = TRUE,pch=19)


install.packages("pheatmap")
setwd("D:\\")
library(pheatmap) 
data=read.table("xx.txt",header=T,row.names = 1,sep="\t") 
matrix=cor(data) 
write.table(matrix,"coefficient_matrix.txt",sep="\t") 
pheatmap(matrix,cluster_rows=F,cluster_cols = F,display_numbers = T,fontsize = 14) #?к??ж??????࣬????????ͼ????ʾ??ֵ??



