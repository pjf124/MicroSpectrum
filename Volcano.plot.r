setwd("D:\\")
data=read.table("xx.txt",header=T,sep="\t",stringsAsFactors=F)
dim(data)
head(data)
data[,4]=NA
data[data[,3]>1.30103 & data[,2]<0.584963 & data[,2]>-0.584963,4]="gray"
data[data[,3]<1.30103,4]="gray"
data[data[,3]>1.30103 & data[,2]>=0.584963,4]="red"
data[data[,3]>1.30103 & data[,2]<=-0.584963,4]="blue"


plot(data[,2],data[,3],col=data[,4],pch=19,cex=0.5,xlab="log2(FoldChange)",ylab="-log10(PValue)")
abline(h=1.30103,lty=2,col="palegreen4")
abline(v=0.584963,lty=2,col="palegreen4")
abline(v=-0.584963,lty=2,col="palegreen4")




dev.off()

