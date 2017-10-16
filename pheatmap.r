args<-commandArgs(T)
if(length(args)<2){
print('Rscript heatmap.r heatmap.csv heatmap.pdf')
print('length of args wrong!')
q()
}
data<-args[1]
out<-args[2]
library(gplots)
library(pheatmap)
dt<-read.table(data,header=T,row.names=1,check.names=F,stringsAsFactors=F)
#pdf(out,width=12,height=10)
pdf(out,onefile=F)
#heatmap(as.matrix(dt),labRow=NA,margins = c(15, 5),cexCol=1,col=colorRampPalette(rev(c("#F2D732","#000000","#0059D5")))(20))
#heatmap.2(as.matrix(dt),margins = c(17, 5),col=colorRampPalette(rev(c("red","black","green")))(20),trace="none",scale='row')
#heatmap.2(as.matrix(dt),margins = c(5, 20),col=colorRampPalette(rev(c("red","black","green")))(20),trace="none",scale='column',cexRow=1.2,xlab="Samples",cex.lab=5,col.lab="blue")

pheatmap(dt,display_numbers = F, color = colorRampPalette(c("white", "darkorange","red4", "darkred"))(1000),
cellwidth=18, cellheight=15, fontsize=10, fontsize_row=7,cluster_row=TRUE,cluster_cols=TRUE)

dev.off()
