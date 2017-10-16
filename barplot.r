args<-commandArgs(T)
if(length(args)<2){
print('Rscript heatmap.r heatmap.csv heatmap.pdf')
print('length of args wrong!')
q()
}
data<-args[1]
out<-args[2]
library("RColorBrewer")
data<-as.matrix(read.table(data,header=T,row.names=1,sep="\t"))
mycol1<-brewer.pal(8,"Accent")
mycol2<-brewer.pal(9,"Set1")
totcol<-rep(c(mycol1,mycol2),10)
data<-data[order(rowSums(data),decreasing=T),] 
pdf(out,height=6,width=8)
par(xpd=T,mar=par()$mar+c(1,1,1,20))
barplot(data,col=totcol,las=1,cex.names=0.8,ylab="Relative Abundance",xlab="Sample",cex.lab=1.1,border = NA,legend=rownames(data),args.legend = list(x="right",bty="n",inset=-1.2,cex=0.7))  ###col=totcol �涨data�����е�ÿ���������ķ������ɫ��las=1��ʾx��y��ı�ǩ����A1����ˮƽ�ģ�cex.names=0.8��ʾx,y��ı�ǩ����A1�������С��Ĭ��ֵ��0.8����ylab��y��ı��⣬xlabͬ��cex.lab���Ʊ���ĸñ��������С��border=NA��ʾ����ͼ��ÿ��ɫ����Χû�б߿���ɫ��legend=rownames(data)��ʾ��data����������Ϊlegend��args.legend�Ĳ�����������legend()����������x=��right����ʾ��ͼ����������ͼ���Ҳ࣬bty=��n����ʾͼ����Χû�б��ߣ�inset=-0.3�Ƕ�ͼ��λ�ý����˵������������[-1,1]�����ڸĶ������ֵ������������ô��ˣ�cex=0.7����˵�ǿ���ͼ������Ĵ�С��
dev.off()


