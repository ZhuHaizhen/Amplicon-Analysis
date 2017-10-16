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
barplot(data,col=totcol,las=1,cex.names=0.8,ylab="Relative Abundance",xlab="Sample",cex.lab=1.1,border = NA,legend=rownames(data),args.legend = list(x="right",bty="n",inset=-1.2,cex=0.7))  ###col=totcol 规定data矩阵中的每行所画出的方块的颜色；las=1表示x和y轴的标签（如A1）是水平的；cex.names=0.8表示x,y轴的标签（如A1）字体大小是默认值的0.8倍；ylab是y轴的标题，xlab同理；cex.lab控制标题的该标题字体大小；border=NA表示条形图中每个色块周围没有边框颜色；legend=rownames(data)表示将data的行名设置为legend；args.legend的参数设置类似legend()函数，其中x=”right”表示将图例画在条形图的右侧，bty=”n”表示图例外围没有边线，inset=-0.3是对图例位置进行了调整，你可以在[-1,1]区间内改动下这个值就能体会它的用处了，cex=0.7不用说是控制图例字体的大小。
dev.off()


