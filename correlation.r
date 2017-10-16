args=commandArgs(T)
if(length(args)!=2){
	print('/Share/home/wangzj/download/R-3.2.0/bin/Rscript correlation.r <rpkm_file> <correlation pdf>')
	print('rpkm_file:all sample rpkm file')
	print('correlation pdf:output pdf file')
}

rpkm_file<-args[1]
out_pdf<-args[2]
data<-read.table(rpkm_file,header=T,check.names=F,row.names=1)
dt_cor<-cor(data)
library(pheatmap)
pdf(out_pdf,onefile=F)
pheatmap(dt_cor,display_numbers = F, number_format = "%.2f", cluster_rows =F,cluster_cols=F)
dev.off()
