#!/usr/bin/env Rscript
# Copyright (c) 2015 BioHuge Singapore PTE. LTD. All Rights Reserved.
print('Rscript heatmap.r rpkm.xls heatmap.pdf')
args = commandArgs(trailingOnly = TRUE)
if(length(args) < 1){
	args = commandArgs(trailingOnly = FALSE)
	file.arg.name <- "--file="
	script.name = sub(file.arg.name, "", args[grep(file.arg.name, args)])
	cat(paste("Usage:", script.name, "OTUs.tab [output]\n"))
	
	q()
}

tabFile = as.character(args[1])
if(length(args) >= 2){
	outFile = paste(as.character(args[2]), sep="", ".pdf");
	pdf(outFile)
	
}

library(vegan)
x = read.table(tabFile, sep="", row.names=1, header=TRUE) ###" "曾导致运行出错，要去掉中间的空格
x = as.matrix(x)
#y = t(read.table(tabFile, quote="," sep="\t", row.names=1, header=TRUE))

#y=matrix(as.integer(y), nrow=dim(y)[1])
sp1 = specaccum(x)
sp2 = specaccum(x, "random")
plot(sp1,ci.type="poly",col="blue",lwd=2,ci.lty=0,ci.col="lightblue",xlab="Number of samples sequenced", ylab="OTUs detected")
boxplot(sp2,col="yellow",add=TRUE,pch="+")
