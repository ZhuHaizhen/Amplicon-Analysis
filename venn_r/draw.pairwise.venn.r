args = commandArgs(T)
if (length(args) != 2){
	print("Rscript draw.pairwise.venn.r <InFile> <OutFile>")
	print("Example : Rscript draw.pairwise.venn.r pairwise.txt pairwise.pdf")
	q()
}

pdf(args[2])
d<-read.table(args[1],stringsAsFactors = FALSE,check.names = FALSE,quote = "",sep="\t")
par(font=2,font.axis=2,font.lab=2,mar=c(4,8,4,4))
library(RColorBrewer)
library(VennDiagram)
	venn.plot <- draw.pairwise.venn(
		area1 = d[1,2],
		area2 = d[2,2],
		cross.area = d[3,2],
		category = d[1:2,1],
		euler.d = F,
		scaled = F,
		cat.pos = c(-15, 15),
		cat.dist = rep(0.04, 2),
		cex = rep(3, 3),
		fill = c("#66C2A5","#FC8D62"),
		lty = "blank",
		cat.cex = rep(1.2,1.2),
		);
dev.off()
