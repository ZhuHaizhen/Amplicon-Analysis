args = commandArgs(T)
if (length(args) != 2){
	print("Rscript draw.triple.venn.r <InFile> <OutFile>")
	print("Example : Rscript draw.triple.venn.r triple.txt triple.pdf")
	q()
}

pdf(args[2])
data<-read.table(args[1],stringsAsFactors = FALSE,check.names = FALSE,quote = "",sep="\t")
library(VennDiagram)
venn.plot <- draw.triple.venn(
		area1 = data[1,2],
		area2 = data[2,2],
		area3 = data[3,2],
		n12 = data[4,2],
		n23 = data[5,2],
		n13 = data[6,2],
		n123 = data[7,2],
		category = data[1:3,1],
		fill = c("#66C2A5","#FC8D62","#8DA0CB"),
		lty = "blank",
		euler.d = F,
		scaled = F,
		cat.pos = c(-8,8, 180),
		cat.dist = c(0.03, 0.03, 0.025),
		cex = 2,
		cat.cex = 1.2,
		cat.col ="black" 
		)
dev.off()
