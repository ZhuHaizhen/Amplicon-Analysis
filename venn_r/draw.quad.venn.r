args = commandArgs(T)
if (length(args) != 2){
	print("Rscript draw.quad.venn.r <InFile> <OutFile>")
	print("Example : Rscript draw.quad.venn.r quad.txt quad.pdf")
	q()
	}

pdf(args[2],h=9,w=9)
data<-read.table(args[1],stringsAsFactors = FALSE,check.names = FALSE,quote = "",sep="\t")
library(RColorBrewer)
library(VennDiagram)

	venn.plot <- draw.quad.venn(
			area1 = data[1,2],
			area2 = data[2,2],
			area3 = data[3,2],
			area4 = data[4,2],
			n12 = data[5,2],
			n13 = data[6,2],
			n14 = data[7,2],
			n23 = data[8,2],
			n24 = data[9,2],
			n34 = data[10,2],
			n123 = data[11,2],
			n124 = data[12,2],
			n134 = data[13,2],
			n234 = data[14,2],
			n1234 = data[15,2],
			category = data[1:4,1],
			fill = brewer.pal(4,"Set2"),
			lty = "dashed",
			cex = 2,
			cat.cex = 1.2,
			cat.just = list(c(0.4,2),c(0.7,0),c(0.5,-1),c(0.5,0)),
			cat.col = brewer.pal(4,"Set2")
			);
dev.off()
