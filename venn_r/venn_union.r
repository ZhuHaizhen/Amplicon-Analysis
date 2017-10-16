args = commandArgs(T)
if (length(args) != 3){
	print("Rscript venn_union.r <InFile> <OutFile> <group count>")
	print("Example : Rscript venn_union.r pairwise.txt pairwise.pdf 2")
	q()
}
if(args[3] == 2){
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
}

if(args[3]==4){
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
			cat.col = brewer.pal(4,"Set2")
			);
dev.off()
}

if(args[3]==3){
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
}

if(args[3]==5){
pdf(args[2],h=9,w=9)
data<-read.table(args[1],stringsAsFactors = FALSE,check.names = FALSE,quote = "",sep="\t")
library(RColorBrewer)
library(VennDiagram)
venn.plot <- draw.quintuple.venn(
		area1 = data[1,2],
		area2 = data[2,2],
		area3 = data[3,2],
		area4 = data[4,2],
		area5 = data[5,2],
		n12 = data[6,2],
		n13 = data[7,2],
		n14 = data[8,2],
		n15 = data[9,2],
		n23 = data[10,2],
		n24 = data[11,2],
		n25 = data[12,2],
		n34 = data[13,2],
		n35 = data[14,2],
		n45 = data[15,2],
		n123 = data[16,2],
		n124 = data[17,2],
		n125 = data[18,2],
		n134 = data[19,2],
		n135 = data[20,2],
		n145 = data[21,2],
		n234 = data[22,2],
		n235 = data[23,2],
		n245 = data[24,2],
		n345 = data[25,2],
		n1234 = data[26,2],
		n1235 = data[27,2],
		n1245 = data[28,2],
		n1345 = data[29,2],
		n2345 = data[30,2],
		n12345 = data[31,2],
		category = data[1:5,1],
		fill = brewer.pal(5,"Set2"),
		cat.dist = rep(0.25, 5),
		cat.col = brewer.pal(5,"Set2"),
		cat.cex = 1.2,
		alpha = rep(0.2, 5),
		cat.just = rep(list(c(0.5, 0.5)), 5),
		margin = 0.1,
		cex = c(1.5, 1.5, 1.5, 1.5, 1.5, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.55, 1, 0.55, 1, 0.55, 1, 0.55, 1, 0.55, 1, 1, 1, 1, 1, 1.5),
		ind = TRUE
		);
dev.off()
}
