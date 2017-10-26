library(vegan)
library(gridExtra)
library(ggplot2)
design = read.table("design.txt", header=T, row.names= 1, sep="\t") 
weighted = read.table("beta/weighted_unifrac_cave_otu_table_css.txt", sep="\t", header=T, check.names=F)
idx = rownames(design) %in% colnames(weighted) 
sub_design = design[idx,]
weighted = weighted[rownames(sub_design), rownames(sub_design)]
pcoa = cmdscale(weighted, k=3, eig=T)
points = as.data.frame(pcoa$points)
colnames(points) = c("x", "y", "z") 
eig = pcoa$eig
points = cbind(points, sub_design[match(rownames(points), rownames(sub_design)), ])
p1 = ggplot(points,aes(x = x,y = y, color = Cave, shape = Sample_Type)) + theme_bw() + geom_point(alpha = 0.7, size = 2) + labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""), y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep=""),title = "PCoA plot of weighted unifrac distance") + theme(legend.position = "none")
unweighted = read.table("beta/unweighted_unifrac_cave_otu_table_css.txt", sep="\t", header=T, check.names=F)
idx = rownames(design) %in% colnames(unweighted) 
sub_design = design[idx,]
unweighted = unweighted[rownames(sub_design), rownames(sub_design)]
pcoa = cmdscale(unweighted, k=3, eig=T)
points = as.data.frame(pcoa$points)
colnames(points) = c("x", "y", "z") 
eig = pcoa$eig
points = cbind(points, sub_design[match(rownames(points), rownames(sub_design)), ])
p2 = ggplot(points,aes(x = x,y = y, color = Cave, shape = Sample_Type)) + theme_bw() + geom_point(alpha = 0.7, size = 2) + labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""), y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep=""),title = "PCoA plot of unweighted unifrac distance")
p<-grid.arrange(p1,p2,nrow =1,widths = c(0.78,1))
ggsave("beta_pcoa.pdf", p, width = 10, height = 5)