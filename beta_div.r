library(vegan)
library(ggplot2)
design = read.table("design.txt", header=T, row.names= 1, sep="\t") 
weighted = read.table("beta/weighted_unifrac_cave_otu_table_filtered_even.txt", sep="\t", header=T, check.names=F)
idx = rownames(design) %in% colnames(weighted) 
sub_design = design[idx,]
weighted = weighted[rownames(sub_design), rownames(sub_design)]
pcoa = cmdscale(weighted, k=3, eig=T)
points = as.data.frame(pcoa$points)
colnames(points) = c("x", "y", "z") 
eig = pcoa$eig
points = cbind(points, sub_design[match(rownames(points), rownames(sub_design)), ])
p = ggplot(points,aes(x = x,y = y, color = Cave, shape = Sample_Type)) + theme_bw() + geom_point(alpha = 0.7, size = 2) + labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""), y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep=""))
p
ggsave("beta_pcoa_weighted_unifrac.pdf", p, width = 5, height = 3)