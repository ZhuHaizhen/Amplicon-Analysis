library(vegan)
library(ggplot2)
library(gridExtra)
variability_table = function(cca){
  chi = c(cca$tot.chi, cca$CCA$tot.chi, cca$CA$tot.chi)
  variability_table = cbind(chi, chi/chi[1])
  colnames(variability_table) = c("inertia", "proportion")
  rownames(variability_table) = c("total", "constrained", "unconstrained")
  return(variability_table)
}
design = read.table("design.txt", header=T, row.names= 1, sep="\t")
otu_table = read.table("cave_otu_table_css.txt", sep="\t", header=T, row.names= 1)
idx = rownames(design) %in% colnames(otu_table) 
sub_design = design[idx,]
sub_otu_table = otu_table[, rownames(sub_design)]
capscale.cave = capscale(t(sub_otu_table) ~ Cave, data=sub_design, add=F, sqrt.dist=T, distance="bray")
perm_anova.cave = anova.cca(capscale.cave)
var_tbl.cave = variability_table(capscale.cave)
eig = capscale.cave$CCA$eig
variance = var_tbl.cave["constrained", "proportion"]
p.val = perm_anova.cave[1, 4]
points = capscale.cave$CCA$wa[, 1:2]
points = as.data.frame(points)
colnames(points) = c("x", "y")
points = cbind(points, sub_design[match(rownames(points), rownames(sub_design)),])
p = ggplot(points, aes(x=x, y=y, color=Cave)) + theme_bw() + geom_point(alpha=0.7, size=2) + labs(x=paste("CPCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),y=paste("CPCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep="")) + ggtitle(paste(format(100 * variance, digits=3), " % of variance; p=",format(p.val, digits=2),sep=""))
ggsave(paste( "CPCoA.pdf", sep=""), p, width = 5, height = 3)