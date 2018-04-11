library(vegan)
library(ggplot2)
library(plyr)

design.in <- read.table("design.in.txt", header=T, row.names= 1, sep="\t")
weighted.in <- read.table("in.beta/weighted_unifrac_cave.in_otu_table_css.txt", sep="\t", header=T, check.names=F)
unweighted.in <- read.table("in.beta/unweighted_unifrac_cave.in_otu_table_css.txt", sep="\t", header=T, check.names=F)
bray.in <- read.table("in.beta/bray_curtis_cave.in_otu_table_css.txt", sep="\t", header=T, check.names=F)

pcoa.w <- cmdscale(weighted.in, k=3, eig=T)
points.w <- as.data.frame(pcoa.w$points)
colnames(points.w) <- c("x", "y", "z") 
eig.w <- pcoa.w$eig
points.w <- cbind(points.w, design.in[match(rownames(points.w), rownames(design.in)), ])

weighted.in<-as.matrix(weighted.in)
adonis(weighted.in~sample_type+elevation,design.in,permutations = 999)
#              Df SumsOfSqs MeanSqs F.Model   R2     Pr(>F)    
# sample_type   3    0.6812 0.22706  1.1309 0.02261  0.294    
# elevation     2    4.9555 2.47776 12.3409 0.16446  0.001 ***
# Residuals   122   24.4948 0.20078         0.81293           
# Total       127   30.1315                 1.00000

pcoa.u <- cmdscale(unweighted.in, k=3, eig=T)
points.u <- as.data.frame(pcoa.u$points)
colnames(points.u) <- c("x", "y", "z") 
eig.u <- pcoa.u$eig
points.u <- cbind(points.u, design.in[match(rownames(points.u), rownames(design.in)), ])

unweighted.in<-as.matrix(unweighted.in)
adonis(unweighted.in~sample_type+elevation,design.in,permutations = 999)
#              Df SumsOfSqs MeanSqs F.Model   R2     Pr(>F)    
# sample_type   3     0.750 0.24993  1.0506 0.02331  0.320    
# elevation     2     2.400 1.20023  5.0455 0.07461  0.001 ***
# Residuals   122    29.022 0.23788         0.90208           
# Total       127    32.172                 1.00000   

pcoa.b <- cmdscale(bray.in, k=3, eig=T)
points.b <- as.data.frame(pcoa.b$points)
colnames(points.b) <- c("x", "y", "z") 
eig.b <- pcoa.b$eig
points.b <- cbind(points.b, design.in[match(rownames(points.b), rownames(design.in)), ])

bray.in <-as.matrix(bray.in)
adonis(bray.in~sample_type+elevation,design.in,permutations = 999)
#               Df SumsOfSqs MeanSqs F.Model   R2    Pr(>F)    
# sample_type   3     1.156 0.38525  1.0492 0.02362  0.304    
# elevation     2     2.975 1.48761  4.0515 0.06081  0.001 ***
# Residuals   122    44.795 0.36717         0.91557           
# Total       127    48.926                 1.00000  

library(pairwiseAdonis)

cave.in <- read.table("cave.in_otu_table_css.txt", header=T, row.names= 1, sep="\t")
otu<-as.data.frame(t(cave.in))
otu$sample<-row.names(otu)
otu.o<-as.data.frame(otu[order(otu$sample),])
otu.o$elevation<-c(rep("Medium",39),rep("Low",30),rep("Medium",31),rep("High",28))
pairwise.adonis(otu.o[,1:128],otu.o$elevation)
#         pairs  F.Model    R2        p.value p.adjusted sig
# Medium vs Low  3.091459  0.03058081  0.001      0.003   *
# Medium vs High 1.729092  0.01769270  0.035      0.105    
# Low vs High    1.252213  0.02187188  0.192      0.576    

theme_set(theme_bw())

find_hull <- function(df) df[chull(df$x, df$y), ]

points.w$elevation<-factor(points.w$elevation,levels = c("High","Medium","Low"))
points.w$sample_type<-factor(points.w$sample_type,levels = c("Air","Rock","Soil","Water"))

hulls.w<-ddply(points.w,"elevation",find_hull)
hulls.w.a <- ddply(subset(points.w,sample_type=="Air"), "elevation", find_hull)
hulls.w.w <- ddply(subset(points.w,sample_type=="Water"), "elevation", find_hull)
hulls.w.s <- ddply(subset(points.w,sample_type=="Soil"), "elevation", find_hull)
hulls.w.r <- ddply(subset(points.w,sample_type=="Rock"), "elevation", find_hull)

xlab.w<-paste("PCoA 1 (", format(100 * eig.w[1] / sum(eig.w), digits=4), "%)",sep="")
ylab.w<-paste("PCoA 2 (", format(100 * eig.w[2] / sum(eig.w), digits=4), "%)",sep="")

p1 <- ggplot(points.w,aes(x = x,y = y, color = elevation)) + geom_point(alpha = 0.9, size = 3) + labs(x=xlab.w, y=ylab.w,title = "PCoA plot of weighted unifrac distance") + facet_wrap(~sample_type,nrow = 2,scales = "free") + geom_polygon(data = hulls.w.a, alpha = 0.3, aes(fill = factor(elevation)),show.legend = F, linetype = 3) + geom_polygon(data = hulls.w.w, alpha = 0.3, aes(fill = factor(elevation)),show.legend = F, linetype = 3) + geom_polygon(data = hulls.w.s, alpha = 0.3, aes(fill = factor(elevation)),show.legend = F, linetype = 3) + geom_polygon(data = hulls.w.r, alpha = 0.3, aes(fill = factor(elevation)),show.legend = F, linetype = 3)

p2 <- ggplot(points.u,aes(x = x,y = y, color = elevation)) + geom_point(alpha = 0.8, size = 3) + labs(x=xlab, y=ylab,title = "PCoA plot of unweighted unifrac distance")

points.b$elevation<-factor(points.b$elevation,levels = c("High","Medium","Low"))
points.b$sample_type<-factor(points.b$sample_type,levels = c("Air","Rock","Soil","Water"))

hulls.b<-ddply(points.b,"elevation",find_hull)
hulls.b.a <- ddply(subset(points.b,sample_type=="Air"), "elevation", find_hull)
hulls.b.w <- ddply(subset(points.b,sample_type=="Water"), "elevation", find_hull)
hulls.b.s <- ddply(subset(points.b,sample_type=="Soil"), "elevation", find_hull)
hulls.b.r <- ddply(subset(points.b,sample_type=="Rock"), "elevation", find_hull)

xlab.b<-paste("PCoA 1 (", format(100 * eig.b[1] / sum(eig.b), digits=4), "%)",sep="")
ylab.b<-paste("PCoA 2 (", format(100 * eig.b[2] / sum(eig.b), digits=4), "%)",sep="")

p3 <- ggplot(points.b,aes(x = x,y = y, color = elevation)) + geom_point(alpha = 0.8, size = 3) + facet_wrap(~sample_type,nrow = 2,scales = "free") + labs(x=xlab.b, y=ylab.b,title = "PCoA plot of bray curtis distance") + geom_polygon(data = hulls.b.a, alpha = 0.3, aes(fill = factor(elevation)),show.legend = F, linetype = 3) + geom_polygon(data = hulls.b.w, alpha = 0.3, aes(fill = factor(elevation)),show.legend = F, linetype = 3) + geom_polygon(data = hulls.b.s, alpha = 0.3, aes(fill = factor(elevation)),show.legend = F, linetype = 3) + geom_polygon(data = hulls.b.r, alpha = 0.3, aes(fill = factor(elevation)),show.legend = F, linetype = 3)
p4 <- ggplot(points.b,aes(x = x,y = y, color = elevation)) + geom_point(alpha = 0.8, size = 3) + labs(x=xlab.b, y=ylab.b,title = "PCoA plot of bray curtis distance") + geom_polygon(data = hulls.b, alpha = 0.3, aes(fill = factor(elevation)),show.legend = F, linetype = 3)
