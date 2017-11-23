library(tidyr)
library(ggplot2)

l3 <- read.table("L3.in.txt",header=T, sep="\t")
class <- gather(l3, key = Cave, value = Abundance, - Class)
p3 <- ggplot(class,aes(x=Cave, y=Class)) + theme_classic() + geom_tile(aes(fill=Abundance),color = "white") + theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1)) + theme(axis.text.y = element_text(face="italic")) + theme(axis.text.y = element_text(size = 8)) + theme(axis.title = element_text(size = 10)) + theme(legend.title = element_text(size = 10)) + scale_fill_gradient(low = "lightblue",high = "lightcoral")

l2 <- read.table("L2.in.txt",header=T, sep="\t")
phylum<-gather(l2, key = Cave, value = Abundance, - Phylum)
p2 <- ggplot(phylum,aes(x=Cave, y=Phylum)) + theme_classic() + geom_tile(aes(fill=Abundance),color = "white") + theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1)) + theme(axis.text.y = element_text(face="italic")) + theme(axis.text.y = element_text(size = 8)) + theme(axis.title = element_text(size = 10)) + theme(legend.title = element_text(size = 10)) + scale_fill_gradient(low = "lightblue",high = "lightcoral")

l5 <- read.table("L5.in.txt",header=T, sep="\t")
family <- gather(l5, key = Cave, value = Abundance, - Family)
p5 <- ggplot(family,aes(x=Cave, y=Family)) + theme_classic() + geom_tile(aes(fill=Abundance),color = "white") + theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1)) + theme(axis.text.y = element_text(face="italic")) + theme(axis.text.y = element_text(size = 8)) + theme(axis.title = element_text(size = 10)) + theme(legend.title = element_text(size = 10)) + scale_fill_gradient(low = "lightblue",high = "lightcoral")

ggsave(p3,filename = "heatmap_class.pdf",width = 10,height = 8)