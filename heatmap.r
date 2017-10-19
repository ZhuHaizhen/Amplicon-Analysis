library(readr)
library(tidyr)
library(ggplot2)
dataset <- read_csv("E:/lab/A_Cave/raw_data_Zhang/171017/L2.csv")
phylum<-gather(dataset, key = Cave, value = Abundance, - Phylum)
p<-ggplot(phylum,aes(x=Cave, y=Phylum),labs(x= "Cave", y= "Phylum")) + theme_classic() + geom_tile(aes(fill=Abundance)) + theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1)) +theme(axis.text.y = element_text(face="italic")) + theme(axis.text.y = element_text(size = 8))  +theme(axis.title = element_text(size = 10)) + theme(legend.title = element_text(size = 10))+ scale_fill_gradient(low = "lightskyblue",high = "lightsteelblue4")
p
ggsave(p,filename = "heatmap.pdf",width = 10, height = 10,units = c("cm"))