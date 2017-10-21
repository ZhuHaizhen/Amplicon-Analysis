library(readr)
library(tidyr)
library(ggplot2)
library(gridExtra)
dataset <- read_csv("E:/lab/A_Cave/raw_data_Zhang/171017/L2.csv")
L2<-gather(dataset, key = Cave, value = Abundance, - Phylum)
p2 <- ggplot(L2, aes(x = Cave, y = Abundance)) + labs(x="Cave", y="Share of total numbers")+ theme_classic() + geom_bar(stat = "identity", position = "fill", aes(fill= Phylum)) + scale_fill_manual(values = c("plum","lightskyblue","lightyellow4","mistyrose3","moccasin","olivedrab2","orange2","orchid2","palegreen3","palevioletred1","honeydew3","indianred1")) + theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1)) + theme(axis.text.y = element_text(size = 8)) + theme(axis.title = element_text(size = 10)) + theme(legend.title = element_text(size = 10))
dataset <- read_csv("E:/lab/A_Cave/raw_data_Zhang/171017/L3.csv")
L3<-gather(dataset, key = Cave, value = Abundance, - Class)
p3<- ggplot(L3, aes(x = Cave, y = Abundance)) + labs(x="Cave", y="Share of total numbers")+ theme_classic() + geom_bar(stat = "identity", position = "fill", aes(fill= Class)) + scale_fill_manual (values = c("plum2","powderblue","rosybrown1","salmon","seagreen1","seashell","sienna2","skyblue1","slateblue1","slategray","snow4","springgreen3","tan2","thistle3","tomato","turquoise3","violet","wheat","yellow2","aquamarine")) + theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1)) + theme(axis.text.y = element_text(size = 8)) + theme(axis.title = element_text(size = 10)) + theme(legend.title = element_text(size = 10))
p3
p <- grid.arrange(p2,p3,nrow =1)
p
ggsave(p,filename = "stackingbar.pdf",width = 20, height = 15,units = c("cm"))