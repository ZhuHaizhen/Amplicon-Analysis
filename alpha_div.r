library(ggplot2)
library(tidyr)
alpha = read.table("alpha_facet.txt", header=T, row.names= 1, sep="\t")
alpha_facet = gather(alpha,key = diversity, value = indices, -Cave)
theme_set(theme_bw())
p = ggplot(alpha_facet, aes(x=Cave, y=indices, color=Cave)) + geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") +  geom_jitter( position=position_jitter(0.17), size=1, alpha=0.7) + labs(x="Cave", y="Diversity indices") + theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1)) + facet_wrap(~ diversity,scales = "free", ncol = 2)
p
ggsave(p,filename = "alpha diversity.pdf")