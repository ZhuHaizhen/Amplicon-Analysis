alpha <- read.table("alpha.txt", header=T, sep="\t",row.names = 1)
shapiro.test(alpha$shannon)  #p-value = 0.0807
bartlett.test(shannon~status,alpha)  #p-value = 0.3702
lm<- aov(shannon~status,alpha)
summary(lm)  #Pr(>F)=0.0104 *

alpha.in <- read.table("in.alpha.txt", header=T, sep="\t",row.names = 1)
shapiro.test(alpha.in$shannon)  #p-value = 0.07478
bartlett.test(shannon~cave,alpha.in)  #p-value = 0.09618
cave.aov<- aov(shannon~cave,data = alpha.in)
summary(cave.aov)  #p-value > 0.05

bartlett.test(shannon~sample_type,alpha.in)  #p-value = 0.01993
kruskal.test(shannon~sample_type,alpha.in)  #chi-squared = 16.621, df = 3, p-value = 0.0008456

library(pgirmess)
kruskalmc(shannon~sample_type,alpha.in,probs = 0.05)  #Air-Soil TRUE

air<-subset(alpha.in,sample_type=="Air")
shapiro.test(air$shannon)  #p-value = 0.09046
bartlett.test(shannon~cave,air)  #p-value = 0.143
soil<-subset(alpha.in,sample_type=="Soil")
shapiro.test(soil$shannon)  #p-value = 0.2202
bartlett.test(shannon~cave,soil)  #p-value = 0.4259
rock<-subset(alpha.in,sample_type=="Rock")
shapiro.test(rock$shannon)  #p-value = 0.8158
bartlett.test(shannon~cave,rock)  #p-value = 0.7221
water<-subset(alpha.in,sample_type=="Water")
shapiro.test(water$shannon)  #p-value = 0.268
bartlett.test(shannon~cave,water)

lm1<-aov(shannon~cave, data=air)
summary(lm1)  #Pr(>F)=0.0221 *
TukeyHSD(lm1)  #Cave 8-Cave 1 0.0149125
lm2<-aov(shannon~cave, data=soil)
summary(lm2)  #Pr(>F)=0.439
lm3<-aov(shannon~cave, data=rock)
summary(lm3)  #Pr(>F)=0.401
lm4<-aov(shannon~cave, data=water)
summary(lm4)  #Pr(>F)=0.441

cave1<- subset(alpha.in,cave=="Cave 1")
shapiro.test(cave1$shannon)  #p-value = 0.901
bartlett.test(shannon~sample_type,cave1)  #p-value = 0.325
lm5<- aov(shannon~sample_type,data = cave1)
summary(lm5)  #Pr(>F)=0.0013 **
TukeyHSD(lm5)  #Soil-Air   0.0006246, Soil-Rock  0.0498672
cave2<- subset(alpha.in,cave=="Cave 2")
shapiro.test(cave2$shannon)  #p-value = 0.6415
bartlett.test(shannon~sample_type,cave2)  #p-value = 0.5757
lm6<- aov(shannon~sample_type,data = cave2)
summary(lm6)  #Pr(>F)=0.483
cave3<- subset(alpha.in,cave=="Cave 3")
shapiro.test(cave3$shannon)  #p-value = 0.112
bartlett.test(shannon~sample_type,cave3)
lm7<- aov(shannon~sample_type,data = cave3)
summary(lm7)  #Pr(>F)=0.704
cave4<- subset(alpha.in,cave=="Cave 4")
shapiro.test(cave4$shannon)  #p-value = 0.791
bartlett.test(shannon~sample_type,cave4)  #p-value = 0.914
lm8<- aov(shannon~sample_type,data = cave4)
summary(lm8)  #Pr(>F)=0.111
cave5<- subset(alpha.in,cave=="Cave 5")
shapiro.test(cave5$shannon)  #p-value = 0.9913
bartlett.test(shannon~sample_type,cave5)  #p-value = 0.8099
lm9<- aov(shannon~sample_type,data = cave5)
summary(lm9)  #Pr(>F)=0.119
cave6<- subset(alpha.in,cave=="Cave 6")
shapiro.test(cave6$shannon)  #p-value = 0.1853
bartlett.test(shannon~sample_type,cave6)  #p-value = 0.1135
lm10<- aov(shannon~sample_type,data = cave6)
summary(lm10)  #Pr(>F)=0.0772
cave7<- subset(alpha.in,cave=="Cave 7")
shapiro.test(cave7$shannon)  #p-value = 0.4259
bartlett.test(shannon~sample_type,cave7)  #p-value = 0.05941
lm11<- aov(shannon~sample_type,data = cave6)
summary(lm11)  #Pr(>F)=0.0772
cave8<- subset(alpha.in,cave=="Cave 8")
shapiro.test(cave8$shannon)  #p-value = 0.5621
bartlett.test(shannon~sample_type,cave8)  #p-value = 0.7991
lm12<- aov(shannon~sample_type,data = cave8)
summary(lm12)  #Pr(>F)=0.178

library(ggplot2)
library(gridExtra)
theme_set(theme_bw())
p0<- ggplot(alpha, aes(x=status, y=shannon, color=status)) + geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") + labs(x="Sample position", y="Shannon indices") + theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1)) + scale_color_manual(values = c("lightgoldenrod2","orchid"))
p1<- ggplot(alpha.in, aes(x=sample_type, y=shannon, color=sample_type)) + geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") + labs(x="Sample type", y="Shannon indices") + theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1)) + scale_color_brewer(palette = "Set2")
p2<- ggplot(alpha.in, aes(x=cave, y=shannon, color=cave)) + geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") + labs(x="Cave", y="Shannon indices") + theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1)) + facet_wrap(~ sample_type,scales = "free", ncol = 2) + scale_color_brewer(palette = "Paired")
plots<- list(p0,p1,p2)
lay<- rbind(c(1,2),c(3,3))
p<- grid.arrange(grobs = plots,layout_matrix = lay,widths = c(0.6,1))
ggsave(p,filename = "shannon.pdf",width = 8,height = 10)

p3<- ggplot(alpha.in, aes(x=cave, y=observed_otus, color=cave)) + geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") + labs(x="Cave", y="Observed OTUs") + theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1)) + facet_wrap(~ sample_type,scales = "free", ncol = 2) + theme(legend.position = "none") + scale_color_brewer(palette = "Paired")
p4<- ggplot(alpha.in, aes(x=cave, y=chao1, color=cave)) + geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") + labs(x="Cave", y="Chao1 indices") + theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1)) + facet_wrap(~ sample_type,scales = "free", ncol = 2) + scale_color_brewer(palette = "Paired")
p5<- ggplot(alpha.in, aes(x=cave, y=simpson, color=cave)) + geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") + labs(x="Cave", y="Simpson indices") + theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1)) + facet_wrap(~ sample_type,scales = "free", ncol = 2) + theme(legend.position = "none") + scale_color_brewer(palette = "Paired")
p6<- ggplot(alpha.in, aes(x=cave, y=PD_whole_tree, color=cave)) + geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") + labs(x="Cave", y="PD whole tree indices") + theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1)) + facet_wrap(~ sample_type,scales = "free", ncol = 2) + scale_color_brewer(palette = "Paired")
pp<- grid.arrange(p3,p4,p5,p6,nrow = 2,widths = c(0.8,1))
ggsave(pp,filename = "alpha_other.pdf",width = 10,height = 10)

