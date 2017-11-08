alpha <- read.table("alpha.txt", header=T, sep="\t")
rownames(alpha)<-alpha$sampleID
alpha.out<-alpha[c("C1A01","C1S01","C2S01","G1A01","G1R01","G1S01","G3A01","G3R01","G3S01","S7A02","S7S01","S7S02","S8A01","S8S01","Y2A01","Y2R01","Y2S01","Y3W01","Y3A01","Y3R01","Y3S01"),]
alpha.in<-alpha[setdiff(row.names(alpha),row.names(alpha.out)),]

shapiro.test(alpha$shannon)  #p-value = 0.0807
bartlett.test(shannon~status,alpha)  #p-value = 0.3702
lm<- aov(shannon~status,alpha)
summary(lm)  #p-value = 0.0104 **

shapiro.test(alpha.in$shannon)  #p-value = 0.1411 normal
bartlett.test(shannon~cave,alpha.in) #p-value = 0.1384
cave.aov<- aov(shannon~cave,data = alpha.in)
summary(cave.aov)  #p-value > 0.05

bartlett.test(shannon~sample_type,alpha.in)  #p-value = 0.02074
kruskal.test(shannon~sample_type,alpha.in)  #chi-squared = 17.467, df = 3,p-value = 0.0005665 ***

#Nemenyi pairwise test
library(pgirmess)
library(coin)
library(multcomp)
kruskalmc(shannon~sample_type,alpha.in,probs = 0.05)  #Air-Rock   11.944444     23.06695      FALSE,Air-Soil   35.608108     22.91056       TRUE,Air-Water  15.815789     27.75107      FALSE,Rock-Soil  23.663664     22.91056       TRUE,Rock-Water  3.871345     27.75107      FALSE,Soil-Water 19.792319     27.62121      FALSE
mult<- independence_test(shannon ~ sample_type, data = alpha.in,ytrafo = function(data) trafo(data, numeric_trafo = rank),xtrafo = function(data) trafo(data, factor_trafo = function(x)model.matrix(~x - 1) %*% t(contrMat(table(x), "Tukey"))),teststat = "max", distribution = approximate(B = 90000)) 
pvalue(mult, method = "single-step")  #Rock - Air   0.5209777778,Soil - Air   0.0001111111,Water - Air  0.1431333333,Soil - Rock  0.0271666667,Water - Rock 0.9526666667,Water - Soil 0.0334222222

air<-subset(alpha.in,sample_type=="Air")
shapiro.test(air$shannon)  #p-value = 0.07229
bartlett.test(shannon~cave,air)  #p-value = 0.08787
soil<-subset(alpha.in,sample_type=="Soil")
shapiro.test(soil$shannon)  #p-value = 0.2253
bartlett.test(shannon~cave,soil)  #p-value = 0.469
rock<-subset(alpha.in,sample_type=="Rock")
shapiro.test(rock$shannon)  #p-value = 0.9732
bartlett.test(shannon~cave,rock)  #p-value = 0.7817
water<-subset(alpha.in,sample_type=="Water")
shapiro.test(water$shannon)  #p-value = 0.336
bartlett.test(shannon~cave,water)  

lm1<-aov(shannon~cave, data=air)
summary(lm1)  #p-value = 0.0132 *
TukeyHSD(lm1)  #Cave 2-Cave 1 0.9995259,Cave 3-Cave 1 0.1197331,Cave 4-Cave 1 0.2942780,Cave 5-Cave 1 0.2959676,Cave 6-Cave 1 0.1943033,Cave 7-Cave 1 0.3286307,Cave 8-Cave 1 0.0104958,Cave 3-Cave 2 0.4419694,Cave 4-Cave 2 0.6874214,Cave 5-Cave 2 0.7188977,Cave 6-Cave 2 0.5498916,Cave 7-Cave 2 0.7534732,Cave 8-Cave 2 0.0578450,Cave 4-Cave 3 0.9999850,Cave 5-Cave 3 0.9996933,Cave 6-Cave 3 1.0000000,Cave 7-Cave 3 0.9993038,Cave 8-Cave 3 0.8369481,Cave 5-Cave 4 0.9999999,Cave 6-Cave 4 0.9999984,Cave 7-Cave 4 0.9999990,Cave 8-Cave 4 0.7281799,Cave 6-Cave 5 0.9999356,Cave 7-Cave 5 1.0000000,Cave 8-Cave 5 0.6007343,Cave 7-Cave 6 0.9998270,Cave 8-Cave 6 0.8367049,Cave 8-Cave 7 0.5660884
lm2<-aov(shannon~cave, data=soil)
summary(lm2)  #p-value = 0.533
lm3<-aov(shannon~cave, data=rock)
summary(lm3)  #p-value = 0.325
lm4<-aov(shannon~cave, data=water)
summary(lm4)  #p-value = 0.449

cave1<- subset(alpha.in,cave=="Cave 1")
shapiro.test(cave1$shannon)  #p-value = 0.9202
bartlett.test(shannon~sample_type,cave1)  #p-value = 0.317
lm5<- aov(shannon~sample_type,data = cave1)
summary(lm5)  #p-value = 0.00116 **
TukeyHSD(lm5)  #Rock-Air 0.1892265,Soil-Air 0.0005562,Water-Air 0.3223726,Soil-Rock 0.0441296,Water-Rock 0.9999710,Water-Soil 0.1301283
cave2<- subset(alpha.in,cave=="Cave 2")
shapiro.test(cave2$shannon)  #p-value = 0.5513
bartlett.test(shannon~sample_type,cave2)  #p-value = 0.5046
lm6<- aov(shannon~sample_type,data = cave2)
summary(lm6)  #p-value = 0.423
cave3<- subset(alpha.in,cave=="Cave 3")
shapiro.test(cave3$shannon)  #p-value = 0.02829
kruskal.test(shannon~sample_type,cave3)  #p-value = 0.4774
cave4<- subset(alpha.in,cave=="Cave 4")
shapiro.test(cave4$shannon)  #p-value = 0.7179
bartlett.test(shannon~sample_type,cave4)  #p-value = 0.9406
lm7<- aov(shannon~sample_type,data = cave4)
summary(lm7)  #p-value = 0.104
cave5<- subset(alpha.in,cave=="Cave 5")
shapiro.test(cave5$shannon)  #p-value = 0.9325
bartlett.test(shannon~sample_type,cave5)  #p-value = 0.8232
lm8<- aov(shannon~sample_type,data = cave5)
summary(lm8)  #p-value = 0.121
cave6<- subset(alpha.in,cave=="Cave 6")
shapiro.test(cave6$shannon)  #p-value = 0.1811
bartlett.test(shannon~sample_type,cave6)  #p-value = 0.1031
lm9<- aov(shannon~sample_type,data = cave6)
summary(lm9)  #p-value = 0.0561
cave7<- subset(alpha.in,cave=="Cave 7")
shapiro.test(cave7$shannon)  #p-value = 0.4522
bartlett.test(shannon~sample_type,cave7)  #p-value = 0.04255
kruskal.test(shannon~sample_type,cave7)  #p-value = 0.4591
cave8<- subset(alpha.in,cave=="Cave 8")
shapiro.test(cave8$shannon)  #p-value = 0.6269
bartlett.test(shannon~sample_type,cave8)  #p-value = 0.799
lm10<- aov(shannon~sample_type,data = cave8)
summary(lm10)  #p-value = 0.174

library(ggplot2)
library(gridExtra)
theme_set(theme_bw())
p0<- ggplot(alpha.in, aes(x=status, y=shannon, color=status)) + geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") + labs(x="Sample position", y="Shannon indices") + theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1)) + scale_color_manual(values = c("lightgoldenrod2","orchid"))
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