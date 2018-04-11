alpha.in <- read.table("in.alpha.txt", header=T, sep="\t",row.names = 1)
shapiro.test(alpha.in$shannon) #W = 0.98132, p-value = 0.07478
bartlett.test(shannon~elevation,alpha.in) #Bartlett's K-squared = 9.8389, df = 2, p-value = 0.007303
kruskal.test(shannon~elevation,alpha.in) #Kruskal-Wallis chi-squared = 0.9767, df = 2, p-value = 0.6136

bartlett.test(shannon~sample_type,alpha.in) #Bartlett's K-squared = 9.8456, df = 3, p-value = 0.01993
kruskal.test(shannon~sample_type,alpha.in) # significant, Kruskal-Wallis chi-squared = 16.621, df = 3, p-value = 0.0008456

air.in.shannon<-subset(alpha.in,sample_type=="Air")
water.in.shannon<-subset(alpha.in,sample_type=="Water")
soil.in.shannon<-subset(alpha.in,sample_type=="Soil")
rock.in.shannon<-subset(alpha.in,sample_type=="Rock")

wilcox.test(air.in.shannon$shannon,water.in.shannon$shannon) #W = 272, p-value = 0.2208
wilcox.test(air.in.shannon$shannon,soil.in.shannon$shannon) # significant, W = 310, p-value = 5.41e-05
wilcox.test(air.in.shannon$shannon,rock.in.shannon$shannon) #W = 508, p-value = 0.1166
wilcox.test(water.in.shannon$shannon,rock.in.shannon$shannon) #W = 345, p-value = 0.965
wilcox.test(water.in.shannon$shannon,soil.in.shannon$shannon) #W = 269, p-value = 0.1572
wilcox.test(rock.in.shannon$shannon,soil.in.shannon$shannon) # significant, W = 398, p-value = 0.002799

shapiro.test(air.in.shannon$shannon) #W = 0.948, p-value = 0.09046
bartlett.test(shannon~elevation,air.in.shannon) #Bartlett's K-squared = 6.0277, df = 2, p-value = 0.0491
kruskal.test(shannon~elevation,air.in.shannon) #Kruskal-Wallis chi-squared = 3.6231, df = 2, p-value = 0.1634

shapiro.test(water.in.shannon$shannon) #W = 0.94039, p-value = 0.268
bartlett.test(shannon~elevation,water.in.shannon) #Bartlett's K-squared = 1.6081, df = 2, p-value = 0.4475
kruskal.test(shannon~elevation,water.in.shannon) #Kruskal-Wallis chi-squared = 3.0186, df = 2, p-value = 0.2211
lm.water.in<-aov(shannon~elevation, data=water.in.shannon)
summary(lm.water.in) #F value=1.474,p-value=0.259

shapiro.test(soil.in.shannon$shannon) #W = 0.96118, p-value = 0.2202
bartlett.test(shannon~elevation,soil.in.shannon) #Bartlett's K-squared = 0.72886, df = 2, p-value = 0.6946
kruskal.test(shannon~elevation,soil.in.shannon) #Kruskal-Wallis chi-squared = 1.5615, df = 2, p-value = 0.4581
lm.soil.in<-aov(shannon~elevation, data=soil.in.shannon)
summary(lm.soil.in) #F value=0.266,p-value=0.768

shapiro.test(rock.in.shannon$shannon) #W = 0.98216, p-value = 0.8158
bartlett.test(shannon~elevation,rock.in.shannon) #Bartlett's K-squared = 1.6344, df = 2, p-value = 0.4417
kruskal.test(shannon~elevation,rock.in.shannon) #Kruskal-Wallis chi-squared = 2.3947, df = 2, p-value = 0.302
lm.rock.in<-aov(shannon~elevation, data=rock.in.shannon)
summary(lm.rock.in) #F value-1.746,p-value=0.19

low.in.shannon<-subset(alpha.in,elevation=="Low")
medium.in.shannon<-subset(alpha.in,elevation=="Medium")
high.in.shannon<-subset(alpha.in,elevation=="High")

shapiro.test(low.in.shannon$shannon) #W = 0.97561, p-value = 0.7006
bartlett.test(shannon~sample_type,low.in.shannon) #Bartlett's K-squared = 3.7967, df = 3, p-value = 0.2843
kruskal.test(shannon~sample_type,low.in.shannon) #Kruskal-Wallis chi-squared = 5.0029, df = 3, p-value = 0.1716
lm.low.in<-aov(shannon~sample_type, data=low.in.shannon)
summary(lm.low.in) #F value=1.591,p-value=0.216

shapiro.test(medium.in.shannon$shannon) #W = 0.97973, p-value = 0.3152
bartlett.test(shannon~sample_type,medium.in.shannon) #Bartlett's K-squared = 5.0788, df = 3, p-value = 0.1661
kruskal.test(shannon~sample_type,medium.in.shannon) # significant, Kruskal-Wallis chi-squared = 15.359, df = 3, p-value = 0.001534
lm.medium.in<-aov(shannon~sample_type, data=medium.in.shannon)
summary(lm.medium.in) # F value=6.261,p-value=0.000834
TukeyHSD(lm.medium.in) # p(Soil-Air)=0.0003830,p(Soil-Rock)=0.0726691

shapiro.test(high.in.shannon$shannon) #W = 0.97397, p-value = 0.69
bartlett.test(shannon~sample_type,high.in.shannon) #Bartlett's K-squared = 3.1872, df = 3, p-value = 0.3637
kruskal.test(shannon~sample_type,high.in.shannon) #Kruskal-Wallis chi-squared = 4.2057, df = 3, p-value = 0.2401
lm.high.in<-aov(shannon~sample_type, data=high.in.shannon)
summary(lm.high.in) #F value=2.14,p-value=0.122

library(ggplot2)
theme_set(theme_bw())

alpha.in$elevation<-factor(alpha.in$elevation,levels = c("Low","Medium","High"))
alpha.in$sample_type<-factor(alpha.in$sample_type,levels = c("Air","Rock","Soil","Water"))

p1<- ggplot(alpha.in, aes(x=sample_type, y=shannon, color=elevation)) + geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") + labs(x="Sample type", y="Shannon indices") + theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1)) + scale_color_brewer(palette = "Set2") + scale_x_discrete(limits=c("Air","Water","Soil","Rock"))
p1

p2<- ggplot(alpha.in, aes(x=elevation, y=shannon, color=sample_type)) + geom_boxplot(alpha=1, outlier.size=0, size=0.7, width=0.5, fill="transparent") + labs(x="Sample type", y="Shannon indices") + theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1)) + scale_color_brewer(palette = "Set2") + scale_x_discrete(limits=c("Low","Medium","High"))
p2

p3<-boxplot(shannon~sample_type+elevation,alpha.in,col=c("palevioletred1","palegreen","paleturquoise1","mediumorchid1"),xlab="Elevation",ylab="Shannon indices",boxwex=0.7,par(las=0),xaxt="n")
axis(side = 1, at = c(2.5,6.5,10.5), labels = c( "Low", "Medium","High"))
legend("bottomleft",c("Air","Rock","Soil","Water"),fill=c("palevioletred1","palegreen","paleturquoise1","mediumorchid1"),cex=0.85)
abline(v=4.5,lty=3)
abline(v=8.5,lty=3)

p4<-boxplot(shannon~elevation+sample_type,alpha.in,col=c("lightgoldenrod1","lightsalmon","lightpink1"),xlab="Sample type",ylab="Shannon indices",boxwex=0.7,par(las=0),xaxt="n")
axis(side = 1, at = c(2,5,8,11), labels = c( "Air", "Rock","Soil","Water"))
legend("bottomleft",c("Low","Medium","High"),fill=c("lightgoldenrod1","lightsalmon","lightpink1"),cex=0.75,title="Elevation",horiz = FALSE)
abline(v=3.5,lty=3)
abline(v=6.5,lty=3)
abline(v=9.5,lty=3)
