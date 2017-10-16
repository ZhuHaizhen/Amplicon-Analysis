args = commandArgs(T)

if (length(args) != 2){
				print("Rscript barplot.r <InFile> <OutFile> <Title> <ylab> <beside(T/F)> <ymin> <ymax>")
				print("Example : Rscript barplot.r test.txt test.pdf Test Percentage T 0 100")
				q()
}
file = args[1]
out = args[2]
data<-read.table(file,header=T,row.names=1,check.names=F)
d<-dist(t(data))
# "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski" ,default is euclidean
dt<-hclust(d,method='average')
#"ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC),default is complete
pdf(out,w=20,h=18)
par(cex.main=2.5,cex.lab=2,cex.axis=1.5,cex=2,lwd=2)
plot(dt)
dev.off()


