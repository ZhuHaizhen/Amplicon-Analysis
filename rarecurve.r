args<-commandArgs(T)
if(length(args)<2){
print('Rscript heatmap.r rarecurve.csv rarecurve.pdf')
print('length of args wrong!')
q()
}
infile<-args[1]
outfile<-args[2]

library(vegan)
pdf(outfile,onefile=F,height=7,width=8)
y <- read.table(infile, header=T, row.names=1, sep="")
raremax<-min(rowSums(y)) #给每个样品求和，然后取出和最小的数值

col1 <- c("red", "darkred", "forestgreen", "hotpink", "blue", "darkmagenta","goldenrod2")
col <- rep(col1,10)
lty <- c("solid", "dashed", "longdash", "dotdash")
#lwd <- c(3, 3,3,3,3,3)
pars <- expand.grid(col = col, lty = lty, stringsAsFactors = FALSE)


out <- (rarecurve(y, step = 20, sample = raremax, xlab = "Sequences Per Sample",
     ylab = "Observed Species", col = col, lwd =3, cex = 0.6, label= TRUE))
Nmax <- sapply(out, function(x) max(attr(x, "Subsample")))
Smax <- sapply(out, max)
plot(c(1, max(Nmax)), c(1, max(Smax)), main= "Rarefaction Curve", xlab = "Sequences Per Sample",
     ylab = "Observed Species", lwd = 3, type = "n")
legend("right",legend=row.names(y), col = col, ncol = 2, cex = 0.8, lwd = 3, text.col = col)
#abline(v = raremax)

for (i in seq_along(out)) {
    N <- attr(out[[i]], "Subsample")
    with(pars, lines(N, out[[i]], col = col[i], lty = lty[i], lwd = 3))
}


#pdf(outfile,onefile=F,height=6,width=8)
dev.off()