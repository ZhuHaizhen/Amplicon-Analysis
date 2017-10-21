library(vegan)
otu = read.table("NMDS.txt",header = T,row.names = 1,sep = "\t")
design = read.table("design_NMDS.txt", header=T, row.names= 1, sep="\t")
decorana(otu)
otu.cca = cca(otu,design)
otu.cca
plot(otu.cca,display=c("sp","bp"),scaling=3)
permutest(otu.cca,permu=999)
ef=envfit(otu.cca,design,permu=999)
ef