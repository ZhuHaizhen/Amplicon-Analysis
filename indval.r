library(indicspecies)
vignette("indicspeciesTutorial", package="indicspecies")
ind <- read.table("cave.in_indval.txt",fill = TRUE,header = TRUE,row.names = 1,check.names = FALSE)
ind.mat <- as.matrix(ind)

library(vegetarian)
ind.rel <- normalize.rows(ind)
bind.rel <- cbind(ind.rel,ind[,0])
f <- ind[0,]
colnames(bind.rel) = colnames(f)

ind.a <- read.table("cave_indval.txt",fill = TRUE,header = TRUE,row.names = 1,check.names = FALSE)

ind.rel.a <- normalize.rows(ind.a)
bind.rel.a <- cbind(ind.rel.a,ind.a[,0])
f.a <- ind.a[0,]
colnames(bind.rel.a) = colnames(f.a)

grp.stat <- c(rep(1,21),rep(2,128))
indval.stat <- multipatt(bind.rel.a,grp.stat,func = "IndVal",duleg = TRUE,control = how(nperm = 999))
summary(indval.stat,indvalcomp = TRUE)

grp.cave <- c(rep(1,21),rep(2,18),rep(3,16),rep(4,14),rep(5,17),rep(6,14),rep(7,18),rep(8,10))
indval.cave <- multipatt(bind.rel,grp.cave,func = "IndVal",duleg = TRUE,control = how(nperm = 999))
summary(indval.cave,indvalcomp = TRUE)

grp.samp <- c(rep(1,6),rep(2,6),rep(3,6),rep(4,3),rep(1,4),rep(2,5),rep(3,6),rep(4,3),rep(1,5),rep(2,5),rep(3,5),rep(4,1),rep(1,4),rep(2,4),rep(3,4),rep(4,2),rep(1,5),rep(2,5),rep(3,4),rep(4,3),rep(1,4),rep(2,4),rep(3,4),rep(4,2),rep(1,5),rep(2,5),rep(3,5),rep(4,3),rep(1,3),rep(2,2),rep(3,3),rep(4,2))
indval.samp <- multipatt(bind.rel,grp.samp,func = "IndVal",duleg = TRUE,control = how(nperm = 999))
summary(indval.samp,indvalcomp = TRUE)