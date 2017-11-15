library(indicspecies)
vignette("indicspeciesTutorial", package="indicspecies")
ind <- read.table("cave_indval.txt",fill = TRUE,header = TRUE,row.names = 1,check.names = FALSE)
ind.mat <- as.matrix(ind)

library(vegetarian)
ind.rel <- normalize.rows(ind)
bind.rel <- cbind(ind.rel,ind[,0])
f <- ind[0,]
colnames(bind.rel) = colnames(f)

grp <- c(rep(1,21),rep(2,18),rep(3,16),rep(4,14),rep(5,17),rep(6,14),rep(7,18),rep(8,10))
indval <- multipatt(bind.rel,grp,func = "IndVal",duleg = TRUE,control = how(nperm = 999))
summary(indval,indvalcomp = TRUE)