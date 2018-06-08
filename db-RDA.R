library(ggvegan)
otu.tab <- read.csv("otutab.txt", row.names = 1, header=T, sep="\t")
env.data <- read.csv("new_meta.txt", row.names = 1, fill = T, header=T, sep="\t")

#transform data
otu <- t(otu.tab)
#data normolization (Legendre and Gallagher,2001)
##by log
env.data.log <- log1p(env.data)
##delete NA
env <- na.omit(env.data.log)

###hellinger transform
otu.hell <- decostand(otu, "hellinger")

#DCA analysis  
sel <- decorana(otu.hell)
sel

otu.tab.0 <- rda(otu.hell ~ 1, env) #no variables

otu.tab.1<- rda(otu.hell ~ ., env)

#Variance Inflation Factor analysis
vif.cca(otu.tab.1)

#delete collinear factors, delete the largest variant, till all variants are less than 10
otu.tab.1 <- rda(otu.hell ~ N+P+K+Ca+Mg+pH+Al+Fe+Mn+Zn+Mo, env.data.log)

vif.cca(otu.tab.1)
otu.tab.1 <- rda(otu.hell ~ N+P+K+Mg+pH+Al+Fe+Mn+Zn+Mo, env.data.log)
vif.cca(otu.tab.1)
otu.tab.1 <- rda(otu.hell ~ N+P+K+Mg+pH+Fe+Mn+Zn+Mo, env.data.log)
vif.cca(otu.tab.1)#all variants are less than 10

# determine the lowest AIC by step model
mod.u <- step(otu.tab.0, scope = formula(otu.tab.1), test = "perm")
mod.d <- step(otu.tab.0, scope = (list(lower = formula(otu.tab.0), upper = formula(otu.tab.1))))
mod.d # Mg is the best factor, but we keep all non-colinear factors for plotting

#choose variables for best model and rda analysis again#
otu.rda.f <- rda(otu.hell ~ N+P+K+Mg+pH+Fe+Mn+Zn+Mo, env)

anova(otu.rda.f)
anova(otu.rda.f, by = "term")
anova(otu.rda.f, by = "axis")

p<- autoplot(otu.rda.f, arrows = TRUE,axes = c(1, 2), geom =  c("point", "text"), layers = c( "species","sites", "biplot", "centroids"), legend.position = "right", title = "db-RDA")
p + theme_bw()+theme(panel.grid=element_blank())