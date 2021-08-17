##Walkthrough
##http://environmentalcomputing.net/introduction-to-mvabund/ 

##References
##https://besjournals.onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2011.00127.x
##https://cran.microsoft.com/snapshot/2020-04-15/web/packages/mvabund/mvabund.pdf 

library(mvabund)

###Simple models one factor

Small.data<-cbind(sm.fact,sm.df)
par(mar=c(10,10,2,2)) # adjusts the margins
boxplot(Small.data[,6:26],horizontal = TRUE,las=2, main="Abundance")
Small_spp <- mvabund(Small.data[,6:26])
meanvar.plot(Small_spp)


mod3 <- manyglm(Small_spp ~ Small.data$Status, family="poisson")
plot(mod3)
anova.manyglm(mod3)
anova.manyglm(mod3, p.uni="adjusted")


Medium.data<-cbind(med.fact,med.df)
par(mar=c(2,10,2,2)) # adjusts the margins
boxplot(Medium.data[,6:94],horizontal = TRUE,las=2, main="Abundance")
Medium_spp <- mvabund(Medium.data[,6:94])
meanvar.plot(Medium_spp)

mod1 <- manyglm(Medium_spp ~ Medium.data$Status, family="poisson")
plot(mod1)
anova.manyglm(mod1)
anova.manyglm(mod1, p.uni="adjusted")


Large.data<-cbind(lar.fact,lar.df)
Large_spp <- mvabund(Large.data[,6:81])
par(mar=c(10,10,2,2)) # adjusts the margins
boxplot(Large.data[,6:81],horizontal = TRUE,las=2, main="Abundance", ylim = c(0,10))

meanvar.plot(Large_spp)

mod2 <- manyglm(Large_spp ~ Large.data$Status, family="negative_binomial")
plot(mod2)
anova.manyglm(mod2)
anova.manyglm(mod2, p.uni="adjusted")

library(texreg)

###complex models


mod4 <- manyglm(Large_spp ~ Large.data$Status, family="negative_binomial")
plot(mod4)
anova.manyglm(mod4)
anova.manyglm(mod4, p.uni="adjusted", nBoot=99, test= "LR")
summary.manyglm(mod4, p.uni="adjusted", nBoot=99, test= "LR")

Largeresults <- anova(mod4, resamp="perm.resid", p.uni="adjusted")
