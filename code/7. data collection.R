
# Experimental Design -----------------------------------------------------

  ## Power analysis
library(pwr)
pwr.t.test(n=NULL, sig.level=0.05, type="paired", alternative="two.sided", power=0.8, d=0.3)

  ## Latin Square design
library(SensoMineR)
wdes_5P10J <- WilliamsDesign(5)

wdes_5P20J <- do.call(rbind, replicate(2, wdes_5P10J, simplify=FALSE))
rownames(wdes_5P20J) <- paste("judge", 1:20, sep="")

library(AlgDesign)
nbJ=13
nbP=5
nbR=nbP

wdes_5P10J <- WilliamsDesign(nbP)
tab <- cbind(prod=as.vector(t(wdes_5P10J)), judge=rep(1:nbJ,each=nbR), rank=rep(1:nbR,nbJ))
optdes_5P13J <- optFederov(~prod+judge+rank, data=tab, augment=TRUE, nTrials=nbJ*nbP, rows=1:(nbJ*nbP), nRepeats = 100)
xtabs(optdes_5P13J$design)

nbJ=13
nbP=5
nbR=nbP

optdes_5P13J <- optimaldesign(nbP, nbP, nbR)$design
tab <- cbind(prod=as.vector(t(optdes_5P13J)),judge=rep(1:nbJ,each=nbR),rank=rep(1:nbR,nbJ))
add <- cbind(prod=rep(1:nbP,nbR),judge=rep(nbJ+1,nbP*nbR),rank=rep(1:nbR,each=nbP))
optdes_5P14J <- optFederov(~prod+judge+rank,data=rbind(tab,add), augment=TRUE, nTrials=(nbJ+1)*nbP,
                           rows=1:(nbJ*nbP), nRepeats = 100)

  ## Balanced Incomplete Block Design
