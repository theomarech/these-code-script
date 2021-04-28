## you require the following R packages
## pcalg, randomForest, party, glmnet
## and (for maximum likelihood) the regr-Funktion from
## http://stat.ethz.ch/~stahel/regression/regr.R

## generate the DAG
set.seed(1)

## create a DAG
require(pcalg)

G <- randomDAG(n=50, prob=0.01,lB=0.1,uB=1)
## n: number of nodes
## prob: edge probability (i.e. the smaller the sparser the graph)
## lB, uB: lower and upper bound for entries in the adjacency matrix (the larger these numbers, the stronger the relationships among variables)


## generate binary data according to this DAG
source("generate.binary.data.R")

## Note: generate.gaussian.data.R and generate.mixed.data.R are also available as R function though not used in this small tutorial

## generate a data frame with binary data from that DAG
dat.bin <- rmvDAGBinary(100, weightMatrix= wgtMatrix(G))

## StabLASSO (based on glmnet) actually requires a different data format
dat.bin.num <- NULL
for (i in 1:ncol(dat.bin))
    dat.bin.num <- cbind(dat.bin.num,as.numeric(dat.bin[,i]))
dat.bin.num <- ifelse(dat.bin.num==2,1,-1)



## the functions for the LASSO-based estimation
source("glmnet.R")
source("GRaFo.R")
source("cforest.R")
source("glm.R")
source("additional_functions.R")



##########################
## The n.subset argument #
##########################
## In the following, if n.subset = 1 all data is used, hence this allows to obtain the "raw" estimation (without stability selection)
## For stability selection, we always used n.subset = 100 which performs 100x subsampling, each with size floor(n/2) of the total sample


## ##########################
## The Estimation Procedures
## ##########################
# - see n.subset comments above -

#############
# StabLASSO #
#############
# works also for Gaussian data with family = "gaussian"
# data that had more than 2 levels was dichotomized and the family = "binomial" used
# note the different data format for StabLASSO (no factor coding)
est.StabLASSO <- glmnet.stab.sel(data=dat.bin.num, n.subsets=1, family="binomial", alpha=1)

#########
# GRaFo #
#########
## GRaFo and the other algorithms require data frames, where factors are coded as such
## data is a data frame where factors are coded as such
## no special arguments have provided to differentiate among mixed or purely numeric/purely factor-type data sets (this information is obtained directly from the variable type, i.e. numeric or factor)
est.GRaFO <- GRaFo(data=dat.bin, ntree=500, n.subsets=1)
#est.GRaFO <- GRaFo(data=dat.bin, ntree=500, n.subsets=100)

###############
# StabcForest #
###############
# data: same as GRaFo
# set cond.var.imp=TRUE if you want to obtain conditional variable importanance (to our experience that is *extremely* slow) and cond.var.imp=FALSE for the marginal importance also used by GRaFo
est.StabcForest <- cforest.stab.sel(data=dat.bin, ntree=500, n.subsets=1, cond.var.imp=FALSE)

#######################
## Maximum Likelihood #
#######################
## the ML estimation requires the function regr.R that can be obtained here: http://stat.ethz.ch/~stahel/regression/regr.R
est.ML <- glm.stab.sel(dat.bin, n.subsets=1)
# we ultimately used this only in the Mixed data case without stability selection (by setting n.subsets=1)

########################
## How to obtain a CIG #
########################
# We have to decide on E(V) (i.e. the expected number of false edges)
# ev.vec allows us to specify a whole vector of E(V) values simultaneously
CIG.StabLASSO <- get.CIG.estimate(est.StabLASSO, ev.vec=c(1,5,20))
CIG.GRaFo <- get.CIG.estimate(est.GRaFO, ev.vec=c(1,5,20))

## This function returns different CIG estimates, one for each E(V) bound
## E(V) is the number of expected false positives
## Hence, the smaller E(V), the smaller the estimated graph will be
## The result is the adjacency matrix of a CIG that can e.g. be visualized using the igraph R package
## Reading example: if there is a 1 at position [1,8] then nodes 1 and 8 are dependent conditional all the other variables

## very basic igraph example
require(igraph)
tkplot(graph.adjacency(CIG.GRaFo[[1]]))
tkplot(graph.adjacency(CIG.GRaFo[[2]]))
## Note: The stability mechanics will, of course, only work if n.subset is set to 100 above, otherwise way too many false positive edges
