## How-to use:

set.seed(123)
# min & max number of categories
C.min <- 3
C.max <- 5
n <- 100 # sample size
## generate the DAG
p <- 50 # number of nodes
s <- 0.01 # sparsity (the smaller the sparser the graph)
var <- 1 # variance of the error
require(pcalg)
G <- randomDAG(n=p, prob=s,lB=0.1,uB=1)
weightMatrix <- wgtMatrix(G)
## here we include signs for the weight matrix
signs <- matrix(sample(c(-1,1), p*p, replace=TRUE), nrow=p, ncol=p)
signs[lower.tri(signs)] <- 0
signs <- signs+t(signs)
weightMatrix <- signs * weightMatrix

## here we generate the no. of categories for each variable
C.vector <- sample(C.min:C.max,p,replace=TRUE)

## here we generate the Psi and Phi sign vectors as described
## in the publication
get.effect.signs <- function(no.cats)
{
    effect <- NULL
    while (length(unique(effect))<2)
        effect <- sample(c(1,-1),no.cats,replace=TRUE)
    return(effect)
}

Psi.signs <- list()
Phi.signs <- list()
for (j in 1:p)
{
    Psi.signs[[j]] <- list()
    Phi.signs[[j]] <- list()
    for (i in 1:p)
            if (weightMatrix[j,i]!=0)
            {
                Psi.signs[[j]][[i]] <- get.effect.signs(C.vector[i])
                Phi.signs[[j]][[i]] <- get.effect.signs(C.vector[j])
            }
}




## we now define the main function which is then executed afterwards:

rmvDAGMixed <- function (n, weightMatrix, C.vector, Psi.signs, Phi.signs,C.min, C.max, errMat, dichotomous=FALSE)
{
    ## Note: dichotomous==TRUE is only needed if the data is supposed to be used by the StabLASSO algorithm (categories are then collapsed as outlined in the paper)

    p <- ncol(weightMatrix)
    if ((ncol(errMat) != p/2) | nrow(errMat)!=n)
        stop("errMat has the wrong format")

    ## ############################
    ## HELPER FUNCTIONS
    ## ############################

    matrix.to.vector <- function(mat)
    {
        x.vec <- rep(0,nrow(mat))
        for (i in 1:ncol(mat))
        {
            x.vec[mat[,i]==1] <- i
        }
        return(x.vec)
    }

    sim.multinom.X <- function(pi.vec, n) {
        return(t(rmultinom(n=n, size = 1, prob=pi.vec)))
    }

    get.individual.effect <- function(a,X.vec,Psi.signs)
    {
        ## The strength of the effect of X_i on X_j
        ## here, we do not, yet, take Phi into account
        has.cats <- unique(X.vec)[order(unique(X.vec))]
        effect.signs <- Psi.signs[has.cats]
        indiv.effect <- 0

        index <- 1
        for (k in has.cats)
        {
            indiv.effect <- indiv.effect + a * ifelse(X.vec==k,1,-1) * effect.signs[index]
            index <- index + 1
        }
        return(indiv.effect)
    }





    p <- nrow(weightMatrix)

    types <- rep(c("G","M"),times=p/2)

    X <- matrix(0, n, p)
    X[, 1] <- errMat[, 1]
    error.index <- 2

    for (j in 2:p)
    {
        if (types[j] == "G")
        {
            eta <- matrix(0,nrow=n,ncol=1)
            for (i in 1:(j-1))
            {
                if (weightMatrix[j,i] != 0)
                {
                    if (types[i]=="G")
                        eta <- eta + weightMatrix[j,i] * X[,i]
                    if (types[i]=="M")
                    {
                        effect.of.i <- get.individual.effect(a=weightMatrix[j,i], X.vec=X[,i], Psi.signs=Psi.signs[[j]][[i]])
                        eta <- eta + effect.of.i
                    }
                }
            }
            X[,j] <- eta + errMat[,error.index]
            error.index <- error.index + 1

        }
        if (types[j] == "M")
        {
            C <- C.vector[j]
            eta <- matrix(0,nrow=n,ncol=C)
            for (i in 1:(j-1))
            {
                if (weightMatrix[j,i] != 0)
                {
                    if (types[i]=="G")
                    {
                        signs.vec <- Phi.signs[[j]][[i]]
                        for (h in 1:C)
                        {
                            eta[,h] <- signs.vec[h] * X[,i]
                        }
                    }
                    if (types[i]=="M")
                    {
                        effect.of.i <- get.individual.effect(a=weightMatrix[j,i], X.vec=X[,i], Psi.signs=Psi.signs[[j]][[i]])
                        signs.vec.Xj <- Phi.signs[[j]][[i]]

                        index <- 1
                        for (h in 1:C)
                        {
                            eta[,h] <- eta[,h] + signs.vec.Xj[index] * effect.of.i
                            index <- index + 1
                        }

                    }
                }
            }
            pi.mat <- matrix(nrow=nrow(eta),ncol=C)
            for (i in 1:nrow(eta))
            {
                if (sum(abs(eta[i,])) > 0) ## normalize to pi.vec row-wise
                {
                    pi.mat[i,] <- exp(eta[i,])/sum(exp(eta[i,]))
                } else {
                    pi.mat[i,] <- rep(1/C,C)
                }
            }
            X[,j] <- matrix.to.vector(t(apply(pi.mat,1,sim.multinom.X, n=1)))
        }
    }
    X <- data.frame(X)
    for (i in seq(2,p,2))
        X[,i] <- as.factor(X[,i])

    if (dichotomous==FALSE)
    {
        for (i in which(types=="G"))
            X[,i] <- (X[,i] - mean(X[,i])) / sd(X[,i])

        return(X)
    } else { ## we need this to check how Logistic LASSO performs with dichotomized data
        for (i in 1:length(types))
        {
            if (types[i]=="G")
            {
                X[,i] <- ifelse(X[,i]<=median(X[,i]),-1,1) ## Do NOT save as factor - only used in glmnet (!)
            } else {
                cats <- levels(X[,i]) ## get available levels
                cats.freq <- table(X[,i])/n*100 ## get corresponding frequencies in percent for each category
                perc.cat <- c() # percent in first category for a certain split
                ## idee save all subsets that are possible
                ## then simply use these indices to build up the sum and find the dichotomization s.t. -1/1 split of catefories is as close to
                ## 50 percent of observations per category as possible
                library(gregmisc)
                codings <- list()
                index1 <- 1
                index2 <- 1
                for (subset.size in 1:(length(cats)-1))
                {
                    index2 <- 1
                    B <- combinations(length(cats), subset.size, cats) # used to run through all possible subsets up to "no. of levels(X[,i]) - 1"
                    for (j in 1:nrow(B))
                    {
                        codings[[index1]] <- B[index2,] # the actual index list over al possible subsets
                        index1 <- index1 + 1
                        index2 <- index2 + 1
                    }
                }

                ## build up the cumulative sum over the frequencies of the categories in one of the target dichotomous category
                ## (if this coding scheme was used)
                for (dichot in 1:length(codings))
                {
                    perc.cat <- c(perc.cat, sum(cats.freq[codings[[dichot]]]))
                }
                ## check the codings difference from 50 percent ("perfect" allocation wrt the no. of observations)
                Diff.50 <- abs(50-perc.cat)
                select.one <- which(Diff.50 == min(Diff.50))
                selected.coding <- select.one[sample(1:length(select.one),1)] ## if more than one coding is close to optimum sample one randomly

                new.factor <- ifelse(X[,i] %in% codings[[selected.coding]], -1, 1) ## Do NOT save as factor - only used in glmnet (!)
                X[,i] <- new.factor ## assign the dichotomous version
            }
        } ## end of for loop over all variables (check type for dichotomization)

## In the dichotomous case no such standardization is needed as all values are -1, 1 already
##        for (i in which(types=="G"))
##            X[,i] <- (X[,i] - mean(X[,i])) / sd(X[,i])

        return(as.matrix(X))
    } ## end of dichotomization
} ## end of data generation


## Finally, generate the data:
d <- rmvDAGMixed(n=n, weightMatrix, C.vector=C.vector, Psi.signs=Psi.signs, Phi.signs=Phi.signs,C.min=C.min, C.max=C.max, errMat=matrix(rnorm(n*p/2,mean=0,sd=sqrt(var)),nrow=n,ncol=p/2), dichotomous=FALSE)
