#################################
# estimation function for GRaFo #
#################################
estimate.forests <- function(dep.var.index=dep.var.index,subset.data,ntree=ntree)
{
### INPUT
### dep.var.index: which column to be used as target variable
### subset.data: data set to be used
### ntree: no. of trees in forest

    ## importance is the object of desire of this function
    ## if a variable is not selected it will have rank rank(-Inf) = worst rank
    imp <- rep(-Inf,ncol(subset.data))
    names(imp) <- colnames(subset.data)

    ## the dependent variable
    target.var <- subset.data[,dep.var.index]

    ################
    ## Regression: # (if dependent variable is continuous and has at least 6 different values)
    ################
    if (is.numeric(target.var))
    {
        ## Random forests cannot handle continous variables with less than 6 unique values
        if (length(table(target.var)[table(target.var)>0])>=6)
        {
            ## estimate mean decrease in accuracy of the random forest
            imp.tmp <- importance(randomForest(subset.data[,-dep.var.index],target.var,importance=TRUE,ntree=ntree,proximity=FALSE,keep.forest=FALSE))[,1]
            imp[names(imp.tmp)] <- imp.tmp
        } else { ## if less than 6 unique values
            warning(paste("A continuous variable had less than 6 unique values in a subset and was neglected"))
        } ## end else if less than 6 unique values
    } else { ## end of check whether the dependent variable is continuous

        ## ##################
        ##  Classification: # (if dependent variable is a factor)
        ## ##################
        ## is there any variation in the dependent variable?
        if (is.factor(target.var) & var(na.exclude(target.var))>0)
        {
            ## Random forests cannot handle empty factor levels thus every specified level
            ## has to have entries (check by "actual" and "specified" vector length)
            ## this can happen due to the subsampling - we will correct below for such a case:
            actual.levels.in.subset <- names(table(target.var))[table(target.var)>=1]
            specified.levels.in.subset <- levels(target.var)

            ## correction: remove non-appearing factor levels from target variable
            if (length(actual.levels.in.subset)!=length(specified.levels.in.subset))
            {
                target.var <- factor(target.var,levels=actual.levels.in.subset)
            }

            ## estimate the random forest
            imp.tmp <- importance(randomForest(subset.data[,-dep.var.index],target.var,importance=TRUE,ntree=ntree,proximity=FALSE,keep.forest=FALSE))
            ## assign rank to importance vector (the larger the importance the smaller the rank)
            imp.tmp <- imp.tmp[,dim(imp.tmp)[[2]]-1]
            imp[names(imp.tmp)] <- imp.tmp
        } else { ## if number of levels is correct
            warning(paste("A factor had only a single level in a subset and was neglected"))
        } ## end of check if the dependent variable is a factor
    }

    ## assign rank to importance vector (the larger the importance the smaller (=better) the rank)
    imp <- rank(-imp)

    return(imp)
} ## end estimate.forests()


## Graphical Random Forests (GRaFo)
GRaFo <- function(data,ntree,n.subsets)
{
    ## INPUT
    ## data: data frame, factors have to be marked as such

    ## measure time of the entire procedure
    time.start <- Sys.time()

    ## data consistency checks
    ## Reminder: Please also ensure that columns containing discrete variables are marked as 'factor
    if (!is.data.frame(data))
        stop("The specified data is not of type data.frame")

    if (!(nrow(data) > 0))
        stop("The data frame has an invalid number of rows")

    if (!(ncol(data) > 0))
        stop("The data frame has an invalid number of columns")

    ## number of subjects
    n <- nrow(data)
    ## number of covariables
    p <- ncol(data)

    ## library for random forests
    library(randomForest)

    ## check whether labels have been assigned, otherwise assign X1,...,XP
    if (is.null(colnames(data)))
    {
        colnames(data) <- rep(NA,p)
        for (i in 1:p)
            colnames(data)[i] <- paste("X",i,sep="")
    }

###################################
### Estimating the Random Forests #
###################################

    ## all the dependent variables (needed later for apply)
    dep.var.index <- matrix(1:p,ncol=1)

    ## an index matrix for all entries in a (pxp)-matrix
    index.mat <-matrix(1:(p*p),ncol=p, nrow=p)
    ## get the indices of all elements in an upper tri matrix of a pxp matrix in the correct order
    ## note: the main diagonal is NOT included in this index
    index.vec <- as.vector(index.mat[upper.tri(index.mat)])


#########################
### Stability Selection #
#########################

    ## #################
    ## Create subsets  #
    ## #################
    ## Create subsets of data of size floor(n/2); sampled without replacement
    sel.data <- list()
    ## for every random forest get a sample of size floor(n/2)
    if (n.subsets>1)
    {
        for (j in 1:n.subsets)
        {
            sel.data[[j]] <- sample(1:n,floor(n/2),replace=F)
        }
    } else
    sel.data[[1]] <- 1:n ## We use all data. Combined with only one "n.subset" evaluation this is equiv. to no stability selection

    ## ###########################
    ## Derive the Rank Matrices  #
    ## ###########################
    ## we need stab.sel.mat to save the ranks of edge candidates for each subset
    stab.sel.mat <- NULL

    for (subset in 1:n.subsets)
    {
        print(paste("Subset no. ",subset,sep=""))

        ## Random Forest Step: Estimate rank matrix from variable importance - computationally expensive (!)
        X.rank.mat <- apply(dep.var.index,1,estimate.forests,subset.data=data[sel.data[[subset]],],ntree=ntree)

        ## Rank is an integer ranging from 1 (largest importance contribution) to p (smallest importance)
        ## Here we need the MAXIMUM since a higher rank is worse and we want to be conservative,
        ## i.e., we check edge X-Y and Y-X and take the worse (=larger) rank
        X.rank.mat.max <- get.max.upper.triangle(X.rank.mat)
        edge.rank <- X.rank.mat.max[index.vec]

        ## We have assigned a rank for all p*(p-1)/2 edges for the k-th subset
        ## save as k-th row in stab.sel.mat
        stab.sel.mat <- rbind(stab.sel.mat,edge.rank)

    } ## end for loop over all n.subsets

    ## colnames correspond to R's upper-tri indexing of matrices
    colnames(stab.sel.mat) <- index.vec

    print("Done")

    time.end <- Sys.time()

    return(invisible(list(stab.sel.mat=stab.sel.mat,n=n,p=p,n.subsets=n.subsets,time=time.end-time.start)))

}
