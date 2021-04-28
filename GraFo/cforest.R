###################################
# estimation function for cforest #
###################################
estimate.cforests <- function(dep.var.index=dep.var.index,subset.data,ntree=ntree,cond.var.imp)
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

    formula <- formula(paste(colnames(subset.data)[dep.var.index],"~",paste(colnames(subset.data)[-dep.var.index], collapse="+")))

    ################
    ## Regression:
    ################
    if (is.numeric(target.var))
    {
        if (var(na.exclude(target.var))>0)
        {
            ## estimate unbiased cforest
            imp.tmp <- cforest(formula, data=subset.data, control = cforest_unbiased(ntree = ntree))
            ## obtain the variable importance for the cforest
            if (cond.var.imp == TRUE) ## if conditional version is to be calculated
            {
                imp.tmp <- varimp(imp.tmp, conditional=cond.var.imp)
            } else ## otherwise the unconditional i.e. marginal importance
            {
                imp.tmp <- varimp(imp.tmp, conditional=cond.var.imp)
            }

            imp[names(imp.tmp)] <- imp.tmp
        } ## end of check of variance in target.var
    } else { ## end of check whether the dependent variable is continuous

        ## ##################
        ##  Classification: # (if dependent variable is a factor)
        ## ##################
        ## is there any variation in the dependent variable?
        if (is.factor(target.var) & var(na.exclude(target.var))>0)
        {
            ## Some methods cannot handle empty factor levels
            ## this can happen due to the subsampling - we will correct below for such a case:
            actual.levels.in.subset <- names(table(target.var))[table(target.var)>=1]
            specified.levels.in.subset <- levels(target.var)

            ## correction: remove non-appearing factor levels from target variable
            if (length(actual.levels.in.subset)!=length(specified.levels.in.subset))
            {
                target.var <- factor(target.var,levels=actual.levels.in.subset)
            }

            ## estimate unbiased cforest
            imp.tmp <- cforest(formula, data=subset.data, control = cforest_unbiased(ntree = ntree))

            ## obtain the variable importance for the cforest
            if (cond.var.imp == TRUE) ## if conditional version is to be calculated
            {
                imp.tmp <- varimp(imp.tmp, conditional=cond.var.imp)
            } else ## otherwise the unconditional
            {
                imp.tmp <- varimp(imp.tmp, conditional=cond.var.imp)
            }

            ## assign rank to importance vector (the larger the importance the smaller the rank)
            imp[names(imp.tmp)] <- imp.tmp
        } ## end of factor case
    } ## end of estimation

    ## assign rank to importance vector (the larger the importance the smaller (=better) the rank)
    imp <- rank(-imp)

    return(imp)
} ## end estimate.cforests()


## graphical models with cforests
cforest.stab.sel <- function(data,ntree,n.subsets,cond.var.imp)
{
    ## INPUT
    ## data: data frame, factors have to be marked as such
    ## OUTPUT

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
    library(party)

    ## check whether labels have been assigned, otherwise assign X1,...,XP
    if (is.null(colnames(data)))
    {
        colnames(data) <- rep(NA,p)
        for (i in 1:p)
            colnames(data)[i] <- paste("X",i,sep="")
    }

########################################
### Estimating the Conditional Forests #
########################################

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
    ## for every cforest get a sample of size floor(n/2)
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

        ## cforest Step: Estimate rank matrix from variable importance - computationally expensive (!)
        X.rank.mat <- apply(dep.var.index,1,estimate.cforests,subset.data=data[sel.data[[subset]],],ntree=ntree,cond.var.imp=cond.var.imp)

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
