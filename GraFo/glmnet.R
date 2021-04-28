## a helper function
find.first.non.zero <- function(vec)
{
    ## find the first element of a vector ("vec") which is non-zero and return TRUE at this position
    cond <- which(vec!=0)[1]
    if (is.na(cond))
        helper <- rep(FALSE, length(vec))
    if (!is.na(cond))
        helper <- c(rep(FALSE, cond-1), TRUE, rep(FALSE,length(vec)-cond))
    return(helper)
}

## another helper function
find.negative <- function(vec)
{
    ## check if any element in vec is negative and return TRUE if yes, FALSE otherwise
    return(any(vec < 0))
}



glmnet.stab.sel <- function(data, n.subsets, family, alpha)
{

    ## measure time of the entire procedure
    time.start <- Sys.time()

    ## load glmnet
    require(glmnet)

    ## The main function for stability selection with glmnet
    p <- ncol(data)
    n <- nrow(data)

    ## check whether labels have been assigned, otherwise assign X1,...,XP
    if (is.null(colnames(data)))
    {
        count.cols <- 1 : ncol(data)
        X.cols <- rep("X", ncol(data))
        colnames(data) <- paste(X.cols, count.cols, sep="")
    }


####################
### Create subsets #
####################
    ## Create subsets of data of size floor(n/2); sampled without replacement
    sel.data <- list()
    ## for every random forest get a sample of size floor(n/2)
    if (n.subsets>1)
    {
        for (j in 1:n.subsets)
        {
            sel.data[[j]] <- sample(1:n,floor(n/2),replace=F)
        }
    } else {
        sel.data[[1]] <- 1:n ## We use all data. Combined with only one "n.subset" evaluation this is equiv. to no stability selection
    }

################
### run glmnet #
################

    ## an index matrix we use to return from vector to matrix notation to generate stab.sel.mat
    index.mat <-matrix(1:(p*p),ncol=p, nrow=p)
    ## note: the main diagonal is NOT included in this index
    index.vec <- as.vector(index.mat[upper.tri(index.mat)])

    ## the frequency matrix for stability selection
    stab.sel.mat <- NULL

    for (subset in 1:n.subsets)
    {
        print(paste("Subset no. ",subset,sep=""))
        edge.mat <- matrix(0, ncol=p, nrow=p)

        beta.res <- matrix(0, p, p)
        lambda.res <- matrix(0, p, p)
        rownames(beta.res) <- colnames(beta.res) <- rownames(lambda.res) <- colnames(lambda.res) <- colnames(data)


################
### estimation #
################

        ## dep.var: skip dep. variables which are constant (this crushes glmnet!)
        for (dep.var in which(apply(data[sel.data[[subset]],],2,var)!=0))
        {
            ## get beta estimates
            res <- glmnet(x=data[sel.data[[subset]],-dep.var],y=data[sel.data[[subset]],dep.var],family=family, alpha=alpha)
            ## create a 0/1 version which beta's are part of the solution
            ## Note: glmnet has returned an unstable solution once a formerly selected beta
            ##       has been dropped. This index and any following will be dropped from the solution space
            BETA <- as.matrix(res$beta)
            BETA.NULL <- ifelse(BETA != 0, 1, 0)

            non.zero.indices <- t(apply(BETA,1,find.first.non.zero))
            non.zero.indices.vec <- as.vector(non.zero.indices)

            ## get the row indices (in BETA) corresponding to selected betas
            row.indices <- rep(rownames(BETA),ncol(BETA))[non.zero.indices.vec]
            colS <- colSums(non.zero.indices)
            ## get the column indices (in BETA) corresponding to selected betas
            col.indices <- rep((1:ncol(BETA))[colS > 0], colS[colS > 0])

            ## now get the beta value and lambda values at their entry point (first appearance)
            ## Note that the row and col indices already form pairs (no further ordering required)
             for (i in 1:length(row.indices))
             {
                 beta.res[row.indices[i], dep.var] <- BETA[row.indices[i], col.indices[i]]
                 lambda.res[row.indices[i], dep.var] <- res$lambda[col.indices[i]]
             }
    }

#####################################################################################
### preprocessing: getting the more conservative solution for each pair x-y and y-x #
#####################################################################################

        ## first of all, following our conservative philosophy, we get the min of lambda of
        ## x -> y and y -> x edges, then we take the corresponding beta values
        ## first, get the difference
        DIFF <- lambda.res[index.vec] - t(lambda.res)[index.vec] <= 0
        ## now define get the minimum of the edges and save in upper triangular matrix
        lambda.res.new <- matrix(0,p,p)
        lambda.res.new[index.vec][DIFF] <- lambda.res[index.vec][DIFF]
        lambda.res.new[index.vec][!DIFF] <- t(lambda.res)[index.vec][!DIFF]
        ## now get the corresponding beta valus into the upper triangular matrix
        beta.res.new <- matrix(0,p,p)
        beta.res.new[index.vec][DIFF] <- beta.res[index.vec][DIFF]
        beta.res.new[index.vec][!DIFF] <- t(beta.res)[index.vec][!DIFF]

        ## the following calculations will be performed on a vector level
        ## the indices will be useful to get back into matrix notation for edge.mat
        ## recall: index.vec is simply the upper.tri matrix of the (pxp) lambda matrix
        lambda <- lambda.res.new[index.vec]
        beta <- beta.res.new[index.vec]

#############################################################################
### finish line: order estimates and produce the stability selection matrix #
#############################################################################

        ## now we order by decreasing lambda
        ## possibly tied ranks of betas possible (but since it is a numeric artefact of the glmnet procedure (insufficient precision) this is also quite unlikely
        edge.rank <- length(lambda)-rank(lambda)+1
        edge.rank[lambda==0] <- Inf

        ## We have assigned a rank for all p*(p-1)/2 edges for the k-th subset
        ## save as k-th row in stab.sel.mat
        stab.sel.mat <- rbind(stab.sel.mat,edge.rank)
    }

    # colnames correspond to R's upper-tri indexing of matrices
    colnames(stab.sel.mat) <- index.vec

    print("Done")

    time.end <- Sys.time()

    return(invisible(list(stab.sel.mat=stab.sel.mat, n=n, p=p, n.subsets=n.subsets, time=time.end-time.start)))
}  ## end glmnet stability selection function
