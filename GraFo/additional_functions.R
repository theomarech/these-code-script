## Some functions that may be needed by some of the estimation functions


get.CIG.estimate <- function(stab.sel.obj, ev.vec)
{
    ## INPUT: stab.sel.obj: object from gmwrf procedure
    ##        ev.vec: vector of E(V) values to be evaluated
    ## OUTPUT: TP/FP estimates

    ## initial setup
    n.subsets <- nrow(stab.sel.obj$stab.sel.mat)
    p <- stab.sel.obj$p
    q.max.vec <- floor(sqrt((2*0.75-1)*ev.vec*p*(p-1)/2))

    ## ######################################
    ## list 0-matrices where we save one CIG for each ev level in ev.vec
    ## ######################################
    count.mat <- matrix(0,nrow=p,ncol=p)
    rownames(count.mat) <- colnames(count.mat) <- 1:p

    ## ######################################
    ## get an overview which ranks are present in the data - this line is slow (~30 sec for p=200) but only executed once so its OK
    ## ######################################
    rank.names <- names(table(stab.sel.obj$stab.sel.mat))
    rank.matrix <- matrix(0,nrow=n.subsets,ncol=length(rank.names))
    colnames(rank.matrix) <- rank.names

    ## ############################################
    ## next, get overview which ranks are present for each individual subset
    ## ############################################
    for (subset in 1:n.subsets)
    {
        one.line <- table(stab.sel.obj$stab.sel.mat[subset,])
        rank.matrix[subset,names(one.line)] <- one.line
    }

    ## ###################################################
    ## next, we have to ientify the largest rank for which we do NOT select more edges than allowed by the specified q
    ## ###################################################
    max.rank.matrix <- matrix(0,nrow=n.subsets,ncol=length(ev.vec)) # in worst case we select no edges (no edge has 0 rank)
    for (subset in 1:n.subsets)
    {
        index <- 1
        for (q in q.max.vec)
        {
            temp <- which(q >= cumsum(rank.matrix[subset,])) # here we build the cumulative sum over the ranks...
            if (length(temp)>0)
            {
                max.rank.matrix[subset,index] <- as.numeric(names(rank.matrix[subset,length(temp)])) # ... and save the largest rank for which q is still larger than the cumulative sum
            } # else: 0 by definiition of max.rank.matrix
            index <- index + 1
        }
    }

    est.CIG.list <- list()
    index <- 1
    for (ev in ev.vec)
    {
        est.CIG.list[[index]] <- count.mat
        selected.edges <- c()
        for (subset in 1:n.subsets)
        {
            selected.edges <- c(selected.edges, as.numeric(names(which(stab.sel.obj$stab.sel.mat[subset,] <= max.rank.matrix[subset,index]))))
        }

        ## Now get all edges with frequency larger or equal to pi=0.75 * n.subsets
        a <- table(selected.edges); a <- a[a>=0.75 * n.subsets]
        est.CIG.list[[index]][as.numeric(names(a))] <- ifelse(a!=0,1,0) # here we assign the
        est.CIG.list[[index]] <- est.CIG.list[[index]] + t(est.CIG.list[[index]])

        ## used in selected.edges
        index <- index + 1
    }

    return(est.CIG.list)

} ## end of get.CIG.estimate






#######################################################
### my.round performs rounding as known from school,  #
### not like the R internal round command             #
### example                                           #
### round(0.5) == 0                                   #
### my.round(0.5, digits=0) == 1                      #
###                                                   #
### "digits" ensures that a number will have exactly  #
### the desired number of digits, e.g,                #
### my.round(0.001, digits=2) == "0.00"               #
#######################################################
my.round <- function(x,digits) {
    x <- x * 10^(digits)

    ## we do not know how many characters follow before "." thus we need
    ## an additional index
    i.dot <- 0
    for (i in 1:nchar(x))
     {
         if (substr(x,i,i) == ".")
             i.dot <- i
     }

    ## only do sth. if x has decimals
    if (i.dot != 0)
    {

        if (substr(x,i.dot+1, i.dot+1) %in% c(0,1,2,3,4))
        {
            x <- floor(x) / 10^(digits)
        }
        else
        {
            x <- ceiling(x) / 10^(digits)
        }
        ## do sth. here to ensure we always have the desired no. of decimals
        ## it is annoying to have "0" entries in a table lacking decimals (!)
    }
    else
    {
        x <- x / 10^(digits)
    }

    return(format.default(x,nsmall=digits))
}

## #############################################
## estimate threshold for stability selection #
## ############################################
## INPUT: l: the number of edges to be estimated
##        q: the number of chosen edges
##        ev: the number of expected false edges
threshold.fct <- function(l,q,ev)
{
    # Note: In the stability selection formula l = l M&B, 2009), i.e. the number of potential edges
    threshold <- (q^2/(l*ev)+1)/2
    print(paste("Threshold:",round(threshold,2)))
    if(threshold>1)
    {
        show(warning("Threshold >1"))
    }
    return(threshold)
}


################################
# convert a list into a vector #
################################
make.vector.from.list <- function(list.object)
{
#INPUT: List (which does _not_ contain sublists
#OUTPUT: vector
    vec <- c()
    for (i in 1:length(list.object))
        vec <- c(vec,list.object[[i]])
    return(vec)
}

########################################################
# convert the upper triangle of a matrix into a vector #
########################################################
make.vector.from.upper.triangular.matrix.old <- function(matrix)
{
#INPUT: Quadratic Matrix
#OUTPUT: Vector built from upper triangular matrix excluding the diagonal
    if (dim(matrix)[1]!=dim(matrix)[2])
        stop("Not a quadratic matrix!")
    vec <- c()
    # the -1 ensures we neglect the diagonal elements
    for (i in 1:(dim(matrix)[1] - 1))
        for (j in (i+1):dim(matrix)[1])
            vec <- c(vec,matrix[i,j])
    return(vec)


# example
# A <- matrix(1:9,ncol=3)
# rownames(A) <- c("a","b","c")
# colnames(A) <- c("d","e","f")
# make.vector.from.upper.triangular.matrix(A)
}
make.vector.from.upper.triangular.matrix <- function(matrix)
{
#INPUT: Quadratic Matrix
#OUTPUT: Vector built from upper triangular matrix excluding the diagonal
    if (dim(matrix)[1]!=dim(matrix)[2])
        stop("Not a quadratic matrix!")
    vec <- c()
    p <- dim(matrix)[1]
    # the -1 ensures we neglect the diagonal elements
    for (i in 1:(dim(matrix)[1] - 1))
         vec <- c(vec,matrix[i,(i+1):p])
    return(vec)


# example
# A <- matrix(1:9,ncol=3)
# rownames(A) <- c("a","b","c")
# colnames(A) <- c("d","e","f")
# make.vector.from.upper.triangular.matrix(A)
}


################################################################################################################################
# analogue to the vector made from the upper triangular matrix we also need the edge names of the corresponding matrix entries #
################################################################################################################################
make.edge.names.from.upper.triangular.matrix <- function(matrix)
{
#INPUT: Quadratic Matrix
#OUTPUT: Get the edge names of the upper triangular matrix entries (excluding diagonal)
    if (dim(matrix)[1]!=dim(matrix)[2])
        stop("Not a quadratic matrix!")
    vec <- c()
    # the -1 ensures we neglect the diagonal elements
    for (i in 1:(dim(matrix)[1] - 1))
                vec <- c(vec,paste(rownames(matrix)[i],"~",colnames(matrix),sep=""))
    return(vec)
# example
# A <- matrix(1:9,ncol=3)
# rownames(A) <- c("a","b","c")
# colnames(A) <- c("d","e","f")
# make.edge.names.from.upper.triangular.matrix(A)
}

######################################################################
# transform a quadratic matrix into a symmetric one using min or max #
######################################################################
make.symmetric.matrix <- function(matrix,procedure)
{
#INPUT: Quadratic Matrix, Rule how to process (how to make the matrix symmetric)
#OUTPUT: Symmetric Matrix
    if (dim(matrix)[1]!=dim(matrix)[2])
        stop("Not a quadratic matrix!")
    if (! procedure %in% c("min","max","abs"))
        stop("Please choose procedure min, max or abs")

    for (i in 1:dim(matrix)[1])
        for (j in 1:dim(matrix)[1])
        {
            if (procedure=="min")
                matrix[i,j] <- matrix[j,i] <- min(matrix[i,j],matrix[j,i])
            if (procedure=="max")
                matrix[i,j] <- matrix[j,i] <- max(matrix[i,j],matrix[j,i])
            if (procedure=="abs")
                matrix[i,j] <- matrix[j,i] <- max(abs(matrix[i,j]),abs(matrix[j,i]))
        }
    return(matrix)
}


get.min.upper.triangle <- function(A) {
    ## INPUT: Quadratic Matrix A
    ## OUTPUT: Maximum of each pair A[i,j], A[[j,i] in upper triangle of matrix
    if (dim(A)[1]!=dim(A)[2])
        stop("Not a quadratic matrix!")

    p <- dim(A)[1]
    ## True/False check: Is entry in upper triangle larger than the one in lower triangle
    DIFF <- A[upper.tri(A)] - t(A)[upper.tri(A)] <= 0
    A.up.tri.max <- matrix(0,p,p)
    ## save all values where _upper_ tri is larger
    A.up.tri.max[upper.tri(A)][DIFF] <- A[upper.tri(A)][DIFF]
    ##  save all values where _lower__ tri is larger
    A.up.tri.max[upper.tri(A)][!DIFF] <- t(A)[upper.tri(A)][!DIFF]
    A.up.tri.max

    ## example:
    ## A <- matrix(1:16,ncol=4)
    ## A[4,1] <- 99
    ## get.max.upper.triangle(A)
}

get.max.upper.triangle <- function(A) {
###INPUT: Quadratic Matrix A
###OUTPUT: Maximum of each pair A[i,j], A[[j,i] in upper triangle of matrix
    if (dim(A)[1]!=dim(A)[2])
        stop("Not a quadratic matrix!")

    p <- dim(A)[1]
    ## True/False check: Is entry in upper triangle larger than the one in lower triangle
    DIFF <- A[upper.tri(A)] - t(A)[upper.tri(A)] >= 0
    A.up.tri.max <- matrix(0,p,p)
    ## save all values where _upper_ tri is larger
    A.up.tri.max[upper.tri(A)][DIFF] <- A[upper.tri(A)][DIFF]
    ##  save all values where _lower__ tri is larger
    A.up.tri.max[upper.tri(A)][!DIFF] <- t(A)[upper.tri(A)][!DIFF]
    A.up.tri.max

### example:
### A <- matrix(1:16,ncol=4)
### A[4,1] <- 99
### get.max.upper.triangle(A)
}

