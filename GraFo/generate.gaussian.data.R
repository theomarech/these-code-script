######################################################################################
# Based on Kalisch's pcalg rmvDAG function
######################################################################################

rmvDAGGauss <- function (n, weightMatrix, errMat)
{
    p <- nrow(weightMatrix)

    if ((ncol(errMat) != p) | nrow(errMat)!=n)
        stop("errMat has the wrong format")

    X <- matrix(0, n, p)
    ## here we changed the condition if (sum(weightMatrix) > 0) {
    if (TRUE %in% (unique(weightMatrix)!= 0)) {
        X[, 1] <- errMat[, 1]

        for (j in 2:p) {
            ij <- 1:(j - 1)
            X[, j] <- X[, ij, drop = FALSE] %*% weightMatrix[j,
                               ij] + errMat[, j]
        }

        ## standardize data
        for (i in 1:p)
            X[,i] <- (X[,i] - mean(X[,i])) / sd(X[,i])

        return(as.data.frame(X))
    } else {
        return(as.data.frame(errMat))
    }
}
