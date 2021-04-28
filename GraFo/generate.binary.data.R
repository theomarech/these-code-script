## This function generates binary data
rmvDAGBinary <- function (n, weightMatrix)
{
    p <- nrow(weightMatrix)

    X <- matrix(0, n, p)

    X[, 1] <- 2 * rbinom(n,1,0.5) -1
    for (j in 2:p) {
        ij <- 1:(j - 1)
        a <- X[, ij, drop = FALSE] %*% weightMatrix[j, ij]
        pi.j <- exp(a) / (1 + exp(a))
        X[, j] <- 2 * rbinom(n,1,pi.j) - 1
    }
    X <- as.data.frame(X)
    for (i in 1:p)
        X[,i] <- factor(X[,i],levels=c(-1,1))

    return(data.frame(X))
}
