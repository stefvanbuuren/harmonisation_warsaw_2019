mice.impute.normdump <- function (y, ry, x, ...) 
{
    x <- cbind(1, as.matrix(x))
    parm <- .norm.draw(y, ry, x, ...)
    betadump <<- c(betadump,parm$beta) 
    return(x[!ry, ] %*% parm$beta + rnorm(sum(!ry)) * parm$sigma)
}

