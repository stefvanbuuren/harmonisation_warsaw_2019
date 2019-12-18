
mice.impute.pmmdump <- function (y, ry, x, ...) 
{
    x <- cbind(1, as.matrix(x))
    parm <- .norm.draw(y, ry, x, ...)
    yhatobs <- x[ry, ] %*% parm$coef
    yhatmis <- x[!ry, ] %*% parm$beta
    betadump <<- c(betadump,parm$beta)
    return(apply(as.array(yhatmis), 1, .pmm.match, yhat = yhatobs, 
        y = y[ry], ...))
}

