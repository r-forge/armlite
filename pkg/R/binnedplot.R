# ====================================================================
# Functions for plotting the binned residuals
# ====================================================================

binnedplot <- function(x, y, nclass=NULL, 
    xlab="Expected Values", ylab="Average residual", 
    main="Binned residual plot", 
    cex.pts=0.8, col.pts=1, col.int="gray")
{

    n <- length(x)     
    if (is.null(nclass)){
        if (n >= 100){
            nclass=floor(sqrt(length(x)))
        }
        if (n > 10 & n < 100){
            nclass=10
        }
        if (n <=10){
            nclass=floor(n/2)
        }
    }
    
    aa <- data.frame(binned.resids (x, y, nclass)$binned)
    
    plot(range(aa$xbar), range(aa$ybar, aa$X2se, -aa$X2se), 
        xlab=xlab, ylab=ylab, type="n", main=main)
    abline (0,0, lty=2)
    lines (aa$xbar, aa$X2se, col=col.int)
    lines (aa$xbar, -aa$X2se, col=col.int)
    points (aa$xbar, aa$ybar, pch=19, cex=cex.pts, col=col.pts)
}

binned.resids <- function (x, y, nclass=floor(sqrt(length(x)))){
    
    breaks.index <- floor(length(x)*(1:(nclass-1))/nclass)
    x.sort <- sort(x)
    breaks <- -Inf
    for (i in 1:(nclass-1)){
        x.lo <- x.sort[breaks.index[i]]
        x.hi <- x.sort[breaks.index[i]+1]
        if (x.lo==x.hi){
            if (x.lo==min(x)){
                x.lo <- -Inf
            }
            else {
                x.lo <- max (x[x<x.lo])
            }
        }
        breaks <- c (breaks, (x.lo + x.hi)/2)
    }
    
    breaks <- c (breaks, Inf)
    breaks <- unique(breaks)
    nclass <- length(breaks) - 1
    output <- NULL
    xbreaks <- NULL
    x.binned <- as.numeric (cut (x, breaks))
    
    for (i in 1:nclass){
        items <- (1:length(x))[x.binned==i]
        x.range <- range(x[items])
        xbar <- mean(x[items])
        ybar <- mean(y[items])
        n <- length(items)
        #p <- xbar                 
        sdev <- sd(y[items])
        output <- rbind (output, c(xbar, ybar, n, x.range, 2*sdev/sqrt(n)))
    }
    
    colnames (output) <- c("xbar", "ybar", "n", "x.lo", "x.hi", "2se")
    return (list (binned=output, xbreaks=xbreaks))
}
