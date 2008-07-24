setMethod("display", signature(object = "lm"),
    function(object, digits=2)
    {
    call <- object$call
    summ <- summary (object)
    coef <- summ$coef[,1:2,drop=FALSE]
    dimnames(coef)[[2]] <- c("coef.est","coef.se")
    n <- summ$df[1] + summ$df[2]
    k <- summ$df[1]
    print (call)
    pfround (coef, digits)
    cat("---\n")
    cat (paste ("n = ", n, ", k = ", k,
    "\nresidual sd = ", fround (summ$sigma, digits),
    ", R-Squared = ", fround (summ$r.squared, 2), "\n", sep=""))
    }
)



setMethod("display", signature(object = "bayesglm"),
    function(object, digits=2)
    {
    call <- object$call
    summ <- summary(object, dispersion = object$dispersion)
    coef <- matrix( NA, length( object$coefficients ),2 )
    rownames(coef) <- names( object$coefficients )          ## M
    dimnames(coef)[[2]] <- c( "coef.est", "coef.se" )
    coef[ rownames( coef ) %in% rownames( summ$coef[, 1:2, drop = FALSE]) , ] <- summ$coef[ , 1:2, drop = FALSE ]  ## M
    #n <- summ$df[1] + summ$df[2]
    n <- summ$df.residual
    k <- summ$df[1]
    print(call)
    pfround(coef, digits)
    cat("---\n")
    cat(paste("n = ", n, ", k = ", k, "\nresidual deviance = ", 
        fround(summ$deviance, 1), ", null deviance = ", fround(summ$null.deviance, 
            1), " (difference = ", fround(summ$null.deviance - 
            summ$deviance, 1), ")", "\n", sep = ""))
    dispersion <- if (is.null(object$dispersion)) 
        summ$dispersion
    else object$dispersion
    if (dispersion != 1) {
        cat(paste("overdispersion parameter = ", fround(dispersion, 
            1), "\n", sep = ""))
        if (family(object)$family == "gaussian") {
            cat(paste("residual sd is sqrt(overdispersion) = ", 
                fround(sqrt(dispersion), digits), "\n", sep = ""))
        }
    }
    }
)



setMethod("display", signature(object = "glm"),
    function(object, digits=2)
    {
    call <- object$call
    summ <- summary(object, dispersion = object$dispersion)
    coef <- summ$coef[, 1:2, drop = FALSE]
    dimnames(coef)[[2]] <- c("coef.est", "coef.se")
    n <- summ$df[1] + summ$df[2]
    k <- summ$df[1]
    print(call)
    pfround(coef, digits)
    cat("---\n")
    cat(paste("  n = ", n, ", k = ", k, "\n  residual deviance = ", 
        fround(summ$deviance, 1), ", null deviance = ", fround(summ$null.deviance, 
            1), " (difference = ", fround(summ$null.deviance - 
            summ$deviance, 1), ")", "\n", sep = ""))
    dispersion <- if (is.null(object$dispersion))
      summ$dispersion
    else object$dispersion
    if (dispersion != 1) {
      cat(paste("  overdispersion parameter = ",
                fround(dispersion, 1), "\n", sep = ""))
      if (family(object)$family=="gaussian") {
        cat(paste("  residual sd is sqrt(overdispersion) = ",
                  fround(sqrt(dispersion), digits), "\n", sep = ""))
      }
    }
    }
)


setMethod("display", signature(object = "polr"),
    function(object, digits=2)
    {
    call <- object$call
    summ <- summary(object)
    coef <- summ$coef[, 1:2, drop = FALSE]
    dimnames(coef)[[2]] <- c("coef.est", "coef.se")
    n <- summ$n  
    k <- nrow (coef)
    k.intercepts <- length (summ$zeta)
    print(call)
    pfround(coef, digits)
    cat("---\n")
    cat(paste("n = ", n, ", k = ", k, " (including ", k.intercepts,
        " intercepts)\nresidual deviance = ",
        fround(deviance(object), 1), 
        ", null deviance is not computed by polr",
        "\n", sep = ""))
    #cat("AIC:", fround(AIC(object), 1), "\n")
    }
)
