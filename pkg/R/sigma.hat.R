setMethod("sigma.hat", signature(object = "lm"),
    function(object)
    {
    object.class <- class(object)[[1]]
    sigma <- summary(object)$sigma
    return (sigma)
    }
)


setMethod("sigma.hat", signature(object = "glm"),
    function(object)
    {
     dispersion <- if (is.null(object$dispersion))
        summary(object)$dispersion
    else object$dispersion
    object.class <- class(object)[[1]]
    if (object$family$family == "gaussian") {
        sigma <- sqrt(dispersion)
    }
    else {
        sigma <- summary(object, correlation = TRUE)$sigma
    }
    return(sigma)
    }
)
