setMethod("se.coef", signature(object = "lm"),
    function(object)
    {
    object.class <- class(object)[[1]]
    sqrt (diag(vcov(object)))
    }
)


setMethod("se.coef", signature(object = "glm"),
    function(object)
    {
    object.class <- class(object)[[1]]
    sqrt (diag(vcov(object)))
    }
)
