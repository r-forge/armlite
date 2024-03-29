setOldClass("family")
setOldClass("mcmc.list")
setOldClass("polr")
setOldClass("bugs")

setClass("balance",
     representation(
            rawdata = "data.frame",
            matched = "data.frame",
            factor = "logical")
)




setClass("bayesglm",
     representation(
            formula = "formula",
            family = "family",
            prior.mean = "numeric", 
            prior.scale = "numeric", 
            prior.df = "numeric"),
    contains = "glm"
)



setClass("bayespolr",
     representation(
            formula = "formula",
            Hess = "logical",
            method = "character",
            prior.mean = "numeric", 
            prior.scale = "numeric", 
            prior.df = "numeric",
            prior.mean.for.cutpoints = "numeric", 
            prior.scale.for.cutpoints = "numeric",  
            prior.df.for.cutpoints = "numeric"),
     contains = "polr"
)

setClass("GO")
