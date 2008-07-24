\name{sigma.hat}
\docType{genericFunction}
\alias{sigma.hat}
\alias{sigma.hat,lm-method}
\alias{sigma.hat,glm-method}



\title{Extract Residual Errors}

\description{This generic function extracts residual errors from a fitted model.
}

\usage{
sigma.hat(object,\dots)

\S4method{sigma.hat}{lm}(object)
\S4method{sigma.hat}{glm}(object)

}

\arguments{
  \item{object}{any fitted model object of lm and glmclass}
  \item{\dots}{other arguments}
}

\author{Andrew Gelman \email{gelman@stat.columbia.edu};
        Yu-Sung Su \email{ys463@columbia.edu}
    }

\seealso{\code{\link{display}},
        \code{\link{summary}},
        \code{\link{lm}},
        \code{\link{glm}}
}

\examples{
   n <- 100
   x <- rnorm (n)
   y1 <- rnorm (n)
   y2 <- rbinom(n, 1, .25)
   
   M1 <- lm (y1 ~ x) 
   sigma.hat(M1)
   
   M2 <- bayesglm (y1 ~ x, prior.scale=Inf, prior.df=Inf)
   sigma.hat(M2) # should be same to sigma.hat(M1)

   M3 <- glm (y2 ~ x, family=binomial(link="logit"))
   sigma.hat(M3)

}
\keyword{manip}
\keyword{methods}
