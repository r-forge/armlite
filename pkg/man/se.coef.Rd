\name{se.coef}
\docType{genericFunction}
\alias{se.coef}
\alias{se.coef,lm-method}
\alias{se.coef,glm-method}

\title{Extract Standard Errors of Model Coefficients}
  
\description{
   These functions extract standard errors of model coefficients
   from objects returned by modeling functions.
}
\usage{
se.coef (object, \dots)

\S4method{se.coef}{lm}(object)
\S4method{se.coef}{glm}(object)
}

\arguments{
  \item{object}{object of lm, glm, lmer and glmer fit}
  \item{\dots}{other arguments}
}
\value{
  \code{se.coef} gives lists of standard errors for \code{coef},
}
\details{
  \code{se.coef} extracts standard errors from objects 
    returned by modeling functions.
}

\seealso{
  \code{\link{display}},
  \code{\link[stats]{coef}},
  \code{\link{sigma.hat}},
}

\references{Andrew Gelman and Jennifer Hill, 
            Data Analysis Using Regression and Multilevel/Hierarchical Models, 
            Cambridge University Press, 2006.}

\author{Andrew Gelman \email{gelman@stat.columbia.edu}; 
        Yu-Sung Su \email{ys463@columbia.edu}
        }
        
\examples{
   n <- 100
   x <- rnorm (n)
   y1 <- rnorm (n)
   y2 <- rbinom(n, 1, .25)

#  lm fit
   M1 <- lm (y1 ~ x)
   se.coef (M1)
   
#  glm fit
   M2 <- glm (y2 ~ x)
   se.coef (M2)
}
\keyword{manip}
\keyword{methods}
\keyword{models}
