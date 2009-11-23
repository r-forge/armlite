\name{display}
\docType{genericFunction}
\alias{display}
\alias{display,lm-method}
\alias{display,bayesglm-method}
\alias{display,bayesglm.h-method}
\alias{display,glm-method}
\alias{display,polr-method}

\title{Functions for Processing lm, glm, and polr Output}

\description{This generic function gives a
    clean printout of lm, glm, and polr objects.}

\usage{
display (object, ...)

\S4method{display}{lm}(object, digits=2, detail=FALSE)
\S4method{display}{bayesglm}(object, digits=2, detail=FALSE)
\S4method{display}{bayesglm.h}(object, digits=2, detail=FALSE)
\S4method{display}{glm}(object, digits=2, detail=FALSE)
\S4method{display}{polr}(object, digits=2, detail=FALSE)
}

\arguments{
  \item{object}{The output of a call to lm, glm, polr, or related regressions 
    function with n data points and k predictors.}
  \item{...}{further arguments passed to or from other methods.}
  \item{digits}{number of significant digits to display.}
  \item{detail}{defaul is \code{FALSE}, if \code{TRUE}, display p-values or z-values.}
  }

\details{This generic function gives a
    clean printout of lm, glm, and polr objects, focusing 
    on the most pertinent pieces of information: 
    the coefficients and their standard errors, the sample size, 
    number of predictors, residual standard deviation, and R-squared.
    Note: R-squared is automatically displayed to 2 digits, and deviances are
    automatically displayed to 1 digit, no matter what.
}

\value{Coefficients
    and their standard errors, the sample size, number of predictors, 
    residual standard
    deviation, and R-squared}
    
\references{Andrew Gelman and Jennifer Hill, 
            Data Analysis Using Regression and Multilevel/Hierarchical Models, 
            Cambridge University Press, 2006.}

\author{Andrew Gelman \email{gelman@stat.columbia.edu}; 
        Yu-Sung Su \email{ys463@columbia.edu};
        Maria Grazia Pittau \email{grazia@stat.columbia.edu}
        }
        
\note{Output are the model, the regression coefficients and standard errors,
 and the residual sd and R-squared (for a linear model),
 or the null deviance and residual deviance (for a generalized linear model).
}

\seealso{\code{\link[base]{summary}},
         \code{\link[stats]{lm}},
         \code{\link[stats]{glm}},
         \code{\link[MASS]{polr}}
         }

\examples{    
  n <- 100                      
  y1 <- rnorm(n)
  y2 <- rbinom(n, 1, .35)
  x <- rnorm(n)
  
# display a simple linear model

   M1 <- lm (y1 ~ x)
   display (M1)
  
# display a simple logit model

   M2 <- glm (y2 ~ x, family=binomial(link="logit"))
   display (M2)
 
# Then the ordered logit model from polr

   M3 <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
   display(M3)
   
   M4 <- bayespolr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
   display(M4) 
}


\keyword{manip}
\keyword{methods}
