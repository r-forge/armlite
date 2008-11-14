\name{sim}
\docType{genericFunction}
\alias{sim}
\alias{sim,lm-method}
\alias{sim,glm-method}


\title{Functions to Get Posterior Distributions}

\description{
This generic function gets posterior simulations of sigma and beta from a lm object, or
simulations of beta from a glm object
}

\usage{
sim(object, ...)

\S4method{sim}{lm}(object, n.sims = 100)
\S4method{sim}{glm}(object, n.sims = 100)
}

\arguments{
  \item{object}{the output of a call to "lm" with n data points and k predictors.}
  \item{...}{further arguments passed to or from other methods.}
  \item{n.sims}{number of independent simulation draws to create.}

}

\value{
  \item{coef}{matrix (dimensions n.sims x k) of n.sims random draws of coefficients.
    In the previous version, the slot name is called \code{beta}.}
   \item{sigma}{vector of n.sims random draws of sigma
  (for glm's, this just returns a vector of 1's or else of the
  square root of the overdispersion parameter if that is in the model)}
  }

\references{Andrew Gelman and Jennifer Hill, 
            Data Analysis Using Regression and Multilevel/Hierarchical Models, 
            Cambridge University Press, 2006.}

\author{Andrew Gelman \email{gelman@stat.columbia.edu};
        Yu-Sung Su \email{ys463@columbia.edu};
        M.Grazia Pittau \email{grazia@stat.columbia.edu}
        }

\seealso{\code{\link{display}},
        \code{\link{lm}},
        \code{\link{glm}}
}

\examples{

 n <- 100
# Linear regression 
 x <- rnorm(n)
 y <- rnorm(n)
 M1 <- lm (y ~ x)
 display(M1)
 M1.sim <- sim(M1)
 ## to get the uncertainty for the simulated estimates
 t(apply(M1.sim$coef, 2, quantile))
 quantile(M1.sim$sigma)

 
# Logistic regression 
 y <- rbinom (n, 1, invlogit(2*x))
 M2 <- glm (y ~ x, family=binomial(link="logit"))
 display(M2)
 M2.sim <- sim (M2)
}

\keyword{models}
\keyword{methods}
