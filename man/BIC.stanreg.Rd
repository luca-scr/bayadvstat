% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modsel.R
\name{BIC.stanreg}
\alias{BIC.stanreg}
\title{Bayesian Information Criterion for `stanreg` model object.}
\usage{
\method{BIC}{stanreg}(object, estimate = c("Mean", "MAP"), ...)
}
\arguments{
\item{object}{an object returned by `stan_glm()` function.}

\item{estimate}{a string specifying the estimate to use in evaluating the 
log-likelihood: `"Mean"` for the posterior mean, `"MAP"` for the maximum
a posteriori.}

\item{\dots}{additional arguments to be passed to the low level functions.}
}
\value{
The value of BIC.
}
\description{
Return BIC for fitted `stanreg` model.
}
\details{
Only available for `family = gaussian(link = "identity")`.
}
\examples{
x = rnorm(100)
y = 1 + 0.5*x + rnorm(100, 0, 0.5)
mod = stan_glm(y ~ x, data = data.frame(x, y))
BIC(mod)

}
