#' @name BIC.stanreg
#' @aliases BIC.stanreg
#' 
#' @title Bayesian Information Criterion for `stanreg` model object.
#' 
#' @description Return BIC for fitted `stanreg` model. 
#' 
#' @param object an object returned by `stan_glm()` function. 
#' @param estimate a string specifying the estimate to use in evaluating the 
#'  log-likelihood: `"Mean"` for the posterior mean, `"MAP"` for the maximum
#'  a posteriori.
#' @param \dots additional arguments to be passed to the low level functions.
#' 
#' @details Only available for `family = gaussian(link = "identity")`.
#' 
#' @return The value of BIC.
#'
#' @examples
#' x = rnorm(100)
#' y = 1 + 0.5*x + rnorm(100, 0, 0.5)
#' mod = stan_glm(y ~ x, data = data.frame(x, y))
#' BIC(mod)
#' 
#' @export

BIC.stanreg <- function(object, estimate = c("Mean", "MAP"), ...)
{
  stopifnot(inherits(object, "stanreg"))
  stopifnot(object$family$family == "gaussian" & 
            object$family$link == "identity")
  estimate = match.arg(estimate, several.ok = FALSE)
  
  # extract sampling draws
  samples = as.matrix(object)
  # get design model matrix
  X = model.matrix(object$model, data = object$data)   
  n = length(object$residuals)
  # get parameter estimates
  switch(estimate, 
         "Mean" = { b = colMeans(samples[, 1:ncol(X), drop=FALSE])
                    sigma = mean(samples[, ncol(X)+1]) },
         "MAP" =  { map = apply(samples, 2, function(b) 
                                { with(density(b, bw = "SJ"),
                                       x[which.max(y)]) })
                    b = map[1:ncol(X)]
                    sigma = map[ncol(X)+1] } )
  Xb = X %*% b
  # loglik at posterior estimate
  loglik = sum(dnorm(object$y, mean = Xb, sd = sigma, log = TRUE))
  # effective number of parameters
  k = length(coef(object))+1
  # compute BIC
  BIC = -2*loglik + log(n)*k
  
  return(list(BIC = BIC, k = k))
}

#' @name DIC
#' @aliases DIC
#' 
#' @title Deviance Information Criterion for `stanreg` model object.
#' 
#' @description Return DIC for fitted `stanreg` model. 
#' 
#' @param object an object returned by `stan_glm()` function. 
#' @param \dots additional arguments to be passed to the low level functions.
#' 
#' @details Only available for `family = gaussian(link = "identity")`.
#' 
#' @return The value of DIC.
#'
#' @examples
#' x = rnorm(100)
#' y = 1 + 0.5*x + rnorm(100, 0, 0.5)
#' mod = stan_glm(y ~ x, data = data.frame(x, y))
#' DIC(mod)
#' 
#' @export

DIC <- function(object, ...)
{
  stopifnot(inherits(object, "stanreg"))
  stopifnot(object$family$family == "gaussian" & 
            object$family$link == "identity")
  
  # extract sampling draws
  samples = as.matrix(object)
  # get design model matrix
  X = model.matrix(object$model, data = object$data)
  n = length(object$residuals)
  # average parameters 
  b = colMeans(samples[, 1:ncol(X), drop=FALSE])
  Xb = X %*% b
  sigma = mean(samples[, ncol(X)+1])
  # deviance at posterior mean
  D_mean = -2 * sum(dnorm(object$y, mean = Xb, sd = sigma, log = TRUE))
  # log-likelihood at parameters draws
  loglik = double(nrow(samples))
  for(s in 1:nrow(samples))
  {
    Xb = X %*% samples[s, 1:ncol(X)]
    sigma = samples[s, ncol(X)+1]
    # loglik at s-th draw
    loglik[s] = sum(dnorm(object$y, mean = Xb, sd = sigma, log = TRUE))
  }
  # effective number of parameters
  k = 2*var(loglik)
  # compute DIC
  DIC = D_mean + 2*k
  
  return(list(DIC = DIC, k = k))
}

#' @name WAIC
#' @aliases WAIC
#' 
#' @title Watanabe–Akaike Information Criterion for `stanreg` model object.
#' 
#' @description Return WAIC for fitted `stanreg` model. 
#' 
#' @param object an object returned by `stan_glm()` function. 
#' @param \dots additional arguments to be passed to the low level functions.
#' 
#' @details Only available for `family = gaussian(link = "identity")`.
#' 
#' @return The value of WAIC.
#'
#' @examples
#' x = rnorm(100)
#' y = 1 + 0.5*x + rnorm(100, 0, 0.5)
#' mod = stan_glm(y ~ x, data = data.frame(x, y))
#' WAIC(mod)
#' 
#' @export

WAIC <- function(object, ...)
{
  stopifnot(inherits(object, "stanreg"))
  stopifnot(object$family$family == "gaussian" & 
            object$family$link == "identity")
  
  # extract sampling draws
  samples = as.matrix(object)
  # get design model matrix
  X = model.matrix(object$model, data = object$data)
  n = length(object$residuals)
  # 
  loglik = matrix(as.double(NA), nrow = nrow(samples), ncol = n)
  for(s in 1:nrow(samples))
  {
    Xb = X %*% samples[s, 1:ncol(X)]
    sigma = samples[s, ncol(X)+1]
    # loglik at s-th draw
    loglik[s,] = dnorm(object$y, mean = Xb, sd = sigma, log = TRUE)
  }
  
  lpd <- sum(log( colSums(exp(loglik)) / nrow(samples) ))
  # lpd <- mclust::logsumexp(t(loglik)) - log(nrow(samples))
  # effective number of parameters
  k <- sum(apply(loglik, 2, var))
  # compute WAIC
  WAIC <- -2*lpd + 2*k

  return(list(WAIC = WAIC, k = k))
}

#' @name compare_models
#' @aliases compare_models
#' 
#' @title Models comparison based on information criteria for `stanreg` 
#'   model objects.
#' 
#' @description Compare fitted models using a selected information criterion.
#' 
#' @param \dots model objects returned by `stan_glm()` function. 
#' @param criterion a string specifying the criterion to use. 
#' 
#' @details Only available for `family = gaussian(link = "identity")`.
#' 
#' @return Return a `data.table` of selected information criterion,
#'  the effective number of estimated parameters, and the criterion
#'  difference with the one with the smallest value. 
#'
#' @examples
#' x = rnorm(100)
#' y = 1 + 0.5*x + rnorm(100, 0, 0.5)
#' mod1 = stan_glm(y ~ 1, data = data.frame(x, y))
#' mod2 = stan_glm(y ~ x, data = data.frame(x, y))
#' mod3 = stan_glm(y ~ x + I(x^2), data = data.frame(x, y))
#' compareModels(mod1, mod2, mod3)
#' 
#' @export

compareModels <- function(..., criterion = c("BIC", "DIC", "WAIC")) 
{
  models = list(...)
  nModels = length(models)
  criterion = match.arg(criterion, several.ok = FALSE)
  switch(criterion, 
         "BIC" = { 
           tab = lapply(1:nModels, function(m) BIC(models[[m]]))
           tab = do.call(rbind, lapply(tab, data.frame))
           tab = cbind(tab, "ΔBIC" = tab$BIC - min(tab$BIC)) },
         "DIC" = {
           tab = lapply(1:nModels, function(m) DIC(models[[m]]))
           tab = do.call(rbind, lapply(tab, data.frame))
           tab = cbind(tab, "ΔDIC" = tab$DIC - min(tab$DIC)) },
         "WAIC" = {
           tab = lapply(1:nModels, function(m) WAIC(models[[m]]))
           tab = do.call(rbind, lapply(tab, data.frame))
           tab = cbind(tab, "ΔWAIC" = tab$WAIC - min(tab$WAIC))
         })
  return(tab[])
}

