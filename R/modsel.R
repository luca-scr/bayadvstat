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
#' \dontrun{
#' x = rnorm(100)
#' y = 1 + 0.5*x + rnorm(100, 0, 0.5)
#' mod = stan_glm(y ~ x, data = data.frame(x, y))
#' BIC(mod)
#' }
#' 
#' @importFrom stats BIC
#' @export

BIC.stanreg <- function(object, estimate = c("Mean", "MAP"), ...)
{
  stopifnot(inherits(object, "stanreg"))
  stopifnot(object$family$family == "gaussian" & 
            object$family$link == "identity")
  estimate = match.arg(estimate, several.ok = FALSE)
  
  # extract sampling draws
  draws = as.matrix(object)
  # get design model matrix
  X = model.matrix(object$model, data = object$data)   
  n = length(object$residuals)
  # get parameter estimates
  switch(estimate, 
         "Mean" = { b = colMeans(draws[, 1:ncol(X), drop=FALSE])
                    sigma = mean(draws[, ncol(X)+1]) },
         "MAP" =  { map = apply(draws, 2, function(b) 
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

#' @name BIC.brmsfit
#' @aliases brmsfit
#' 
#' @title Bayesian Information Criterion for `stanreg` model object.
#' 
#' @description Return BIC for fitted `brmsfit` model. 
#' 
#' @param object an object returned by `brm()` function. 
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
#' mod = brm(y ~ x, data = data.frame(x, y), 
#'           family = gaussian(link = "identity"))
#' BIC(mod)
#' 
#' @export

BIC.brmsfit <- function(object, estimate = c("Mean", "MAP"), ...)
{
  stopifnot(inherits(object, "brmsfit"))
  stopifnot(object$family$family == "gaussian" & 
            object$family$link == "identity")
  estimate = match.arg(estimate, several.ok = FALSE)
  
  # extract sampling draws
  draws = as.matrix(object)
  # get response and design model matrix
  mf = model.frame(formula(object), data = object$data)
  y = model.response(mf)
  X = model.matrix(mf, data = object$data) 
  # X = model.matrix(formula(object$formula), data = object$data)   
  n = nobs(object)
  # get parameter estimates
  switch(estimate, 
         "Mean" = { b = colMeans(draws[, 1:ncol(X), drop=FALSE])
                    sigma = mean(draws[, ncol(X)+1]) },
         "MAP" =  { map = apply(draws, 2, function(b) 
                                { with(density(b, bw = "SJ"),
                                       x[which.max(y)]) })
                    b = map[1:ncol(X)]
                    sigma = map[ncol(X)+1] } )
  Xb = X %*% b
  # loglik at posterior estimate
  loglik = sum(dnorm(y, mean = Xb, sd = sigma, log = TRUE))
  # effective number of parameters
  k = length(b)+1
  # compute BIC
  BIC = -2*loglik + log(n)*k
  
  return(list(BIC = BIC, k = k))
}

#' @title Deviance Information Criterion
#' 
#' @description Generic method for computing Deviance Information 
#' Criterion (DIC)
#' 
#' @param object a fitted model object.
#' @param \dots other arguments.
#' 
#' @return The value of DIC.
#' 
#' @export

DIC <- function(object, ...) 
{
  UseMethod("DIC")
}

#' @title Deviance Information Criterion
#' 
#' @description Return DIC for fitted `stanreg` model. 
#' 
#' @param object an object returned by `stan_glm()` function. 
#' @param estimate a string specifying the point estimate to use.
#' @param \dots additional arguments to be passed to the low level functions.
#' 
#' @details Only available for `family = gaussian(link = "identity")`.
#' 
#' @return The value of DIC.
#'
#' @examples
#' \dontrun{
#' x = rnorm(100)
#' y = 1 + 0.5*x + rnorm(100, 0, 0.5)
#' mod = stan_glm(y ~ x, data = data.frame(x, y))
#' DIC(mod)
#' }
#' 
#' @export

DIC.stanreg <- function(object, estimate = c("Mean", "MAP"), ...)
{
  stopifnot(inherits(object, "stanreg"))
  stopifnot(object$family$family == "gaussian" & 
            object$family$link == "identity")
  estimate = match.arg(estimate, several.ok = FALSE)

  # extract sampling draws
  draws = as.matrix(object)
  # get design model matrix
  X = model.matrix(object$model, data = object$data)
  n = length(object$residuals)
  # get parameter estimates
  switch(estimate, 
         "Mean" = { b = colMeans(draws[, 1:ncol(X), drop=FALSE])
                    sigma = mean(draws[, ncol(X)+1]) },
         "MAP" =  { map = apply(draws, 2, function(b) 
                                { with(density(b, bw = "SJ"),
                                       x[which.max(y)]) })
                    b = map[1:ncol(X)]
                    sigma = map[ncol(X)+1] } )
  Xb = X %*% b
  # deviance at posterior mean
  Dev = -2 * sum(dnorm(object$y, mean = Xb, sd = sigma, log = TRUE))
  # log-likelihood at parameters draws
  loglik = double(nrow(draws))
  for(s in 1:nrow(draws))
  {
    Xb = X %*% draws[s, 1:ncol(X)]
    sigma = draws[s, ncol(X)+1]
    # loglik at s-th draw
    loglik[s] = sum(dnorm(object$y, mean = Xb, sd = sigma, log = TRUE))
  }
  # effective number of parameters
  k = 2*var(loglik)
  # compute DIC
  DIC = Dev + 2*k
  
  return(list(DIC = DIC, k = k))
}

#' @title Deviance Information Criterion
#' 
#' @description Return DIC for fitted `brmsfit` model. 
#' 
#' @param object an object returned by `brm()` function. 
#' @param estimate a string specifying the point estimate to use.
#' @param \dots additional arguments to be passed to the low level functions.
#' 
#' @details Only available for `family = gaussian(link = "identity")`.
#' 
#' @return The value of DIC.
#'
#' @examples
#' x = rnorm(100)
#' y = 1 + 0.5*x + rnorm(100, 0, 0.5)
#' mod = brm(y ~ x, data = data.frame(x, y), 
#'           family = gaussian(link = "identity"))
#' DIC(mod)
#' 
#' @export

DIC.brmsfit <- function(object, estimate = c("Mean", "MAP"), ...)
{
  stopifnot(inherits(object, "brmsfit"))
  stopifnot(object$family$family == "gaussian" & 
            object$family$link == "identity")
  estimate = match.arg(estimate, several.ok = FALSE)

  # extract sampling draws
  draws = as.matrix(object)
  # get response and design model matrix
  mf = model.frame(formula(object), data = object$data)
  y = model.response(mf)
  X = model.matrix(mf, data = object$data) 
  # X = model.matrix(formula(object$formula), data = object$data)   
  n = nobs(object)
  # get parameter estimates
  switch(estimate, 
         "Mean" = { b = colMeans(draws[, 1:ncol(X), drop=FALSE])
                    sigma = mean(draws[, ncol(X)+1]) },
         "MAP" =  { map = apply(draws, 2, function(b) 
                                { with(density(b, bw = "SJ"),
                                       x[which.max(y)]) })
                    b = map[1:ncol(X)]
                    sigma = map[ncol(X)+1] } )
  Xb = X %*% b
  # deviance at posterior mean/map
  Dev = -2 * sum(dnorm(y, mean = Xb, sd = sigma, log = TRUE))
  # log-likelihood at parameters draws
  loglik = double(nrow(draws))
  for(s in 1:nrow(draws))
  {
    Xb = X %*% draws[s, 1:ncol(X)]
    sigma = draws[s, ncol(X)+1]
    # loglik at s-th draw
    loglik[s] = sum(dnorm(y, mean = Xb, sd = sigma, log = TRUE))
  }
  # effective number of parameters
  k = 2*var(loglik)
  # compute DIC
  DIC = Dev + 2*k
  
  return(list(DIC = DIC, k = k))
}



#' @title Watanabe-Akaike Information Criterion
#' 
#' @description Generic method for computing Watanabe-Akaike 
#' Information Criterion (WAIC)
#' 
#' @param object a fitted model object.
#' @param \dots other arguments.
#' 
#' @return The value of WAIC.
#' 
#' @export

WAIC <- function(object, ...) 
{
  UseMethod("WAIC")
}

#' @title Watanabe-Akaike Information Criterion for `stanreg` model object.
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
#' \dontrun{
#' x = rnorm(100)
#' y = 1 + 0.5*x + rnorm(100, 0, 0.5)
#' mod = stan_glm(y ~ x, data = data.frame(x, y))
#' WAIC(mod)
#' }
#' 
#' @export

WAIC.stanreg <- function(object, ...)
{
  stopifnot(inherits(object, "stanreg"))
  stopifnot(object$family$family == "gaussian" & 
            object$family$link == "identity")
  
  # extract sampling draws
  draws = as.matrix(object)
  # get design model matrix
  X = model.matrix(object$model, data = object$data)
  n = length(object$residuals)
  # 
  loglik = matrix(as.double(NA), nrow = nrow(draws), ncol = n)
  for(s in 1:nrow(draws))
  {
    Xb = X %*% draws[s, 1:ncol(X)]
    sigma = draws[s, ncol(X)+1]
    # loglik at s-th draw
    loglik[s,] = dnorm(object$y, mean = Xb, sd = sigma, log = TRUE)
  }
  
  lpd <- sum(log( colSums(exp(loglik)) / nrow(draws) ))
  # lpd <- mclust::logsumexp(t(loglik)) - log(nrow(draws))
  # effective number of parameters
  k <- sum(apply(loglik, 2, var))
  # compute WAIC
  WAIC <- -2*lpd + 2*k

  return(list(WAIC = WAIC, k = k))
}

#' @title Watanabe-Akaike Information Criterion for `brmsfit` model object.
#' 
#' @description Return WAIC for fitted `brmsfit` model. 
#' 
#' @param object an object returned by `brm()` function. 
#' @param \dots additional arguments to be passed to the low level functions.
#' 
#' @details Only available for `family = gaussian(link = "identity")`.
#' 
#' @return The value of WAIC.
#'
#' @examples
#' x = rnorm(100)
#' y = 1 + 0.5*x + rnorm(100, 0, 0.5)
#' mod = brm(y ~ x, data = data.frame(x, y), 
#'           family = gaussian(link = "identity"))
#' WAIC(mod)
#' 
#' @export

WAIC.brmsfit <- function(object, ...)
{
  stopifnot(inherits(object, "brmsfit"))
  stopifnot(object$family$family == "gaussian" & 
            object$family$link == "identity")
  
  # extract sampling draws
  draws = as.matrix(object)
  # get response and design model matrix
  mf = model.frame(formula(object), data = object$data)
  y = model.response(mf)
  X = model.matrix(mf, data = object$data) 
  n = nobs(object)
  # 
  loglik = matrix(as.double(NA), nrow = nrow(draws), ncol = n)
  for(s in 1:nrow(draws))
  {
    Xb = X %*% draws[s, 1:ncol(X)]
    sigma = draws[s, ncol(X)+1]
    # loglik at s-th draw
    loglik[s,] = dnorm(y, mean = Xb, sd = sigma, log = TRUE)
  }
  
  lpd <- sum(log( colSums(exp(loglik)) / nrow(draws) ))
  # lpd <- sum(mclust::logsumexp(t(loglik)) - log(nrow(draws)))
  # effective number of parameters
  k <- sum(apply(loglik, 2, var))
  # compute WAIC
  WAIC <- -2*lpd + 2*k

  return(list(WAIC = WAIC, k = k))
}

#' @name compareModels
#' @aliases compareModels
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
#' 
#' \dontrun{
#' mod1 = stan_glm(y ~ 1, data = data.frame(x, y))
#' mod2 = stan_glm(y ~ x, data = data.frame(x, y))
#' mod3 = stan_glm(y ~ x + I(x^2), data = data.frame(x, y))
#' compareModels(mod1, mod2, mod3, criterion = "BIC")
#' compareModels(mod1, mod2, mod3, criterion = "DIC")
#' compareModels(mod1, mod2, mod3, criterion = "WAIC")
#' }
#' 
#' Mod1 = brm(y ~ 1, data = data.frame(x, y))
#' Mod2 = brm(y ~ x, data = data.frame(x, y))
#' Mod3 = brm(y ~ x + I(x^2), data = data.frame(x, y))
#' compareModels(Mod1, Mod2, Mod3, criterion = "BIC")
#' compareModels(Mod1, Mod2, Mod3, criterion = "DIC")
#' compareModels(Mod1, Mod2, Mod3, criterion = "WAIC")
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
           tab = cbind(tab, "\u0394BIC" = tab$BIC - min(tab$BIC)) },
         "DIC" = {
           tab = lapply(1:nModels, function(m) DIC(models[[m]]))
           tab = do.call(rbind, lapply(tab, data.frame))
           tab = cbind(tab, "\u0394DIC" = tab$DIC - min(tab$DIC)) },
         "WAIC" = {
           tab = lapply(1:nModels, function(m) WAIC(models[[m]]))
           tab = do.call(rbind, lapply(tab, data.frame))
           tab = cbind(tab, "\u0394WAIC" = tab$WAIC - min(tab$WAIC))
         })
  return(tab[])
}

