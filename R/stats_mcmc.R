#' @name summary.CmdStanFit
#' 
#' @title Summary of MCMC parameters sampling
#' 
#' @description
#' Summary statistics for parameters from MCMC sampling via Stan.
#' 
#' @param object An object of class `'CmdStanFit'` obtained by sampling
#' from `CmdStanModel` via [cmdstanr::cmdstan_model()].
#' @param pars Character vector of parameters to include.
#' @param level The level of the credible intervals. 
#' @param \dots Further arguments passed to or from other methods.
#' 
#' @return 
#' Returns a `data.frame`.
#' 
#' @aliases summary.CmdStanFit print.summary.CmdStanFit
#' 
#' @examples
#' data = list(y = 352, n = 1012, alpha = 2, beta = 2)
#' stan_file = file.path(system.file(package = "bayadvstat"), "stan", "beta-binomial.stan")
#' model = cmdstan_model(stan_file)
#' fit = model$sample(data = data)
#' summary(fit)
#' 
#' stan_file = file.path(system.file(package = "bayadvstat"), "stan", "normal-normal.stan")
#' model = cmdstan_model(stan_file)
#' y = c(9.37, 10.18, 9.16, 11.60, 10.33)
#' data = list(y = y, n = length(y),                  # data
#'             mu_prior = 5, sigma_prior = sqrt(10),  # prior hyperparameters on mu
#'             tau = 1)                               # prior hyperparameters on sigma
#' fit = model$sample(data = data,
#'                    iter_warmup = 1000,
#'                    iter_sampling = 4000,
#'                    chains = 4)
#' summary(fit)
#'                     
#' @exportS3Method
summary.CmdStanFit <- function(object, pars = NULL, level = 0.95, ...) 
{
  stopifnot(inherits(object, "CmdStanFit"))
  #
  draws = object$draws(format = "df", variables = pars)
  pars = names(draws)
  pars = pars[!grepl("^\\..|__$", pars)]
  #
  info = list(model = object$metadata()$model_name)
  info = if(inherits(object, "CmdStanMCMC"))
    append(info, 
           list(algorithm = toupper(object$metadata()$algorithm),
                num_chains = object$num_chains(),
                # thin = object$metadata()$thin,
                iter_warmup = object$metadata()$iter_warmup,
                iter_sampling = object$metadata()$iter_sampling,
                mcmc_sample_size = object$metadata()$iter_sampling*object$num_chains(),
                accept_rate = mean(object$sampler_diagnostics()[,,"accept_stat__"])))
  else
    append(info, 
           list(algorithm = object$metadata()$method,
                draws = object$metadata()$draws))
  #
  stats = summary(draws,
                  "mean" = ~ mean(.), 
                  "map" = ~ sample_mode(.), 
                  "sd" = ~ sd(.), 
                  "median" = ~ median(.),
                  "mad" = ~ mad(.),
                  "q" = ~ quantile2(., probs = c((1-level)/2, (1+level)/2)),
                  "hpd" = ~ HPD_interval(., level = level)[1:2],
                  "ESS" = ~ ess_basic(.), 
                  "Rhat" = ~ rhat(.))
  stats = as.data.frame(stats)
  stats = stats[stats$variable %in% pars,]
  #
  out = list(info = info, stats = stats)
  class(out) = "summary.CmdStanFit"
  return(out)
}

#' @exportS3Method
print.summary.CmdStanFit <- function(x, digits = min(4, getOption("digits")), ...)
{
  cat(cli::rule(left = cli::style_bold("Stan Bayesian estimation"), width = 59), "\n")
  cli::cat_line("Model               = ", x$info$model)
  cli::cat_line("Algorithm           = ", x$info$algorithm)
  if(x$info$algorithm == "HMC")
  {
    cli::cat_line("Chains              = ", x$info$num_chains)
    # cli::cat_line("Thin                = ", x$info$thin)
    cli::cat_line("Warmup iterations   = ", x$info$iter_warmup)
    cli::cat_line("Sampling iterations = ", x$info$iter_sampling)
    cli::cat_line("MCMC sample size    = ", x$info$mcmc_sample_size)
    cli::cat_line("Acceptance rate     = ", round(x$info$accept_rate,4))
  } else
  {
    cli::cat_line("Draws               = ", x$info$draws)
  }
  # cat(cli::rule(width = 59), "\n")
  cat(cli::rule(left = "Parameters", width = 59), "\n")
  stats = x$stats
  rownames(stats) = stats$variable
  print(stats[,colnames(stats) != "variable"], digits = digits)
}

#' @name sample_mode
#' 
#' @title Sample mode
#' 
#' @description Computes the sample mode.
#' 
#' @param x a vector of data values
#' @param \dots additional arguments to be passed to base [density()] function for computing KDE.
#'
#' @return A numerical value.
#'
#' @examples
#' x = rnorm(1000, 10, 1)
#' sample_mode(x)
#' 
#' @export

sample_mode <- function(x, ...) 
{
  x = as.vector(x)
  dens = density(x, ...)
  dens$x[which.max(dens$y)]
}

#' @name HPD_interval
#' 
#' @title Highest posterior density (HPD) intervals from MCMC sampling
#' 
#' @description Computes the highest posterior density intervals from 
#' MCMC sample draws at specified probability level.
#' 
#' @param draws a vector of MCMC draws
#' @param level a numerical value in (0,1) specifying the nominal level of credible interval
#' @param \dots additional arguments to be passed to the low level functions.
#'
#' @return A named list containing 
#' * `lower` the lower limit of credible interval
#' * `upper` the upper limit of credible interval 
#' * `level` the nominal level of credible interval 
#' * `probr` the empirical probability level of computed credible interval 
#'
#' @examples
#' x = rnorm(1e7, 0, 1)
#' HPD_interval(x)
#' 
#' x = rchisq(1e7, df = 5)
#' HPD_interval(x)
#' 
#' @export
HPD_interval <- function(draws, level = 0.95, ...) 
{
  draws = as.vector(draws)
  level = max(0, min(as.numeric(level), 1))
  ndraws = length(draws)
  sdraws = sort(draws)
  m = floor(level * ndraws)           # interval length
  i = ndraws - m                      # index to build contiguos intervals
  w = sdraws[m + 1:i] - sdraws[1:i]  # width of intervals
  out = list(lower = sdraws[which.min(w)],
             upper = sdraws[which.min(w)+m],
             level = level, 
             prob = m/ndraws)
  names(out)[1] = paste0("hpd", signif((1-level)/2*100,4))
  names(out)[2] = paste0("hpd", signif((1+level)/2*100,4))
  return(out)
}


