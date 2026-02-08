#' @name plotDiagnostic
#' 
#' @title Diagnostic plots for MCMC sampling
#' 
#' @description Draw diagnostic plots to be used for checking MCMC convergence. 
#' 
#' @param object an object of class `CmdStanMCMC` or `brmsfit`.
#' @param pars an optional character vector of parameter names to monitor.
#' @param what a string specifying the type of diagnostic plot to draw; available options are `"trace"`, `"dens"`, and `"acf"`. 
#' @param lag a numerical value for the maximum lag to use when computing autocorrelation in ACF plot.
#' @param \dots additional arguments to be passed to the low level functions.
#'
#' @return A `ggplot2` object containing the requested diagnostic plot. 
#'
#' @details
#' The resulting plots are inspired by those used in 
#' Kruschke J.K. (2015) *Doing Bayesian Data Analysis*, 2nd ed.
#' 
#' @examples
#' data = list(y = 352, n = 1012, alpha = 2, beta = 2)
#' stan_file = file.path(system.file(package = "bayadvstat"), "stan", "beta-binomial.stan")
#' model = cmdstan_model(stan_file)
#' fit = model$sample(data = data)
#' plotDiagnostic(fit, what = "trace")
#' plotDiagnostic(fit, what = "dens")
#' plotDiagnostic(fit, what = "acf")
#' # use patchwork to collect graphs:
#' plotDiagnostic(fit, what = "trace") +
#'   plotDiagnostic(fit, what = "dens") +
#'   plotDiagnostic(fit, what = "acf") +
#'   plot_layout(guides = "collect") 
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
#' plotDiagnostic(fit, pars = "mu", what = "trace")
#' plotDiagnostic(fit, pars = "sigma", what = "dens")
#' plotDiagnostic(fit, pars = "mu", what = "acf")
#' wrap_plots(plotDiagnostic(fit, what = "trace"), ncol = 1)
#' 
#' x = rnorm(100)
#' y = 1 + 0.5 * x + rnorm(100)
#' mod = brm(y ~ x, data = data.frame(x, y), 
#'           family = gaussian())
#' p = plotDiagnostic(mod, pars = c("b_Intercept", "b_x", "sigma"), 
#'                    what = "trace")
#' wrap_plots(p) + plot_layout(guides = "collect") 
#'    
#' @export
plotDiagnostic <- function(object, pars = NULL, 
                           what = c("trace", "dens", "acf"), 
                           lag = 20, ...)
{
  stopifnot(inherits(object, "CmdStanMCMC") | 
            inherits(object, "brmsfit"))
  what  = match.arg(what, choices = eval(formals(plotDiagnostic)$what))
  lag = as.numeric(lag)
  
  draws = posterior::as_draws_df(object, variable = pars)
  nchains = length(unique(draws$.chain))
  # select and check par among available parameters (by dropping those with
  # names starting with "." or ending with "__")
  pars_names = colnames(draws)
  pars_names = pars_names[!grepl("^\\..|__$", pars_names)]
  pars = if(is.null(pars)) pars_names else pars_names[match(pars, pars_names)]
  stopifnot(any(pars %in% pars_names))
  if(length(pars) > 1)
  {
    out = lapply(pars, function(x) 
      plotDiagnostic(object, what = what, pars = x, lag = lag))
    return(out)
  }
  
  par = pars[1]
  if(what == "trace")
  {
    draws_par = posterior::extract_variable_matrix(draws, variable = par)
    rhat = posterior::rhat(draws_par)
    plot = bayesplot::mcmc_trace(draws, pars = par, size = 1.5, ...)
    plot = suppressMessages(
      plot + 
        scale_color_manual("Chain", values = .chain_colors(nchains)) +
        labs(subtitle = paste0("Rhat = ", round(rhat,4)), 
             x = "Iterations") )
    return(plot)
  }
  
  if(what == "dens")
  {
    plot = bayesplot::mcmc_dens_overlay(draws, pars = par, ...)
    lims = ggplot_build(plot)$layout$panel_params[[1]]
    xlim = extendrange(r = lims$x.range, f = 0.05)
    ylim = extendrange(r = lims$y.range, f = c(0,0.05))
    plot = suppressMessages(
      plot + 
        scale_color_manual(values = .chain_colors(nchains)) +
        labs(y = "density") +
        lims(x = xlim, y = ylim) +
        theme(axis.text.y = element_text(),
              axis.ticks.y = element_line()) )
    return(plot)
  }

  if(what == "acf")
  {
    draws_par = posterior::extract_variable_matrix(draws, variable = par)
    acf = apply(draws_par, 2, function(x) 
                stats::acf(x, lag.max = lag, plot = FALSE)$acf[-1,,1])
    #    
    ess_ratio = bayesplot::neff_ratio(object, pars = par)
    ess = round(ess_ratio*nrow(draws))
    subtitle = paste0("ESS = ", ess, " (", round(ess_ratio*100,2), "%)")
    #
    df = data.frame(Chain = as.factor(rep(1:nchains, each = lag)),
                    Lag   = rep(1:lag, times = nchains),
                    acf = as.vector(acf))
    plot = ggplot(data = df, 
                  aes(x = Lag, y = acf, color = Chain)) +
      geom_hline(yintercept = 0, lty = 2, col = "darkgrey") +
      geom_line() +
      geom_point(size = 5, col = theme_get()$rect$fill) +
      geom_point() +
      scale_color_manual(values = .chain_colors(nchains)) +
      labs(subtitle = subtitle, 
           y = paste0("Autocorrelation (", par, ")")) +
      bayesplot_theme_get()
    return(plot)
  }

  return()
}

#' @name plotPosterior
#' 
#' @title Posterior plots for MCMC sampling
#' 
#' @description Draw posterior plots for parameters from MCMC convergence. 
#' 
#' @param object an object of class `CmdStanMCMC` or `brmsfit`.
#' @param pars an optional character vector of parameter names to monitor.
#' @param center a string specifying the type of center statistics to draw; available options are `"mean"`, `"map"`, and `"median"`. 
#' @param level a numerical value in (0,1) specifying the level of credible intervals.
#' @param digits significant digits to use for showing posterior statistics.
#' @param \dots additional arguments to be passed to the low level functions.
#'
#' @details
#' The resulting plot is inspired by those used in 
#' Kruschke J.K. (2015) *Doing Bayesian Data Analysis*, 2nd ed.
#' 
#' @return A `ggplot2` object (or a list of object) containing the requested 
#' posterior plot(s).
#'
#' @examples
#' stan_file = file.path(system.file(package = "bayadvstat"), "stan", "beta-binomial.stan")
#' model = cmdstan_model(stan_file)
#' data = list(y = 352, n = 1012, alpha = 2, beta = 2)
#' fit = model$sample(data = data)
#' plotPosterior(fit)
#' plotPosterior(fit, center = "map")
#' plotPosterior(fit, level = 0.9)
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
#' plotPosterior(fit, pars = "mu")
#' plotPosterior(fit, pars = "sigma")
#' patchwork::wrap_plots(plotPosterior(fit))
#' 
#' x = rnorm(100)
#' y = 1 + 0.5 * x + rnorm(100)
#' mod = brm(y ~ x, data = data.frame(x, y), 
#'           family = gaussian())
#' plotPosterior(mod, pars = c("b_Intercept", "b_x", "sigma"))
#' 
#' @export
plotPosterior <- function(object, pars = NULL, 
                          center = c("mean", "map", "median"), 
                          level = 0.95, 
                          digits = min(4, getOption("digits")),
                          ...)
{
  stopifnot(inherits(object, "CmdStanMCMC") | 
            inherits(object, "brmsfit"))
  center  = match.arg(center, choices = eval(formals(plotPosterior)$center))
  level = as.numeric(min(max(0, level), 1))
  
  draws = posterior::as_draws_df(object, variable = pars)
  nchains = length(unique(draws$.chain))
  # select and check par among available parameters (by dropping those with
  # names starting with "." or ending with "__")
  pars_names = colnames(draws)
  pars_names = pars_names[!grepl("^\\..|__$", pars_names)]
  pars = if(is.null(pars)) pars_names else pars_names[match(pars, pars_names)]
  stopifnot(any(pars %in% pars_names))
  if(length(pars) > 1)
  {
    out = lapply(pars, function(x) 
      plotPosterior(object, pars = x, center = center, 
                    level = level, digits = digits))
    return(out)
  }

  par = pars[1]
  r = extendrange(draws[[par]], f = 0)
  brks = seq(min(r), max(r), 
             length.out = max(nclass.FD(draws[[par]]), 
                              nclass.Sturges(draws[[par]])))
  
  plot = ggplot(data = draws, 
                aes(.data[[par]])) + 
    geom_histogram(aes(y = after_stat(density)), 
                   breaks = brks, 
                   fill = .get_color("lh"),
                   color = bayesplot_theme_get()$panel.background$fill) +
    scale_x_continuous(breaks = pretty(draws[[par]], 5)) +
    scale_y_continuous(expand = expansion(mult = c(0.03,0.08)))
  
  text_size = 1.1*theme_get()$text$size*(25.4 / 72.27)
  # get ranges of the data
  ylim = ggplot_build(plot)$layout$panel_scales_y[[1]]$range$range
  xlim = ggplot_build(plot)$layout$panel_scales_x[[1]]$range$range
  # ranges of the plot axis
  yrange = ggplot_build(plot)$layout$panel_params[[1]]$y.range
  xrange = ggplot_build(plot)$layout$panel_params[[1]]$x.range

  if(!is.null(center))
  {
    cent = switch(center, 
                  "mean" = mean(draws[[par]], na.rm = TRUE),
                  "map" = sample_mode(draws[[par]], na.rm = TRUE, ...),
                  "median" = median(draws[[par]], na.rm = TRUE) )
    plot = plot + 
      geom_segment(x = cent, xend = cent, y = 0, yend = ylim[2],
                   lty = 2, col = .get_color("dh")) +
      annotate("text", 
               label = paste0(center, " = ", signif(cent,digits)),
               x = cent, y = (ylim[2] + yrange[2])/2, vjust = 0,
               # fontface = "bold", 
               size = text_size)
  }
  
  hpd = unlist(HPD_interval(draws[[par]], level = level))
  if(!is.null(hpd))
  {
    plot = plot + 
      annotate("segment", 
               x = hpd[1], xend = hpd[2],
               y = min(yrange)/2, yend = min(yrange)/2,
               lwd = 1.5, col = .get_color("dh")) +
      annotate("text", 
               label = signif(hpd[1],digits),
               x = hpd[1], y = abs(min(yrange)), hjust = 0,
               # fontface = "bold", 
               size = text_size) +
      annotate("text", 
               label = signif(hpd[2],digits), 
               x = hpd[2], y = abs(min(yrange)), hjust = 1,
               # fontface = "bold", 
               size = text_size) +
      annotate("text", 
               label = paste0(signif(hpd[3]*100,digits), "% HPD"),
               x = sum(hpd[1:2])/2, y = 3*abs(min(yrange)), hjust = 0.5,
               # fontface = "bold", 
               size = text_size)
  }  

  plot = plot + bayesplot_theme_get()
  return(plot)
}

# Functions from bayesplot package ------------------------------------

.chain_colors <- function (n) 
{
  all_clrs <- unlist(color_scheme_get())
  clrs <- switch(as.character(n), 
                 "1" = .get_color("m"), 
                 "2" = .get_color(c("l", "d")), 
                 "3" = .get_color(c("l", "m", "d")), 
                 "4" = all_clrs[-c(2, 4)], 
                 "5" = all_clrs[-3], 
                 "6" = all_clrs, rep_len(all_clrs, n))
  unname(rev(clrs))
}

.get_color <- function (levels) 
{
  levels <- .full_level_name(levels)
  stopifnot(all(levels %in% .scheme_level_names()))
  color_vals <- color_scheme_get()[levels]
  unlist(color_vals, use.names = FALSE)
}

.full_level_name <- function (x) 
{
  map <- c(l = "light", lh = "light_highlight", m = "mid", 
           mh = "mid_highlight", d = "dark", dh = "dark_highlight", 
           light = "light", light_highlight = "light_highlight", 
           mid = "mid", mid_highlight = "mid_highlight", dark = "dark", 
           dark_highlight = "dark_highlight")
  unname(map[x])
}

.scheme_level_names <- function () 
{
  c("light", "light_highlight", 
    "mid", "mid_highlight", 
    "dark", "dark_highlight")
}
