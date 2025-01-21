#' @name describe
#' @aliases describe
#' 
#' @title Summary statistics for numerical data
#' 
#' @description Compute summary statistics for the input data.
#' 
#' @param x a vector (univariate), data.frame or matrix (multivariate case)
#'  of numerical values.
#' @param probs a numeric vector of probabilities with values in [0,1] used
#'  for computing quantiles.
#' @param \dots additional arguments to be passed to the low level functions.
#' 
#' @return A `data.frame` containing a description of the dataset. 
#'
#' @examples
#' 
#' x = rnorm(200, 10, 5)
#' describe(x)
#' xy = data.frame(x = rnorm(200, 10, 5), y = rchisq(200, 3))
#' describe(xy)
#' describe(xy[,"y"])
#' 
#' @export

describe <- function(x, probs = c(0, 0.25, 0.75, 1), ...)
{
  xname = deparse(substitute(x))
  x = as.data.frame(x)
  p = ncol(x)
  cnames = if(p == 1) xname else colnames(x)
  out = vector(mode = "list", length = p)
  for(j in 1:p)
  {
    xx = as.vector(x[,j])
    n_na = as.integer(sum(is.na(xx)))
    xx = na.omit(xx)
    n = as.integer(length(xx))
    out[[j]] = data.frame("n" = n, row.names = xname)
    if(n_na > 0) 
      out[[j]] = cbind(out[[j]], "missing" =  n_na)
    out[[j]] = cbind(out[[j]],
                     "mean" = mean(xx), 
                     "sd" = sd(xx), 
                     "median" = median(xx), 
                     "mad" = mad(xx),
                     t(posterior::quantile2(xx, probs = probs)))
  }
  out = do.call(rbind, out)
  rownames(out) = cnames
  return(out)
}
