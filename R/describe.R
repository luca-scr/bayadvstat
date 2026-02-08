#' @name describe
#' @aliases describe
#' 
#' @title Descriptive statistics
#' 
#' @description Compute descriptive statistics for input data.
#' 
#' @param data a matrix, data.frame, or vector.
#' @param by the name of a variable to condition on,
#' @param probs a vector of values in \eqn{[0,1]} specifying the quantiles to 
#'  be computed. 
#' @param digits significant digits.
#' @param x an object of class `describe`.
#' @param \dots additional arguments.
#'
#' @return A `data.frame` containing a description of the dataset. 
#'
#' @examples
#' 
#' describe(iris)
#' describe(iris, by = Species)
#'
#' @export

describe <- function(data, by, probs = c(0, 0.25, 0.5, 0.75, 1), ...)
{
  data_name = deparse(substitute(data))
  if(!isa(data, "data.frame"))
  {
    vars_name <- if(is.vector(data)) data_name else colnames(data)
    data <- as.data.frame(data)
    if(!is.null(vars_name))
      colnames(data) <- vars_name
  }
  vars_name <- colnames(data)
  
  if(!missing(by))
  {
    by_name <- deparse(substitute(by))
    #
    if(!is.null(data[[by_name]]))
    {
      by <- as.factor(data[[by_name]])
    } else
    if(exists(by_name))
    {
      by <- as.factor(by)
    } else
    {
      stop(by_name, "not available in data.frame", data_name,
           "or object not found")
    }
    #
    x <- split(data[setdiff(vars_name, by_name)], by)
    out <- vector("list", length = nlevels(by))
    for(i in seq(nlevels(by)))
    {
      out[[i]] <- describe(x[[i]])
      names(out[[i]]$describe) <- setdiff(vars_name, by_name)
    }
    names(out) <- levels(by)
    out$by <- by_name
    class(out) <- c("describe")
    return(out)
  }

  nvar <- length(vars_name)
  obj <- vector(mode = "list", length = nvar)
  # names(obj) <- if(nvar > 1) vars_name else data_name
  names(obj) <- vars_name
  type <- rep(NA, nvar)

  opt.warn <- options("warn")  # save default warning option
  options(warn=-1)             # and suppress warnings
  for(j in seq(nvar))
  {
    x <- data[,j]
    if(inherits(x, "factor") | 
       inherits(x, "character") | 
       inherits(x, "logical"))
    {
      type[j] <- "factor"
      out <- summary(as.factor(x))
      obj[[j]] <- data.frame("count" = out, 
                             "%" = out/sum(out)*100, 
                             check.names = FALSE)
    } else if(inherits(x, "POSIXt") | inherits(x, "Date"))
    {
      type[j] <- "POSIXt"
      obj[[j]] <- as.data.frame(summary(x)[c(1,6)])
    } else
    {
      type[j] <- "numeric"
      x <- na.omit(x)
      q <- quantile(x, probs = probs)
      names(q) <- paste0("q", probs * 100)
      out <- cbind(data.frame("n" = length(x), 
                              "mean" = mean(x), 
                              "sd" = sd(x)), 
                   t(q))
      row.names(out) <- vars_name[j]
      obj[[j]] <- out
    }
  }

  obj <- list(name = data_name, describe = obj, type = type)
  class(obj) <- "describe"
  options(warn = opt.warn$warn)
  return(obj)
}

#' @rdname describe
#' @export

print.describe <- function(x, digits = getOption("digits"), ...)
{

  if(!is.null(x$by))
  {
    by <- which(sapply(x, class) == "describe")
    for(i in by)
    {
      label <- paste(x$by, "=", names(x)[i])
      cat(paste("--", label, 
                paste(rep("-", getOption("width")-nchar(label)-4), collapse = "")),
          "\n")
      print(x[[i]])
      if(i < length(by)) cat("\n")
    }
    return(invisible())
  }

  descr <- x$describe
  isNum   <- (x$type == "numeric")
  isFct   <- (x$type == "factor")
  isPOSIX <- (x$type == "POSIXt")
  
  if(sum(isNum) > 0)
  {
    out1 <- do.call("rbind", descr[isNum])
    print(zapsmall(out1, digits = digits))
  }
  
  if(sum(isFct) > 0)
  { 
    out1 <- descr[isFct]
    for(j in seq(out1))
    {
      cat("\n")
      outj <- zapsmall(out1[[j]], digits = digits)
      outj <- cbind(rownames(outj), outj)
      colnames(outj) <- c(names(out1)[j], colnames(outj)[-1])
      print(outj, row.names = FALSE)
    }
  }
    
  if(sum(isPOSIX) > 0)
  {
    out1 <- descr[isPOSIX]
    for(j in seq(out1))
    {
      cat("\n")
      outj <- t(out1[[j]])
      rownames(outj) <- names(out1)[j]
      print(noquote(outj))
    }
  }
  
  invisible()
}
