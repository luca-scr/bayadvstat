.onAttach <- function(...) 
{
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()
  bayadvstat_attach(needed)
  bayesplot_theme_set(theme_bw(base_family = "sans"))
  color_scheme_set(scheme = "red")
}

is_attached <- function(x) 
{
  paste0("package:", x) %in% search()
}
