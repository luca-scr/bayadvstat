# bayadvstat: Bayesian Advanced Statistics

## Description

An R meta-package accompanying the course *Introduction to Bayesian Inference* (Statistica Avanzata - Modulo II) at Alma Mater Studiorum – Università di Bologna, Italy.

## Install

- Update **R** to the latest version available at https://cloud.r-project.org

- Install **Stan** by installing:

  1. R package *cmdstanr* by running the following command in R
  (preferably in a fresh R session or restarting your current session)
  ```{r}
  install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
  ```

  2. *cmdstanr* requires a working installation of *CmdStan*, the shell interface to Stan.\
  To check that your toolchain is set up properly use:
  ```{r}
  check_cmdstan_toolchain()
  ```
  If your toolchain is configured correctly then CmdStan can be installed by calling:
  ```{r}
  install_cmdstan()
  ```
  To check the path to the CmdStan installation and the CmdStan version number you can use:
  ```{r}
  cmdstan_path()
  cmdstan_version()
  ```

  More info are available at https://mc-stan.org/cmdstanr/articles/cmdstanr.html


- Install/update `devtools` package to the latest version:
	  
  ```{r}
  install.packages("devtools")
  ```

- Install/update `bayadvstat` package

  ```{r}
  devtools::install_github("luca-scr/bayadvstat")
  ```

## Usage

At each new R session just run the following command:

```{r}
library(bayadvstat)
```

To check that everything is working fine:

```{r}
# get stan file
pkg_path = system.file(package = "bayadvstat")
stan_file = file.path(pkg_path, "stan", "beta-binomial.stan")
# print stan file
cat(readLines(stan_file), sep = "\n")
# compile stan file
model = cmdstanr::cmdstan_model(stan_file)

# fit model
fit = model$sample(data = list(y = 7, n = 11,         # data
                               alpha = 2, beta = 5),  # prior
                   iter_warmup = 1000,
                   iter_sampling = 4000,
                   chains = 4,
                   seed = 123)
fit
```
