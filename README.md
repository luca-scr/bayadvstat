# bayadvstat: Bayesian Advanced Statistics

## Description

An R meta-package accompanying the course *Introduction to Bayesian Inference* (Statistica Avanzata - Modulo 2), Alma Mater Studiorum – Università di Bologna, Campus Rimini, Italy.

## Install

- Update **R** to the latest version available at https://cloud.r-project.org

- Install the C++ toolchain:

  - On Windows OS, install [RTools](https://cran.r-project.org/bin/windows/Rtools/)

  - On Linux and MacOS, see [instructions](https://mc-stan.org/docs/cmdstan-guide/installation.html#cpp-toolchain)

- Install **Stan**:

  - install R package *cmdstanr* by running the following command in R
    (preferably in a fresh R session or restarting your current session)
    ```{r}
    install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
    ```

  - *cmdstanr* requires a working installation of *CmdStan*, the shell interface to Stan.
    ```{r}
    library(cmdstanr)
    ```
    To check that your toolchain is set up properly use:
    ```{r}
    check_cmdstan_toolchain(fix = TRUE)
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

  More info are available at [Getting started with CmdStanR](https://mc-stan.org/cmdstanr/articles/cmdstanr.html)


- Install/update `devtools` package to the latest version:
	  
  ```{r}
  install.packages("devtools")
  ```

- Install/update `bayadvstat` package from GitHub:

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
stan_file = file.path(system.file(package = "bayadvstat"), 
                      "stan", "beta-binomial.stan")
# compile stan file
model = cmdstan_model(stan_file)
# print stan code
print(model)
# fit model
fit = model$sample(data = list(y = 7, n = 11,         # data
                               alpha = 2, beta = 5),  # prior
                   iter_warmup = 1000,
                   iter_sampling = 4000,
                   chains = 4,
                   seed = 123)
summary(fit)
plotDiagnostic(fit)
plotDiagnostic(fit, what = "dens")
plotDiagnostic(fit, what = "acf")
plotPosterior(fit)
```

