# Setup this repository

# R packages will be installed from these repositories
repos <- c(
  list(
    raveieeg = "https://rave-ieeg.r-universe.dev",
    crandefault = "https://cloud.r-project.org"
  ),
  as.list(renv:::renv_init_repos())
)


renv::init(bare = TRUE, repos = repos, restart = FALSE)
renv::install("ravemanager", prompt = FALSE, repos = repos)

# Make sure the libpath is under the project
ravemanager:::guess_libpath()

ravemanager::install(python = FALSE)
renv::install(c("BH", "RcppArmadillo", "RcppEigen"))

# Choose 2 if there is uninstalled packages, then proceed
renv::snapshot(repos = repos)
renv::status()


renv::isolate()
renv::init(bare = TRUE, repos = repos, restart = TRUE)
#### Restart!

repos <- c(
  list(
    raveieeg = "https://rave-ieeg.r-universe.dev",
    crandefault = "https://cloud.r-project.org"
  ),
  as.list(renv:::renv_init_repos())
)

# Make sure repos is correct
getOption("repos")

renv::snapshot()






renv::status()
renv::snapshot()
