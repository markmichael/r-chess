library(plumber)
source("objects.R")
source("methods.R")

pr("api.R") |>
  pr_run(port = 8001)
