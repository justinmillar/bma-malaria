runAnalysis <- function(model = c("base", "interactions", "splines")){

  if (model == "base") {
    dir.create("data/out")
    source("R/base/standardize-data.R")
    source("R/base/format-gibbs-input.R")
    source("R/base/gibbs-sampler.R")
  }
 
  if (model == "interactions") {
    dir.create("data/out")
    source("R/int/standardize-data.R")
    source("R/int/format-gibbs-input.R")
    source("R/int/gibbs-sampler.R")
  }
  
  if (model == "splines") {
    dir.create("data/out")
    source("R/spl/standardize-data.R")
    source("R/spl/format-gibbs-input.R")
    source("R/spl/gibbs-sampler.R")
  }
}
