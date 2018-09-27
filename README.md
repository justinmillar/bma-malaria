# Detecting local risk factors for residual malaria in northern Ghana using Bayesian model averaging

This repository holds the data and R code for running the analysis from "Detecting local risk factors for residual malaria in northern Ghana using Bayesian model averaging", published in *Malaria Jounral*. This article is Open Access, so please refer to it for information on the data (and it's sources), and the underlying models and algorthim in the analysis. 

## Running the analyses

The raw data `raw-data.csv` and a descriptive  metadata file `raw-meta.csv` are found in the `data/raw/` subdirectory. The analysis consists of three models: the base model, the seasonal interaction model, and the linear spline model. Each of these models can be running using the `runAnalysis()` function by specifying the argument `model = c("base", "interactions", "splines")`. The function will create a new subdirectory `data/out/` to store the outputs, and then source the specific scripts to:

* Standardize covariates (and create new covariates if neceassary)
* Format data for the Gibbs sampler
* Run the Gibbs sampler

Each model type has it's own subdirectory for these scripts, however there are only slight changes which either genarate new covariates or set conditions for the Gibbs sampler (e.g., do not remove the interaction term when using the `death` or `swap` functions).

## Output files

Running `runAnalysis()` will retain intermediate files during the cleaning/creating and formatting steps, as well as the main outputs from the Gibbs sampler: `gibbs_betas.csv` and `gibbs_others.csv`. 
