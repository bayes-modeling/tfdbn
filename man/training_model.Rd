% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{training_model}
\alias{training_model}
\title{Training bayesian network}
\usage{
training_model(
  training_type,
  data,
  number_layers,
  bl,
  wl,
  n_cluster,
  algorithms,
  number_bootstrap = 100,
  debug = FALSE
)
}
\arguments{
\item{training_type}{Training type, discrete or continuous}

\item{data}{Data for training}

\item{number_layers}{Number of bayesian network layer}

\item{bl}{List of blacklist}

\item{wl}{List of whitelist}

\item{n_cluster}{Number of core for training}

\item{algorithms}{Algorithms for learning network structure and parameters}

\item{number_bootstrap}{Number of bootstrap}

\item{debug}{Debug mode}
}
\value{
An object list of trained model and training parameters
}
\description{
Training bayesian network
}
\examples{
data("data_small")
data("preprocessed")
continuous_data <- preprocessed$data
desire_layers <- preprocessed$desire_layers
bl <- bl_wl$blacklist
wl <- bl_wl$whitelist
n_cluster = 4
algorithms <- c("tabu", "hc")
number_bootstrap = 100
trained <- training_model("continuous", continuous_data, number_layers, bl, wl, n_cluster, algorithms, number_bootstrap)
}
