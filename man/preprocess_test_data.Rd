% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{preprocess_test_data}
\alias{preprocess_test_data}
\title{Pre-processing data before calculate score}
\usage{
preprocess_test_data(
  data,
  type,
  index_column,
  time_column,
  time_values,
  continuous_kpi_names,
  continuous_static_kpi_names,
  discrete_static_kpi_names,
  current_layers,
  desire_layers,
  quantile_number = -1,
  debug = FALSE
)
}
\arguments{
\item{data}{A data frame which rows are financial reports and columns is KPI.}

\item{type}{Data will preprocess as discrete or continuous}

\item{index_column}{Column name of company id}

\item{time_column}{Column name of time value}

\item{time_values}{Time values by year}

\item{continuous_kpi_names}{Column names of continuous dynamic KPI}

\item{continuous_static_kpi_names}{Column names of continuous static (un-change over time) KPI}

\item{discrete_static_kpi_names}{Columns names of discrete static (un-change over time) KPI}

\item{current_layers}{Maximum number of layers in bayesian network, equal number of time values}

\item{desire_layers}{Number of layers for training bayesian network, less than or equal current layers}

\item{quantile_number}{For type is "discrete", number of quantile for each continuous variable}

\item{debug}{Debug mode}
}
\value{
A processed data frame with pre-process params
}
\description{
Pre-processing data before calculate score
}
\examples{
data("preprocessed")
data("data_small")
type <- preprocessed$type
index_column <- preprocessed$index_column
time_column <- preprocessed$time_column
time_values <- preprocessed$time_values
continuous_kpi_names <- preprocessed$continuous_kpi_names
continuous_static_kpi_names <- preprocessed$continuous_static_variables
discrete_static_kpi_names <- preprocessed$discrete_static_variables
current_layers <- preprocessed$current_layers
desire_layers <- preprocessed$desire_layers
quantile_number <- -1
test_data <- tfdbn::preprocess_test_data(data, type, index_column, time_column, time_values,
                                         continuous_kpi_names, continuous_static_kpi_names,
                                         discrete_static_kpi_names, current_layers, desire_layers, quantile_number)
}
