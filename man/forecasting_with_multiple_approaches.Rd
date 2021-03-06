% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecasting.with.sub.seasons.R
\name{forecasting_with_multiple_approaches}
\alias{forecasting_with_multiple_approaches}
\title{forecast with multiple approches with parellel computing for one time series}
\usage{
forecasting_with_multiple_approaches(
  ts,
  type.of.ts,
  method.option,
  level.value
)
}
\arguments{
\item{ts}{M4[[1]]}

\item{type.of.ts}{'M' for monthly data and 'Q' for quarterly data}

\item{method.option}{'ets' or 'arima'}

\item{level.value}{95}
}
\value{
list(smape,mase) : smape and mase for every step forecasting(h=1, h=1-2,...)
}
\description{
forecast with multiple approches with parellel computing for one time series
}
