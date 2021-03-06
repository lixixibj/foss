% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.multiple.sub.seasonal.series.R
\name{get.new.horizen1}
\alias{get.new.horizen1}
\title{decide to forecast how many points need to be forecast in the horizen in every new ts forecasting}
\usage{
get.new.horizen1(combination.months, formatted.date.of.the.test.data)
}
\arguments{
\item{combination.months}{combined month, eg,c(1,2,3)
@param formatted.date.of.the.test.data  formatted date for the test data of ts}
}
\value{
result=list(new.horizen=counter,forecasts.position=forecasts.position)
         counter: the number of horizen for every new ts
         forecasts.position: the position of ervery forecasts
}
\description{
decide to forecast how many points need to be forecast in the horizen in every new ts forecasting
}
