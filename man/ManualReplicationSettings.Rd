% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replication_settings.R
\name{ManualReplicationSettings}
\alias{ManualReplicationSettings}
\title{Create manual replication settings.}
\usage{
ManualReplicationSettings(data)
}
\arguments{
\item{data}{data frame with 1 row per replicate, must contain a column named 'REPLICATE'
with unique integers from 1 to nrow(data), other columns are model parameters to use.}
}
\value{
replication settings
}
\description{
Create manual replication settings.
}
\details{
Use these settings to import custom replicated model parameters.
}
