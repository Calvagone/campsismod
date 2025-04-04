% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic.R, R/parameters.R, R/campsis_model.R
\name{setMinMax}
\alias{setMinMax}
\alias{setMinMax,parameters,parameter,numeric,numeric-method}
\alias{setMinMax,parameters,character,numeric,numeric-method}
\alias{setMinMax,campsis_model,parameter,numeric,numeric-method}
\alias{setMinMax,campsis_model,character,numeric,numeric-method}
\title{Set the minimum and maximum value on a model parameter.}
\usage{
setMinMax(object, parameter, min, max, ...)

\S4method{setMinMax}{parameters,parameter,numeric,numeric}(object, parameter, min, max, ...)

\S4method{setMinMax}{parameters,character,numeric,numeric}(object, parameter, min, max, ...)

\S4method{setMinMax}{campsis_model,parameter,numeric,numeric}(object, parameter, min, max, ...)

\S4method{setMinMax}{campsis_model,character,numeric,numeric}(object, parameter, min, max, ...)
}
\arguments{
\item{object}{model or parameters object}

\item{parameter}{parameter object (Theta, Omega or Sigma)}

\item{min}{minimum value for this parameter when parameter uncertainty is enabled}

\item{max}{maximum value for this parameter when parameter uncertainty is enabled}

\item{...}{extra arguments, unused}
}
\value{
updated object
}
\description{
Set the minimum and maximum value on a model parameter.
}
