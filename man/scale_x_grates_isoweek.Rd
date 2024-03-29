% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale-isoweek.R
\name{scale_x_grates_isoweek}
\alias{scale_x_grates_isoweek}
\title{isoweek scale}
\usage{
scale_x_grates_isoweek(
  ...,
  breaks = ggplot2::waiver(),
  n.breaks = 6L,
  format = NULL
)
}
\arguments{
\item{...}{Not currently used.}

\item{breaks}{A \verb{<grates_isoweek>} vector of the desired breaks.}

\item{n.breaks}{\verb{[integer]}

Approximate number of breaks calculated using \code{scales::breaks_pretty}
(default 6L).

Will only have an effect if \code{breaks = waiver()}.}

\item{format}{Format to use if "Date" scales are required.

If NULL (default) then labels are in the standard yearweek format (YYYY-Www).

If not NULL then the value is used by \code{format.Date()} and can be any input
acceptable by that function.}
}
\value{
A scale for use with ggplot2.
}
\description{
ggplot2 scale for an \verb{<grates_isoweek>} vector.
}
