% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/period-class.R
\name{period_class}
\alias{period_class}
\alias{as_period}
\alias{as_period.default}
\alias{as_period.Date}
\alias{as_period.POSIXt}
\alias{as_period.character}
\alias{as_period.factor}
\alias{new_period}
\alias{is_period}
\title{Period class}
\usage{
as_period(x, n, ...)

\method{as_period}{default}(x, n = 1L, offset = 0L, ...)

\method{as_period}{Date}(x, n = 1L, offset = 0L, ...)

\method{as_period}{POSIXt}(x, n = 1L, offset = 0L, ...)

\method{as_period}{character}(x, n = 1L, offset = 0L, ...)

\method{as_period}{factor}(x, n = 1L, offset = 0L, ...)

new_period(x = integer(), n = 1L, offset = 0L)

is_period(xx)
}
\arguments{
\item{x, xx}{\R objects.

For \code{as_period()} this is the object to be coerced.

For \code{new_period()} this represents the number of periods since the Unix
Epoch (1970-01-01) and a specified offset.}

\item{n}{\verb{[integer]}

Number of days that are being grouped.}

\item{...}{Only used for character input where additional arguments are passed through
to \code{as.Date()}.}

\item{offset}{\verb{[integer]} or, for \code{as_period()}, a \verb{[date]}.

Value you wish to start counting periods from relative to the Unix Epoch:
\itemize{
\item For integer values this is stored scaled by \code{n}
(\code{offset <- as.integer(offset) \%\% n}).
\item For date values this is first converted to an integer offset
(\code{offset <- floor(as.numeric(offset))}) and then scaled via \code{n} as above.
}}
}
\value{
A \verb{<grates_period>} object.
}
\description{
\verb{<grates_period>} objects represent groupings of \code{n} consecutive days
calculated relative to an \code{offset}. It is useful for when you wish to group
an arbitrary number of dates together (e.g. 10 days).
}
\details{
Internally \code{grates_period} objects are stored as the integer number, starting
at 0, of  periods since the Unix Epoch (1970-01-01) and a specified offset. Here
periods are taken to mean groupings of \code{n} consecutive days. For storage and
calculation purposes, \code{offset} is scaled relative to \code{n}
(i.e. \code{offset <- offset \%\% n}) and values of \code{x} stored relative to this
scaled offset.

\code{as_period()} is a generic for coercing input in to \verb{<grates_period>} objects.
It is the recommended way for constructing period objects as it allows the
\code{offset} to be specified as a \code{date} (rather than an integer value relative to
the Unix Epoch).
\itemize{
\item Character input is first parsed using \code{as.Date()}.
\item POSIXct and POSIXlt are converted with their timezone respected.
}

\code{new_period()} is a minimal constructor for \verb{<grates_period>}
objects aimed at developers. It takes, as input, the number of periods since
the Unix Epoch and the specified \code{offset}. \code{double} vectors will
be converted via \code{as.integer(floor(x))}.
}
\examples{

# coercion from date
dat <- as.Date("2012-12-01")
as_period(dat + 0:3, n = 2, offset = dat)

# direct construction
new_period(1:10)

}
