## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 7,
  fig.height = 5
)

## -----------------------------------------------------------------------------
library(grates)

# Choose some consecutive dates that begin on a Friday
first <- as.Date("2021-01-01")
weekdays(first)
dates <- first + 0:9

# Below we use a Friday-week grouping
weeks <- as_yearweek(dates, firstday = 5L)
(dat <- data.frame(dates, weeks))

# we can also use the constructor function if we already have weeks and years
yearweek(year =c(2020L, 2021L), week = c(1L, 10L), firstday = 5L)

# epiweeks always start on a Sunday
(epiwk <- as_epiweek(Sys.Date()))
weekdays(as.Date(epiwk))

# isoweeks always start on a Sunday
(isowk <- as_isoweek(Sys.Date()))
weekdays(as.Date(isowk))

## -----------------------------------------------------------------------------
library(ggplot2)

# use simulated linelist data from the outbreaks package
dat <- outbreaks::ebola_sim_clean
dat <- dat$linelist$date_of_infection

# calculate the total number for across each week
week_dat <- aggregate(
    list(cases = dat),
    by = list(week = as_epiweek(dat)),
    FUN = length
)

head(week_dat)

# plot the output
(week_plot <-
    ggplot(week_dat, aes(week, cases)) + 
    geom_col(width = 1, colour = "white") +
    theme_bw())

## -----------------------------------------------------------------------------
week_plot + scale_x_grates_epiweek(format = "%Y-%m-%d")

## -----------------------------------------------------------------------------
# calculate the total number for across 14 day periods with no offset.
# note - 0L is the default value for the offset but we specify it explicitly
# here for added clarity
period_dat <- aggregate(
    list(cases = dat),
    by = list(period = as_period(dat, n = 14L, offset = 0L)),
    FUN = length
)

head(period_dat)

# lower date bounds are used for the x axis
ggplot(period_dat, aes(period, cases)) +
    geom_col(width = 1, colour = "white") +
    theme_bw( ) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    xlab("")

# using a date as an offset
start <- as.Date("2020-01-03")
dates <- start + 0:9
offset <- as.Date("2020-01-01")
data.frame(dates, period = as_period(dates, n = 7L, offset = offset))

## -----------------------------------------------------------------------------
# calculate the monthly number of cases
(month_dat <- aggregate(
    list(cases = dat),
    by = list(month = as_yearmonth(dat)),
    FUN = length
))

# plot with centred labels
(month_plot <- 
    ggplot(month_dat, aes(month, cases)) + 
    geom_col(width = 1, colour = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    xlab(""))

# again we can have non-centred date labels by applying the associated scale
month_plot + scale_x_grates_yearmonth(format = "%Y-%m-%d")

# yearquarter works similarly
(quarter_dat <- aggregate(
    list(cases = dat),
    by = list(quarter = as_yearquarter(dat)),
    FUN = length
))

ggplot(quarter_dat, aes(quarter, cases)) + 
    geom_col(width = 1, colour = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    xlab("")

# year also works similarly
(year_dat <- aggregate(
    list(cases = dat),
    by = list(year = as_year(dat)),
    length
))

ggplot(year_dat, aes(year, cases)) + 
    geom_col(width = 1, colour = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    xlab("")

# Construction functions can also be used
yearmonth(2022L, 11L)
yearquarter(2022L, 4L)
year(2022L)

## -----------------------------------------------------------------------------
# calculate the bimonthly number of cases
(bimonth_dat <- aggregate(
    list(cases = dat),
    by = list(group = as_month(dat, n = 2L)),
    FUN = length
))

# by default lower date bounds are used for the x axis
(bimonth_plot <- 
    ggplot(bimonth_dat, aes(group, cases)) + 
    geom_col(width = 1, colour = "white") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    xlab(""))

## -----------------------------------------------------------------------------
month_plot + scale_x_grates_yearmonth(format = NULL)

## -----------------------------------------------------------------------------
# use the unique epiweeks from the earlier example
x <- week_dat$week

# min, max and range
(minx <- min(x))
(maxx <- max(x))
rangex <- range(x)

# seq method works if both `from` and `to` are epiweeks
seq(from = minx, to = maxx, by = 6L)

# but will error informatively if `to` is a different class
try(seq(from = minx, to = 999, by = 6L))

# conversion of yearweek objects back to dates will return the date at the
# lower bound of each yearweek interval
dat <- head(week_dat)
transform(dat, new_date = as.Date(week))

# addition (subtraction) of wholenumbers will add (subtract) the corresponding
# number of weeks to (from) the object
(dat <- transform(dat, plus4 = week + 4L, minus4 = week - 4L))

# addition of two yearweek objects will error as the intention is unclear
try(transform(dat, willerror = week + week))

# Subtraction of two yearweek objects gives the difference in weeks between them
transform(dat, difference = plus4 - minus4)

# epiweeks can be combined with themselves but not other classes (assuming an
# epiweek object is the first entry)
c(minx, maxx)
identical(c(minx, maxx), rangex)
try(c(minx, 1L))

