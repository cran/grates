## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 7,
  fig.height = 5
)

## ----setup, message=FALSE-----------------------------------------------------
library(grates)
library(ggplot2)
library(outbreaks)
library(dplyr)

## ----yearweekdemo-------------------------------------------------------------
# create weekday names
wdays <- weekdays(as.Date(as_yearweek(as.Date("2020-01-01"), firstday = 1L)) + 0:6)
wdays <- setNames(1:7, wdays)

# example of how weeks vary by firstday over December and January
dates <- as.Date("2020-12-29") + 0:5
dat <- lapply(wdays, function(x) as_yearweek(dates, x))
bind_cols(dates = dates, dat)

## ----yearweekconventions, error=TRUE------------------------------------------
dates <- as.Date("2021-01-01") + 0:30
weeks <- as_yearweek(dates, firstday = 5) # firstday = 5 to match first day of year
head(weeks, 8)
str(weeks)
dat <- tibble(dates, weeks)

# addition of wholenumbers will add the corresponding number of weeks to the object
dat %>% 
  mutate(plus4 = weeks + 4)

# addition of two yearweek objects will error as it is unclear what the intention is
dat %>% 
  mutate(plus4 = weeks + weeks)

# Subtraction of wholenumbers works similarly to addition
dat %>% 
  mutate(minus4 = weeks - 4)

# Subtraction of two yearweek objects gives the difference in weeks between them
dat %>% 
  mutate(plus4 = weeks + 4, difference = plus4 - weeks)

# weeks can be combined if they have the same firstday but not otherwise
wk1 <- as_yearweek("2020-01-01")
wk2 <- as_yearweek("2021-01-01")
c(wk1, wk2)
wk3 <- as_yearweek("2020-01-01", firstday = 2)
c(wk1, wk3)

# load some simulated linelist data
dat <- ebola_sim_clean$linelist

# Example of week plot
week_plot <- 
  dat %>%
  mutate(week = as_yearweek(date_of_infection), firstday = 7) %>% 
  count(week, name = "cases") %>% 
  na.omit() %>% 
  ggplot(aes(week, cases)) + geom_col(width = 1, colour = "white") + theme_bw()

week_plot

## ----yearweekplots2-----------------------------------------------------------
week_plot + scale_x_grates_yearweek(format = "%Y-%m-%d", firstday = 7)

## ----month--------------------------------------------------------------------
month_dat <- 
  dat %>%
  mutate(date = as_month(date_of_infection, n = 2)) %>% 
  count(date, name = "cases") %>% 
  na.omit()

month_plot <- 
  ggplot(month_dat, aes(date, cases)) + 
    geom_col(width = 2, colour = "white") +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
    xlab("")

month_plot

month_plot + scale_x_grates_month(format = NULL, n = 2, origin = 0)

## ----yearothers---------------------------------------------------------------
# create weekday names
dates <- seq(from = as.Date("2020-01-01"), to = as.Date("2021-12-01"), by = "1 month")

as_quarter(dates)
as_year(dates)
as_quarter(dates[1]) + 0:1
as_year(dates[1]) + 0:1

dat %>%
  mutate(date = as_quarter(date_of_infection)) %>% 
  count(date, name = "cases") %>% 
  na.omit() %>% 
  ggplot(aes(date, cases)) + 
    geom_col(width = 1, colour = "white") + 
    scale_x_grates_quarter(n.breaks = 10) +
    theme_bw() + 
    xlab("")

dat %>%
  mutate(date = as_year(date_of_infection)) %>% 
  count(date, name = "cases") %>% 
  na.omit() %>% 
  ggplot(aes(date, cases)) + 
    geom_col(width = 1, colour = "white") + 
    scale_x_grates_year(n.breaks = 2) +
    theme_bw() + 
    xlab("")

## ----period-------------------------------------------------------------------
dat %>%
  mutate(date = as_period(date_of_infection, n = 14)) %>% 
  count(date, name = "cases") %>% 
  na.omit() %>% 
  ggplot(aes(date, cases)) + 
    geom_col(width = 14, colour = "white") + 
    theme_bw() + 
    xlab("")

dat %>%
  mutate(date = as_period(date_of_infection, n = 28)) %>% 
  count(date, name = "cases") %>% 
  na.omit() %>% 
  ggplot(aes(date, cases)) + 
    geom_col(width = 28, colour = "white") + 
    theme_bw() + 
    xlab("")

