# The algorithm (encapsulated in the functions below) to convert between dates
# and months relative to the UNIX Epoch comes from the work of Davis Vaughan in
# the unreleased [datea](https://github.com/DavisVaughan/datea/) package.

# ------------------------------------------------------------------------- #
.month_to_days <- function(month) {
    # 719162 is the number of days from 01-01-01 to the Unix Epoch (1970-01-01)
    year <- month %/% 12L + 1970L
    month <- month %% 12L + 1L
    .days_before_year(year) + .days_before_yearmonth(year, month) - 719162L
}

# ------------------------------------------------------------------------- #
.days_before_year <- function(year = integer()) {
    year <- year - 1L
    (year * 365) + (year %/% 4) - (year %/% 100) + (year %/% 400)
}

# ------------------------------------------------------------------------- #
#' @importFrom fastymd is_leap_year
.days_before_yearmonth <- function(year, month) {
    DAYS_BEFORE_yearmonth <- c(0L, 31L, 59L, 90L, 120L, 151L, 181L, 212L, 243L, 273L, 304L, 334L)
    DAYS_BEFORE_yearmonth[month] + ((month > 2) & is_leap_year(year))
}
