test_that("epiweek constructor and coercion to date works", {
    dates <- seq.Date(from = as.Date("1970-01-04"), length.out = 4, by = "week")
    storage.mode(dates) <- "double" # needed for R <= 4.1.3
    yw4 <- new_epiweek(0:3)
    expect_identical(as.Date(yw4), dates)
    expect_identical(format(yw4), c("1970-W01", "1970-W02", "1970-W03", "1970-W04"))
    expect_identical(format(new_epiweek()), character())

    dat <- as.Date("1900-01-01") + seq.int(from = 0L, to = 200L * 365, by = 180L)
    expected <- as_epiweek(dat)
    tmp <- as.character(expected)
    years <- as.integer(substr(tmp, 1L, 4L))
    weeks <- as.integer(substr(tmp, 7L, 8L))
    expect_identical(epiweek(years, weeks), expected)
})

test_that("epiweek, pre-epoch dates work", {
    dates <- seq.Date(from = as.Date("1900-01-04"), length.out = 4, by = "week")
    storage.mode(dates) <- "double" # needed for R <= 4.1.3
    dates2 <- seq.Date(from = as.Date("1900-01-04") - 28L, length.out = 4, by = "week")
    storage.mode(dates2) <- "double" # needed for R <= 4.1.3
    expect_identical(as.Date(as_epiweek(dates)), dates - 4L)
    expect_identical(as.Date(as_epiweek(dates) - 4L), dates2 - 4L)

})

test_that("epiweek, january 1 dates", {
    # january 1 dates ---------------------------------------------------------
    # Slightly modified version of tests from Zhian Kamvar's aweek package.
    # This checks that the tricky behaviour at the end/beginning of the year is
    # handled correctly.

    # Epi weeks (firstday sunday)
    yrs <- 2001:2019
    dates <- sprintf("%d-01-01", yrs)
    dates <- as.Date(c(dates, NA_character_))
    dates_epi   <- as_epiweek(dates)
    epiweeks <- c(1, 1, 1, 53, 52, 1, 1, 1, 53, 52, 52, 1, 1, 1, 53, 52, 1, 1, 1, NA)
    epiyears <- c(2001, 2002, 2003, 2003, 2004, 2006, 2007, 2008, 2008, 2009,
                  2010, 2012, 2013, 2014, 2014, 2015, 2017, 2018, 2019, NA)
    expected <- sprintf("%d-W%02d", epiyears, epiweeks)
    expected[length(expected)] <- NA_character_
    expect_identical(as.character(dates_epi), expected)
    expect_identical(dates_epi, suppressWarnings(epiweek(epiyears, epiweeks)))
    expect_identical(
        suppressWarnings(as_epiweek(as.character(dates_epi), format = "yearweek")),
        dates_epi
    )
})

test_that("epiweek, POSIXlt coercion works", {
    nz <- as.POSIXlt("2021-01-03 02:00:00", tz = "NZ")
    utc <- as.POSIXlt("2021-01-03 00:00:00", tz = "UTC")
    result <- as.POSIXlt(as_epiweek(nz))
    expect_identical(result, utc)

    pos <- as.POSIXlt("2021-01-04", tz = "UTC")
    expect_identical(as.Date(as_epiweek(pos)), as.Date("2021-01-03"))

    dat <- "2021-01-03"
    res <- as.POSIXlt(as_epiweek(dat))
    expect_s3_class(res, "POSIXlt")
    expect_identical(julian(res), julian(as.POSIXlt("2021-01-03", tz = "UTC")))
    expect_error(
        as.POSIXlt(as_epiweek(dat), tz = "GMT"),
        "<grates_epiweek> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
        fixed = TRUE
    )
})

test_that("epiweek, POSIXct coercion works", {
    nz <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "NZ"))
    dat <- as_epiweek(nz)
    result <- as.POSIXct(dat)
    expect_identical(result, as.POSIXct(as.POSIXlt("2021-01-03", tz = "UTC")))

    dat <- "2021-01-04"
    pos <- as.POSIXct(as.POSIXlt("2021-01-04", tz = "UTC"))
    res7 <- as_epiweek(pos)
    expect_identical(as.Date(res7), as.Date("2021-01-03"))

    dat <- "2021-01-03"
    res <- as.POSIXct(as_epiweek(dat))
    expect_identical(res, as.POSIXct(as.POSIXlt(dat), tz = "UTC"))
    expect_error(
        as.POSIXct(as_epiweek(dat), tz = "GMT"),
        "<grates_epiweek> objects can only be converted to UTC. If other timezones are required, first convert to <Date> and then proceed as desired.",
        fixed = TRUE
    )
})

test_that("epiweek, character coercion works", {
    dat <- "2021-01-04"
    res <- as_epiweek(dat)
    expect_identical(as.Date(res), as.Date(dat) - 1L)

    dat <- as.factor("2021-01-04")
    res <- as_epiweek(dat)
    expect_identical(as.Date(res), as.Date(dat) - 1L)

    dat <- "2020-12-28"
    res <- as.character(as_epiweek(dat))
    expect_identical(res, "2020-W53")
})

test_that("as_epiweek, misc errors and warnings", {
    expect_error(as_epiweek("bob"))

    expect_error(
        as_epiweek(TRUE),
        "Not implemented for class [logical].",
        fixed = TRUE
    )

    expect_warning(
        as_epiweek("bob", format = "yearweek"),
        "Some entries invalid for given `year` and `week` values. Returning these as NA.",
        fixed = TRUE
    )
})


test_that("epiweek, as.list works", {
    dat <- as_epiweek(c("2020-12-28", "2021-01-04"))
    res <- list(as_epiweek("2020-12-28"), as_epiweek("2021-01-04"))
    expect_identical(res, as.list(dat))
})

test_that("epiweek, accessors", {
    dat <- as_epiweek(as.Date("2020-12-28"))
    expect_identical(get_year(dat), 2020L)
    expect_identical(get_week(dat), 53)
    expect_error(get_year("bob"))
    expect_error(get_week("bob"))
    dat <- as_epiweek(Sys.Date())
    expect_true(is_epiweek(dat))
    expect_false(is_epiweek("bob"))
})

test_that("epiweek, is_epiweek works", {
    dat <- as_epiweek(Sys.Date())
    expect_true(is_epiweek(dat))
    expect_false(is_epiweek("bob"))
})

test_that("epiweek, subsetting works", {
    x <- Sys.Date()
    dat <- as_epiweek(x) + 0:1
    dat <- setNames(dat, c("a", "b"))
    expect_identical(dat[1], c(a=as_epiweek(x)))
    expect_identical(dat[[2]], as_epiweek(x + 7))
    dat[1] <- as_epiweek(x+14)
    expect_identical(dat[1], c(a=as_epiweek(x + 14)))
    dat[[2]] <- dat[[1]]
    expect_identical(dat[[2]], dat[[1]])
    expect_error(
        dat[1] <- "bob",
        "Can only assign <grates_epiweek> objects in to an <grates_epiweek> object.",
        fixed = TRUE
    )
    expect_error(
        dat[1] <- as_yearweek(x, 7),
        "Can only assign <grates_epiweek> objects in to an <grates_epiweek> object.",
        fixed = TRUE
    )
})

test_that("epiweek, combine works", {
    x <- Sys.Date()
    dat <- as_epiweek(x)
    expect_identical(c(dat, dat), as_epiweek(c(x, x)))
    dat2 <- as_yearweek(x, 7)
    expect_error(
        c(dat, "bob"),
        "Unable to combine <grates_epiweek> objects with other classes.",
        fixed = TRUE
    )
    expect_error(
        c(dat, dat2),
        "Unable to combine <grates_epiweek> objects with other classes.",
        fixed = TRUE
    )
})

test_that("epiweek operators and math work", {
    # comparison operators ----------------------------------------------------
    x <- Sys.Date()
    dat <- as_epiweek(x)
    expect_true(dat == dat)
    expect_false(dat != dat)
    expect_true(dat <= dat + 1)
    expect_true(dat >= dat - 1)
    expect_true(dat < dat + 1)
    expect_true(dat > dat - 1)
    expect_true(dat != dat + 1)
    expect_error(
        dat == 1,
        "Can only compare <grates_epiweek> objects with <grates_epiweek> objects.",
        fixed = TRUE
    )

    # addition ----------------------------------------------------------------
    x <- as.Date("2021-01-04")
    y <- as.Date("2021-01-03")
    dat1 <- as_epiweek(x)
    dat2 <- dat1 + 0:1
    expect_identical(as.Date(dat2), c(y, y + 7))
    expect_identical(dat2, 0:1 + dat1)
    expect_identical(+dat1, dat1)
    expect_error(
        dat1 + 1.5,
        "Can only add integers to <grates_epiweek> objects.",
        fixed = TRUE
    )
    expect_error(
        dat1 + dat1,
        "Cannot add <grates_epiweek> objects to each other.",
        fixed = TRUE
    )

    # subtraction -------------------------------------------------------------
    x <- as.Date("2021-01-04")
    y <- as.Date("2021-01-03")
    dat1 <- as_epiweek(x)
    dat2 <- dat1 - 0:1
    expect_identical(as.Date(dat2), c(y, y - 7))
    expect_identical(as.integer(dat2 - dat1), c(0L, -1L))
    expect_error(
        1 - dat1,
        "Can only subtract from a <grates_epiweek> object, not vice-versa.",
        fixed = TRUE
    )
    expect_error(
        -dat1,
        "Cannot negate a <grates_epiweek> object.",
        fixed = TRUE
    )
    expect_error(
        dat1 - 1.5,
        "Can only subtract whole numbers and other <grates_epiweek> objects from <grates_epiweek> objects.",
        fixed = TRUE
    )

    # Other operations error
    x <- as_epiweek(as.Date("2021-01-05"))
    expect_error(dat * 3)
    expect_error(dat / 3)
    expect_error(dat ^ 3)
    expect_error(dat %% 3)
    expect_error(dat %/% 3)
    expect_error(dat & 3)
    expect_error(dat | 3)
    expect_error(!dat)

    # Math
    x <- as_epiweek(as.Date("2021-01-04"))
    dat <- c(x + 0:1, new_epiweek(NA_integer_))
    expect_identical(is.nan(dat), c(FALSE, FALSE, FALSE))
    expect_identical(is.finite(dat), c(TRUE, TRUE, FALSE))
    expect_identical(is.infinite(dat), c(FALSE, FALSE, FALSE))
    expect_error(abs(dat))

})

test_that("epiweek, miscellaneous work", {
    expect_identical(new_epiweek(-1.5), new_epiweek(-2L))
    expect_error(new_epiweek("bob"), "`x` must be integer.", fixed = TRUE)
    dat <- Sys.Date()
    dat <- c(dat, dat - 45L)
    dat <- as_epiweek(dat)
    expect_identical(rep(dat, 2L), c(dat, dat))
    expect_identical(rep(dat, each = 2L), c(dat[[1]], dat[[1]], dat[[2]], dat[[2]]))
    expect_identical(unique(c(dat, dat)), dat)
    dat <- as_epiweek(as.Date("1970-01-04"))
    expect_identical(
        seq(dat, dat + 6L, by = 2L),
        new_epiweek(c(0L, 2L, 4L, 6L))
    )
    expect_error(
        seq(dat, dat + 11, by = 2.5),
        "`by` must be an integer of length 1.",
        fixed = TRUE
    )

    expect_error(
        seq(dat, as.integer(dat + 11), by = 2.5),
        "`to` must be a <grates_epiweek> object of length 1.",
        fixed = TRUE
    )
    expect_identical(as.integer(new_epiweek(100L)), 100L)
    expect_identical(as.double(new_epiweek(100L)), 100)
    expect_identical(min(c(dat, dat+11)), dat)
    expect_identical(max(c(dat, dat+11)), dat+11)
    expect_identical(range(seq(dat, dat + 12, by = 2L)), c(dat, dat+12))
    expect_error(
        any(dat),
        "`any()` is not supported for <grates_epiweek> objects.",
        fixed = TRUE
    )

    expect_identical(epiweek(1L,1L),epiweek(1.5,1.5))
    expect_error(epiweek(1L), "Cannot recycle a vector of length 0:", fixed = TRUE)
    expect_error(epiweek(1:2,1:3), "Can only recycle vectors of length 1:", fixed = TRUE)
    expect_error(epiweek(year = character()), "`year` must be integer.", fixed = TRUE)
    expect_error(epiweek(week = character()), "`week` must be integer.", fixed = TRUE)
    expect_false(is.numeric(epiweek(1L,1L)))
})

test_that("epiweek boundary functions work", {
    dates <- as.Date("2020-01-01") + 0:14
    weeks <- as_epiweek(dates)
    starts <- as.Date(weeks)
    ends <- starts + 7 - 1L
    expect_identical(date_start(weeks), starts)
    expect_identical(date_end(weeks), ends)
})
