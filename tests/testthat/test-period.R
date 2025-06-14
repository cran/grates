test_that("period constructor and coercion to date works", {
    dat <- 0:3
    datesw <- seq.Date(from = as.Date("1970-01-01"), length.out = 4, by = 7)
    storage.mode(datesw) <- "double" # needed for R <= 4.1.3

    expect_identical(as.Date(new_period(dat, n = 7)), datesw)
    expect_error(new_period(dat, n = -1L))

    # formatting --------------------------------------------------------------
    dat <- 0:3
    expect_identical(
        format(new_period(dat, n = 7)),
        c(
            "1970-01-01 to 1970-01-07",
            "1970-01-08 to 1970-01-14",
            "1970-01-15 to 1970-01-21",
            "1970-01-22 to 1970-01-28"
        )
    )

    expect_identical(format(new_period()), character())
})

test_that("period, pre-epoch dates work", {
    dates <- seq.Date(from = as.Date("1900-01-01"), length.out = 4, by = 7)
    dates2 <- seq.Date(from = as.Date("1900-01-01") - 28, length.out = 4, by = 7)
    storage.mode(dates) <- "double" # needed for R <= 4.1.3
    storage.mode(dates2) <- "double" # needed for R <= 4.1.3
    expect_identical(as.Date(as_period(dates, n=7, offset = as.integer(as.Date("1900-01-01")))), dates)
    expect_identical(as.Date(as_period(dates, n=7, offset = as.integer(as.Date("1900-01-01"))) - 4), dates2)
})

test_that("period, january 1 dates", {
    dates <- as.Date("2020-01-01") + (0:61)
    dat <- as_period(dates, n = 2, offset = as.integer(as.Date("2020-01-01")))
    expect_identical(as.Date(dat), rep(dates[c(TRUE, FALSE)], each = 2))
})

test_that("period, POSIXlt coercion works", {
    nz <- as.POSIXlt("2021-02-04", tz = "NZ")
    result <- as_period(nz, offset = as.integer(as.Date("2021-02-03")),  n = 2)
    result <- as.POSIXlt(result)
    expected <- as.POSIXlt("2021-02-03", tz = "UTC")
    expect_identical(result, expected)

    dat <- as.POSIXlt("2021-01-04", tz = "UTC")
    res <- as_period(dat, n = 2, offset = as.integer(as.Date("2021-01-01")))
    expect_identical(res, as_period(as.Date("2021-01-04"), n = 2, offset = as.integer(as.Date("2021-01-01"))))
    expected <- as.Date("2021-01-03")
    expect_identical(as.Date(res), expected)

    dat <- "2021-01-04"
    res <- as.POSIXlt(as_period(dat, offset = as.integer(as.Date("2021-01-03")), n = 2))
    expect_s3_class(res, "POSIXlt")
    expect_identical(julian(res), julian(as.POSIXlt("2021-01-03", tz = "UTC")))
})

test_that("period, POSIXct coercion works", {
    nz <- as.POSIXct(as.POSIXlt("2021-02-04", tz = "NZ"), tz = "NZ")
    result <- as.POSIXct(
        as_period(nz, offset = as.integer(as.Date("2021-02-03")), n = 2),
        tz = "UTC"
    )
    expected <- as.POSIXct(as.POSIXlt("2021-02-03", tz = "UTC"), tz = "UTC")
    expect_identical(result, expected)

    dat <- "2021-01-04"
    res <- as.POSIXct(as_period(dat, offset = as.integer(as.Date("2021-01-03")), n = 2))
    expect_identical(res, as.POSIXct(as.POSIXlt("2021-01-03"), tz = "UTC"))
})

test_that("period, character coercion works", {
    dat <- "2021-01-04"
    res <- as_period(dat, n = 2, offset = as.integer(as.Date(dat)))
    expected <- as.Date("2021-01-04")
    expect_identical(as.Date(res), expected)

    dat <- as.factor("2021-01-04")
    res <- as_period(dat, n = 3, offset = as.integer(as.Date(dat)) - 1)
    expected <- as.Date("2021-01-03")
    expect_identical(as.Date(res), expected)

    dat <- "2020-12-28"
    res <- as.character(as_period(dat, n = 3))
    expect_identical(res, "2020-12-28 to 2020-12-30")
})

test_that("as_period, misc errors and warnings", {
    expect_error(as_period(0:3))
    expect_error(as_period(TRUE))
    expect_error(as_period("bob"))
    expect_error(as_period("2021-W53"))

})

test_that("period, as.list works", {
    dat <- as_period(c("2020-12-28", "2021-01-04"), n = 2)
    res <- list(
        as_period("2020-12-28", n = 2),
        as_period("2021-01-04", offset = as.integer(as.Date("2020-12-28")), n = 2))
    expect_identical(res, as.list(dat))
})

test_that("period, accessors", {
    expect_error(get_n("bob"))
    expect_error(get_offset("bob"))

    dat <- as_period(
        as.Date("2020-12-28"),
        offset = as.integer(as.Date("2020-12-26")),
        n = 55
    )
    expect_identical(get_n(dat), 55L)
    expect_identical(get_offset(dat), as.integer(as.Date("2020-12-26")) %% 55L)
    expected <- as.Date("2020-12-26")
    expect_identical(as.Date(dat), expected)
})

test_that("period, is_period works", {
    dat <- as_period(Sys.Date(), n = 2)
    expect_true(is_period(dat))
    expect_false(is_period("bob"))
})

test_that("period, subsetting works", {
    x <- as.Date("2021-01-15")
    dat <- as_period(x, n = 31) + 0:1
    expect_identical(dat[1], as_period(x, n = 31))
    expect_identical(dat[[2]], as_period(x + 31, n = 31))

    dat[1] <- dat[2]
    expect_identical(dat[1], as_period(x + 31, n = 31))

    expect_error(dat[1] <- "bob")
    expect_error(dat[1] <- as_yrwk(x))
})

test_that("period, combine works", {
    x <- Sys.Date()
    dat1 <- as_period(x, n = 2)
    dat2 <- as_yearweek(x)
    expect_error(c(dat1, "bob"))
    expect_error(c(dat1, dat2))

    x <- as.Date("2020-05-26")
    dat <- as_period(x, n = 2)
    expect_identical(c(dat, dat), as_period(c(x, x), n = 2))
})

test_that("period operators and math work", {
    # comparison operators ----------------------------------------------------
    x <- Sys.Date()
    dat <- as_period(x, n = 2L)
    expect_true(dat == dat)
    expect_false(dat != dat)
    expect_true(dat == dat)
    expect_true(dat <= dat + 1)
    expect_true(dat >= dat - 1)
    expect_true(dat < dat + 1)
    expect_true(dat > dat - 1)
    expect_true(dat != dat + 1)
    expect_error(
        dat == TRUE,
        "Can only compare <grates_period> objects with <grates_period> objects.",
        fixed = TRUE
    )

    # addition ----------------------------------------------------------------
    x <- as.Date("2021-01-05")
    dat1 <- as_period(x, n = 2L)
    dat2 <- dat1 + 0:1
    y <- as.Date("2021-01-05")
    expected <- as.Date(c(y, y + 2))
    expect_identical(as.Date(dat2), expected)
    expect_identical(dat2, 0:1 + dat1)
    expect_identical(+dat1, dat1)
    expect_error(
        dat1 + 1.5,
        "Can only add integers to <grates_period> objects.",
        fixed = TRUE
    )
    expect_error(
        dat1 + dat1,
        "Cannot add <grates_period> objects to each other.",
        fixed = TRUE
    )

    # subtraction -------------------------------------------------------------
    x <- as.Date("2021-01-05")
    dat1 <- as_period(x, n = 2)
    dat2 <- dat1 - 0:1
    y <- as.Date("2021-01-05")
    expected <- as.Date(c(y, y - 2))
    expect_identical(as.Date(dat2), expected)

    expect_error(
        1 - dat1,
        "Can only subtract from a <grates_period> object, not vice-versa.",
        fixed = TRUE
    )
    expect_error(
        -dat1,
        "Cannot negate a <grates_period> object.",
        fixed = TRUE
    )
    expect_error(
        dat1 - 1.5,
        "Can only subtract whole numbers and other <grates_period> objects from <grates_period> objects.",
        fixed = TRUE
    )
    expect_error(
        dat1 + dat1,
        "Cannot add <grates_period> objects to each other.",
        fixed = TRUE
    )

    # Other operations error
    x <- as_period(as.Date("2021-01-05"), n = 2)
    expect_error(dat * 3)
    expect_error(dat / 3)
    expect_error(dat ^ 3)
    expect_error(dat %% 3)
    expect_error(dat %/% 3)
    expect_error(dat & 3)
    expect_error(dat | 3)
    expect_error(!dat)

    # Math
    x <- as_period(as.Date("2021-01-05"), n = 2)
    dat <- c(x + 0:1, new_period(NA_integer_, n = 2L))
    expect_identical(is.nan(dat), c(FALSE, FALSE, FALSE))
    expect_identical(is.finite(dat), c(TRUE, TRUE, FALSE))
    expect_identical(is.infinite(dat), c(FALSE, FALSE, FALSE))
    expect_error(abs(dat))
})

test_that("period, miscellaneous work", {
    expect_identical(new_period(-1.5), new_period(-2L))

    expect_identical(
        as.integer(as_period(Sys.Date(), offset = Sys.Date())),
        as.integer(Sys.Date())
    )
    expect_error(new_period("bob"), "`x` must be integer.", fixed = TRUE)
    expect_error(
        new_period(0, 1.5),
        "`n` must be an integer of length 1.",
        fixed = TRUE
    )
    expect_error(
        new_period(0, 0:1),
        "`n` must be an integer of length 1.",
        fixed = TRUE
    )

    expect_error(
        new_period(0, -1),
        "`n` must be an integer >= 1.",
        fixed = TRUE
    )

    expect_error(
        new_period(0, offset = 1.5),
        "`offset` must be an integer of length 1.",
        fixed = TRUE
    )

    expect_error(
        new_period(0, offset = 1:2),
        "`offset` must be an integer of length 1.",
        fixed = TRUE
    )

    expect_error(
        as_period(Sys.Date(), 0:1),
        "`n` must be an integer of length 1.",
        fixed = TRUE
    )

    expect_error(
        as_period(Sys.Date(), 1.5),
        "`n` must be an integer of length 1.",
        fixed = TRUE
    )

    expect_error(
        as_period(Sys.Date(), offset = 1.5),
        "`offset` must be an integer or date of length 1.",
        fixed = TRUE
    )

    expect_error(
        as_period(Sys.Date(), offset = 1:2),
        "`offset` must be an integer or date of length 1.",
        fixed = TRUE
    )

    expect_error(
        as_period(Sys.Date(), origin = Sys.Date()),
        "The `origin` argument is now defunct. Please use `offset`.",
        fixed = TRUE
    )

    expect_error(
        as_period(as.factor(Sys.Date()), origin = Sys.Date()),
        "The `origin` argument is now defunct. Please use `offset`.",
        fixed = TRUE
    )

    expect_error(
        as_period(Sys.time(), origin = Sys.Date()),
        "The `origin` argument is now defunct. Please use `offset`.",
        fixed = TRUE
    )

    expect_error(
        as_period(as.POSIXlt(Sys.time()), origin = Sys.Date()),
        "The `origin` argument is now defunct. Please use `offset`.",
        fixed = TRUE
    )

    dat <- Sys.Date()
    dat <- c(dat, dat - 45L)
    dat <- as_period(dat)
    expect_identical(rep(dat, 2L), c(dat, dat))
    expect_identical(rep(dat, each = 2L), c(dat[[1]], dat[[1]], dat[[2]], dat[[2]]))
    expect_identical(unique(c(dat, dat)), dat)
    dat <- as_period(as.Date("1970-01-01"))
    expect_identical(
        seq(dat, dat + 6L, by = 2L),
        new_period(c(0L, 2L, 4L, 6L))
    )
    expect_error(
        seq(dat, dat + 11, by = 2.5),
        "`by` must be an integer of length 1.",
        fixed = TRUE
    )
    dat2 <- as_period(as.Date(dat+11), n = 3)
    expect_error(
        seq(dat, dat2, by = 2),
        "`to` must have the same period grouping as `from`",
        fixed = TRUE
    )

    expect_error(
        seq(dat, as.integer(dat + 11), by = 2.5),
        "`to` must be a <grates_period> object of length 1.",
        fixed = TRUE
    )
    expect_identical(as.integer(new_period(100L)), 100L)
    expect_identical(as.double(new_period(100L)), 100)
    expect_identical(min(c(dat, dat+11)), dat)
    expect_identical(max(c(dat, dat+11)), dat+11)
    expect_identical(range(seq(dat, dat + 12, by = 2L)), c(dat, dat+12))
    expect_error(
        any(dat),
        "`any()` is not supported for <grates_period> objects.",
        fixed = TRUE
    )
    expect_error(
        as_period("2020-01-01", origin = as.Date("2020-01-01")),
        "The `origin` argument is now defunct. Please use `offset`.",
        fixed = TRUE
    )
    expect_error(
        as_period(NA_character_),
        "Unable to parse any entries of `x` as Dates.",
        fixed = TRUE
    )
    expect_error(
        c(as_period(Sys.Date(), n = 2L), as_period(Sys.Date(), n = 3L)),
        "Unable to combine <grates_period> objects with different groupings.",
        fixed = TRUE
    )
    expect_error(
        c(as_period("2020-01-01", n = 2L, offset = 1L), as_period("2020-01-01", n = 2L, offset = 0L)),
        "Unable to combine <grates_period> objects with different offsets.",
        fixed = TRUE
    )
    expect_false(c(as_period(Sys.Date(), n = 2L) == as_period(Sys.Date(), n = 3L)))
    expect_true(c(as_period(Sys.Date(), n = 2L) != as_period(Sys.Date(), n = 3L)))

    dat1 <- as_period(Sys.Date(), n = 2L)
    dat2 <- dat1 + 1L
    expect_identical(dat2 - dat1, 1L)

    expect_error(
        as_period(Sys.Date(), n = 2L) - as_period(Sys.Date(), n = 3L),
        "<grates_period> objects must have the same period grouping and offset to perform subtraction.",
        fixed = TRUE
    )

    expect_false(is.numeric(dat1))
})

test_that("period boundary functions work", {
    dates <- as.Date("2020-01-01") + 0:365
    periods <- as_period(dates, n = 10L)
    starts <- as.Date(periods)
    ends <- starts + 10L - 1L
    expect_identical(date_start(periods), starts)
    expect_identical(date_end(periods), ends)
})
