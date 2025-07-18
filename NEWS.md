# grates 1.5.0

## breaking change

* `is.numeric()` now returns `FALSE` for all grates objects. This makes the
  underlying implementation more opaque to end users and feels more consistent
  with other behaviour (e.g. no/limited support for "Math" methods).
  
## Internal changes

* We now use [fastymd](https://cran.r-project.org/package=fastymd) internally
  which can give a small performance improvement for larger workloads when
  working with `yearweek` and `yearmonth` objects.

# grates 1.4.3

* Fixes vignette build.

# grates 1.4.2

* Reworking of manual pages and vignette.
* `scale_x_grates_period()` can now handle a `date` value for `offset`.

# grates 1.4.1

* No longer downloads css/js when building vignette.

# grates 1.4.0

* The format argument of the scale functions for `<grates_yearweek>`,
  `<grates_epiweek>` and `<grates_isoweek>` can now be set to "week" to drop
  the year value from graph labels, i.e.
  
  ```
  plot + scale_x_grates_epiweek(format = "week")
  ```

# grates 1.3.0

* (Re)introduction of the `<grates_int_period>` object and associated scale
  function. This functionality should be viewed as experimental for the time
  being and is marked as such in the documentation.

# grates 1.2.2

* All images in the vignette now have alt text.

* Minor documentation improvements.

# grates 1.2.1

* Package now depends on R (>= 3.6.0).

# grates 1.2.0

* New functions `date_start()` and `date_end()` for accessing the
  boundary elements of `<grates>` objects.

* New function `%during%` for testing whether a scalar date is contained within
  the range (inclusive) of a `<grates>` object.

# grates 1.1.1

* Users of the 3.5.0 release of
  [ggplot2](https://CRAN.R-project.org/package=ggplot2) will have noticed some
  additional "warnings" appearing within the plot. These have now been handled
  internally so please raise an issue if they reappear.
* Minor internal and non-breaking changes motivated by
  [lintr](https://CRAN.R-project.org/package=lintr).

# grates 1.1.0

* The scale functions (e.g. `scale_x_grates_isoweek()`) gain a `breaks`
  argument to allow exact specification of breaks.

# grates 1.0.1

* Fix erroneous tests flagged by CRAN.
* No user facing changes.

# grates 1.0.0

For the first major release of grates a significant refactor has been undertaken
that builds upon lessons learnt over the last two years. Whilst we have tried to
limit breaking changes some functionality has been removed and some function
parameters altered (see details below):

## breaking changes

* `<grates_month>` objects are now always stored relative to the UNIX epoch.
  This is equivalent to setting `origin = 0` in the previous release. Calling
  the function with an `origin` argument will now error.

* Trying to create a `<grates_month>` object with `n` set to 1 will now error.
  Uses are encouraged to use `<grates_yearmonth>` for this case.

* `<grates_int_period>` is now a defunct as it did not fit with the scope of the
  package (i.e grouped dates, not grouped integers). In particular the
  `int_period()`, `as_int_period()` and `is_int_period()` will now error on use.

* The `origin` parameter from `<grates_period>` as been renamed to `offset` to
  better reflect its usage. Users will need to update uses of `period()`,
  `as_period()` and `scale_x_grates_period()` to reflect this.
  
* `as_yearweek()` no longer parsers character strings of the form "YYYY-Www"
  (e.g. "2020-W01").
  
* Constructors `yearweek()` and `isoweek()` and `epiweek()` now
  allow construction of grates objects directly from year and week integer
  vectors. `yearmonth()` and `yearquarter()` constructors have been similarly
  changed to allow construction from year and month/quarter integer vectors.
  
* The old incarnation of direct constructors now begin with a `new_` prefix
  (e.g. `new_month()`, `new_yearweek()`, `new_epiweek()`, ...). This is to
  distinguish them from more user friendly constructors that we have introduced
  (see above).

## new functions and classes

* A new `yearmonth` class (`<grates_yearmonth>`) and associated functions have
  been introduced. This object is similar to what was previously obtained via a
  call of `month(x, n = 1L, origin = 0L)` (now defunct - see above).

* New `isoweek` and `epiweek` classes (`<grates_isoweek>` and
  `<grates_epiweek>` respectively) and associated functions. Internally these
  are similar to the corresponding `<grates_yearweek>` objects but with a
  marginally more efficient implementation.

## bug fixes

* `is.numeric()` methods for grates objects previously returned FALSE. Calls to
  these methods now dispatch to the default implementation based on the
  underlying type and should now return TRUE.

## miscellaneous

* Hard dependencies on [clock](https://CRAN.R-project.org/package=clock) and
  [vctrs](https://CRAN.R-project.org/package=vctrs) have now been removed.


# grates 0.3.1

* Fix for changes made to POSIXlt objects in R-devel.

# grates 0.3.0

## New functions

* `seq()` methods now implemented for all grates objects.

## bug fixes

* Conversion functions now preserve names.

* Bug fixes for cast functions operating on objects of the same class but
  with different attributes.

# grates 0.2.0

* This is a breaking release that changes the underlying implementations of the
  different grate constructors and associated scales for ggplot2. There has also
  been some renaming of function arguments to bring greater consistency across
  packages.

* We now make more use of the high level API introduced by the 
  [clock](https://CRAN.R-project.org/package=clock) package for working with
  R's date and date-time types.

# grates 0.1.2

## bug fixes

* Fixed bug affecting `scale_x_period()`

# grates 0.1.1

* Initial release
