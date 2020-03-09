#' Convert vector to ones and zeros.
#'
#' Converts a vector into ones and zeroes, any values which fall outside of what
#' is set by the arguments gets converted to missing. This vector also receives
#' a special class of \code{"onezero"} which makes succeeding functions go quicker.
#'
#' @return A numeric vector of class \code{"onezero"}.
#'
#' @param x A vector.
#' @param one Value in \code{x} to receive values of 1.
#' @param zero Value in \code{x} to receive values of 0. If left unspecified,
#' all remaining values assumed to receive value of 0.
#' @param keep.na Only needed if \code{zero} is not specified, whether or not
#' to retain missing values during conversion, default \code{TRUE}.
#'
#' @importFrom dplyr case_when
#'
#' @examples
#'
#' a <- c("zero", "one", "two")
#' onezero(a, "one", "zero")
#'
#' b <- c(1, -99, -99, 1, 1)
#' onezero(b, 1, -99)
#'
#' z <- c("a", "b", "c", NA)
#' onezero(z, one = "b")
#' onezero(z, one = "b", keep.na = FALSE)
#'
#' @export
onezero <- function(x, one, zero, keep.na = TRUE) {

    # make sure ones exist
    if (is.na(one)) {

        chk <- any(is.na(x))

        if (!chk) stop("Stop!")

    } else {

        chk <- any(x == one)
        if (!chk) stop("Stop!")

    }


    if (!missing(zero)) {

        if (is.na(zero)) {

            chk <- any(is.na(x))

            if (!chk) stop("Stop!")

        } else {

            chk <- any(x == zero)
            if (!chk) stop("Stop!")

        }

        x <- case_when(

            # special cases - one
            is.na(one) & is.na(x) ~ 1,
            is.infinite(one) & is.infinite(x) ~ 1,

            # special cases - 0
            is.na(zero) & is.na(x) ~ 0,
            is.infinite(zero) & is.infinite(x) ~ 0,

            # "normal" values
            x == one ~ 1,
            x == zero ~ 0,
            TRUE ~ NA_real_

        )
    }

    if (missing(zero)) {

        x <- case_when(

            # special cases - one
            is.na(one) & is.na(x) ~ 1,
            is.infinite(one) & is.infinite(x) ~ 1,

            x == one ~ 1,
            keep.na & is.na(x) ~ NA_real_,

            TRUE ~ 0

        )
    }

    x

}
