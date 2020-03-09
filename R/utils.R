
parse_oz <- function(.data, .wt) {

    if (missing(.wt)) {

        wt.vec <- rep(1, times = nrow(.data))
        is.weighted <- FALSE

    } else {

        wt.name  <- deparse(substitute(.wt))

        if (!wt.name %in% names(.data)) {
            stop(paste0(
                "Variable '", wt.name, "' not found in data."
            ))
        }

        wt.index <- which(names(.data) == wt.name)
        wt.vec   <- .data[[wt.index]]

        # cannot have missing weights
        if (any(is.na(wt.vec))) {
            stop("One or more rows have missing weights.")
        }

        # must have positive non-zero weights
        if (any(wt.vec <= 0)) {
            stop("All weights must be positive non-zero.")
        }

        .data <- .data[-wt.index]
        is.weighted <- TRUE

    }

    oz.check <- sapply(.data, is_onezero)

    bad.vars <- names(oz.check[!oz.check])

    if (length(bad.vars) > 0) {

        bad.vars.message <- paste0(
            "The following variables do not meet the requirements of `is_onezero`:\n",
            paste(bad.vars, collapse = ", ")
        )

    } else {

        bad.vars.message <- NULL

    }


    list(
        data = .data,
        weights = wt.vec,
        weighted = is.weighted,
        bad.vars = bad.vars,
        bad.vars.message = bad.vars.message
    )

}
