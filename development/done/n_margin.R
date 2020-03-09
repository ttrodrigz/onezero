

n_marginal <- function(data, wt, tibble = TRUE) {

    # deal with weights
    wts.parsed <- parse_wts(.data = data, .wt = wt)

    # check for non-onezero variables
    if (length(wts.parsed[["bad.vars"]]) > 0) {
        stop(paste0(
            "The following variables do not meet the requirements of `is_onezero`:\n",
            paste(wts.parsed[["bad.vars"]], collapse = ", ")
        ))
    }

    # parse out data
    data        <- wts.parsed[["data"]]
    wgt.vec     <- wts.parsed[["weights"]]
    is.weighted <- wts.parsed[["weighted"]]

    # weighted marginal counts
    out <- sapply(data, function(x) sum(x * wgt.vec, na.rm = TRUE))

    # clean up output
    if (tibble) {

        out <- enframe(out, "a", "n")

    } else {

        out.string <- paste0(
            ifelse(
                is.weighted,
                "Calculation: Marginal Counts - Weighted",
                "Calculation: Marginal Counts"
            ),
            "\n",
            "   Notation: \U2211(A)",
            "\n",
            "    Read as: Counts of where A occurs.",
            "\n",
            "\n"
        )

        cat(out.string)

    }

    return(out)


}

n_marginal(pets[-5], tibble = F)
