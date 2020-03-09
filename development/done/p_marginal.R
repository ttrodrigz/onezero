#' Marginal proportions.
#'
#' Calculate marginal proportions across all variables in a data frame.
#'
#' @param data A data frame containing columns of 1's and 0's.
#' @param wt An optional column of weights.
#' @param tibble Whether or not to return a tibble in favor of a vector (default \code{TRUE}.
#'
#' @importFrom tibble enframe
#' @importFrom stats weighted.mean
#'
#' @return Either a \code{tibble} or vector of proportions.
#'
#' @examples
#'
#' p_marginal(pets, wt)
#' p_marginal(pets[-5])
#' p_marginal(pets, wt, tibble = FALSE)
#'
#' @export
p_marginal <- function(data, wt, tibble = TRUE) {

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

    # calculate probabilities
    out <- sapply(
        X = data,
        weighted.mean, w = wgt.vec, na.rm = TRUE
    )

    # clean up output
    if (tibble) {

        out <- enframe(out, "a", "p")

    } else {

        out.string <- paste0(
            ifelse(
                is.weighted,
                "Calculation: Marginal Probability - Weighted",
                "Calculation: Marginal Probability"
            ),
            "\n",
            "   Notation: P(A)",
            "\n",
            "    Read as: Probability of A",
            "\n",
            "\n"
        )

        cat(out.string)

    }

    return(out)

}
