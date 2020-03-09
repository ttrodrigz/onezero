#' Pairwise Conditional probabilities.
#'
#' Calculates conditional probabilities (probability of A given B) across all
#' pairs of variables in a data frame.  Note that this uses pairwise complete
#' observations.
#'
#' @param data A data frame containing columns of 1's and 0's.
#' @param wt An optional column of weights.
#' @param tibble Whether or not to return a tibble in favor of a matrix (default \code{TRUE}).
#'
#' @return Either a \code{tibble} or \code{matrix} of probabilities.
#'
#' @importFrom stats weighted.mean
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' p_conditional(pets, wt)
#' p_conditional(pets[-5])
#' p_conditional(pets, wt, tibble = FALSE)
#'
#' @export
p_conditional <- function(data, wt, tibble = TRUE) {

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

    # initialize matrix to receive joint probs
    M <- ncol(data)
    VN <- names(data)

    out <- matrix(
        nrow = M,
        ncol = M,
        dimnames = list("Prob of" = VN, "Given" = VN)
    )

    for (j in 1:M) {

        # filter on j, take mean of vector
        condition.met <- data[[j]] == 1
        out[, j] <- sapply(
            X = data[condition.met, ],
            FUN = weighted.mean,
            w = wgt.vec[condition.met],
            na.rm = TRUE
        )

    }

    # clean up output
    if (tibble) {

        out <-
            out %>%
            as_tibble(rownames = "a") %>%
            pivot_longer(
                cols = -1,
                names_to = "b",
                values_to = "p_a_given_b"
            )

    } else {

        out.string <- paste0(
            ifelse(
                is.weighted,
                "Calculation: Conditional Probability - Weighted",
                "Calculation: Conditional Probability"
            ),
            "\n",
            "   Notation: P(A | B)",
            "\n",
            "    Read as: Probability of A given B",
            "\n",
            "\n"
        )

        cat(out.string)

    }

    return(out)


}
