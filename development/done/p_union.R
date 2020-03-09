#' Calculates probability of union.
#'
#' Calculates the probability of union (probability of A or B) across
#' all pairs of variables in a data frame. Note that this uses pairwise complete
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
#' p_union(pets, wt)
#' p_union(pets[-5])
#' p_union(pets, wt, tibble = FALSE)
#'
#' @export
p_union <- function(data, wt, tibble = TRUE) {

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
        dimnames = list("Prob of" = VN, "Or" = VN)
    )

    for (i in 1:M) {

        for (j in 1:M) {

            # don't need to waste time evaluating this
            if (i == j) next

            # creates logical vector of when i & j are both 1,
            # can take the weighted mean of it to get the
            # probability

            # note: this uses pairwise complete by finding locations

            na.locations  <- is.na(data[[i]]) | is.na(data[[j]])
            condition.met <- data[[i]] == 1 | data[[j]] == 1

            out[i, j] <- weighted.mean(
                x = condition.met[!na.locations],
                w = wgt.vec[!na.locations],
                na.rm = TRUE
            )

            # fill other side of matrix
            out[j, i] <- out[i, j]

        }
    }

    # fill in diag with marginals
    diag(out) <- sapply(data, weighted.mean, w = wgt.vec, na.rm = TRUE)

    # clean up output
    if (tibble) {

        out <-
            out %>%
            as_tibble(rownames = "a") %>%
            pivot_longer(
                cols = -1,
                names_to = "b",
                values_to = "p_a_or_b"
            )

    } else {

        out.string <- paste0(
            ifelse(
                is.weighted,
                "Calculation: Probability of Union - Weighted",
                "Calculation: Probability of Union"
            ),
            "\n",
            "   Notation: P(A \U22C3 B)",
            "\n",
            "    Read as: Probability of A or B",
            "\n",
            "\n"
        )

        cat(out.string)

    }

    return(out)

}

