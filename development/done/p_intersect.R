#' Calculates probability of intersection (joint).
#'
#' Calculates the probability of intersection (probability of A and B) across
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
#' p_intersect(pets, wt)
#' p_intersect(pets[-5])
#' p_intersect(pets, wt, tibble = FALSE)
#'
#' @export
p_intersect <- function(data, wt, tibble = TRUE) {

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

    for (i in 1:M) {

        for (j in 1:M) {

            # don't need to waste time evaluating this
            if (i == j) next

            # creates logical vector of when i & j are both 1,
            # can take the weighted mean of it to get the
            # out probability

            # note: this probability de facto uses pairwise complete
            # since the ij == 1 check retains NA's and the weighted.mean
            # removes them
            condition.met <- data[[i]] == 1 & data[[j]] == 1
            out[i, j] <- weighted.mean(condition.met, wgt.vec, na.rm = TRUE)

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
                values_to = "p_a_and_b"
            )

    } else {

        out.string <- paste0(
            ifelse(
                is.weighted,
                "Calculation: Probability of Intersection (Joint) - Weighted",
                "Calculation: Probability of Intersection"
            ),
            "\n",
            "   Notation: P(A \U22C2 B)",
            "\n",
            "    Read as: Probability of A and B",
            "\n",
            "\n"
        )

        cat(out.string)

    }

    return(out)

}

#' @rdname p_intersect
#'
#' @examples
#'
#' p_joint(pets, wt)
#' p_joint(pets[-5])
#' p_joint(pets, wt, tibble = FALSE)
#'
#' @export
p_joint <- p_intersect
