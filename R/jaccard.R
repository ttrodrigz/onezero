#' Jaccard Index (or Distance).
#'
#' Calculates the Jaccard similarity index (or distance) across all pairwise
#' sets of columns in a data frame.
#'
#' @param data A data frame.
#' @param wt An optional column of weights.
#' @param tibble Whether or not to return results in a \code{tibble}, or
#' something simpler. Default \code{TRUE}.
#' @param distance Whether or not to return Jaccard distance. Default
#' \code{FALSE}.
#'
#' @return A \code{tibble} or \code{matrix} of Jaccard indices or distances.
#'
#' @importFrom stats weighted.mean
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' jaccard(pets, wt)
#' jaccard(pets[-5])
#' jaccard(pets, wt, tibble = FALSE)
#' jaccard(pets, wt, distance = TRUE)
#'
#' @export
jaccard <- function(data, wt, tibble = TRUE, distance = FALSE) {


    # run parser --------------------------------------------------------------

    # do the parsing
    if (missing(wt)) {
        oz.parsed <- parse_oz(data)
    } else {
        oz.parsed <- parse_oz(data, wt)
    }

    # stop early if detected bad variables
    if (!is.null(oz.parsed[["bad.vars.message"]])) {
        stop(oz.parsed[["bad.vars.message"]])
    }

    data        <- oz.parsed[["data"]]
    wgt.vec     <- oz.parsed[["weights"]]
    is.weighted <- oz.parsed[["weighted"]]


    # initialize matrix -------------------------------------------------------

    M <- ncol(data)
    VN <- names(data)

    out <- matrix(
        nrow = M,
        ncol = M,
        dimnames = list(VN, VN)
    )


    # jaccard -----------------------------------------------------------------

    for (i in 1:M) {

        for (j in 1:M) {

            # evaluate later
            if (i == j) next

            condition.joint <- data[[i]] == 1 & data[[j]] == 1

            condition.union <- data[[i]] == 1 | data[[j]] == 1

            na.locations  <- is.na(data[[i]]) | is.na(data[[j]])

            # stolen from `intersection()`
            .joint <- weighted.mean(
                condition.joint,
                wgt.vec,
                na.rm = TRUE
            )

            # stolen from `union()`
            .union <- weighted.mean(
                x = condition.union[!na.locations],
                w = wgt.vec[!na.locations],
                na.rm = TRUE
            )

            # jaccard is ratio of intersection to union
            out[i, j] <- .joint / .union

            # fill other side of matrix
            out[j, i] <- out[i, j]

        }
    }

    diag(out) <- 1

    value.suffix <- NULL

    if (distance) {
        out <- 1 - out
        value.suffix <- "_dist"
    }


    # tibble results? ---------------------------------------------------------

    if (tibble) {

        out <-
            out %>%
            as_tibble(rownames = "var_a") %>%
            pivot_longer(
                cols = -1,
                names_to = "var_b",
                values_to = paste0("jaccard", value.suffix)
            )
    }

    return(out)

}
