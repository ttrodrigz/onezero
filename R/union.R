#' Probability (or Counts) of Union.
#'
#' Calculates the probability (or counts) of union across all pairwise
#' sets of columns in a data frame. This is the probability of either "A" or
#' "B" occurring.
#'
#' @param data A data frame.
#' @param wt An optional column of weights.
#' @param tibble Whether or not to return results in a \code{tibble}, or
#' something simpler. Default \code{TRUE}.
#' @param stat Either "prob" to return probabilities, or "count" to return
#' counts.
#'
#' @return A \code{tibble} or \code{matrix} of probability or counts of
#' union.
#'
#' @importFrom stats weighted.mean
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' union(pets, wt)
#' union(pets[-5])
#' union(pets, wt, tibble = FALSE)
#' union(pets, wt, stat = "count")
#'
#' @export
union <- function(data, wt, tibble = TRUE, stat = "prob") {


    # error checking ----------------------------------------------------------

    stats.avail <- c("prob", "count")

    if (!stat %in% stats.avail) {
        stop(paste0(
            "Stat '",
            stat,
            "' not recognized, please use one of 'prob' or 'count'."
        ))
    }


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
        ncol = M
    )


    # probabilities -----------------------------------------------------------

    if (stat == "prob") {

        for (i in 1:M) {

            for (j in 1:M) {

                # evaluate later
                if (i == j) next

                # identify pairwise complete
                na.locations  <- is.na(data[[i]]) | is.na(data[[j]])

                # conditon met when either are 1
                condition.met <- data[[i]] == 1 | data[[j]] == 1

                # take weighted mean subsetting by pairwise complete
                out[i, j] <- weighted.mean(
                    x = condition.met[!na.locations],
                    w = wgt.vec[!na.locations],
                    na.rm = TRUE
                )

                # fill other side of matrix
                out[j, i] <- out[i, j]

            }
        }

        diag(out) <- sapply(data, weighted.mean, w = wgt.vec, na.rm = TRUE)

        dimnames(out) <- list("Prob of A" = VN, "Or B" = VN)

        # used for tibbling
        value.prefix <- "p"

    }

    # counts ------------------------------------------------------------------

    if (stat == "count") {

        for (i in 1:M) {

            for (j in 1:M) {

                # evaluate later
                if (i == j) next

                # identify pairwise complete
                na.locations  <- is.na(data[[i]]) | is.na(data[[j]])

                # condition met if the sum of the two elements in the vector
                # are greater than zero, this is done to retain NAs
                condition.met <- (data[[i]] + data[[j]]) > 0

                # sum over weight vector subsetting by condition met
                out[i, j] <- sum(wgt.vec[condition.met], na.rm = TRUE)

                # fill other side of matrix
                out[j, i] <- out[i, j]

            }
        }

        diag(out) <- sapply(data, function(x) sum(x * wgt.vec, na.rm = TRUE))

        dimnames(out) <- list("Counts of A" = VN, "Or B" = VN)

        # used for tibbling
        value.prefix <- "n"

    }

    # tibble results? ---------------------------------------------------------

    if (tibble) {

        out <-
            out %>%
            as_tibble(rownames = "var_a") %>%
            pivot_longer(
                cols = -1,
                names_to = "var_b",
                values_to = paste(value.prefix, "a_or_b", sep = "_")
            )

    }

    return(out)

}
