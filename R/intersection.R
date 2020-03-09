#' Probability (or Counts) of Intersection.
#'
#' Calculates the probability (or counts) of intersection across all pairwise
#' sets of columns in a data frame. This is the probability of both "A" and "B"
#' occurring, otherwise known as a joint probability.
#'
#' @param data A data frame.
#' @param wt An optional column of weights.
#' @param tibble Whether or not to return results in a \code{tibble}, or
#' something simpler. Default \code{TRUE}.
#' @param stat Either "prob" to return probabilities, or "count" to return
#' counts.
#'
#' @return A \code{tibble} or \code{matrix} of probability or counts of
#' intersection.
#'
#' @importFrom stats weighted.mean
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' intersection(pets, wt)
#' intersection(pets[-5])
#' intersection(pets, wt, tibble = FALSE)
#' intersection(pets, wt, stat = "count")
#'
#' @export
intersection <- function(data, wt, tibble = TRUE, stat = "prob") {


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
        ncol = M,
        dimnames = list(VN, VN)
    )


    # probabilities -----------------------------------------------------------

    if (stat == "prob") {

        for (i in 1:M) {

            for (j in 1:M) {

                # evaluate later
                if (i == j) next

                # creates logical vector of when i & j are both 1
                condition.met <- data[[i]] == 1 & data[[j]] == 1

                # take weighted mean of condition and weights
                out[i, j] <- weighted.mean(
                    condition.met,
                    wgt.vec,
                    na.rm = TRUE
                )

                # fill other side of matrix
                out[j, i] <- out[i, j]

            }
        }

        diag(out) <- sapply(data, weighted.mean, w = wgt.vec, na.rm = TRUE)

        # used for tibbling
        value.prefix <- "p"

    }


    # counts ------------------------------------------------------------------

    if (stat == "count") {

        for (i in 1:M) {

            for (j in 1:M) {

                # evaluate later
                if (i == j) next

                # creates logical vector of when i & j are both 1
                condition.met <- data[[i]] == 1 & data[[j]] == 1

                # sum the weight vector to get counts
                out[i, j] <- sum(wgt.vec[condition.met], na.rm = TRUE)

                # fill other side of matrix
                out[j, i] <- out[i, j]

            }
        }

        diag(out) <- sapply(data, function(x) sum(x * wgt.vec, na.rm = TRUE))

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
                values_to = paste(value.prefix, "a_and_b", sep = "_")
            )

    }

    return(out)

}

#' @rdname intersection
#'
#' @examples
#'
#' joint(pets, wt)
#' joint(pets[-5])
#' joint(pets, wt, tibble = FALSE)
#' joint(pets, wt, stat = "count")
#'
#' @export
joint <- intersection
