#' Conditional Probabilities (or Counts).
#'
#' Calculates the conditional probability (or counts) across all pairwise
#' sets of columns in a data frame. This is the probability of "A" given "B".
#'
#' @param data A data frame.
#' @param wt An optional column of weights.
#' @param tibble Whether or not to return results in a \code{tibble}, or
#' something simpler. Default \code{TRUE}.
#' @param stat Either "prob" to return probabilities, or "count" to return
#' counts.
#'
#' @return A \code{tibble} or \code{matrix} of conditional probabilities or
#' counts.
#'
#' @importFrom stats weighted.mean
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>%
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' conditional(pets, wt)
#' conditional(pets[-5])
#' conditional(pets, wt, tibble = FALSE)
#' conditional(pets, wt, stat = "count")
#'
#' @export
conditional <- function(data, wt, tibble = TRUE, stat = "prob") {


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

        dimnames(out) <- list("Prob of A" = VN, "And B" = VN)

        # used for tibbling
        value.prefix <- "p"

    }


    # counts ------------------------------------------------------------------

    if (stat == "count") {

        for (j in 1:M) {

            # filter on j, take mean of vector
            condition.met <- data[[j]] == 1

            out[, j] <- sapply(
                X = data[condition.met, ],
                FUN = function(x)
                    sum(x * wgt.vec[condition.met],
                        na.rm = TRUE)
            )

        }

        dimnames(out) <- list("Counts of A" = VN, "Given B" = VN)

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
                values_to = paste(value.prefix, "a_given_b", sep = "_")
            )

    }

    return(out)

}
