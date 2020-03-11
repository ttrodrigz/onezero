#' Marginal Probabilities (or Counts).
#'
#' Calculate marginal probabilities or counts across all columns in a data
#' frame.
#'
#' @param data A data frame.
#' @param wt An optional column of weights.
#' @param tibble Whether or not to return results in a \code{tibble}, or
#' something simpler. Default \code{TRUE}.
#' @param stat Either "prob" to return probabilities, or "count" to return
#' counts.
#'
#' @return A \code{tibble} or vector of marginal probabilities or counts.
#'
#' @importFrom stats weighted.mean
#' @importFrom tibble enframe
#'
#' @examples
#' marginal(pets, wt)
#' marginal(pets[-5])
#' marginal(pets, wt, tibble = FALSE)
#' marginal(pets, wt, stat = "count")
#'
#' @export
marginal <- function(data, wt, tibble = TRUE, stat = "prob") {


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


    # probabilities -----------------------------------------------------------

    if (stat == "prob") {

        out <- sapply(
            X = data,
            weighted.mean,
            w = wgt.vec,
            na.rm = TRUE
        )

        if (tibble) {

            out <- enframe(out, "var_a", "p_a")

        }

    }


    # counts ------------------------------------------------------------------

    if (stat == "count") {

        out <- sapply(
            data,
            function(x) sum(x * wgt.vec, na.rm = TRUE)
        )

        if (tibble) {

            out <- enframe(out, "var_a", "n_a")

        }


    }


    return(out)


}
