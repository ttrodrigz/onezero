
n_union <- function(data, wt, tibble = TRUE) {

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
        dimnames = list("A" = VN, "B" = VN)
    )

    # browser()

    for (i in 1:M) {

        for (j in 1:M) {

            # don't need to waste time evaluating this
            if (i == j) next

            # creates logical vector of when i & j are both 1,
            # can take the weighted mean of it to get the
            # probability

            # note: this uses pairwise complete by finding locations
            # na.locations <- is.na(data[[i]]) | is.na(data[[j]])
            condition.met  <- data[[i]] == 1 | data[[j]] == 1

            # multiplying the conditions met by (not) NA locations reveals
            # a vector of final 1s and 0s, summing the product of that and the
            # weight vector gets the final weighted count
            # out[i, j] <- sum((condition.met * !na.locations) * wgt.vec)

            # this actually seems correct instead...
            out[i, j] <- sum(wgt.vec[condition.met], na.rm = TRUE)

            # fill other side of matrix
            out[j, i] <- out[i, j]

        }
    }

    # fill in diagonal with marginal counts
    diag(out) <- sapply(data, function(x) sum(x * wgt.vec, na.rm = TRUE))

    # clean up output
    if (tibble) {

        out <-
            out %>%
            as_tibble(rownames = "a") %>%
            pivot_longer(
                cols = -1,
                names_to = "b",
                values_to = "n_a_or_b"
            )

    } else {

        out.string <- paste0(
            ifelse(
                is.weighted,
                "Calculation: Counts of Union - Weighted",
                "Calculation: Counts of Union"
            ),
            "\n",
            "   Notation: \U2211(A \U22C3 B)",
            "\n",
            "    Read as: Counts of where either A or B occur.",
            "\n",
            "\n"
        )

        cat(out.string)

    }

    return(out)

}

n_union(pets, wt, tibble = F)
n_intersect(pets, wt, tibble = F)
