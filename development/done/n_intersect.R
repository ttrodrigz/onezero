
n_intersect <- function(data, wt, tibble = TRUE) {

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

    for (i in 1:ncol(out)) {

        for (j in 1:nrow(out)) {

            # don't need to waste time evaluating this
            if (i == j) next

            # creates logical vector of when i & j are both 1,
            # take sum of indexed weight vec to get co-occurrence
            condition.met <- data[[i]] == 1 & data[[j]] == 1
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
                values_to = "n_a_and_b"
            )

    } else {

        out.string <- paste0(
            ifelse(
                is.weighted,
                "Calculation: Counts of Intersection (Joint) - Weighted",
                "Calculation: Counts of Intersection"
            ),
            "\n",
            "   Notation: \U2211(A \U22C2 B)",
            "\n",
            "    Read as: Counts of where both A and B occur.",
            "\n",
            "\n"
        )

        cat(out.string)

    }

    return(out)


}

n_intersect(pets, wt, F)
