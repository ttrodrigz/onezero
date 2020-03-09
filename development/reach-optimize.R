

# set up ------------------------------------------------------------------

library(tidyverse)
library(onezero)

# some data ---------------------------------------------------------------

N <- 1000
M <- 15

set.seed(101)

mat <- matrix(
    data = rbinom(N * M, size = 1, prob = 1/3),
    nrow = N,
    ncol = M,
    dimnames = list(NULL, paste0("I", 1:M))
)

df <- as_tibble(mat)


# function to return combinations -----------------------------------------

m_choose_k <- function(data, k) {

    # needs to be a matrix

    if (is.data.frame(data)) {
        data <- as.matrix(data)
    } else if (is.matrix(data)) {
        # all good
    } else {
        stop("Input to `data` must either be a matrix or data frame.")
    }


    # column names for later

    VN <- colnames(data)
    M <- ncol(data)

    if (is.null(VN)) {
        VN <- paste0("V", 1:M)
    }

    # make sure picked a valid k

    if (k < 1 | k > M) {
        stop(
            paste0(
                "Input to `k` must be a value between 1 and the number of columns in your data (",
                M,
                ").")
        )
    }

    design <- t(combn(M, k, function(x) tabulate(x, M)))

    colnames(design) <- VN

    design

}

m_choose_k(mat, k = 4)


# inner function for reach inside optimizer -------------------------------

# this function prob wouldn't be exported/directly callable

inner_reach <- function(.data, included, .cutoff = 1) {

    # drop FALSE keeps from reducing 1-col matrix to vector
    .data <- .data[, included, drop = FALSE]
    hits <- rowSums(.data, na.rm = TRUE)
    reached <- hits >= .cutoff

    c("reach" = mean(reached, na.rm = TRUE),
      "times_reached" = sum(reached * hits, na.rm = TRUE))

}


# brute optimal sets ------------------------------------------------------

best_k_of_m <- function(data, k, cutoff = 1) {

    # needs to be a matrix

    if (is.data.frame(data)) {
        data <- as.matrix(data)
    } else if (is.matrix(data)) {
        # all good
    } else {
        stop("Input to `data` must either be a matrix or data frame.")
    }


    # column names for later

    VN <- colnames(data)
    M <- ncol(data)

    if (is.null(VN)) {
        VN <- paste0("V", 1:M)
    }

    # make sure picked a valid k

    if (k < 1 | k > M) {
        stop(
            paste0(
                "Input to `k` must be a value between 1 and the number of columns in your data (",
                M,
                ").")
        )
    }

    # calculate reach stats ---------------------------------------------------

    out <- t(combn(
        x = M, m = k,
        FUN = function(x) inner_reach(
            .data = data,
            included = x,
            .cutoff = 1
        )
    ))

    colnames(out) <- c("reach", "times_reached")

    design <- m_choose_k(data = data, k = k)

    final <- cbind(
        combo = 1:nrow(out),
        out,
        design
    )


    as_tibble(final)


}

# test it out
best_k_of_m(df, 8)

lapply(1:4, best_k_of_m, data = pets[-5])

lapply(1:ncol(df), best_k_of_m, data = df)


# testing performance
# microbenchmark::microbenchmark(
#     lapply(1:ncol(df), best_k_of_m, data = df)
# )
# Min: 2.28
# Med: 2.43
# Max: 3.06
