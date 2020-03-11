
library(RcppAlgos)
library(microbenchmark)

comboGeneral(5, 2)

M <- 10
k <- 4


comboGeneral(M, k, FUN = function(x) tabulate(x, M)) %>%
    do.call(rbind, .)

# function to return combinations -----------------------------------------

m_choose_k_faster <- function(data, k) {

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

    design <-
        t(comboGeneral(M, k, FUN = function(x) tabulate(x, M))) %>%
        do.call(rbind, .)

    colnames(design) <- VN

    design

}

m_choose_k_faster(mat, k = 4)


best_k_of_m_faster <- function(data, k, cutoff = 1) {

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

    # browser()

    out <- t(comboGeneral(
        v = M, m = k,
        FUN = function(x) inner_reach(
            .data = data,
            included = x,
            .cutoff = 1
        )
    )) %>%
        do.call(rbind, .)

    colnames(out) <- c("reach", "times_reached")

    design <- m_choose_k_faster(data = data, k = k)

    final <- cbind(
        combo = 1:nrow(out),
        out,
        design
    )


    as_tibble(final)


}

best_k_of_m_faster(mat, 7)

mck <- function(M = 20, k = 10) t(combn(M, k, function(x) tabulate(x, M)))
mck_fast <- function(M = 20, k = 10) {
    t(comboGeneral(M, k, FUN = function(x) tabulate(x, M))) %>%
        do.call(rbind, .)
}

microbenchmark(
    mck(),
    mck_fast()
)

newbench <- microbenchmark(
    bkom = best_k_of_m(mat, k = 8),
    bkom_faster = best_k_of_m_faster(mat, k = 8),
    unit = "s"
)
