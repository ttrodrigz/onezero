
microbenchmark(
    m_choose_k(mat, 8),
    m_choose_k_faster(mat, 8),
    times = 100L,
    unit = "s"
)

M <- 15
k <- 8


rch <- function() t(combn(
    x = M, m = k,
    FUN = function(x) inner_reach(
        .data = mat,
        included = x,
        .cutoff = 1
    )
))


rch.faster <- function() do.call(rbind, t(comboGeneral(
    v = M, m = k,
    FUN = function(x) inner_reach(
        .data = mat,
        included = x,
        .cutoff = 1
    )
)))

# using rcpp barely faster
# rch() median = 0.4914
# rch.faster() median = 0.4791
microbenchmark(
    rch(),
    rch.faster(),
    unit = "s"
)

list(rch, rch.faster) %>%
    lapply(as.data.frame) %>%
    lapply(as_tibble) %>%
    reduce(bind_cols) %>%
    summarise(mean(V1 == reach),
              mean(V2 == times_reached))
