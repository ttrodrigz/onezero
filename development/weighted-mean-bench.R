xx <- rnorm(1e7)
yy <- rnorm(1e7)

microbenchmark::microbenchmark(
    weighted.mean(xx, yy),
    weighted_mean(xx, yy),
    unit = "s"
)

microbenchmark::microbenchmark(
    sum(xx),
    sum_cpp(xx),
    unit = "s"
)
