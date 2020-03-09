
pets <- tibble::tribble(
    ~dog, ~cat, ~fish, ~bird,  ~wt,
      1L,   1L,    0L,    0L,  0.5,
      1L,   NA,    1L,    0L,    1,
      0L,   0L,    1L,    0L, 0.75,
      1L,   1L,    0L,    1L,  0.5,
      1L,   0L,    1L,    0L,    2,
      0L,   1L,    1L,    0L,    1,
      1L,   1L,    0L,    0L,  0.5,
      1L,   0L,    1L,    1L,  1.5,
      0L,   1L,    0L,    0L, 1.75,
      1L,   1L,    0L,    0L,  0.5
    )




usethis::use_data(pets, overwrite = TRUE)
