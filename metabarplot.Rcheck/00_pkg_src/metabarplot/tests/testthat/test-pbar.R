test_that("pbar builds a ggplot from abundance table", {
  x <- tibble::tibble(
    Sample = c("S1", "S1", "S2", "S2"),
    Phylum = c("Firmicutes", "Proteobacteria", "Firmicutes", "Proteobacteria"),
    Abundance = c(0.4, 0.6, 0.7, 0.3)
  )

  p <- pbar(abund_df = x, taxrank = "Phylum")
  expect_s3_class(p, "ggplot")
})
