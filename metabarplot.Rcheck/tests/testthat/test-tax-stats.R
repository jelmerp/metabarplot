test_that("tax_stats returns expected columns", {
  x <- tibble::tibble(
    method = "demo",
    Genus = c("A", "B", NA),
    reads = c(80, 15, 5)
  )

  out <- tax_stats(
    tax_df = x,
    tax_rank = "Genus",
    abund_column = "reads",
    unknown_label = "unknown",
    rare_label = "other (rare)"
  )

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("Genus", "Abundance", "Sample") %in% names(out)))
  expect_equal(unique(out$Sample), "demo")
  expect_true(all(out$Abundance >= 0))
})
