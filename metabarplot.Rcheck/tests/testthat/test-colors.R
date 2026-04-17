test_that("color palettes are hex vectors", {
  expect_type(cols1, "character")
  expect_type(cols_brewerplus, "character")
  expect_type(cols_kelly, "character")

  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols1)))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols_brewerplus)))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols_kelly)))
})
