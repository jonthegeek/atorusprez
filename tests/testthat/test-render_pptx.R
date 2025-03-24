test_that("render_pptx generates a deck with the expected theme", {
  pptx_path <- test_path("fixtures", "example.pptx")
  withr::local_file(pptx_path)
  test_result <- render_pptx(
    test_path("fixtures", "example.qmd")
  )
  expect_snapshot({
    get_clean_pptx_hash(pptx_path)
  })
})

test_that("render_pptx applies a date if supplied", {
  pptx_path <- test_path("fixtures", "example.pptx")
  withr::local_file(pptx_path)
  test_result <- render_pptx(
    test_path("fixtures", "example.qmd"),
    date = "2025-01-01"
  )
  expect_identical(get_pptx_date(pptx_path), "January 1, 2025")
})

test_that("render_pptx removes date if NULL explicitly supplied", {
  pptx_path <- test_path("fixtures", "example.pptx")
  withr::local_file(pptx_path)
  test_result <- render_pptx(
    test_path("fixtures", "example.qmd"),
    date = NULL
  )
  expect_identical(get_pptx_date(pptx_path), NA_character_)
})
