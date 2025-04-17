test_that(".apply_code_layout switches code slides to code layout", {
  pptx_path <- withr::local_tempfile(fileext = ".pptx")
  fs::file_copy(
    test_path("fixtures", "phuse_OS21", "no_postprocessing.pptx"),
    pptx_path
  )
  expect_no_error({
    test_result <- .apply_code_layout(pptx_path)
  })
  expect_equal(test_result, pptx_path)
  pres <- officer::read_pptx(pptx_path)
  withr::defer(unlink(pres$package_dir, recursive = TRUE))
  expect_equal(pres$slide$get_slide(1)$layout_name(), "slideLayout1.xml")
  expect_equal(pres$slide$get_slide(2)$layout_name(), "slideLayout2.xml")
  expect_equal(pres$slide$get_slide(3)$layout_name(), "slideLayout2.xml")
  expect_equal(pres$slide$get_slide(4)$layout_name(), "slideLayout1.xml")
})
