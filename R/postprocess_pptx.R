# Note: Codecov doesn't see this on individual coverage checks, but sees it in
# the overall checks.

.postprocess_pptx <- function(pptx_path) {
  .apply_code_layout(pptx_path)
}

.apply_code_layout <- function(pptx_path) {
  pres <- officer::read_pptx(pptx_path)
  on.exit(unlink(pres$package_dir, recursive = TRUE), add = TRUE)

  original_layout_filename <- .get_layout_filename(pres, "Title and Content")
  slides_updated <- purrr::map_lgl(
    seq_along(pres),
    \(slide_number) {
      .slide_needs_code_layout(
        pres$slide$get_slide(slide_number),
        original_layout_filename
      )
    }
  )

  if (any(slides_updated)) {
    fs::path(
      pres$package_dir,
      "ppt/slides/_rels",
      paste0("slide", which(slides_updated)),
      ext = "xml.rels"
    ) |>
      purrr::walk(
        \(rel_path) {
          .replace_layout(
            rel_path,
            original_layout_filename,
            .get_layout_filename(pres, "Title and Code")
          )
        }
      )
    officer:::pack_folder(pres$package_dir, target = pptx_path)
  }

  return(invisible(pptx_path))
}

.slide_needs_code_layout <- function(slide,
                                     original_layout_filename) {
  if (slide$layout_name() != original_layout_filename) {
    return(FALSE)
  }

  # Look for any <p:txBody> where all <a:t> nodes are inside <a:r> nodes that
  # explicitly declare Courier
  typefaces <- xml2::xml_find_all(
    slide$get(),
    ".//p:sp[p:nvSpPr/p:cNvPr[@name='Content Placeholder 2']]/p:txBody"
  ) |>
    xml2::xml_find_all(".//a:r") |>
    xml2::xml_find_first(".//a:rPr/a:latin") |>
    xml2::xml_attr("typeface")

  return(
    length(typefaces) && all(!is.na(typefaces) & typefaces == "Courier")
  )
}

.replace_layout <- function(rel_path,
                            original_layout_filename,
                            code_layout_filename) {
  readLines(rel_path) |>
    stringr::str_replace(original_layout_filename, code_layout_filename) |>
    writeLines(rel_path)
}

.get_layout_filename <- function(pres, layout_name) {
  layouts <- pres$slideLayouts$names()
  return(names(layouts)[layouts == layout_name])
}
