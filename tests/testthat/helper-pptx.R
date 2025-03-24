get_clean_pptx_hash <- function(pptx_path) {
  pptx_dir <- unzip_pptx(pptx_path)

  # Remove metadata file, which has a date.
  metadata_path <- fs::path(pptx_dir, "docProps/core.xml")
  if (fs::file_exists(metadata_path)) {
    fs::file_delete(metadata_path)
  }

  # Hash the remaining files
  all_files <- fs::dir_ls(pptx_dir, type = "file", recurse = TRUE)
  file_hashes <- vapply(all_files, digest::digest, character(1), file = TRUE)

  digest::digest(paste0(sort(file_hashes), collapse = ""))
}

get_pptx_date <- function(pptx_path) {
  pptx_dir <- unzip_pptx(pptx_path)
  metadata_path <- fs::path(pptx_dir, "docProps/custom.xml")
  xml2::xml_text(
    xml2::xml_find_first(
      xml2::read_xml(metadata_path),
      ".//d1:property[@name='date']"
    )
  )
}

unzip_pptx <- function(pptx_path, call = rlang::caller_env()) {
  pptx_dir <- withr::local_tempdir(.local_envir = call)
  fs::dir_create(pptx_dir)
  unzip(pptx_path, exdir = pptx_dir)
  return(pptx_dir)
}
