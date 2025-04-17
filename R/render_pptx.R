#' Render qmd as pptx
#'
#' Render a qmd as pptx, applying the Atorus theme.
#'
#' @inheritDotParams quarto::quarto_render output_file execute execute_params
#'   execute_dir execute_daemon execute_daemon_restart execute_debug use_freezer
#'   cache cache_refresh metadata metadata_file debug quiet profile quarto_args
#'   pandoc_args as_job
#' @param path The path to the `qmd` file to render. By default, a new file will
#'   be created in the same directory as `path`, with extension `.pptx`.
#' @param date A date to include on the title slide of the deck. Set this to
#'   `NULL` if you do not wish to include a date.
#' @param quiet Whether to suppress output during the rendering process. Passed
#'   on to [quarto::quarto_render()].
#'
#' @returns The path to the resulting `pptx` file, invisibly (this function is
#'   called to generate the `pptx` as a side effect).
#' @export
render_pptx <- function(path, date = Sys.Date(), quiet = TRUE, ...) {
  dots <- rlang::list2(...)
  dots$metadata <- .reconcile_metadata(dots$metadata, date)
  dots$output_format <- NULL
  dots$input <- NULL
  output_file <- dots$output_file %||% fs::path_ext_set(basename(path), "pptx")
  pptx_path <- fs::path(fs::path_dir(path), output_file)
  rlang::inject({
    quarto::quarto_render(
      path,
      output_format = "pptx",
      quiet = quiet,
      !!!dots
    )
  })
  return(.postprocess_pptx(pptx_path))
}

.reconcile_metadata <- function(metadata, date) {
  metadata <- c(
    list(
      `reference-doc` = system.file("template.pptx", package = "atorusprez"),
      `date-format` = "long",
      date = date
    ),
    metadata,
    list(
      incremental = TRUE
    )
  )
  metadata <- metadata[lengths(metadata) > 0]
  metadata <- metadata[unique(names(metadata))]
  if ("date" %in% names(metadata)) {
    metadata$date <- as.character(metadata$date)
  }
  return(metadata)
}
