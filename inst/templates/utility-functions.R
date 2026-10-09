#' Read an option from a YAML spec with a default fallback
#'
#' Look up `name` in a named list `spec` (typically parsed from a YAML file)
#' and return the stored value if it is not `NULL`, otherwise return
#' `default`. Used throughout the MN2mrg workflow to give YAML-driven
#' configuration a consistent fallback pathway when a field is omitted.
#'
#' @param spec A named list, typically loaded from YAML with
#'   [yaml::read_yaml()].
#' @param name Character. Name of the option to look up in `spec`.
#' @param default Value returned when `spec[[name]]` is `NULL`.
#'
#' @return `spec[[name]]` when it is not `NULL`; otherwise `default`.
#'
#' @export
set_option <- function(spec, name, default) {
  option <- spec[[name]]
  if (is.null(option)) {
    out <- default
  } else {
    out <- spec[[name]]
  }
  return(out)
}

#' Read the final estimation table from a NONMEM .ext file
#'
#' Locate the last `TABLE NO` header in a NONMEM `.ext` file, which marks the
#' start of the final estimation method's output, and return everything from
#' that point onward as a data frame. Used to pull final parameter estimates
#' and objective function values for translation verification and forest-plot
#' bootstrapping.
#'
#' @param ext Character. Path to a NONMEM `.ext` file.
#'
#' @return A data frame containing the last estimation table on success.
#'   Returns the sentinel value `1` (numeric) when `ext` does not exist,
#'   printing a warning message to stdout. Callers should treat any
#'   non-data.frame return as a failed read.
#'
#' @export
read_ext <- function(ext) {
  if (file.exists(ext)) {
    rawExt <- readr::read_lines(file = ext)
    lastTable <- max((1:length(rawExt))[stringr::str_detect(rawExt, "TABLE NO")])
    results <- read.table(textConnection(rawExt[lastTable:length(rawExt)]),
                          header = TRUE, skip = 1)
    return(results)
  } else {
    cat(paste0("\nFile ", ext, " does not exist!\n\n"))
    return(1)
  }
}

#' Initialise a Quarto document with a title-only front matter
#'
#' Write a minimal Quarto YAML front matter to `qmd` so the file is ready to
#' receive figure blocks appended by [plot_quarto()] and eventually rendered
#' by [render_quarto()]. Overwrites any existing file at `qmd`.
#'
#' @param qmd Character. Path to the target `.qmd` file.
#' @param stem Character. Base name for downstream output artifacts. Currently
#'   informational only; retained for future use and caller compatibility.
#' @param title Character. Title written into the Quarto front matter.
#'
#' @return Invisibly `NULL`. Side effect: `qmd` is created with a YAML front
#'   matter block.
#'
#' @export
init_quarto <- function(qmd, stem, title) {
  cat("---\n", file = qmd, append = FALSE)
  cat("title:", title, "\n", file = qmd, append = TRUE)
  cat("format: pdf\n", file = qmd, append = TRUE)
  cat("editor: source\n", file = qmd, append = TRUE)
  cat("---\n\n\n", file = qmd, append = TRUE)
}

#' Append a LaTeX figure block to a Quarto document
#'
#' Append a raw LaTeX `figure` environment to `qmd` that includes the image at
#' `path` with a bold `title` and a `subcaption` for the caption. Intended to
#' be called after [init_quarto()] and before [render_quarto()].
#'
#' @param qmd Character. Path to an existing Quarto document created by
#'   [init_quarto()].
#' @param path Character. Path to the figure file (PNG or PDF, resolved by
#'   LaTeX at render time).
#' @param title Character. Figure title (rendered as `\caption{...}`).
#' @param caption Character. Figure subcaption (rendered as `\subcaption{...}`).
#'
#' @return Invisibly `NULL`. Side effect: a LaTeX block is appended to `qmd`.
#'
#' @export
plot_quarto <- function(qmd, path, title, caption) {
  cat("```{=latex}\n", file = qmd, append = TRUE)
  cat("\\begin{figure}\n", file = qmd, append = TRUE)
  cat("\\captionsetup{justification=raggedright,singlelinecheck=false}\n",
      file = qmd, append = TRUE)
  cat("\\caption{", title, "}\n", file = qmd, append = TRUE)
  cat("\\includegraphics{", path, "}\n", file = qmd, append = TRUE)
  cat("\\subcaption{", caption, "}\n", file = qmd, append = TRUE)
  cat("\\end{figure}\n", file = qmd, append = TRUE)
  cat("```\n\n\n", file = qmd, append = TRUE)
}

#' Render a Quarto document to PDF
#'
#' Thin wrapper around [quarto::quarto_render()] that suppresses code echo
#' in the rendered output. Used by the forest-plot pipeline to produce the
#' final PDF from a Quarto file assembled with [init_quarto()] and
#' [plot_quarto()].
#'
#' @param qmd Character. Path to the Quarto document to render.
#'
#' @return Invisibly `NULL`. Side effect: `qmd` is rendered to PDF alongside
#'   it.
#'
#' @export
render_quarto <- function(qmd) {
  quarto::quarto_render(qmd, execute = list(echo = FALSE))
}
