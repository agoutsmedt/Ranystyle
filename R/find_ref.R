#' Find Bibliographic References in Documents Using Anystyle
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function utilizes the [anystyle](https://anystyle.io/) Ruby gem to analyze PDF or text documents and
#' extract all references it finds. The input can be a single or multiple PDF documents,
#'
#' @param input Vector of file paths to the PDF documents to be analyzed.
#' @param path The path where the parsed file(s) will be saved. If NULL, files are saved
#' in the current working directory. If empty, the parsed data is returned directly and not saved.
#' @param output_format Desired output format, one of "ref", "xml", "bib", or "json". "ref" is a text file
#' with one reference per line. Default is "json".
#' @param no_layout Logical; if TRUE, anystyle '--no-layout' option is used (e.g., use this
#' if your document uses a multi-column layout). Default is FALSE.
#' @param overwrite Logical; if TRUE, existing files at the output location will be overwritten. Default is FALSE.
#'
#' @return
#' If path is empty, returns a list (or a single element if only one input) containing the extracted references in the specified format. If path is specified, parsed files are saved at the location and the function returns NULL.
#'
#' @examples
#' \dontrun{
#' # Find references in a single PDF and return as JSON
#' find_ref(
#'   input = "path/to/document.pdf",
#'   output_format = "json"
#' )
#'
#' # Find references from a folder of documents and save as XML
#' find_ref(
#'   input = "path/to/documents/", path = "path/to/output",
#'   output_format = "xml"
#' )
#' }
#'
#' @export
find_ref <- function(input = NULL,
                     path = NULL,
                     output_format = c("ref", "xml", "bib", "json"),
                     no_layout = FALSE,
                     overwrite = FALSE) {
  # Input validation
  if (is.null(input) || !is.character(input)) {
    cli::cli_alert_danger("Input must be a non-null character vector.")
    stop("Invalid input: must be a character vector.", call. = FALSE)
  }

  if (!all(grepl("\\.pdf$", input))) {
    cli::cli_alert_danger("All elements of the input vector must be paths to '.pdf' files.")
    stop("Invalid input: all elements must be paths to '.pdf' files.", call. = FALSE)
  }

  if (any(!file.exists(input))) {
    missing_files <- input[!file.exists(input)]
    cli::cli_alert_danger(paste("The following files do not exist:", paste(missing_files, collapse = ", ")))
    stop("One or more input files do not exist.", call. = FALSE)
  }

  # Default output_format to "json" if none is provided and issue a warning
  if (length(output_format) != 1) {
    output_format <- "json"
    cli::cli_alert_warning("No output format specified. Using 'json' as the default.")
  }

  # Check if the specified output_format is valid
  if (!output_format %in% c("ref", "xml", "bib", "json")) {
    cli::cli_alert_danger("Invalid output format specified. Please choose one of 'ref', 'xml', 'bib', or 'json'.")
    stop("Invalid output format specified.", call. = FALSE)
  }

  if (is.null(path)) path <- "./"

  # Check if files already exist when overwrite FALSE
  if (!overwrite) {
    for (doc in input) {
      output_file <- file.path(path, paste0(tools::file_path_sans_ext(basename(doc)), ".", output_format))
      if (file.exists(output_file)) {
        cli::cli_alert_danger(paste0("The file '", output_file, "' already exists and overwrite is FALSE."))
        stop("File already exists.", call. = FALSE)
      }
    }
  }

  # Start function after check and routines
  overwriting <- ifelse(overwrite, " --overwrite ", " ")
  layout <- ifelse(no_layout, " --no-layout ", " ")

  data <- vector(mode = "list", length = length(input))
  names(data) <- basename(input)
  for (doc in input) {
    command <- paste0(
      "anystyle",
      overwriting,
      "-f ",
      output_format,
      " find",
      layout,
      doc,
      " ",
      path
    )
    print(command)
    result <- system(command, intern = TRUE)
    if (path == "") data[[paste0(basename(doc))]] <- paste0(result, collapse = "")
    if (path == "" & length(result) == 0) cli::cli_alert_warning(paste0("No references were found in ", doc))
  }
  if (length(data) == 1) data <- data[[1]]
  if (path == "") {
    return(data)
  }
}
