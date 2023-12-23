#' Parse bibliographic references using anystyle
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function utilizes the [anystyle](https://anystyle.io/) Ruby gem to parse bibliographic references.
#' It segments references (one per line) and converts them into structured formats.
#' The input can be
#' - the paths of one or several text documents containing one full reference per line.
#' - a character vector with each element of the vector containing one full reference.
#'
#' @param input Vector of file paths to the text documents containing bibliographic references,
#' or a single string of reference(s). If input is not a file, it is written to a temporary file for parsing.
#' @param path The path where the parsed file(s) will be saved. If NULL, files are saved in
#' the current working directory. If empty, the parsed data is returned directly and not saved.
#' @param output_format Desired output format, one of "bib", "json", "xml" or "ref". "ref" is a text file
#' with one reference per line. Default is "json".
#' @param overwrite Logical; if TRUE, existing files at the output location will be overwritten.
#' Default is FALSE.
#'
#' @return
#' If path is empty, returns a list (or a single element if only one input) containing the
#' parsed references in the specified format. If path is specified, parsed files are saved
#' at the location and the function returns NULL.
#'
#' @examples
#' \dontrun{
#' # Parse a single reference and return as JSON
#' parse_ref(
#'   input = "Doe, J. Title of the Article. Journal Name 2020, 13, 123-456.",
#'   output_format = "json"
#' )
#'
#' # Parse references from a file and save as BibTeX
#' parse_ref(
#'   input = "path/to/references.txt", path = "path/to/output",
#'   output_format = "bib"
#' )
#' }
#'
#' @export
parse_ref <- function(input = NULL,
                      path = NULL,
                      output_format = c("bib", "json", "xml", "ref"),
                      overwrite = FALSE) {
  # saving the references in the string
  if (!all(grepl("\\.txt", input))) {
    write(input, "ref_to_parse.txt")
    input <- "ref_to_parse.txt"
  }

  # Check if input is NULL or not a character vector
  if (is.null(input) || !is.character(input)) {
    cli::cli_alert_danger("Input must be a non-null character vector.")
    stop("Invalid input: must be a character vector.", call. = FALSE)
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
  if (!output_format %in% c("bib", "json", "xml", "ref")) {
    cli::cli_alert_danger("Invalid output format specified. Please choose one of 'bib', 'json', or 'xml'.")
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
  overwriting <- ifelse(overwrite, " --overwrite ", " --no-overwrite ")

  data <- vector(mode = "list", length = length(input))
  names(data) <- basename(input)
  for (doc in input) {
    command <- paste0(
      "anystyle",
      overwriting,
      "-f ",
      output_format,
      " parse ",
      doc,
      " ",
      path
    )

    print(command)
    result <- system(command, intern = TRUE)
    if (input[1] == "ref_to_parse.txt") {
      invisible(file.remove("ref_to_parse.txt"))
    }
    if (path == "") data[[paste0(basename(doc))]] <- paste0(result, collapse = "")
    if (path == "" & length(result) == 0) cli::cli_alert_warning(paste0("No references were found in ", basename(doc)))
  }
  if (length(data) == 1) data <- data[[1]]
  if (path == "") {
    return(data)
  }
}
