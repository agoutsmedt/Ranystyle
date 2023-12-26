#' Create a Word Bibliography from References in PDF or Text Documents
#'
#' `r lifecycle::badge("experimental")`
#'
#' These functions, [find_ref_to_word_bibliography()] and [parse_ref_to_word_bibliography()],
#' are designed to extract or parse references from PDF or text documents respectively,
#' and then create a formatted bibliography in a Word document using RMarkdown. The references
#' are formatted according to the specified Citation Style Language (CSL) file, if provided.
#'
#' @param input The path to the document (PDF for [find_ref_to_word_bibliography()] and
#'        text or vector of character (with one reference per element)
#'        for [parse_ref_to_word_bibliography()] from which to extract or parse references.
#' @param output_file Optional; The path where the Word document should be saved, with the
#'        path incorporating the name of the output. If provided, it must end with '.doc' or '.docx'.
#'        If not provided, the function will create a file with the same name as the
#'        input file, but with a '.docx' extension, in the user's home directory.
#' @param csl_path Optional; the path to a CSL file that specifies the formatting
#'        for the bibliography. If not provided, the default CSL of Rmarkdown will be used.
#' @param no_layout A logical value indicating whether to ignore the layout of
#'        the document when extracting or parsing references. This is typically
#'        more relevant for PDF documents.
#'
#' @return The path to the created Word document containing the bibliography.
#'
#' @details
#' Both functions operate by first extracting or parsing references from the specified
#' document using the [find_ref()] or [parse_ref()] function respectively and saving the results
#' in a temporary '.bib' file. They then construct an RMarkdown document with YAML front matter
#' specifying the title, output format (Word), and the path to the bibliography file and
#' optional CSL file. Finally, they render the RMarkdown document to a Word file, placing
#' the formatted bibliography in the resulting document.
#'
#' @examples
#' \dontrun{
#' # Assuming you have a PDF document named 'sample.pdf' in your working directory
#' # and you want to create a Word document named 'bibliography.docx':
#' find_ref_to_word_bibliography(input = "sample.pdf",
#' output_file = "full/path/to/bibliography.docx")
#'
#' # If you also have a CSL file for formatting named 'apa.csl' (document saved in the directory file):
#' find_ref_to_word_bibliography(input = "sample.pdf",
#' output_file = "bibliography.docx", csl_path = "apa.csl")
#' }
#'
#' @export
find_ref_to_word_bibliography <- function(input,
                                          output_file = NULL,
                                          csl_path = NULL,
                                          no_layout = FALSE) {
  if(length(input) > 1) {
    cli::cli_alert_danger("Please input only one PDF document.")
    stop("Multiple documents inputted, but only one is allowed.", call. = FALSE)
  }

  # Check if output_file is provided and has a valid extension
  if (!is.null(output_file) && !grepl("\\.docx?$", output_file)) {
    cli::cli_alert_danger("The output file must end with '.doc' or '.docx'.")
    stop("Invalid output file extension.", call. = FALSE)
  }

  # Extract references from the PDF
  temp_dir <- tempdir()
  find_ref(
    input = input,
    path = temp_dir,
    output_format = "bib",
    overwrite = TRUE,
    no_layout = no_layout
  )
  biblio_name <- paste0(tools::file_path_sans_ext(basename(input)), ".bib")

  # Create an RMarkdown yaml content
  yaml_front_matter <- "---
title: 'Generated Bibliography'
output: word_document
bibliography: '`r biblio_name`'
nocite: '@*'
"

  # Conditionally add the CSL path to the YAML if provided
  if (!is.null(csl_path)) {
    yaml_front_matter <- paste(yaml_front_matter, "csl: ", csl_path, "\n", sep = "")
  }

  yaml_front_matter <- paste(yaml_front_matter, "---\n", sep = "")

  # Create an RMarkdown document content with a title and optional CSL in the YAML front matter
  rmd_content <- paste(
    yaml_front_matter,
    "# Bibliography\n",
    sep = "\n"
  )

  # Create a temporary RMarkdown file
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(rmd_content, temp_rmd)

  # Determine the output path
  if (is.null(output_file)) {
    # Set the default output path to the user's home directory with a specified filename
    default_filename <- paste0(tools::file_path_sans_ext(basename(input)), ".docx")
    output_file <- file.path(normalizePath("~"), default_filename)
  }
  if (tools::file_path_sans_ext(output_file) == tools::file_path_sans_ext(basename(output_file))){ # when the user just specify the name of the file
    default_filename <- output_file
    output_file <- file.path(normalizePath("~"), default_filename)
  }

  # Render the RMarkdown file to Word
  rmarkdown::render(temp_rmd, output_file = output_file)

  # Return the path to the created Word document
  if (is.null(output_file)) output_file <- stringr::str_replace(temp_rmd, "\\.Rmd", ".docx")
  return(output_file)
}

#' @rdname find_ref_to_word_bibliography
#' @export
parse_ref_to_word_bibliography <- function(input,
                                           output_file = NULL,
                                           csl_path = NULL) {
  # Check if the input is a .txt file and more than one is provided
  if(all(grepl("\\.txt$", input)) & length(input) > 1) {
    cli::cli_alert_danger("Please input only one text file.")
    stop("Multiple text files inputted, but only one is allowed.", call. = FALSE)
  }

  # Check if output_file is provided and has a valid extension
  if (!is.null(output_file) && !grepl("\\.docx?$", output_file)) {
    cli::cli_alert_danger("The output file must end with '.doc' or '.docx'.")
    stop("Invalid output file extension.", call. = FALSE)
  }

  # Extract references from the PDF
  temp_dir <- tempdir()
  parse_ref(
    input = input,
    path = temp_dir,
    output_format = "bib",
    overwrite = TRUE
  )

  # changing name of the biblio if references given by a vector
  if (!all(grepl("\\.txt", input))) {
      input <- "ref_to_parse.txt"
    }
  biblio_name <- paste0(tools::file_path_sans_ext(basename(input)), ".bib")

  # Create an RMarkdown yaml content
  yaml_front_matter <- "---
title: 'Generated Bibliography'
output: word_document
bibliography: '`r biblio_name`'
nocite: '@*'
"

  # Conditionally add the CSL path to the YAML if provided
  if (!is.null(csl_path)) {
    yaml_front_matter <- paste(yaml_front_matter, "csl: ", csl_path, "\n", sep = "")
  }

  yaml_front_matter <- paste(yaml_front_matter, "---\n", sep = "")

  # Create an RMarkdown document content with a title and optional CSL in the YAML front matter
  rmd_content <- paste(
    yaml_front_matter,
    "# Bibliography\n",
    sep = "\n"
  )

  # Create a temporary RMarkdown file
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(rmd_content, temp_rmd)

  # Determine the output path
  if (is.null(output_file)) {
    # Set the default output path to the user's home directory with a specified filename
    default_filename <- paste0(tools::file_path_sans_ext(basename(input)), ".docx")
    output_file <- file.path(normalizePath("~"), default_filename)
  }
  if (tools::file_path_sans_ext(output_file) == tools::file_path_sans_ext(basename(output_file))){ # when the user just specify the name of the file
    default_filename <- output_file
    output_file <- file.path(normalizePath("~"), default_filename)
  }

  # Render the RMarkdown file to Word
  rmarkdown::render(temp_rmd, output_file = output_file)

  # Return the path to the created Word document
  if (is.null(output_file)) output_file <- stringr::str_replace(temp_rmd, "\\.Rmd", ".docx")
  return(output_file)
}
