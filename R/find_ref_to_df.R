#' Convert Found or Parsed References to Data Frames
#'
#' `r lifecycle::badge("experimental")`
#'
#' These functions convert references found in PDF documents or parsed from text files into tidy data frames.
#' [find_ref_to_df()] utilizes the [find_ref()] function for PDFs, and [parse_ref_to_df()] utilizes
#' the [parse_ref()] function for text files.
#'
#' @param input Vector of file paths to the documents to be analyzed (PDF for [find_ref_to_df()]
#' and text for [parse_ref_to_df()]).
#' @param no_layout Logical; if TRUE, the '--no-layout' option is used in [find_ref_to_df()],
#' which might be necessary for some PDFs (e.g., use this
#' if your document uses a multi-column layout). Ignored in [parse_ref_to_df()]. Default is FALSE.
#' @param clean_ref Logical; if TRUE, cleans the references using the [clean_ref()] function
#' after conversion (applicable to both functions). Default is TRUE. See [clean_ref()] for
#' details on what the function does.
#'
#' @return A tidy data frame with one row per reference, including metadata (author, title, etc...),
#' unique identifiers for each reference and document, and the complete original reference.
#'
#' @details
#' [find_ref_to_df()] analyzes PDF documents and extracts all references, converting them into a
#' structured data frame.
#' It requires the 'anystyle' Ruby gem and uses both the 'find' and 'parse' features
#' ([find_ref()] and [parse_ref()] respectively) to gather detailed information about each reference.
#'
#' [parse_ref_to_df()] works similarly but is designed for text documents. It parses structured references
#' from text files and converts them into a data frame.
#'
#' These functions Creates unique identifiers for each reference within a document and across the entire set of documents.
#'    - `id_doc`: A unique identifier for each document based on its position in the input.
#'    - `id_ref`: A unique identifier for each reference within its document. It's a combination of `id_doc` and
#'  the reference's row number within the document, ensuring each reference across all documents has a unique ID.
#'
#' @examples
#' \dontrun{
#' # For a PDF document
#' references_df <- find_ref_to_df(input = c(
#'   "path/to/document1.pdf",
#'   "path/to/document2.pdf"
#' ))
#'
#' # For a text file
#' references_df <- parse_ref_to_df(input = "path/to/references.txt")
#' }
#'
#' @seealso
#' [find_ref()], [parse_ref()], and [clean_ref()] for related functionality.
#'
#' @export
find_ref_to_df <- function(
    input = NULL,
    no_layout = FALSE,
    clean_ref = TRUE){
  results <- find_ref(
    input = input,
    path = "",
    output_format = "json",
    no_layout = no_layout
  )

  find_ref(
    input = input,
    output_format = "ref",
    no_layout = no_layout,
    overwrite = TRUE
  )

  full_ref <- vector("list", length(input))
  names(full_ref) <- basename(input)
  for (ref in input) {
    ref_file <- stringr::str_replace(basename(ref), "\\.pdf", ".ref")
    if(file.exists(ref_file)){ # to handle document with no reference
    options(warn = -1) # remove unnecessary warnings
    full_ref[[basename(ref)]] <- tibble::tibble(full_ref = readLines(ref_file))
    options(warn = 0)
    file.remove(ref_file)
    }
  }

  data_ref <- build_data_ref(results, input) %>%
    dplyr::mutate(full_ref = dplyr::bind_rows(full_ref)$full_ref)

  if (clean_ref) data_ref <- clean_ref(data_ref)
  return(data_ref)
}

#' @rdname find_ref_to_df
#' @export
parse_ref_to_df <- function(
    input = NULL,
    clean_ref = TRUE) {
  results <- parse_ref(input,
    path = "",
    output_format = "json"
  )

  if (all(stringr::str_detect(input, "\\.txt"))) {
    parse_ref(
      input = input,
      output_format = "ref",
      overwrite = TRUE
    )

    full_ref <- vector("list", length(input))
    names(full_ref) <- basename(input)
    for (ref in input) {
      ref_file <- stringr::str_replace(basename(ref), "\\.txt", ".ref")
      if(file.exists(ref_file)){ # to handle document with no reference
        options(warn = -1) # remove unnecessary warnings
        full_ref[[basename(ref)]] <- tibble::tibble(full_ref = readLines(ref_file))
        options(warn = 0)
        file.remove(ref_file)
      }
    }

    data_ref <- build_data_ref(results) %>%
      dplyr::mutate(full_ref = dplyr::bind_rows(full_ref)$full_ref)
  } else {
    data_ref <- build_data_ref(results, input)
  }

  if (clean_ref) data_ref <- clean_ref(data_ref)
  return(data_ref)
}

#' Build Data Reference Frame
#'
#' This function converts the results from [find_ref()] or [parse_ref()] into a tidy data frame.
#' It handles JSON structures and unnests them where possible to create a comprehensive reference data frame.
#'
#' @param data The raw data from [find_ref()] or [parse_ref()].
#'
#' @return A tidy data frame with one row per reference and additional details.
#'
#' @details
#' Internally used by [find_ref_to_df()] and [parse_ref_to_df()], this function processes the raw JSON or text data
#' into a structured data frame. It uses `safely_unnest` to handle nested lists within the data.
#'
#' @keywords internal

build_data_ref <- function(data, input = input) {
  if (!is.list(data)) list(data)
  names(data) <- basename(input)
  data <- data[sapply(data, function(x) x != "")] # excluding empty elements of the list (when no references)


  data_ref <- purrr::map(data, ~ jsonlite::fromJSON(.) %>% # extracting JSON information
    tibble::as_tibble() %>%
    dplyr::mutate(id_ref = dplyr::row_number(), .before = dplyr::everything())) %>%
    dplyr::bind_rows(.id = "doc") %>%
    dplyr::group_by(.data$doc) %>%
    dplyr::mutate(
      id_doc = dplyr::cur_group_id(),
      id_ref = paste0(.data$id_doc, "_", .data$id_ref),
      .before = dplyr::everything()
    ) %>%
    dplyr::ungroup() %>%
    safely_unnest()
  return(data_ref)
}

#' Safely Unnest Data
#'
#' This helper function is used to unnest data frames that contain list columns, doing so only when it's safe (i.e., the list has no more than one element per row).
#'
#' @param data A data frame potentially containing list columns.
#'
#' @return A data frame with nested data unnested where it was safe to do so.
#'
#' @keywords internal


safely_unnest <- function(data) {
  . <- NULL
  # Find columns with no more than one element that we can safely unnest
  safe_list_columns <- purrr::map(data, ~ purrr::map_int(., length)) %>%
    purrr::map_lgl(., ~ all(. < 2)) %>%
    which(isTRUE(.)) %>%
    names()

  data <- data %>%
    tidyr::unnest(dplyr::where(is.list) & dplyr::any_of(safe_list_columns), keep_empty = TRUE)
}
