#' Clean and Organize Reference Data
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function tidies up the data frame of references obtained from [find_ref_to_df()] or [parse_ref_to_df()].
#' It performs various cleaning and reorganization tasks to present the data in a more structured and readable format.
#'
#' @param data A data frame of references to be cleaned, as produced by [find_ref_to_df()] or [parse_ref_to_df()].
#'
#' @return A cleaned and reorganized data frame with one row per reference and additional details.
#'
#' @details
#' The function performs the following steps:
#' - Collapses non-essential columns (i.e. other than `date` and `title`), combining multiple
#' entries into a single text string.
#' - Reorganizes `author` information, creating a full name from given, particle, and family components. Also
#' add an `author_order` column. The column will remain in a list format.
#' - Separates and cleans dates and titles, organizing them into primary and additional information.
#' The first date and first title are save in `date` and `title`. The other dates and titles are
#' saved in `other_date` and `other_title`.
#' - Extracts the year from the date and places it in a separate column.
#' - Removes extraneous punctuation and trims whitespace from character columns.
#' - Relocates key columns to a more standardized order for easier analysis and readability.
#'
#' This function can be used directly, or indirectly through [find_ref_to_df()] or [parse_ref_to_df()]
#' by setting the `clean_ref` parameter to TRUE.
#'
#' @export
clean_ref <- function(data) {
  data <- data %>%
    collapse_non_essential_columns() %>%
    reorganize_author() %>%
    separate_data_and_title() %>%
    dplyr::mutate(year = stringr::str_extract(.data$date, "[:digit:]{4}") %>% as.integer()) %>%
    dplyr::relocate(.data$author, .data$title, .data$year, .data$`container-title`, .data$volume, .data$pages, .data$location, .data$publisher, .data$type, .after = .data$id_ref) %>%
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), ~ stringr::str_remove_all(., "^[:punct:]+|[:punct:]+$")), # basic cleaning content of columns
      dplyr::across(dplyr::where(is.character), ~ stringr::str_trim(., "both"))
    )
}

#' Collapse Non-Essential Columns (Internal)
#'
#' Collapses non-essential columns in the reference data frame, converting lists and multiple entries into single text strings.
#' This is used as a preliminary step in cleaning and organizing the data.
#'
#' @param data A data frame of references with potential list columns.
#'
#' @return A data frame with non-essential information collapsed.
#'
#' @keywords internal
collapse_non_essential_columns <- function(data) {
  . <- NULL
  # We unlist non essential list columns and collapse with a break of line (for the list information to be recovered later)
  non_essential_columns <- purrr::map(data, ~ purrr::map_int(., length)) %>%
    purrr::map_lgl(., ~ any(. > 1)) %>%
    which(isTRUE(.)) %>%
    names() %>%
    .[-which(. %in% c("author", "title", "date"))]

  data <- data %>%
    dplyr::group_by(.data$id_ref) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(non_essential_columns),
        ~ paste0(unlist(.), collapse = "\n")
      ),
      dplyr::across(
        dplyr::all_of(non_essential_columns),
        ~ ifelse(. == "", NA_character_, .)
      )
    )
}

#' Reorganize Author Information (Internal)
#'
#' Reorganizes the 'author' column in the reference data frame, combining given, particle, and family name components into full names.
#' This helps standardize author information for further analysis and display.
#'
#' @param data A data frame of references with an 'author' column.
#'
#' @return A data frame with reorganized author information.
#'
#' @keywords internal
reorganize_author <- function(data) {
  . <- NULL
  # we unnest the authors column and create a new column with the full name of the authors
  data <- data %>%
    tidyr::unnest_wider(.data$author, names_sep = "_") %>%
    tidyr::unnest(dplyr::starts_with("author"), keep_empty = TRUE) %>%
    dplyr::mutate(
      author_particle = if ("author_particle" %in% names(.)) .data$author_particle else "", # In case there is no author_particle col
      author = paste(.data$author_given, .data$author_particle, .data$author_family) %>%
        stringr::str_remove_all(., "^NA | NA$| NA")
    ) %>%
    dplyr::group_by(.data$id_ref) %>%
    dplyr::mutate(author_order = 1:dplyr::n()) %>%
    tidyr::nest(author = dplyr::starts_with("author")) %>%
    dplyr::ungroup()
}

#' Separate and Clean Dates and Titles (Internal)
#'
#' Separates primary and additional date and title information in the reference data frame and performs basic cleaning operations.
#' This helps clarify the key information for each reference and removes unnecessary characters.
#'
#' @param data A data frame of references with 'date' and 'title' columns.
#'
#' @return A data frame with separated and cleaned date and title information.
#'
#' @keywords internal
separate_data_and_title <- function(data) {
  . <- NULL
  data <- data %>%
    tidyr::unnest_wider(c(.data$date, .data$title), names_sep = "_") %>%
    dplyr::rename(
      date = .data$date_1,
      title = .data$title_1
    ) %>%
    {
      if ("date_2" %in% names(.)) tidyr::unite(., "other_date", dplyr::starts_with("date_"), sep = "\n", na.rm = TRUE) else .
    } %>%
    {
      if ("title_2" %in% names(.)) tidyr::unite(., "other_title", dplyr::starts_with("title_"), sep = "\n", na.rm = TRUE) else .
    } %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("other_"), ~ ifelse(. == "", NA_character_, .)))
}
