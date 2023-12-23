#' Install anystyle and anystyle-cli
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function installs 'anystyle' and 'anystyle-cli' using the system's Ruby gem package manager.
#' It requires Ruby and its gem package manager to be installed on the system.
#' [anystyle](https://anystyle.io/) is a fast and flexible tool for parsing bibliographic references.
#'
#' @details
#' The function executes the 'gem install' command for 'anystyle' and 'anystyle-cli'.
#' It requires that the user has the necessary permissions to install gems on the system.
#' If Ruby or gem is not installed, the function will fail, and you'll need to install Ruby first.
#'
#' Ensure that the Ruby environment is properly configured and that gem can be called from the command line.
#' See more details on Ruby's installation [here](https://www.ruby-lang.org/fr/downloads/) and on
#' RubyGems [here](https://rubygems.org/pages/download?locale=fr).
#'
#' @return
#' Invisible NULL. The function is called for its side effects (installing software) rather than a return value.
#'
#' @examples
#' \dontrun{
#' install_anystyle()
#' }
#'
#' @export
install_anystyle <- function() {
  system("gem install anystyle")
  system("gem install anystyle-cli")
}
