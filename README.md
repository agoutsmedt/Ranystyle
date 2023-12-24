
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ranystyle

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Ranystyle is an R package designed to automate the extraction, parsing,
and cleaning of bibliographic references from PDF and text documents.
Utilizing the power of the ‘anystyle’ Ruby gem, it segments references
and converts them into structured formats suitable for analysis and use.

You can cite this package as:

``` r
citation("Ranystyle")
#> To cite biblionetwork in publications use:
#> 
#>   Goutsmedt, Aurélien, (2023). Ranystyle: Automated Bibliographic
#>   Reference Parsing and Cleaning. R package version 0.0.999.
#>   https://github.com/agoutsmedt/Ranystyle
#> 
#> Une entrée BibTeX pour les utilisateurs LaTeX est
#> 
#>   @Manual{,
#>     title = {Ranystyle: Automated Bibliographic Reference Parsing and Cleaning},
#>     author = {Aurélien Goutsmedt},
#>     year = {2024},
#>     note = {R package version 0.0.999},
#>     url = {https://github.com/agoutsmedt/Ranystyle},
#>   }
#> 
#> As Ranystyle is continually evolving, you may want to cite its version
#> number. Find it with 'help(package=Ranystyle)'.
```

## Installation

You can install the development version of Ranystyle from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("agoutsmedt/Ranystyle")
```

## Example

Here’s a basic example of how you might use Ranystyle to parse and clean
references from a PDF document:

``` r
library(Ranystyle)
# Path to your PDF document
pdf_path <- system.file("extdata", package = "Ranystyle") 
files <- list.files(pdf_path)

# Extract references from the PDF
extracted_refs <- find_ref_to_df(input = paste0(pdf_path, "/", files[1]))
#> [1] "anystyle -f json find C:/Users/goutsmedt/AppData/Local/Temp/RtmpaWpjf1/temp_libpath5104423022cc/Ranystyle/extdata/example_doc_1.pdf "
#> [1] "anystyle --overwrite -f ref find C:/Users/goutsmedt/AppData/Local/Temp/RtmpaWpjf1/temp_libpath5104423022cc/Ranystyle/extdata/example_doc_1.pdf ./"

# Print the extracted references
print(extracted_refs)
#> # A tibble: 81 × 23
#>    id_doc doc         id_ref author   title  year `container-title` volume pages
#>     <int> <chr>       <chr>  <list>   <chr> <int> <chr>             <chr>  <chr>
#>  1      1 example_do… 1_1    <tibble> ECB …  2023 Financial Times   <NA>   <NA> 
#>  2      1 example_do… 1_2    <tibble> Rule…  1983 Journal of Monet… 12     101–…
#>  3      1 example_do… 1_3    <tibble> Idea…  2009 Journal of Europ… 16     701–…
#>  4      1 example_do… 1_4    <tibble> A st…  1999 Scottish Journal… 46     17–39
#>  5      1 example_do… 1_5    <tibble> Tech…  2018 International Po… 12     328–…
#>  6      1 example_do… 1_6    <tibble> Late…  2003 Journal of Machi… 3      <NA> 
#>  7      1 example_do… 1_7    <tibble> Plan…  2022 Zeitschrift Für … 32     707–…
#>  8      1 example_do… 1_8    <tibble> Repu…  2010 <NA>              <NA>   <NA> 
#>  9      1 example_do… 1_9    <tibble> The …  2016 At the Macroecon… <NA>   <NA> 
#> 10      1 example_do… 1_10   <tibble> Effe…  2017 At the EUROFI Co… <NA>   <NA> 
#> # ℹ 71 more rows
#> # ℹ 14 more variables: location <chr>, publisher <chr>, type <chr>, date <chr>,
#> #   other_date <chr>, other_title <chr>, url <chr>, issue <chr>, doi <chr>,
#> #   edition <chr>, genre <chr>, note <chr>, editor <chr>, full_ref <chr>
```

## Credits

*anystyle* has been developed by [Alex
Fenton](https://github.com/a-fent), [Sylvester
Keil](https://github.com/inukshuk), [Johannes
Krtek](https://github.com/flachware) and [Ilja
Srna](https://github.com/namyra). *anystyle* is under copyright:
Copyright 2011-2018 Sylvester Keil. All rights reserved. See the
[Licence](https://github.com/inukshuk/anystyle-cli/blob/master/LICENSE)
for details.

The logo of *Ranystyle* has been generated with DALL·E.
