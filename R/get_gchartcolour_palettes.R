#' A function to scrape all the FT colour palettes. Returns a named list
#'
#' Returns a named list of the palettes on this page https://ft-interactive.github.io/g-chartcolour/
#' scrapes the palette nodes, then calls the get_palette function on each to return tabular results
#' @export
#' @examples
#' get_g_chartcolour_palettes()

get_g_chartcolour_palettes <- function(){

  url <- 'https://ft-interactive.github.io/g-chartcolour/'

  page <- xml2::read_html(url)

  palettes <- rvest::html_nodes(page, '.palette') %>%
    purrr::map_dfr(get_palette)

  palettes.list <- palettes %>%
    split(.$palette)

}

#' A function to tabularise the data in a palette node.
#'
#' This function tabularises a single palette. Is called only inside get_g_chartcolour_palettes
#' @param palette a palette node from https://ft-interactive.github.io/g-chartcolour/
#' @export
#' @examples
#' get_palette()

get_palette <- function(palette){

  name <-  palette %>%
    rvest::html_node('h2') %>%
    rvest::html_attr('id')

  colours <- palette %>%
    rvest::html_nodes('.swatch-container')

  names <- rvest::html_nodes(colours,'b') %>%
    rvest::html_text(.)

  hex_code <- rvest::html_nodes(colours,'.swatch') %>%
    rvest::html_attr('style') %>%
    stringr::str_remove('^.+\\:')

  tibble::tibble(names, hex_code) %>%
    dplyr::mutate(index = seq_along(hex_code),
           names = stringr::str_remove(names,'\\:')) %>%
    dplyr::rowwise(.) %>%
    dplyr::mutate(palette = name,
           hex_code = hex_three_to_hex_six(hex_code)) %>%
    dplyr::ungroup(.)

}


#' A function to extract the swebsafe hex codes from a tabularised palette
#'
#' @param palette a formatted palette node in dataframe format, as returned by the above functions
#' @export
#' @examples
#' get_colours_from_palette()


get_colours_from_palette <- function(palette){

  dplyr::pull(palette, hex_code)

}


#' A function to assign all palettes
#'
#' @param palettes a list of palettes gained through get_g_chartcolour_palettes. If null, the palettes will be scraped. Defaults to NULL
#' @export
#' @examples
#' assign_all_palettes()



assign_all_palettes <- function(palettes = NULL){

  if(is.null(palettes)){

  palettes <- get_g_chartcolour_palettes()

  }


  purrr::map2(palettes,names(palettes),
              function(palette,name){
                assign(name, palette, envir = .GlobalEnv)
              })


}

