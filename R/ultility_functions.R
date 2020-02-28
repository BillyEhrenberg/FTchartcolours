#' A function to collapse a vector into a character string.
#'
#' This function is a wrapper around the paste(c, collapse = '')
#' @param array a vector
#' @param sep what to interpace the array elements with, defualts to the or operator |
#' @export
#' myCollapse()

myCollapse <- function(array, sep = '|'){
  paste(array, collapse = sep)
}

#' Converts a three character hexcode, eg. #FFF to its six-character equivilent
#'
#' This function is a wrapper around the paste(c, collapse = '')
#' @param hex a websafe hexcode, starting with #
#' @export
#' hex_three_to_hex_six()



hex_three_to_hex_six <- function(hex){


  if(nchar(hex) == 4){

    res <- seq(2,nchar(hex),1) %>%
      purrr::map_chr(~{
        substr(hex,.x,.x) %>%
          rep(2) %>%
          myCollapse('')
      }) %>%
      myCollapse('') %>%
      paste0('#',.)

    # print(res)


  } else {

    res <- hex

  }

  res

}
