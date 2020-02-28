#' A function to plot all the palettes in gchartcolour for colour picking
#'
#' This function plots all palettes using ggplot2. Requires the colour palettes to be in the global environment
#' @param g_chartcolour_palettes_df loaded palettes, using get_g_chartcolour_palettes
#' @export
#' @examples
#' plot_g_chartcolour_palettes()


plot_g_chartcolour_palettes <- function(g_chartcolour_palettes_df){

  ggplot2::ggplot(dplyr::bind_rows(g_chartcolour_palettes_df),
                  ggplot2::aes(factor(index),
             factor(palette),
             fill = factor(seq_along(index))
         ))+
    ggplot2::geom_tile()+
    ggplot2::geom_tile(fill = 'white', width = .8, height = .6, alpha = .2)+
    ggplot2::scale_fill_manual(values = g_chartcolour_palettes_df$hex_code)+
    ggplot2::theme(
      legend.position = 'none'
    )+
    ggplot2::geom_text(aes(label = hex_code), size = 3)

}
