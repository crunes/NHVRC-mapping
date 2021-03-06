#' The Urban Institute [ggplot2] theme
#'
#' Color palettes used in the Urban Institute.
#'
#' @export
#' @param palette Palette name.
urban_color_pal <- function(palette = "categorical") {
  palette_list <- palette_urban

  types <- palette_list[[palette]]

  function(n) {
    types[[n]]
  }
}

#' Discrete scale color that aligns with the Urban Institute style
#'
#' @md
#' @param palette Urban Institute palette to use. One of \code{categorical} or \code{sequential}
#' @export
scale_color_urban <- function(palette = "categorical", ...) {
  ggplot2::discrete_scale("colour", "urban", urban_color_pal(palette), ...)
  }

#' Discrete scale color that aligns with the Urban Institute style
#'
#' @md
#' @param palette Urban Institute palette to use. One of \code{categorical} or \code{sequential}
#' @export
scale_colour_urban <- function(palette = "categorical", ...) {
  ggplot2::discrete_scale("colour", "urban", urban_color_pal(palette), ...)
}

#' Discrete scale fill that aligns with the Urban Institute style
#'
#' @md
#' @param palette Urban Institute palette to use. One of \code{categorical} or \code{sequential}
#' @export
scale_fill_urban <- function(palette = "categorical", ...) {
  ggplot2::discrete_scale("fill", "urban", urban_color_pal(palette), ...)
  }

#' Continuous scale fill that aligns with the Urban Institute style
#'
#' @md
#' @export
scale_color_gradientn_urban <- function(...,
                                         colours = c("#CFE8F3","#A2D4EC","#73BFE2","#46ABDB", "#1696D2","#12719E","#0A4C6A","#062635"),
                                         colors = c("#CFE8F3","#A2D4EC","#73BFE2","#46ABDB", "#1696D2","#12719E","#0A4C6A","#062635"),
                                         values = NULL,
                                         space = "Lab",
                                         na.value = "grey50",
                                         guide = "colourbar") {

  colours <- if (missing(colours)) colors else colours

  continuous_scale("colour", "gradientn",
                   scales::gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}

#' Continuous scale fill that aligns with the Urban Institute style
#'
#' @md
#' @export
scale_colour_gradientn_urban <- scale_color_gradientn_urban

#' Continuous scale fill that aligns with the Urban Institute style
#'
#' @md
#' @export
scale_fill_gradientn_urban <- function(...,
                                       colours = c("#CFE8F3","#A2D4EC","#73BFE2","#46ABDB", "#1696D2","#12719E","#0A4C6A","#062635"),
                                       colors = c("#CFE8F3","#A2D4EC","#73BFE2","#46ABDB", "#1696D2","#12719E","#0A4C6A","#062635"),
                                       values = NULL,
                                       space = "Lab",
                                       na.value = "grey50",
                                       guide = "colourbar") {

  colours <- if (missing(colours)) colors else colours

  continuous_scale("fill", "gradientn",
                   scales::gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}


