###Generation Color Palettes in Hex-Code###
  #Notes:
  #   unikn::seecol will create a visual of generated palette
  #   unikn::usecol will create a palette and will return it as a vector

#' My Palette Cerulean
#'
#' @param n the number of steps for color scale; this function will have n=1
#' @import unikn
#' @import rlang
#' @return
#' @export
#'
#' @examples
Cer <- function(n=1){
  try(if({{n}} !=1) stop("Input should be the value 1"))
  #color
  color_scheme = "#0197F6"
  usecol(pal=color_scheme)
}

#' Divergent Scale :: generates a palette with n steps || Gray -> White -> Cerulean
#'
#' @param n   the number of steps for color scale
#' @param a   the alpha/opacity of the colors
#' @import unikn
#' @import rlang
#' @return
#' @export
#'
#' @examples
Cer_Div <- function(n,a=NA){
  #stops
  stopifnot("Input must be positive double" = is.double({{n}}))
  try(if({{n}}<2) stop("Not Enough Steps"))
  #color palette
  color_scheme = c("#B2B5B6","white","#0197F6")
  #generate palette with n steps
  usecol(pal=color_scheme,{{n}},{{a}})
}

#' Continuous Scale :: generates a palette with n steps || White -> Cerulean
#'
#' @param n the number of steps for color scale
#' @param a the alpha/opacity of the colors
#' @import unikn
#' @import rlang
#' @return
#' @export
#'
#' @examples
Cer_Cts <- function(n,a=NA){
  stopifnot("Input must be positive double" = is.double({{n}}))
  try(if({{n}}<2) stop("Not Enough Steps"))
  #color palette
  color_scheme = c("white","#0197F6")
  #generate palette with n steps ||  White -> Blue
  usecol(pal=color_scheme,{{n}},{{a}})
}

#' Viridis Scale :: generates a palette with n steps || Purple -> Blue -> Green -> Yellow
#'
#' @param n the number of steps for color scale
#' @param a the alpha/opacity of the colors
#' @import unikn
#' @import rlang
#' @import viridis
#' @return
#' @export
#'
#' @examples
Vir <- function(n,a=NA){
  stopifnot("Input must be positive double" = is.double({{n}}))
  try(if({{n}}<1) stop("Not Enough Steps"))
  #color palette
  color_scheme = viridis::viridis({{n}})
  #generate palette with n steps ||  Purple -> Blue -> Green -> Yellow
  usecol(pal=color_scheme,{{n}},{{a}})
}

#' Magma Scale :: generates a palette with n steps || Black -> Purple -> Red -> Orange -> Yellow
#'
#' @param n the number of steps for color scale
#' @param a the alpha/opacity of the colors
#' @import unikn
#' @import rlang
#' @import viridis
#' @return
#' @export
#'
#' @examples
Mag <- function(n,a=NA){
  stopifnot("Input must be positive double" = is.double({{n}}))
  try(if({{n}}<1) stop("Not Enough Steps"))
  #color palette
  color_scheme = viridis::magma({{n}})
  #generate palette with n steps ||  Black -> Purple -> Red -> Orange -> Yellow
  usecol(pal=color_scheme,{{n}},{{a}})
}

#' Categorical Scale :: generates a palette with n steps || GreenBlue -> RedOrange -> Purple -> Pink -> Green -> Yellow -> GoldBrown -> Black
#'
#' @param n the number of steps for color scale
#' @param a the alpha/opacity of the colors
#' @param name the name of scale found within ColorBrewer, currently defaulted to Dark2
#' @import unikn
#' @import rlang
#' @import RColorBrewer
#' @return
#' @export
#'
#' @examples
Cat <- function(n,a=NA,name="Dark2"){
  stopifnot("Input must be positive double" = is.double({{n}}))
  try(if({{n}}<3) stop("Not Enough Steps"))
  try(if({{n}}>8) stop("Too Many Steps"))
  #color palette
  color_scheme = RColorBrewer::brewer.pal(n,name)
  #generate palette with n steps ||  GreenBlue -> RedOrange -> Purple -> Pink -> Green -> Yellow -> GoldBrown -> Black
  usecol(pal=color_scheme,{{n}},{{a}})
}

#' G-Y-R Scale :: generates a palette with 3 steps || Green-> Yellow-> Red
#'
#' @param n the number of steps for color scale
#' @param a the alpha/opacity of the colors
#' @import unikn
#' @import rlang
#' @return
#' @export
#'
#' @examples
GYR <- function(n=3,a=NA){
  stopifnot("Input must be positive double" = is.double({{n}}))
  try(if({{n}}!=3) stop("Needs to be 3 steps"))
  #color palette
  color_scheme = c("#51b363","#f0bd27","#e03531")
  #generate palette with n steps ||  Green -> Yellow -> Red
  usecol(pal=color_scheme,{{n}},{{a}})
}

#' Theme tool for generic ggplot
#'
#' @param complete_themes plot background theme
#' @param title_label     plot title
#' @param subtitle_label  plot subtitle
#' @param title_caption   plot caption
#' @param x_label         x axis label
#' @param y_label         y axis label
#' @param ...             theme information
#'
#' @return
#' @export
#' @import ggplot2
#' @import rlang
#' @examples
gg_tool <- function(complete_themes = theme_bw(),
                    title_label = "",
                    subtitle_label = "",
                    title_caption = "",
                    leg.title = "",
                    x_label = "",
                    y_label = "",
                    title.pos = element_text(hjust = 0.5),
                    subtitle.pos = element_text(hjust = 0.5),
                    leg.pos = "right",
                    angle = 30,
                    hjust = 1,
                    vjust = 1.5
){
  list(
    complete_themes,
    labs(title = title_label,subtitle = subtitle_label, caption = title_caption, color=leg.title),
    xlab(label = x_label),
    ylab(label = y_label),
    theme(plot.title = title.pos,
          plot.subtitle = subtitle.pos,
          legend.position = leg.pos,
          axis.text.x = element_text(angle = angle, hjust = hjust, vjust = vjust))
  )
}
