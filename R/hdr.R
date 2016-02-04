#' @include ggthemes-package.R
#' @include ggthemes-data.R
NULL

#' HDR Consulting Theme
#'
#' Theme based on HDR Marketing Color Scheme and HDR Consulting Team preferences.
#'
#' @references
#'
#' \url{http://enterprise/sites/brandcenter/applying-our-brand/Pages/materials-to-download.aspx#templates}
#'
#' @inheritParams ggplot2::theme_grey
#' @family themes hdr
#' @export
#' @examples
#' library("ggplot2")
#' p <- ggplot(mtcars) +
#'      geom_point(aes(x = wt, y = mpg, colour=factor(gear))) +
#'      facet_wrap(~am) +
#'      ggtitle('Diamond Prices')
#' p + scale_colour_hdr() + theme_hdr()
theme_hdr <- function(base_size = 16,
                      base_family = "mono") {
    
    base_color <- hdr_pal()(7)[[7]]
    (theme_foundation(base_size = base_size, base_family = base_family) +
        theme(line = element_line(linetype = 1, colour = base_color),
              rect = element_rect(fill = "white", linetype = 0, colour = NA),
              text = element_text(colour = "black"),
              title = element_text(family = base_family,size = rel(1.5)),
              axis.text = element_text(face = "bold", size = rel(1)),
              axis.line = element_line(),
              legend.background = element_rect(),
              legend.position = "right",
              legend.direction = "vertical",
              legend.box = "vertical",
              panel.border = element_rect(colour = base_color,linetype = 1),
              panel.grid = element_line(colour = base_color, linetype = 1),
              panel.grid.major = element_line(colour = base_color),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_line(colour = base_color,linetype = 6),
              plot.title = element_text(hjust = 0, face = "bold"),
              plot.margin = unit(c(1, 1, 1, 1), "lines"),
              #plot.background = element_rect(color = "black",linetype=1)))#,
              strip.background = element_rect()))
}

#' HDR color palette
#'
#' The standard HDR color palette contains teal,red,green,gold,orange,magenta,grey
#'
#' @family colour hdr
#' @export
#' @examples
#' library("scales")
#' show_col(hdr_pal()(7))
hdr_pal <- function() {
    function(n) {
    hdr.colors <- list(blue = rgb(66,152,181,maxColorValue = 255),
                       red = rgb(200,16,46,maxColorValue = 255),
                       green = rgb(120,190,32,maxColorValue = 255),
                       gold = rgb(255,198,0,maxColorValue = 255),
                       orange = rgb(255,130,0,maxColorValue = 255),
                       magenta = rgb(206,0,88,maxColorValue = 255),
                       grey = rgb(168,169,158,maxColorValue = 255))
    colors <- unlist(hdr.colors)
    unname(colors[seq_len(n)])
    }
}

#' HDR color scales
#'
#' Color scales using the colors in the HDR graphics.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @family colour hdr
#' @rdname scale_hdr
#' @seealso \code{\link{theme_hdr}} for examples.
#' @export
scale_colour_hdr <- function(...) {
    discrete_scale("colour", "hdr", hdr_pal(), ...)
}

#' @rdname scale_hdr
#' @export
scale_color_hdr <- scale_colour_hdr

#' @rdname scale_hdr
#' @export
scale_fill_hdr <- function(...) {
    discrete_scale("fill", "hdr", hdr_pal(), ...)
}