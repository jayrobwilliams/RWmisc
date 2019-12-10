#' Blank Theme
#'
#' A ggplot theme with no grid elements or gray background.
#'
#' @importFrom ggplot2 theme theme_bw element_blank
#'
#' @export
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(x = hp, y = mpg)) +
#' ggplot2::geom_point() +
#' theme_rw()
theme_rw <- function() {
  theme_bw() +
  theme(plot.background = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_blank())
}
