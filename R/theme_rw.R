#' Blank Theme
#'
#' A ggplot theme with no grid elements or gray background.
#'
#' @export
#'
#' @examples
#' ggplot(mtcars, aes(x = hp, y = mpg)) +
#' geom_point() +
#' theme_rw()
theme_rw <- function() {
  theme_bw() +
  theme(plot.background = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_blank())

}
