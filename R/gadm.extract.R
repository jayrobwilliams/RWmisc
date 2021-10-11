#' Extract layers by country from GADM GeoPackage file
#'
#' Extract one or more levels of administrative unit geometries from
#'
#' @param input GeoPackage file to read from
#' @param output name of file to save output to
#' @param countries country or countries to limit results to, if `NULL` returns
#' all countries
#' @param level level(s) of administrative units 0:4 to extract; note level 4 is
#' not available for all countries
#' @param ... additional arguments passed to [sf::st_write()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gadm.extract("gadm34_levels.gpkg", "Nordics.gpkg",
#'              c("Denmark", "Finland", "Iceland", "Norway", "Sweden"),
#'              level = 0:4)
#' }

gadm.extract <- function(input, output, countries = NULL, level = 0:4, ...) {

  ## add file extension to output if missing
  if (!grepl('^.*\\.gpkg$', output)) output <- paste0(output, '.gpkg')

  if (is.null(countries)) {

    for (i in level) {

      ## extract level(s) for all countries
      st_write(st_read(input, layer = paste0('level', i)),
               output, layer = paste0('level', i), ...)

    }

  } else {

    for (i in level) {

      ## extract level(s) for specified countries
      st_write(st_read(input, layer = paste0('level', i),
                       query = paste0('SELECT * FROM ',
                                      paste0('level', i),
                                      ' WHERE NAME_0 IN ("',
                                      paste0(countries, collapse = '", "'),
                                      '")')),
               output, layer = paste0('level', i), ...)

    }

  }

}
