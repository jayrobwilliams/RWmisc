% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/point.poly.dist.R
\name{point.poly.dist}
\alias{point.poly.dist}
\title{Point-Polygon Distances}
\usage{
point.poly.dist(point, poly, max = TRUE, by_element = FALSE)
}
\arguments{
\item{point}{A simplefeatures object of class point.}

\item{poly}{A simplefeatures object of class polygon or multipolygon.}

\item{max}{Logical; return maximum or minimum distance? default \code{TRUE}}

\item{by_element}{Logical; return total maximum or minimum, or for each input
point? default \code{FALSE}}
}
\value{
Maximum or minimum distance between a point and a polygon.
}
\description{
Calculate the maximum or minimum possible distance from a point to the edge
of a given polygon.
}
\examples{
library(sf)
polys <- st_sfc(st_polygon(list(rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0)))),
crs = st_crs('OGC:CRS84'))
points <- st_sfc(st_multipoint(rbind(c(.25, .5), c(.75, .5), c(.5, .5))),
                 crs = st_crs('OGC:CRS84'))
point.poly.dist(points, polys)
}
