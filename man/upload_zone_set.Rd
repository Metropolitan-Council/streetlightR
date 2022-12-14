% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upload_zone_set.R
\name{upload_zone_set}
\alias{upload_zone_set}
\title{Upload a polygon or line zone set}
\usage{
upload_zone_set(
  login_email,
  key = NULL,
  geom_type = "polygon",
  zones,
  zone_set_name,
  zones_ = NULL,
  zone_set_name_ = NULL
)
}
\arguments{
\item{login_email}{character, your StreetLight login email}

\item{key}{character, StreetLight API key. Default is \code{NULL}.}

\item{geom_type}{character, one of \code{"polygon"} or \code{"line"}}

\item{zones}{GeoJSON feature collection \strong{or} sf object
where the features are either lines or polygons and columns include
"name" and "geometry".}

\item{zone_set_name}{character, zone set name}

\item{zones_}{Deprecated. Use \code{zones}}

\item{zone_set_name_}{Deprecated. Use \code{zone_set_name}}
}
\value{
If successful, a list with the zone name, status, and
universally unique ID (uuid).
}
\description{
Upload a polygon or line zone set
}
\examples{
\dontrun{
library(sf)
library(streetlightR)
library(osmdata)

# create and upload an example polygon
example_polygon <- sf::st_sfc(
  sf::st_point(cbind(-93.09, 44.95)),
  crs = 4326
) \%>\%
  sf::st_buffer(100)

upload_zone_set(
  login_email = "you@mail.com",
  geom_type = "polygon",
  zones = example_polygon,
  zone_set_name = paste0("example_polygon_", Sys.time())
)

# create and upload an example polyline
assign(
  "has_internet_via_proxy",
  TRUE,
  environment(curl::has_internet)
)

example_line <- osmdata::opq_osm_id(type = "way", id = 18278450) \%>\%
  osmdata::opq_string() \%>\%
  osmdata::osmdata_sf() \%>\%
  .$osm_lines

upload_zone_set(
  login_email = "you@mail.com",
  geom_type = "line",
  zones = example_line,
  zone_set_name = paste0("example_polyline_", Sys.time())
)
}
}
