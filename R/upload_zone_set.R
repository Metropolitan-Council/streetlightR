#' @title Upload a polygon or line zone set
#'
#' @inheritParams check_streetlight_api
#' @param login_email character, your StreetLight login email
#' @param geom_type character, one of `"polygon"` or `"line"`
#' @param zones GeoJSON feature collection __or__ sf object
#'     where the features are either lines or polygons and columns include
#'     "name" and "geometry".
#' @param zone_set_name character, zone set name
#' @param zone_set_name_ Deprecated. Use `zone_set_name`
#' @param zones_ Deprecated. Use `zones`
#'
#' @return If successful, a list with the zone name, status, and
#'     universally unique ID (uuid).
#' @export
#'
#' @importFrom httr2 req_body_json resp_body_json resp_status_desc req_error req_perform req_headers
#' @importFrom sf st_crs st_transform st_as_sf st_cast
#' @importFrom cli cli_warn
#' @importFrom purrr map2
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(streetlightR)
#' library(osmdata)
#'
#' # create and upload an example polygon
#' example_polygon <- sf::st_sfc(
#'   sf::st_point(cbind(-93.09, 44.95)),
#'   crs = 4326
#' ) %>%
#'   sf::st_buffer(100)
#'
#' upload_zone_set(
#'   login_email = "you@mail.com",
#'   geom_type = "polygon",
#'   zones = example_polygon,
#'   zone_set_name = paste0("example_polygon_", Sys.time())
#' )
#'
#' # create and upload an example polyline
#' assign(
#'   "has_internet_via_proxy",
#'   TRUE,
#'   environment(curl::has_internet)
#' )
#'
#' example_line <- osmdata::opq_osm_id(type = "way", id = 18278450) %>%
#'   osmdata::opq_string() %>%
#'   osmdata::osmdata_sf() %>%
#'   .$osm_lines
#'
#' upload_zone_set(
#'   login_email = "you@mail.com",
#'   geom_type = "line",
#'   zones = example_line,
#'   zone_set_name = paste0("example_polyline_", Sys.time())
#' )
#' }
upload_zone_set <- function(login_email,
                            key = NULL,
                            geom_type = "polygon",
                            zones,
                            zone_set_name,
                            zones_ = NULL,
                            zone_set_name_ = NULL) {
  # check for API key access
  key <- check_api_key_access(key)
  # validate parameters
  purrr::map2(
    names(as.list(match.call())),
    eval(as.list(match.call())),
    validate_parameters
  )

  # warning if using  zone_set_name_
  if (!is.null(zone_set_name_)) {
    cli::cli_warn(c("`zone_set_name_` deprecated. Use 'zone_set_name' instead."))
    zone_set_name <- zone_set_name_
  }

  # warning if using  zones_
  if (!is.null(zones_)) {
    cli::cli_warn(c("`zones_` deprecated. Use 'zones' instead."))
    zones <- zones_
  }


  if (class(zones)[[1]] == "sf" | class(zones)[[2]] == "sfc") {
    if (class(zones)[[2]] == "sfc") {
      # create sf object from sf collection
      zones_sf <- sf::st_as_sf(zones)
    } else {
      zones_sf <- zones
    }

    # check that there aren't too many zones
    # check_zone_size(zones = zones_sf)
    if (nrow(zones_sf) >= 7000) {
      stop(("There are too many zones in this zone set."))
      return()
    }

    # if the coordinate reference system is not WGS84, transform
    if (sf::st_crs(zones_sf)[[2]] != "+proj=longlat +datum=WGS84 +no_defs") {
      zones_sf <- sf::st_transform(zones_sf, crs = "+proj=longlat +datum=WGS84 +no_defs")
    }

    # if geom_type = polygon and zones are not polygons, cast to MULTIPOLYGON
    if (geom_type == "polygon" & class(zones_sf$geometry)[[1]] != "sfc_MULTIPOLYGON") {
      zones_sf <- sf::st_cast(zones_sf, to = "MULTIPOLYGON")
    }

    # if geom_type = line and zones are not lines, case to MUTLILINESTRING
    if (geom_type == "line" & class(zones_sf$geometry)[[1]] != "sfc_MULTILINESTRING") {
      zones_sf <- sf::st_cast(zones_sf, to = "MULTILINESTRING")
    }


    # if the zone names are not unique, re-name with a numeric suffix
    if (length(unique(zones_sf$name)) != nrow(zones_sf)) {
      zones_sf$name <- paste0(zones_sf$name, "_", 1:nrow(zones_sf))
    }

    # convert to geojson
    zones_json <- geojson_list(zones_sf)
  } else if (class(zones)[[1]] == "list") {
    # if already json, return
    zones_json <- zones
  }

  # create zone set info list
  zone_list <- list(
    insight_login_email = login_email,
    geom_type = geom_type,
    zone_set_name = zone_set_name,
    zones = zones_json
  )

  # upload zone set using endpoint
  resp <- streetlight_insight(
    key = key,
    endpoint = "zone_sets"
  ) %>%
    httr2::req_headers(
      "content-type" = "application/json"
    ) %>%
    httr2::req_body_json(zone_list, auto_unbox = TRUE, force = TRUE) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()


  # if status message was not "Created"
  if (httr2::resp_status_desc(resp) != "Created") {
    return(
      # return warning with json response
      cli::cli_warn(c(
        "Zone upload failed with message:",
        httr2::resp_body_json(resp, simplifyVector = TRUE)
      ))
    )
  }

  # if status message is "Created"
  if (httr2::resp_status_desc(resp) == "Created") {
    return(
      # return success message
      cli::cli_alert_success(c("Zone set '{zone_set_name}' uploaded"))
    )
  }
}
