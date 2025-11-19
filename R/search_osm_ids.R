#' @title Search OSM IDs
#'
#' @description
#' Using a polygon, census tract, census blockgroup, zip code, or point, 
#'   search StreetLight's OSM segment database.
#'
#' @param polygon sf object, POLYGON or MULTIPOLYGON 
#' @param tract_id character, 11 digit census tract identifier
#' @param zip_id character, 5-digit ZIP code
#' @param blockgroup_id character, 12 digit block group identifier
#' @param radius numeric, buffer radius around point
#' @param point sf POINT object or list coordinate pair in Longitude, Latitude order.
#' @param unit, buffer distance unit of measurement.
#'   One of `"mi"` or `"km"`. Default is `"mi"`
#'
#' @inheritParams check_streetlight_api
#' @inheritParams create_streetlight_analysis
#' @return If successful, a list with the API response and data frame of intersecting OSM segment IDs.
#'    Columns include `"osm_id"`, `"road_classification"`, and `"osm_segment_count"`.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(streetlightR)
#'
#' zip_response <- search_osm_ids(zip_id = "55104")
#' }
search_osm_ids <- function(key = NULL,
                           polygon = NULL,
                           tract_id = NULL,
                           zip_id = NULL,
                           blockgroup_id = NULL,
                           point = NULL,
                           radius = NULL,
                           unit = "mi") {
  key <- check_api_key_access(key)

  purrr::map2(
    names(as.list(match.call())),
    eval(as.list(match.call())),
    validate_parameters
  )


  if (!is.null(polygon)) {
    if (sf::st_crs(polygon)[[2]] != "+proj=longlat +datum=WGS84 +no_defs") {
      polygon <- sf::st_transform(polygon, crs = "+proj=longlat +datum=WGS84 +no_defs")
    }

    polygon_coordinates <- sf::st_coordinates(polygon) %>%
      subset(select = -c(L1, L2))

    poly_list <- list(
      "geometry" = list(
        "polygon" = list(
          "coordinates" =
            list(
              unlist(polygon_coordinates)
            ),
          "type" = "polygon"
        )
      )
    )
  } else if (!is.null(point)) {
    if (class(point)[[1]] %in% c("sf", "sfc", "sfc_POINT")) {
      point_coordinates <- sf::st_coordinates(point) %>%
        list() %>%
        unlist()
    } else {
      point_coordinates <- point
    }

    poly_list <- list(
      "geometry" = list(
        "radius" = list(
          "point" = list(
            "type" = "point",
            "coordinates" = point_coordinates
          ),
          "buffer" = radius,
          "unit" = unit
        )
      )
    )
  } else if (!is.null(tract_id)) {
    poly_list <- list(
      "geometry" = list(
        tract_id = tract_id
      )
    )
  } else if (!is.null(zip_id)) {
    poly_list <- list(
      "geometry" = list(
        zip_id = zip_id
      )
    )
  } else if (!is.null(blockgroup_id)) {
    poly_list <- list(
      "geometry" = list(
        blockgroup_id = blockgroup_id
      )
    )
  }

  resp <- streetlight_insight(
    key = key,
    endpoint = "osm_ids/search"
  ) %>%
    httr2::req_headers(
      "content-type" = "application/json"
    ) %>%
    httr2::req_body_json(
      poly_list,
      auto_unbox = TRUE
    ) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform()




  resp_content <- httr2::resp_body_json(resp, check_type = FALSE, simplifyVector = TRUE)

  data_resp <- resp_content$data %>% as.data.frame()

  names(data_resp) <- resp_content$columns


  return(
    list(
      "response" = resp_content,
      "data" = data_resp
    )
  )
}
