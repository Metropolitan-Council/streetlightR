#' @title Convert many input types with spatial data to geojson specified as a list
#'
#' @author Scott Chamberlain <myrmecocystus@gmail.com>,
#'    Andy Teucher <andy.teucher@gmail.com>, and
#'    Michael Mahoney <mike.mahoney.218@gmail.com> (<https://orcid.org/0000-0003-2402-304X>)
#'
#'
#' @details This set of functions is adapted from
#'   [geojsonio](https://github.com/ropensci/geojsonio/releases/tag/v0.11.1),
#'   a larger package for converting GeoJSON formats.
#'
#' This function creates a geojson structure as an R list.
#'
#' For sf classes (sf, sfc, sfg), the following conversions are made:
#'
#' - sfg: the appropriate geometry `Point, LineString, Polygon, MultiPoint,
#'  MultiLineString, MultiPolygon, GeometryCollection`
#' - sfc: `GeometryCollection`, unless the sfc is length 1, then the geometry
#' as above
#' - sf: `FeatureCollection`
#'
#' @export
#'
#' @param input Input sf object.
#' @param geometry character, One of "point" (Default) or "polygon."
#' @param type character, The type of collection. One of "FeatureCollection"
#' (default) or "GeometryCollection".
#' @param lat character, Latitude name. The default is `NULL`, and we
#' attempt to guess.
#' @param lon character, Longitude name. The default is `NULL`, and we
#' attempt to guess.
#' @param group character, A grouping variable to perform grouping for
#' polygons - doesn't apply for points
#' @param convert_wgs84 logical, Should the input be converted to the
#' standard CRS for GeoJSON (https://tools.ietf.org/html/rfc7946)
#' (geographic coordinate reference system, using the WGS84 datum, with
#' longitude and latitude units of decimal degrees; EPSG: 4326).
#' Default is `FALSE` though this may change in a future package version.
#' This will only work for `sf` or `Spatial` objects with a CRS
#' already defined. If one is not defined but you know what it is, you
#' may define it in the `crs` argument below.
#' @param crs numeric, The CRS of the input if it is not already defined. This can
#' be an epsg code as a four or five digit integer or a valid proj4 string.
#' This argument will be ignored if `convert_wgs84` is `FALSE`
#' or the object already has a CRS.
#' @param ... Ignored
#'
#' @examples \dontrun{
#' # From sf classes:
#' if (require(sf)) {
#'   ## sfg (a single simple features geometry)
#'   p1 <- rbind(c(0, 0), c(1, 0), c(3, 2), c(2, 4), c(1, 4), c(0, 0))
#'   poly <- rbind(c(1, 1), c(1, 2), c(2, 2), c(1, 1))
#'   poly_sfg <- st_polygon(list(p1))
#'   geojson_list(poly_sfg)
#'
#'   ## sfc (a collection of geometries)
#'   p1 <- rbind(c(0, 0), c(1, 0), c(3, 2), c(2, 4), c(1, 4), c(0, 0))
#'   p2 <- rbind(c(5, 5), c(5, 6), c(4, 5), c(5, 5))
#'   poly_sfc <- st_sfc(st_polygon(list(p1)), st_polygon(list(p2)))
#'   geojson_list(poly_sfc)
#'
#'   ## sf (collection of geometries with attributes)
#'   p1 <- rbind(c(0, 0), c(1, 0), c(3, 2), c(2, 4), c(1, 4), c(0, 0))
#'   p2 <- rbind(c(5, 5), c(5, 6), c(4, 5), c(5, 5))
#'   poly_sfc <- st_sfc(st_polygon(list(p1)), st_polygon(list(p2)))
#'   poly_sf <- st_sf(foo = c("a", "b"), bar = 1:2, poly_sfc)
#'   geojson_list(poly_sf)
#' }
#' }
#'
geojson_list <- function(input, lat = NULL, lon = NULL, group = NULL,
                         geometry = "point", type = "FeatureCollection",
                         convert_wgs84 = FALSE, crs = NULL, ...) {
  UseMethod("geojson_list")
}

# sf classes ---------------------------------

#' @export
geojson_list.sf <- function(input, lat = NULL, lon = NULL, group = NULL,
                            geometry = "point", type = "FeatureCollection",
                            convert_wgs84 = FALSE, crs = NULL, ...) {
  if (convert_wgs84) {
    input <- convert_wgs84(input, crs)
  }

  sf_col <- get_sf_column_name(input)
  ## Get the sfc column
  sfc <- unclass(input[[sf_col]])
  ## remove the sf class so can extract the attributes using `[`
  attr_df <- as.data.frame(input)[, setdiff(names(input), sf_col),
    drop = FALSE
  ]

  type <- "FeatureCollection"
  features <- lapply(
    seq_len(nrow(input)),
    function(i) {
      list(
        type = "Feature",
        properties = as.list(attr_df[i, , drop = FALSE]),
        geometry = unclass(geojson_list(sfc[[i]]))
      )
    }
  )

  out <- list(type = type, features = features)

  as.geo_list(tg_compact(out), from = "sf")
}

#' @export
geojson_list.sfc <- function(input, lat = NULL, lon = NULL, group = NULL,
                             geometry = "point", type = "FeatureCollection", convert_wgs84 = FALSE,
                             crs = NULL, ...) {
  ## Remove names of input otherwise produces invalid geojson
  names(input) <- NULL

  if (convert_wgs84) {
    input <- convert_wgs84(input, crs)
  }
  ## A GeometryCollection except if length 1, then just return the geometry

  if (length(input) == 1) {
    return(geojson_list(input[[1]]))
  } else {
    out <- list(
      type = "GeometryCollection",
      geometries = lapply(input, function(x) unclass(geojson_list(x)))
    )
  }
  as.geo_list(out, from = "sfc")
}

#' @export
geojson_list.sfg <- function(input, lat = NULL, lon = NULL, group = NULL,
                             geometry = "point", type = "FeatureCollection",
                             convert_wgs84 = FALSE, crs = NULL, ...) {
  type <- switch_geom_type(get_geometry_type(input))

  if (type == "GeometryCollection") {
    geometries <- lapply(input, function(x) unclass(geojson_list(x)))
    out <- list(type = type, geometries = geometries)
  } else {
    coordinates <- make_coords(input)
    out <- list(type = type, coordinates = coordinates)
  }
  as.geo_list(out, from = "sfg")
}

switch_geom_type <- function(x) {
  switch(x,
    "POINT" = "Point",
    "LINESTRING" = "LineString",
    "POLYGON" = "Polygon",
    "MULTIPOINT" = "MultiPoint",
    "MULTILINESTRING" = "MultiLineString",
    "MULTIPOLYGON" = "MultiPolygon",
    "GEOMETRY" = "GeometryCollection",
    "GEOMETRYCOLLECTION" = "GeometryCollection"
  )
}

get_sf_column_name <- function(x) attr(x, "sf_column")

## Get the geometry type
get_geometry_type <- function(x) UseMethod("get_geometry_type")
get_geometry_type.sfc <- function(x) strsplit(class(x)[1], "_")[[1]][2]
get_geometry_type.sfg <- function(x) class(x)[2]

## Make coordinates, dropping M dimension if it's there
make_coords <- function(input) {
  dim <- class(input)[1]
  m_loc <- regexpr("M", dim)

  if (m_loc > 0) {
    message("removing M dimension as not supported in GeoJSON format")
    return(drop_m(unclass(input), m_loc))
  }

  unclass(input)
}

drop_m <- function(input, m_loc) UseMethod("drop_m")
drop_m.list <- function(input, m_loc) lapply(input, drop_m, m_loc = m_loc)
drop_m.numeric <- function(input, m_loc) input[-m_loc]
drop_m.matrix <- function(input, m_loc) input[, -m_loc, drop = FALSE]

# regular R classes --------------------------
#' @export
geojson_list.numeric <- function(input, lat = NULL, lon = NULL, group = NULL,
                                 geometry = "point", type = "FeatureCollection", ...) {
  as.geo_list(num_to_geo_list(input, geometry, type), "numeric")
}

#' @export
geojson_list.data.frame <- function(input, lat = NULL, lon = NULL, group = NULL,
                                    geometry = "point", type = "FeatureCollection", ...) {
  tmp <- guess_latlon(names(input), lat, lon)
  as.geo_list(df_to_geo_list(
    x = input, lat = tmp$lat, lon = tmp$lon,
    geometry = geometry, type = type, group = group
  ), "data.frame")
}

#' @export
geojson_list.list <- function(input, lat = NULL, lon = NULL, group = NULL,
                              geometry = "point", type = "FeatureCollection", ...) {
  if (geometry == "polygon") lint_polygon_list(input)
  tmp <- if (!is.named(input)) {
    list(lon = NULL, lat = NULL)
  } else {
    guess_latlon(names(input[[1]]), lat, lon)
  }
  as.geo_list(list_to_geo_list(input,
    lat = tmp$lat, lon = tmp$lon,
    geometry, type, !is.named(input), group
  ), "list")
}

#' @export
geojson_list.geo_list <- function(input, lat = NULL, lon = NULL, group = NULL,
                                  geometry = "point", type = "FeatureCollection", ...) {
  return(input)
}

#' @export
geojson_list.json <- function(input, lat = NULL, lon = NULL, group = NULL,
                              geometry = "point", type = "FeatureCollection", ...) {
  output_list <- jsonlite::fromJSON(input, FALSE, ...)
  as.geo_list(output_list, from = "json")
}

as.geo_list <- function(x, from) structure(x, class = "geo_list", from = from)


lint_polygon_list <- function(x) {
  if (!identical(x[[1]], x[[length(x)]])) {
    stop("First and last point in a polygon must be identical",
      call. = FALSE
    )
  }
}

# helper functions -----
tg_compact <- function(l) Filter(Negate(is.null), l)

json_val_safe <- function(x) {
  tmp <- tryCatch(jsonlite::validate(x), error = function(e) e)
  if (inherits(tmp, "error")) FALSE else tmp
}

to_json <- function(x, precision = 7, ...) {
  if (is.character(x) && json_val_safe(x)) {
    return(structure(x, class = "json"))
  }
  if (is.null(precision)) precision <- 7
  structure(jsonlite::toJSON(x, ...,
    digits = precision, auto_unbox = TRUE,
    force = TRUE
  ), class = c("json", "geo_json"))
}

class_json <- function(x, ..., type = "FeatureCollection") {
  structure(x, class = c("json", "geo_json"))
}


list_to_geo_list <- function(x, lat, lon, geometry = "point", type = "FeatureCollection", unnamed = FALSE, group = NULL) {
  nn <- switch(type,
    FeatureCollection = "features",
    GeometryCollection = "geometries"
  )
  geom <- capwords(match.arg(geometry, c("point", "polygon")))
  if (geom == "Point") {
    z <- lapply(x, function(l) {
      if (!unnamed) {
        if (is.null(l[[lat]]) || is.null(l[[lon]])) {
          return(NULL)
        }
      }
      if (nn == "features") {
        list(
          type = "Feature",
          geometry = list(
            type = geom,
            coordinates = get_vals(l, lat, lon)
          ),
          properties = l[!(names(l) %in% c(lat, lon))]
        )
      } else {
        list(
          type = geom,
          coordinates = get_vals(l, lat, lon)
        )
      }
    })
    z <- stats::setNames(Filter(function(x) !is.null(x), z), NULL)
    structure(list(type, z), .Names = c("type", nn))
  } else {
    if (!unnamed) {
      if (is.null(x[[lat]]) || is.null(x[[lon]])) {
        return(NULL)
      }
    }
    if (nn == "features") {
      if (is.null(group)) {
        z <- list(list(
          type = "Feature",
          geometry = list(type = geom, coordinates = get_vals2(x, unnamed, lat, lon)),
          properties = get_props(x, lat, lon)
        ))
      } else {
        grps <- unique(pluck(x, group, ""))
        z <- lapply(grps, function(w) {
          use <- Filter(function(m) m$group == w, x)
          list(
            type = "Feature",
            geometry = list(type = geom, coordinates = list(unname(get_vals2(use, FALSE, lat, lon)))),
            properties = get_props(use[[1]], lat, lon)
          )
        })
      }
    } else {
      z <- list(type = geom, coordinates = get_vals2(x, unnamed, lat, lon))
    }
    structure(list(type, z), .Names = c("type", nn))
  }
}

get_props <- function(x, lat, lon) {
  x[!(names(x) %in% c(lat, lon))]
}

get_vals2 <- function(v, unnamed, lat, lon) {
  if (unnamed) {
    list(v)
  } else {
    unname(lapply(v, function(g) as.numeric(gsub("^\\s+|\\s+$", "", unlist(unname(g[names(g) %in% c(lat, lon)]))))))
  }
}

get_vals <- function(v, lat, lon) {
  tt <- tryCatch(v[[lon]], error = function(e) e)
  if (inherits(tt, "simpleError")) {
    as.numeric(v)
  } else {
    as.numeric(c(v[[lon]], v[[lat]]))
  }
}

df_to_geo_list <- function(x, lat, lon, geometry, type, group, ...) {
  x <- apply(x, 1, as.list)
  list_to_geo_list(
    x = x, lat = lat, lon = lon, geometry = geometry,
    type = type, unnamed = TRUE, group = group, ...
  )
}

num_to_geo_list <- function(x, geometry = "point", type = "FeatureCollection") {
  geom <- capwords(match.arg(geometry, c("point", "polygon")))
  res <- tryCatch(as.numeric(x), warning = function(e) e)
  if (inherits(res, "simpleWarning")) {
    stop("Coordinates are not numeric", call. = FALSE)
  } else {
    switch(type,
      FeatureCollection = {
        list(
          type = "FeatureCollection",
          features = list(
            list(
              type = "Feature",
              geometry = list(type = geom, coordinates = makecoords(x, geom)),
              properties = NULL
            )
          )
        )
      },
      GeometryCollection = {
        list(
          type = "GeometryCollection",
          geometries = list(
            list(type = geom, coordinates = makecoords(x, geom))
          )
        )
      }
    )
  }
}

makecoords <- function(x, y) {
  switch(y,
    Point = x,
    Polygon = list(unname(split(x, ceiling(seq_along(x) / 2))))
  )
}





bbox2df <- function(x) c(x[1, 1], x[1, 2], x[2, 1], x[2, 2])

sppolytogeolist <- function(x) {
  list(
    type = "Polygon",
    bbox = bbox2df(x@bbox),
    coordinates =
      lapply(x@polygons, function(l) {
        apply(l@Polygons[[1]]@coords, 1, as.list)
      }),
    properties = NULL
  )
}

lines_to_geo_list <- function(x, object = "FeatureCollection") {
  nn <- switch(object,
    FeatureCollection = "features",
    GeometryCollection = "geometries"
  )
  if (length(x@lines) == 1) {
    list(
      type = "LineString",
      bbox = bbox2df(x@bbox),
      coordinates = apply(x@lines[[1]]@Lines[[1]]@coords, 1, as.list),
      properties = NULL
    )
  } else {
    z <- lapply(x@lines, function(l) {
      if (nn == "features") {
        list(
          type = "Feature",
          bbox = bbox2df(x@bbox),
          geometry = list(
            type = ifelse(length(l@Lines) == 1, "LineString", "MultiLineString"),
            coordinates =
              if (length(l@Lines) == 1) {
                apply(l@Lines[[1]]@coords, 1, as.list)
              } else {
                lapply(l@Lines, function(w) {
                  apply(w@coords, 1, as.list)
                })
              }
          ),
          properties = datdat(x, l)
        )
      } else {
        list(
          type = "LineString",
          bbox = bbox2df(x@bbox),
          coordinates = l,
          properties = datdat(x, l)
        )
      }
    })
    z <- stats::setNames(Filter(function(x) !is.null(x), z), NULL)
    structure(list(object, z), .Names = c("type", nn))
  }
}

datdat <- function(x, l) {
  tmp <- data.frame(x@data)[row.names(data.frame(x@data)) == l@ID, ]
  lapply(as.list(tmp), as.character)
}

splinestogeolist <- function(x, object) {
  if (inherits(x, "SpatialLinesDataFrame")) {
    lines_to_geo_list(x, object)
  } else {
    if (length(x@lines) == 1) {
      list(
        type = "LineString",
        bbox = bbox2df(x@bbox),
        coordinates = apply(x@lines[[1]]@Lines[[1]]@coords, 1, as.list),
        properties = NULL
      )
    } else {
      list(
        type = "MultiLineString",
        bbox = bbox2df(x@bbox),
        coordinates =
          lapply(x@lines, function(l) {
            apply(l@Lines[[1]]@coords, 1, as.list)
          }),
        properties = NULL
      )
    }
  }
}



capwords <- function(s, strict = FALSE, onlyfirst = FALSE) {
  cap <- function(s) {
    paste(toupper(substring(s, 1, 1)),
      {
        s <- substring(s, 2)
        if (strict) tolower(s) else s
      },
      sep = "",
      collapse = " "
    )
  }
  if (!onlyfirst) {
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  } else {
    sapply(s, function(x) {
      paste(toupper(substring(x, 1, 1)),
        tolower(substring(x, 2)),
        sep = "", collapse = " "
      )
    }, USE.NAMES = FALSE)
  }
}


pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}

###### code adapted from the leaflet package - source at github.com/rstudio/leaflet
guess_latlon <- function(x, lat = NULL, lon = NULL) {
  if (is.null(lat) && is.null(lon)) {
    lats <- x[grep("^(lat|latitude)$", x, ignore.case = TRUE)]
    lngs <- x[grep("^(lon|lng|long|longitude)$", x, ignore.case = TRUE)]

    if (length(lats) == 1 && length(lngs) == 1) {
      if (length(x) > 2) {
        message(
          "Assuming '", lngs, "' and '", lats,
          "' are longitude and latitude, respectively"
        )
      }
      return(list(lon = lngs, lat = lats))
    } else {
      stop("Couldn't infer longitude/latitude columns, please specify with 'lat'/'lon' parameters", call. = FALSE)
    }
  } else {
    return(list(lon = lon, lat = lat))
  }
}

is.named <- function(x) {
  is.character(names(x[[1]]))
}
