# rotate maps

rotate_coords <- function(x, y, rotation_angle, rotation_center) {
  assertthat::assert_that(identical(class(rotation_center), "numeric"))
  assertthat::assert_that(identical(class(rotation_angle), "numeric"))
  assertthat::assert_that(identical(length(rotation_center), 2L))
  assertthat::assert_that(identical(length(rotation_angle), 1L))
  assertthat::assert_that(identical(length(x), length(y)))

  rot <- -rotation_angle * pi / 180
  newangles <- atan2(y - rotation_center[2], x - rotation_center[1]) + rot
  mags <- sqrt((x - rotation_center[1])^2 + (y - rotation_center[2])^2)
  x <- rotation_center[1] + cos(newangles) * mags
  y <- rotation_center[2] + sin(newangles) * mags
  dplyr::tibble(x = x, y = y)
}

splitrotatepolygon <- function(map, rotate, c1, c2) {
  require(dplyr)
  require(sf)
  onefeature <- (map)[i, ]
  pts2 <-
    onefeature |>
    st_cast("MULTILINESTRING") |>
    st_cast("MULTIPOINT")
  pts3 <- pts2 |>
    reframe(
      lon = unlist(st_coordinates(pts2$geometry)[, "X"]),
      lat = unlist(st_coordinates(pts2$geometry)[, "Y"])
    )
  rotate_coast <- rotate_coords(pts3$lon, pts3$lat, rotate, c(c1, c2)) |>
    cbind(pts3)
  rotate_coast2 <- rotate_coast |>
    arrange("x", "y") |>
    st_as_sf(coords = c("x", "y")) |>
    summarise(geometry = st_combine(geometry)) |>
    st_cast("MULTIPOINT") |>
    st_cast("MULTILINESTRING") |>
    st_cast("MULTIPOLYGON")
  return(rotate_coast2)
}
