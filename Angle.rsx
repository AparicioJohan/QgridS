##Resizing=group
##Angle from two points (degrees)=name
##QgsProcessingParameterPoint|FIRST_POINT|First point|None|False
##QgsProcessingParameterPoint|SECOND_POINT|Second point|None|False
##QgsProcessingParameterBoolean|BEARING|Return bearing (0°=N, clockwise)|True
##OUTPUT_ANGLE=output number
##output_shapefile=output vector

library(sf)

# --- helper: compute angle ---
compute_angle <- function(x1, y1, x2, y2, bearing = TRUE) {
  dx <- x2 - x1
  dy <- y2 - y1

  # Math angle: 0°=East, 90°=North, CCW positive (range -180..180)
  ang_math <- atan2(dy, dx) * 180 / pi

  # Bearing: 0°=North, 90°=East, clockwise (range 0..360)
  ang_bear <- (atan2(dx, dy) * 180 / pi + 360) %% 360

  if (BEARING) ang_bear else ang_math
}

# --- get points depending on mode ---

if (is.null(FIRST_POINT) || is.null(SECOND_POINT)) {
  stop("You selected 'two_point_coordinates' but FIRST_POINT or SECOND_POINT is missing.")
}

# In Processing R provider, point params usually arrive as sfc POINT already
p1 <- FIRST_POINT
p2 <- SECOND_POINT

c1 <- st_coordinates(p1)[1, ]
c2 <- st_coordinates(p2)[1, ]

x1 <- c1["X"]; y1 <- c1["Y"]
x2 <- c2["X"]; y2 <- c2["Y"]

# --- outputs ---
angle_deg <- compute_angle(x1, y1, x2, y2, bearing = BEARING)
OUTPUT_ANGLE <- angle_deg

pts <- st_as_sf(data.frame(id = c("first","second")),
                geometry = st_sfc(st_geometry(p1)[[1]], st_geometry(p2)[[1]]))

output_shapefile = pts

print("-----------Output Angle ---------")
print(angle_deg)
print("---------------------------------")