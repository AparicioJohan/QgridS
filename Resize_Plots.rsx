##Resizing=group
##Resize plots by bbox + rotation=name
##PLOTS=vector
##QgsProcessingParameterRasterLayer|MOSAIC|Reference raster for CRS (optional)|None|True
##ANGLE=number 0
##XSIZE=number 0.8
##YSIZE=number 4
##output_shapefile=output vector

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
})

# --- helpers (yours, lightly cleaned) ----
rect_funct <- function(x, xsize = 0.85, ysize = 4.5) {
  bbox <- st_bbox(x)
  
  ys <- c(ysize, bbox["ymax"] + bbox["ymin"])
  ys <- solve(matrix(c(1, 1, -1, 1), ncol = 2)) %*% ys
  
  xs <- c(xsize, bbox["xmax"] + bbox["xmin"])
  xs <- solve(matrix(c(1, 1, -1, 1), ncol = 2)) %*% xs
  
  bbox["xmax"] <- xs[1]
  bbox["xmin"] <- xs[2]
  bbox["ymax"] <- ys[1]
  bbox["ymin"] <- ys[2]
  
  st_as_sfc(st_bbox(bbox))
}

resize_sf <- function(plot_shape, mosaic = NULL, angle = 0, xsize = 0.8, ysize = 4) {
  
  # 1) build rectangles centered on each feature bbox
  cen <- st_geometry(plot_shape)
  bbox_list   <- lapply(cen, st_bbox)
  points_list <- lapply(bbox_list, st_as_sfc)
  boxes       <- lapply(points_list, \(pt) rect_funct(pt, xsize, ysize))
  
  rects <- do.call(c, boxes)
  st_crs(rects) <- st_crs(cen)
  
  grid <- st_as_sf(rects)
  
  # optionally force CRS to match raster
  if (!is.null(mosaic)) st_crs(grid) <- st_crs(mosaic)
  
  # 2) rotate each rectangle around its own centroid
  a <- angle * pi / 180
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  
  geom <- st_geometry(grid)
  ctr  <- st_centroid(geom)
  geom_rot <- (geom - ctr) * rot(a) + ctr
  
  st_crs(geom_rot) <- st_crs(grid)
  grid_rot <- st_as_sf(geom_rot)
  
  if (!is.null(mosaic)) st_crs(grid_rot) <- st_crs(mosaic)
  
  # 3) replace geometry in original layer
  out <- plot_shape
  st_geometry(out) <- st_geometry(grid_rot)
  
  out
}

# --- QGIS inputs arrive as variables: PLOTS, MOSAIC, ANGLE, XSIZE, YSIZE, OUTPUT ----

# Read vector
plots <- PLOTS   # already sf

# Optional raster CRS reference
mos <- NULL
if (exists("MOSAIC") && !is.null(MOSAIC) && !is.na(MOSAIC) && nzchar(MOSAIC) && MOSAIC != "None") {
  mos <- terra::rast(MOSAIC)
}

# Run
res <- resize_sf(
  plot_shape = plots,
  mosaic     = mos,
  angle      = ANGLE,
  xsize      = XSIZE,
  ysize      = YSIZE
)

output_shapefile = res
