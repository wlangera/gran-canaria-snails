#############################################################################
#############################################################################
## 3 functions to make grids on 1, 2, 5 and 10 km scales with MGRS         ##
## coordinates/grid references for each cell. The functions work for all   ##
## polygons in the northern hemisphere (not near the poles) which are      ##
## confined to a single UTM zone.                                          ##
#############################################################################
#############################################################################


#############################################################################
# tag_grid_mgrs() is a helper function to provide the MGRS coordinates/grid 
# references (further called 'tags') for each cell in a grid with precision 1 
# or 10 km.
#############################################################################

tag_grid_mgrs <- function(utm_centroids, size, utm_zone, mgrs_scheme = "AL") {
  # Error handling
  if (!(size %in% c(1, 2, 5, 10))) {
    stop("Grid size needs to be 1, 2 or 5 10 (kilometers)!")
  }
  
  if (!(mgrs_scheme %in% c("AA", "AL"))) {
    stop("Choose MGRS scheme AA or AL!")
  }
  
  utm_column <- as.numeric(sub("[A-Z]", "", utm_zone))
  utm_row <- sub("[0-9]*", "", utm_zone)
  possible_rows <- c("N", "P", "Q", "R", "S", "T", "U", "V", "W", "X")
  if (!(utm_column %in% 1:60) | !(utm_row %in% possible_rows)) {
    stop("UTM zone must exist in the northern hemisphere 
         (excluding rows Y and Z)!")
  }
  
  # Convert to kilometers and add grid zone designator (utm zone)
  tag_df <- utm_centroids %>%
    mutate(X = X %/% 1000,
           Y = Y %/% 1000,
           utm_zone = utm_zone)
  
  # Add precision on 100 km² (100,000-meter square identifier)
  ## Add column (changes every three UTM zones)
  if (utm_column %% 3 == 0) {
    tag_df <- tag_df %>%
      mutate(column_100km = case_when(
        between(X, 100, 200) ~ "S",
        between(X, 200, 300) ~ "T",
        between(X, 300, 400) ~ "U",
        between(X, 400, 500) ~ "V",
        between(X, 500, 600) ~ "W",
        between(X, 600, 700) ~ "X",
        between(X, 700, 800) ~ "Y",
        between(X, 800, 900) ~ "Z"
      ))
  } else if ((utm_column + 1) %% 3 == 0) {
    tag_df <- tag_df %>%
      mutate(column_100km = case_when(
        between(X, 100, 200) ~ "J",
        between(X, 200, 300) ~ "K",
        between(X, 300, 400) ~ "L",
        between(X, 400, 500) ~ "M",
        between(X, 500, 600) ~ "N",
        between(X, 600, 700) ~ "P",
        between(X, 700, 800) ~ "Q",
        between(X, 800, 900) ~ "R"
      ))
  } else {tag_df <- tag_df %>%
    mutate(column_100km = case_when(
      between(X, 100, 200) ~ "A",
      between(X, 200, 300) ~ "B",
      between(X, 300, 400) ~ "C",
      between(X, 400, 500) ~ "D",
      between(X, 500, 600) ~ "E",
      between(X, 600, 700) ~ "F",
      between(X, 700, 800) ~ "G",
      between(X, 800, 900) ~ "H"
    ))
  }
  
  ## Add row (max. northing value northern hemisphere: 9,300 km)
  zones <- rev(possible_rows)
  row_per_zone <- 9
  kilometers_per_row <- 100
  base_line <- 9300 - match(utm_row, zones) * row_per_zone * kilometers_per_row
  
  # There are 2 alternative lettering schemes: AA (MGRS-new) and AL (MGRS-old)
  if (mgrs_scheme == "AL") {
    tag_df <- tag_df %>%
      mutate(row_100km = case_when(
        between(Y, base_line, base_line + 100) ~ "R",
        between(Y, base_line + 100, base_line + 200) ~ "S",
        between(Y, base_line + 200, base_line + 300) ~ "T",
        between(Y, base_line + 300, base_line + 400) ~ "U",
        between(Y, base_line + 400, base_line + 500) ~ "V",
        between(Y, base_line + 500, base_line + 600) ~ "W",
        between(Y, base_line + 600, base_line + 700) ~ "X",
        between(Y, base_line + 700, base_line + 800) ~ "Y",
        between(Y, base_line + 800, base_line + 900) ~ "Z"
      ))
  } else {
    tag_df <- tag_df %>%
      mutate(row = case_when(
        between(Y, base_line, base_line + 100) ~ "F",
        between(Y, base_line + 100, base_line + 200) ~ "G",
        between(Y, base_line + 200, base_line + 300) ~ "H",
        between(Y, base_line + 300, base_line + 400) ~ "J",
        between(Y, base_line + 400, base_line + 500) ~ "K",
        between(Y, base_line + 500, base_line + 600) ~ "L",
        between(Y, base_line + 600, base_line + 700) ~ "M",
        between(Y, base_line + 700, base_line + 800) ~ "N",
        between(Y, base_line + 800, base_line + 900) ~ "P"
      ))
  }
  
  # Add precision on 10 km² (10,000-meter square identifier)
  columns <- c("A" = 100, "B" = 200, "C" = 300, "D" = 400, 
               "E" = 500, "F" = 600, "G" = 700, "H" = 800)
  rows <- seq_along(sort(unique(tag_df$row_100km))) - 1
  names(rows) <- sort(unique(tag_df$row_100km))
  tag_df <- tag_df %>%
    mutate(column_10km = substr(substr(as.character(X), 
                                       nchar(as.character(X)) - 1,
                                       nchar(as.character(X))), 1, 1),
           row_10km = substr(substr(as.character(Y), 
                                    nchar(as.character(Y)) - 1,
                                    nchar(as.character(Y))), 1, 1))
  
  ## Return if requested grid size was larger than 1
  if (size > 1) {
    tag_df <- tag_df %>%
      mutate(mgrs_tag = paste0(utm_zone, 
                               column_100km, row_100km, 
                               column_10km, row_10km))
    
    return(tag_df$mgrs_tag)
  }
  
  # Add precision 1 km²
  tag_df <- tag_df %>%
    mutate(column_1km = substr(substr(as.character(X), 
                                      nchar(as.character(X)),
                                      nchar(as.character(X))), 1, 1),
           row_1km = substr(substr(as.character(Y), 
                                   nchar(as.character(Y)),
                                   nchar(as.character(Y))), 1, 1),
           mgrs_tag = paste0(utm_zone, 
                             column_100km, row_100km, 
                             column_10km, column_1km, 
                             row_10km, row_1km))
  
  return(tag_df$mgrs_tag)
}


#############################################################################
# The helper function `make_grid_base()` creates a  1 or 10 km grid over the 
# polygon and uses `tag_grid_mgrs()` for to provide each cell with a unique 
# MGRS grid reference.
#############################################################################

make_grid_base <- function(size, map, utm_zone, mgrs_scheme = "AL") {
  # Error handling
  if (!(size %in% c(1, 10))) {
    stop("Grid size needs to be 1 or 10 (kilometers)!")
  }
  
  if (!("sf" %in% class(map))) {
    stop("Map needs to be of class sf!")
  }
  
  if (!(mgrs_scheme %in% c("AA", "AL"))) {
    stop("Choose MGRS scheme AA or AL!")
  }
  suppressWarnings({
    
    # Set offset so the grids are aligned with the crs
    ## Round minimum down to 10 kilometers
    offset <- st_bbox(map)[1:2] - st_bbox(map)[1:2] %% 10000
    names(offset) <- NULL
    
    # Create full grid over the span of the map
    ## Convert size to meters
    large_grid <- map %>%
      st_make_grid(cellsize = size * 1000, offset = offset) %>%
      st_as_sf() %>%
      mutate(id = row_number())
    
    # Get ids of grids that actually overlap with map
    intersect_ids <- large_grid %>%
      st_intersection(map) %>%
      pull(id)
    
    # Filter grids of large grid that overlap based intersecting ids
    small_grid <- large_grid %>% 
      filter(id %in% intersect_ids)
    
    # Calculate MGRS
    ## Calculate central UTM-coordinates of each grid
    utm_centroids <- small_grid %>%
      st_centroid() %>%
      st_coordinates() %>%
      as_tibble()
    
    ## Add tags to grid
    grid_final <- cbind(small_grid, 
                        tag = tag_grid_mgrs(utm_centroids, size, utm_zone,
                                            mgrs_scheme))
    
    # Return
    return(grid_final)
  })
}


#############################################################################
# The final function `make_grid_mgrs()` creates a 1, 2, 5 or 10 km grid over 
# the polygon. For 1 and 10 km it simply uses `make_grid_base()`. For 2 and 
# 5 km we need unique MGRS grid references as well. We start from a 10 km grid 
# and divide this in resp. 5 x 5 and 2 x 2  squares.
#############################################################################

make_grid_mgrs <- function(size, map, utm_zone, mgrs_scheme = "AL") {
  # Error handling
  if (!(size %in% c(1, 2, 5, 10))) {
    stop("Grid size needs to be 1, 2, 5 or 10 (kilometers)!")
  }
  
  if (!("sf" %in% class(map))) {
    stop("Map needs to be of class sf!")
  }
  
  if (!(mgrs_scheme %in% c("AA", "AL"))) {
    stop("Choose MGRS scheme AA or AL!")
  }
  suppressWarnings({
    
    # Return for sizes 1 and 10
    if (size == 1) {return(make_grid_base(1, map, utm_zone))}
    
    grid_10km <- make_grid_base(10, map, utm_zone)
    if (size == 10) {return(grid_10km)}
    
    # Set offset so the grids are aligned with the crs
    ## Round minimum down to 10 kilometers
    offset <- st_bbox(map)[1:2] - st_bbox(map)[1:2] %% 10000
    names(offset) <- NULL
    
    ## Convert size to meters
    ## Set offset so the grids are aligned with the crs
    large_grid <- grid_10km %>%
      st_make_grid(cellsize = size * 1000, offset = offset) %>%
      st_as_sf() %>%
      mutate(id = row_number())
    
    # Calculate MGRS
    ## Calculate central UTM-coordinates of each grid
    utm_centroids <- large_grid %>%
      st_centroid() %>%
      st_coordinates() %>%
      as_tibble()
    
    ## Add tags to grid
    if (size == 5) {
      suffices <- c("C", "D", "A", "B")
      large_grid_tagged <- cbind(large_grid, 
                                 tag = tag_grid_mgrs(utm_centroids, size, 
                                                     utm_zone, mgrs_scheme)) %>% 
        arrange(tag, id) %>% 
        group_by(tag) %>%
        mutate(tag = paste0(tag, suffices))
    } else {
      suffices <- paste0(rep(seq(0, 8, by = 2), 5), 
                         rep(seq(0, 8, by = 2), each = 5))
      large_grid_tagged <- cbind(large_grid, 
                                 tag = tag_grid_mgrs(utm_centroids, size, 
                                                     utm_zone, mgrs_scheme)) %>% 
        arrange(tag, id) %>% 
        group_by(tag) %>%
        mutate(tag = paste0(tag, "_", suffices)) 
    }
    
    # Get ids of grids that actually overlap with map
    intersect_ids <- large_grid_tagged %>%
      st_intersection(map) %>%
      pull(id)
    
    # Filter grids of large grid that overlap based intersecting ids
    grid_final <- large_grid_tagged %>% 
      filter(id %in% intersect_ids)
    
    # Return
    return(grid_final)
  })
}
