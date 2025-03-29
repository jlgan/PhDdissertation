
#sensing tree function 
trees_around <- function(birds, trees, dist) {
  birds.sf = st_as_sf(birds[1,], coords = c("x", "y"), crs = 32651)
  trees.sf = st_as_sf(trees, coords = c("x", "y"), crs = 32651)
  
  buffers <- st_buffer(birds.sf, dist = dist)
  intersections <- st_intersects(buffers, trees.sf)
  
  # Summarize results: List of trees within dist for each movement point
  result <- lapply(intersections, function(tree_id) {
    trees.sf[tree_id, ]
  })
  return(result)
}


#sample the trees in agriculture and reduce the number by a percentage of the total forest trees
treeagri_sample <- function(data, percent_tokeep) {
  
  data %>%
    dplyr::filter(hab == 1) %>% count() -> n_forest_trees
  
  n_tokeep = round(n_forest_trees * percent_tokeep ) %>% pull()
  
  data %>%
    dplyr::filter(hab == 0) %>%
    slice_sample(n = n_tokeep) %>%
    ungroup %>%
    bind_rows(data %>% filter(hab != 0)) -> data
  
  return(data)
}

#move or rest function if you have defined a fixed resting probability (not via sine wave)
moveorrest <- function(moverestprob, move_steps, i) {
  
  if (i>28) {timeofday= (i %% 28) + 1} else {timeofday = i} #timer reset for each day

  restprob = moverestprob %>% filter(timestep == timeofday)
  
  #probability check
  if (runif(1) < restprob$probability) {
      #rest procedure adds a row to the move_steps path
    new_row = move_steps %>% filter(X == i - 1) %>% mutate(t_ = "rest", X = X + 0.5)
    
    move_steps %>% 
      bind_rows(new_row) %>%  # Add the new row
      arrange(X) %>% 
      mutate(X = seq(1, nrow(move_steps) + 1)) -> move_steps 
    }
  return(move_steps)
}

#compute the rest probability from the sine wave function based on f and phi values
sin_rest <- function(f, phi, mins) {
  
  y = 0.25 * sin(f * pi/180 * mins + phi) + 0.25
  
  return(y)
}

#move or rest function based on resting probability function
rest <- function(max_seed_removed, move_steps, i) {
  
  if (i > 28) {timeofday = (i %% 28) + 1} else {timeofday = i} #timer reset for each day
  
  mins = (timeofday - 1) * 30
  
  restprob = sin_rest(f, phi, mins)

  #probability check
  if (runif(1) < restprob) {
    #rest procedure adds a row to the move_steps path
    new_row = move_steps %>% filter(X == i - 1) %>% mutate(t_ = "rest", X = X + 0.5)
    
    move_steps %>% 
      bind_rows(new_row) %>%  # Add the new row
      arrange(X) %>% 
      mutate(X = seq(1, nrow(move_steps) + 1)) -> move_steps 
  }
  return(move_steps)
}


#move procedure
move <- function(birds, i) {
  new_move = data.frame(matrix(ncol = 5, nrow = 1))
  colnames(new_move) <- c('id','x', 'y', 'tick', 'energy')
  new_move$id = birds[i-1,]$id
  new_move$x = move_steps[i,]$x_
  new_move$y = move_steps[i,]$y_
  new_move$tick = i
  new_move$energy = ifelse(birds[i-1,]$energy == 0, 0, birds[i-1,]$energy - 1)
  new_move$p_id = move_steps[i,]$p_id
  birds = bind_rows(birds, new_move)
  return(birds)
}



# Define the eatFruit function
eatFruit <- function(birds, trees, i, max_seed_removed) {
  
  trees_around(birds[i,], trees, dist)[[1]] %>%
    filter(total_fruit > 0) -> fruit_with_bird #is there any bird on the trees with fruits? 
  
  if (nrow(fruit_with_bird) > 0) {
    # Select a mother tree randomly
    mother_tree <- fruit_with_bird[sample(1:nrow(fruit_with_bird), 1), ]
  } else {
    mother_tree <- fruit_with_bird
  }
  
  if (nrow(fruit_with_bird) == 0) {
    birds$message[i] = "No trees with fruits here."
    print("No trees with fruits here.")
    
  } else {
    cat("There are", mother_tree$total_fruit, "fruits on tree", mother_tree$tree_id, "\n")
    n_fruits_to_eat <- sample(1:max_seed_removed, 1)
    #n_fruits_to_eat <- sample(1:2, 1)
    
    # Ensure bird doesn't eat more fruits than available
    if (n_fruits_to_eat > sum(mother_tree$total_fruit)) {
      n_fruits_to_eat <- sum(mother_tree$total_fruit)
    }
    
    cat("bird", tail(birds$id, n=1), "ate", n_fruits_to_eat, "fruit \n")
    birds$message[i] = paste("bird", birds[birds$tick==i,]$id, "ate", n_fruits_to_eat, "fruit")
    
    # Update the mother tree's fruit count
    trees[trees$tree_id == mother_tree$tree_id, ]$total_fruit =  
      trees[trees$tree_id == mother_tree$tree_id, ]$total_fruit - n_fruits_to_eat
    
    # Update bird's lists and traits
    birds$energy[i] = birds$energy[i] + 3 #not important in the IBM for now
    gut[i,1] = tail(birds$id, n=1) #bird_id
    gut[i,2] = i #tick
    gut[i,3] = n_fruits_to_eat #seed_eaten
    gut[i,4] = mother_tree$tree_id #origintree_ID 
    gut[i,5] = 0 #SRT timer
    
  } 
  return(list(birds, trees, gut))
}

#poop procedure
poop <- function(birds, trees, gut, i) {
  
  # Check if gut is not empty and there is SRT >= 3 (time to poop)
  if (any(gut$SRT >= 3, na.rm = TRUE)) {
    
    ready_to_poop =  gut %>% filter(!is.na(SRT) & SRT >= 3 & SRT < 999)
    seedtodepo =  ready_to_poop %>% summarize(seed_pooped=sum(seed_eaten, na.rm = TRUE))
    
    #update birds with number of seeds deposited
    birds[i,]$seed_depo = seedtodepo
    
    gut %>% mutate(pID_seed_depo = ifelse((!is.na(SRT) & SRT >= 3 & SRT < 999), birds$p_id[i], pID_seed_depo),
                   tick_depo = ifelse((!is.na(SRT) & SRT >= 3 & SRT < 999), i, tick_depo),
                   SRT = ifelse((!is.na(SRT) & SRT >= 3 & SRT < 999), 999, SRT))  -> gut
    
  }
  
  return(list(birds, trees, gut))
}


#assigning seed retention time procedure
assign_GRT <- function(seed_SRT_line, gut, i) {
  
  newest_content = gut[i,]
  
  # Check if there is seed not yet deposited in the gut
  seed_needSRT <- newest_content %>%
    filter(newest_content$seed_eaten >= 1 & is.na(newest_content$pID_seed_depo))

  if (nrow(seed_needSRT) > 0) {
    # Populate SRT by drawing from a Poisson distribution
    seed_needSRT %>%
      rowwise() %>%
      mutate(SRT_perseed = list(rpois(n=seed_eaten, lambda= srt_pois_lambda))) %>%
      ungroup() %>%
      tidyr::unnest(SRT_perseed) -> new_srt
    
    new_srt$SRT = new_srt$SRT_perseed
    seed_SRT_line = rbind(seed_SRT_line, new_srt)
  }
  return(seed_SRT_line)
}


#staggered poop procedure

staggered_poop <- function(birds, seed_SRT_line, i) {

  # Check if gut is not empty and there is SRT >= 3 (time to poop)
  if (any(seed_SRT_line$SRT_perseed == 0)) {

    ready_to_poop =  seed_SRT_line %>% filter(SRT_perseed == 0)
    seedtodepo =  ready_to_poop %>% summarize(seed_pooped= nrow(ready_to_poop))

    #update birds with number of seeds deposited
    birds[i,]$seed_depo = seedtodepo

    seed_SRT_line %>% 
      mutate(pID_seed_depo = ifelse((SRT_perseed == 0), birds$p_id[i], pID_seed_depo),
                   tick_depo = ifelse((SRT_perseed == 0), i, tick_depo),
                   SRT_perseed = ifelse((SRT_perseed == 0), 999, SRT_perseed))  -> seed_SRT_line

  }

  return(list(birds, seed_SRT_line))
}


#Package AMT Functions
#the extract_covariates function did not work for when="both", but works for "end" and "start"  
#key changes: specified that extract output is a dataframe and changed the names to refer to covariate; vs original code which was names(x_start)

edited_extract_covariates.steps_xy <- function(x, covariates, where = "both", ...) {
  #validate_covars(covariates)
  if (where == "both") {
    x_start <- data.frame(terra::extract(covariates, as.matrix(x[, c("x1_", "y1_")]), ...))
    names(x_start) <- paste0(names(covariates), "_start")
    x_end <- data.frame(terra::extract(covariates, as.matrix(x[, c("x2_", "y2_")]), ...))
    names(x_end) <- paste0(names(covariates), "_end")
    x_all <- cbind(x_start, x_end)
    names(x_all) = c(names(x_start), names(x_end))
    #cbind(x, x_all) -> x
    x[names(x_all)] <- x_all
  } else {
    x[names(covariates)] <- if (where == "end") {
      terra::extract(covariates, as.matrix(x[, c("x2_", "y2_")]), ...)
    } else if (where == "start") {
      terra::extract(covariates, as.matrix(x[, c("x1_", "y1_")]), ...)
    }
  }
  x
}

#ssf_weights
#does not work with covar that are categorical due to name changes 
#key changes: edited the subsetting of the xyz to simply [,-1] to remove the 1st column (intercept)
ssf_weights <- function(xy, object, compensate.movement = FALSE) {
  checkmate::assert_class(xy, "data.frame")
  checkmate::assert_class(object, "fit_clogit")
  checkmate::assert_logical(compensate.movement)
  
  coefs <- coef(object)
  #names(coefs) <- c("hab_end",  "sl_", "log(sl_)","cos(ta_)")
  ff <- ssf_formula(object$model$formula)
  newdata <- xy
  attr(newdata, "na.action") <- "na.pass"
  xyz <- stats::model.matrix.default(ff, data = newdata, na.action = stats::na.pass)
  #w <- as.matrix(xyz[, names(coefs)]) %*% coefs
  w <- as.matrix(xyz[, -1]) %*% coefs
  
  if (compensate.movement) {
    phi <- movement_kernel1(xy, object$sl_, object$ta_)
    w <- w + phi - log(xy$sl_) # -log(xy$sl) divides by the sl and accounts for the transformation
  }
  w <- exp(w - mean(w[is.finite(w)], na.rm = TRUE))
  w[!is.finite(w)] <- 0
  w
}

#redistribution_kernel
#key changes: used the edited_extract_covariates.steps_xy 
redistribution_kernel <- function (x = make_issf_model(), start = make_start(), map, 
                                   fun = function(xy, map) {
                                     edited_extract_covariates.steps_xy(xy, map, where = "both")
                                   }, covars = NULL, max.dist = get_max_dist(x), n.control = 1e+06, 
                                   n.sample = 1, landscape = "continuous", compensate.movement = landscape == 
                                     "discrete", normalize = TRUE, interpolate = FALSE, as.rast = FALSE, 
                                   tolerance.outside = 0) 
{
  arguments <- as.list(environment())
  checkmate::assert_class(start, "sim_start")
  if (!landscape %in% c("continuous", "discrete")) {
    stop("Argument `landscape` is invalid. Valid values are 'continuous' or 'discrete'.")
  }
  if (landscape == "continuous") {
    xy <- random_steps_simple(start, sl_model = x$sl_, ta_model = x$ta_, 
                              n.control = n.control)
  }
  else {
    xy <- kernel_setup(map, max.dist, start, covars)
  }
  bb.map <- as.vector(terra::ext(map))
  fraction.outside <- mean(xy$x2_ < bb.map["xmin"] | xy$x2_ > 
                             bb.map["xmax"] | xy$y2_ < bb.map["ymin"] | xy$y2_ > 
                             bb.map["ymax"])
  if (fraction.outside > tolerance.outside) {
    warning(paste0(round(fraction.outside * 100, 3), "% of steps are ending outside the study area but only ", 
                   round(tolerance.outside * 100, 3), "% is allowed. ", 
                   "Terminating simulations here."))
    return(NULL)
  }
  xy$t1_ <- start$t_
  xy$t2_ <- start$t_ + start$dt
  xy <- fun(xy, map)
  w <- ssf_weights(xy, x, compensate.movement = compensate.movement)
  r <- if (!as.rast) {
    dplyr::select(xy[sample.int(nrow(xy), size = n.sample, 
                                prob = w), ], x_ = x2_, y_ = y2_, t2_)
  }
  else {
    if (landscape == "continuous") {
      stop("`as.rast` not implemented for `landscape = 'continuous'`")
    }
    else {
      terra::rast(data.frame(xy[, c("x2_", "y2_")], w))
    }
  }
  if (as.rast & normalize) {
    r <- normalize(r)
  }
  res <- list(args = arguments, redistribution.kernel = r)
  class(res) <- c("redistribution_kernel", "list")
  res
}


#simulate_path.redistribution_kernel: copied as is from the amt 
simulate_path.redistribution_kernel <- function(
    x, n.steps = 100, start = x$args$start, verbose = FALSE, ...) {
  
  mod <- x$args
  xy <- tibble(x_ = rep(NA, n.steps + 1), y_ = NA_real_,
               t_ = start$t_ + start$dt * (0:n.steps), dt = start$dt)
  
  xy$x_[1] <- start$x_
  xy$y_[1] <- start$y_
  
  
  for (i in 1:n.steps) {
    rk <- redistribution_kernel(
      x = mod$x,
      start = start,
      # map = mod$map,
      map = hab.raster, #LOOK AT THIS!!!
      fun = mod$fun,
      max.dist = mod$max.dist,
      n.control = mod$n.control,
      n.sample = 1,
      landscape = mod$landscape,
      normalize = TRUE,
      interpolate = FALSE,
      as.rast = FALSE,
      tolerance.outside = mod$tolerance.outside
    )
    
    if (is.null(rk)) {
      warning(paste0("Simulation stopped after ", i - 1, " time steps, because the animal stepped out of the landscape."))
      return(xy)
    }
    
    rk <- rk$redistribution.kernel
    
    # Check that we do not have error (i.e., because stepping outside the landscape)
    # Make new start
    new.ta <- atan2(rk$y_[1] - start$y_[1], rk$x_[1] - start$x_[1])
    
    xy$x_[i + 1] <- rk$x_[1]
    xy$y_[i + 1] <- rk$y_[1]
    start <- make_start(
      as.numeric(xy[i + 1, c("x_", "y_")]), new.ta,
      time = xy$t_[i],
      crs = attr(x$args$start, "crs"))
  }
  return(xy)
}


#seed dispersal distances for staggered pooping
compute_sdd <- function(seed_SRT_line, trees, patches) {
  
  seed_SRT_line_out = data.frame()
  
  if (nrow(seed_SRT_line) > 0) {
    
    seed_SRT_line %>%
      dplyr::select(origintree_ID, pID_seed_depo) %>%
      left_join(trees, by = c("origintree_ID" = "tree_id")) %>%
      dplyr::select(-total_fruit, -p_id) %>% # x,y,hab of the origin   #i just removed this,
      left_join(patches, by = c("pID_seed_depo" = "p_id")) %>%
      #select(-pcolor) %>%
      rename(origintree_ID  = origintree_ID , depo_pID = pID_seed_depo , x_origin = x.x, y_origin = y.x,
             x_depo = x.y, y_depo = y.y, hab_depo=hab) %>%
      na.omit() -> seed_SRT_line_out  # Rename columns directly
    
    origin <- st_as_sf(seed_SRT_line_out, coords = c("x_origin", "y_origin"), crs = 32651 )
    dest <- st_as_sf(seed_SRT_line_out, coords = c("x_depo", "y_depo"), crs = 32651 )
    seed_SRT_line_out$distances <- st_distance(origin, dest,  by_element = TRUE )
    seed_SRT_line_out$sim_num = s_run
    seed_SRT_line_out$total_seeds_depo =  sum(!is.na(seed_SRT_line$pID_seed_depo))
    
  }
    
  return(seed_SRT_line_out)
}

 
#seed dispersal distances for staggered pooping
compute_sdd_treedit <- function(seed_SRT_line, trees, patches) {
  
  seed_SRT_line_out = data.frame()
  
  if (nrow(seed_SRT_line) > 0) {
    
    seed_SRT_line %>%
      dplyr::select(origintree_ID, pID_seed_depo) %>%
      left_join(trees, by = c("origintree_ID" = "tree_id")) %>%
      dplyr::select(-total_fruit, -p_id, -hab) %>% # x,y,hab of the origin   #i just removed this,
      left_join(patches, by = c("pID_seed_depo" = "p_id")) %>%
      #select(-pcolor) %>%
      rename(origintree_ID  = origintree_ID , depo_pID = pID_seed_depo , x_origin = x.x, y_origin = y.x,
             x_depo = x.y, y_depo = y.y, hab_depo=hab) %>%
      na.omit() -> seed_SRT_line_out  # Rename columns directly
    
    origin <- st_as_sf(seed_SRT_line_out, coords = c("x_origin", "y_origin"), crs = 32651 )
    dest <- st_as_sf(seed_SRT_line_out, coords = c("x_depo", "y_depo"), crs = 32651 )
    seed_SRT_line_out$distances <- st_distance(origin, dest,  by_element = TRUE )
    seed_SRT_line_out$sim_num = s_run
    seed_SRT_line_out$total_seeds_depo =  sum(!is.na(seed_SRT_line$pID_seed_depo))
    
  }
  
  return(seed_SRT_line_out)
}

sen_compute_sdd <- function(seed_SRT_line, trees, patches) {
  
  seed_SRT_line_out = data.frame()
  
  if (nrow(seed_SRT_line) > 0) {
    
    seed_SRT_line %>%
      dplyr::select(origintree_ID, pID_seed_depo) %>%
      left_join(trees, by = c("origintree_ID" = "tree_id")) %>%
      #select(-total_fruit, -p_id) %>% # x,y,hab of the origin   #i just removed this,
      left_join(patches, by = c("pID_seed_depo" = "p_id")) %>%
      #select(-pcolor) %>%
      rename(origintree_ID  = origintree_ID , depo_pID = pID_seed_depo , x_origin = x.x, y_origin = y.x,
             x_depo = x.y, y_depo = y.y, hab_depo=hab) %>%
      na.omit() -> seed_SRT_line_out  # Rename columns directly
    
    origin <- st_as_sf(seed_SRT_line_out, coords = c("x_origin", "y_origin"), crs = 32651 )
    dest <- st_as_sf(seed_SRT_line_out, coords = c("x_depo", "y_depo"), crs = 32651 )
    seed_SRT_line_out$distances <- st_distance(origin, dest,  by_element = TRUE )
    seed_SRT_line_out$sim_num = s_run
    seed_SRT_line_out$total_seeds_depo =  sum(!is.na(seed_SRT_line$pID_seed_depo))
    
  }
  
  return(seed_SRT_line_out)
}


# classifying steps to habitats
step_hab <- function(move_steps, combined_raster) {
  move_steps$p_id = NA
  move_steps$hab = NA
  
  for (i in 1:nrow(move_steps)) {
    bird_loc = cbind(move_steps[i,]$x_, move_steps[i,]$y_)
    bird_pixel = raster::extract(combined_raster, bird_loc)
    move_steps[i,]$p_id = bird_pixel[2]
    move_steps[i,]$hab = bird_pixel[1]
  }
  return (move_steps)
}

#simulate tree distribution
simulate_trees <- function(pred_dist_tree, combined_raster, mean_crop_size) {
  
  simulated_treepoints <- rpoispp(lambda = pred_dist_tree)
  sim_tree_df = data.frame(cbind(x=simulated_treepoints$x, y=simulated_treepoints$y))
  
  sim_tree_df %>%
    mutate(p_id = data.frame(extract(combined_raster, sim_tree_df))[,2]) %>%
    mutate(tree_id = seq(1:nrow(sim_tree_df)),
           #total_fruit = 5,  #if you want to set the fruit number at 5 per tree
           total_fruit = rpois(n(), mean_crop_size)) -> trees_sim
  
  return(trees_sim)
}

#simulate tree distribution
sen_simulate_trees <- function(pred_dist_tree, combined_raster) {
  
  simulated_treepoints <- rpoispp(lambda = pred_dist_tree)
  sim_tree_df = data.frame(cbind(x=simulated_treepoints$x, y=simulated_treepoints$y))

  sim_tree_df %>%
    mutate(p_id = data.frame(extract(combined_raster, sim_tree_df))[,2]) %>%
    mutate(tree_id = seq(1:nrow(sim_tree_df))) -> trees_sim
  
  return(trees_sim)
}

  
#setup patches
set_raster <- function(hab.raster) {
    
    names(hab.raster) = "hab"
  
    # Create a unique pixel ID raster
    pixel_id_raster <- hab.raster  # Copy the raster structure
    values(pixel_id_raster) <- seq_len(ncell(hab.raster))  # Assign unique ID to each cell
    
    # Combine habitat and pixel ID into a multi-layer raster
    combined_raster <- stack(hab.raster, pixel_id_raster)  # Create a multi-band raster
    
    # Assign layer names for clarity
    names(combined_raster) <- c("hab", "p_id")
    
    return(combined_raster)
}

set_patches <- function(combined_raster) {
  
    patches <- as.data.frame(rasterToPoints(combined_raster))
    
    # patches <- patches %>%
    #   mutate(pcolor = ifelse(hab == 1, "forest", "agriculture"))
    
    return(patches)
}

save_simplots <- function(hab.raster, move_steps, trees, s_run, percent_tokeep) {
  
  #plot where seed depo per simulation
  test_spdf <- as(hab.raster, "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  
  (sim_track_m3 <- ggplot() +
      geom_raster(data = test_df, aes(x = x, y = y, fill = hab), alpha = 0.7) + 
      scale_fill_gradientn(colours = c("white", "forestgreen")) +
      geom_point(data = trees, aes(x = x, y = y, col = "red"), size = 1, alpha = 0.3 ) +
      geom_path(data = move_steps, aes(x = x_, y = y_), linewidth = 1 , col = "black") +
      coord_equal() +
      labs(title = paste0("simulation ", s_run, " percent_tree: ", percent_tokeep[j])) +
      theme_minimal())
  
  file_name = paste("sim_plots_sameland_treeedited/sim_plot_", s_run, "perc_", percent_tokeep[j], ".tiff", sep = "")
  ggsave(file_name)
 
}

is_out_bounds <- function(move_steps, extent) {
  
  xmin <- extent[1]
  xmax <- extent[2]
  ymin <- extent[3]
  ymax <- extent[4]
  
  out_x_bounds <- move_steps$x_ <= xmin & move_steps$x_ >= xmax
  out_y_bounds <- move_steps$y_ <= ymin & move_steps$y_ >= ymax
  
  any(out_x_bounds | out_y_bounds)
  
  return(any(out_x_bounds | out_y_bounds))
}


compute_areashape_sdd <- function (sdd_out_df) {
  tryCatch({
    sdd_out_df.sp = sdd_out_df %>% dplyr::select(x_depo, y_depo) %>% mutate(seedID = 1)
    #sdd_out_df.sp = bind_rows(sdd_out_df.sp, sdd_out_df.sp)
    coordinates(sdd_out_df.sp) <- c("x_depo", "y_depo")
    proj4string(sdd_out_df.sp) <- CRS("+proj=utm +zone=51 +datum=WGS84 +units=m +no_defs")
    
    #area
    seed.mcp <- mcp(sdd_out_df.sp, percent = 100, unin = "m", unout = "m2")
    area = seed.mcp$area #m2
    #st_write(st_as_sf(seed.mcp), paste0("sim_plots/MCP_kset",kset, "rr", rr, ".shp"), append = FALSE)
    
    #shape
    coords <- coordinates(seed.mcp@polygons[[1]]@Polygons[[1]])
    
    # Calculate distances between consecutive points
    distances <- sqrt(diff(coords[, 1])^2 + diff(coords[, 2])^2)
    
    # Add the distance to close the polygon
    closing_distance <- sqrt((coords[1, 1] - coords[nrow(coords), 1])^2 +
                               (coords[1, 2] - coords[nrow(coords), 2])^2)
    
    # Sum up all distances to get the perimeter
    perim <- sum(distances) + closing_distance
    
    # Compute shape metrics
    par <- perim / area
    si <- perim / (2 * sqrt(pi * area))
    
    
    return(list(area, par, si))
  }, error = function(e) {
    # If an error occurs, return NULL
    return(NULL)
  })
}

