# 38. Image-led unsupervised land-cover learning in R

This is the primary image-led analysis path in this version of the workflow. It does **not** train on the historical point labels. It uses the local QGIS-clipped, four-season Landsat composites to discover recurring spectral/seasonal pixel groups. One pooled clustering model is fitted across all four years so the cluster IDs remain comparable across years.

The first run produces cluster maps, cluster signatures, and annual area/share plots for the clusters. These are exploratory clusters, not automatically confirmed land-cover names. Review the maps and spectral signatures, complete the generated `Tharaka_Cluster_to_LandCover_Crosswalk.csv`, and rerun this section to create the named land-cover trend plots. This avoids pretending that an unsupervised algorithm knows which cluster is farmland or bushland without interpretation.

**Input requirement:** one multiband GeoTIFF per year and seasonal window (16 files total), with the first six bands in this order: blue, green, red, NIR, SWIR1, SWIR2. Filenames must include a target year (`2000`, `2010`, `2020`, or `2025`) and a month/season token (`Feb`, `Sep`, `May`, or `Dec`). If these TIFFs do not exist locally yet, run Section 36 interactively, download the 16 completed exports from Google Drive into `01_data/landsat/qgis_input`, and rerun the document so Section 40 can clip them with QGIS. The code checks the input inventory before processing.

```{r section-38-unsupervised-image-learning, echo=FALSE, message=FALSE, warning=TRUE, fig.width=11, fig.height=8}
# =========================================================
# SECTION 38: IMAGE-LED UNSUPERVISED LAND-COVER LEARNING
# Primary pathway: QGIS-clipped seasonal Landsat GeoTIFFs + R
# Does NOT use the supplied historical point labels for fitting.
# =========================================================

if (!requireNamespace("terra", quietly = TRUE) ||
    !requireNamespace("dplyr", quietly = TRUE) ||
    !requireNamespace("tidyr", quietly = TRUE) ||
    !requireNamespace("purrr", quietly = TRUE) ||
    !requireNamespace("ggplot2", quietly = TRUE) ||
    !requireNamespace("readr", quietly = TRUE) ||
    !requireNamespace("stringr", quietly = TRUE)) {
  stop("Section 38 requires terra, dplyr, tidyr, purrr, ggplot2, readr and stringr.")
}

library(terra)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(readr)
library(stringr)

unsup_years <- c(2000L, 2010L, 2020L, 2025L)
unsup_windows <- c("dry_feb", "dry_sep", "wet_may", "wet_dec")
unsup_bands <- c("blue", "green", "red", "nir", "swir1", "swir2")
unsup_k <- 8L                    # Try 8 initial spectral/seasonal clusters
unsup_sample_per_year <- 2500L  # Increase to 5,000 if the computer permits
set.seed(20261011)

unsup_out_dir <- file.path("Tharaka_Land_Cover_Results", "Unsupervised_Image_Learning")
unsup_feature_dir <- file.path(unsup_out_dir, "Feature_Stacks")
unsup_map_dir <- file.path(unsup_out_dir, "Cluster_Maps")
dir.create(unsup_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(unsup_feature_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(unsup_map_dir, recursive = TRUE, showWarnings = FALSE)

# Look in QGIS-clipped imagery first, then the QGIS input folder.
unsup_input_dirs <- unique(c(
  if (exists("qgis_raster_output_dir", inherits = TRUE)) qgis_raster_output_dir,
  "01_data/landsat/qgis_clipped",
  "01_data/landsat/qgis_input"
))
unsup_input_dirs <- unsup_input_dirs[dir.exists(unsup_input_dirs)]

# Convert file names into a year + seasonal-window inventory.
unsup_build_catalog <- function(files) {
  if (length(files) == 0L) return(tibble::tibble(file = character(), year = integer(), window = character()))
  nm <- tolower(basename(files))
  year_txt <- stringr::str_extract(nm, "2000|2010|2020|2025")
  window_patterns <- c(
    dry_feb = "feb(ruary)?|02[-_ ]?10",
    dry_sep = "sep(tember)?|09[-_ ]?15",
    wet_may = "may|05[-_ ]?05",
    wet_dec = "dec(ember)?|12[-_ ]?05"
  )
  window_txt <- vapply(nm, function(one_name) {
    hits <- names(window_patterns)[vapply(
      window_patterns,
      function(pat) grepl(pat, one_name, ignore.case = TRUE, perl = TRUE),
      logical(1)
    )]
    if (length(hits) == 1L) hits else NA_character_
  }, character(1))
  tibble::tibble(
    file = files,
    year = suppressWarnings(as.integer(year_txt)),
    window = unname(window_txt)
  ) %>%
    filter(!is.na(year), !is.na(window), year %in% unsup_years)
}

unsup_catalog <- NULL
unsup_selected_dir <- NA_character_
for (candidate_dir in unsup_input_dirs) {
  candidate_files <- list.files(
    candidate_dir,
    pattern = "\\.(tif|tiff)$",
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = FALSE
  )
  candidate_catalog <- unsup_build_catalog(candidate_files)
  duplicate_pairs <- candidate_catalog %>% count(year, window) %>% filter(n != 1L)
  expected_pairs <- tidyr::crossing(year = unsup_years, window = unsup_windows)
  missing_pairs <- dplyr::anti_join(expected_pairs, candidate_catalog, by = c("year", "window"))
  if (nrow(duplicate_pairs) == 0L && nrow(missing_pairs) == 0L && nrow(candidate_catalog) == 16L) {
    unsup_catalog <- candidate_catalog %>% arrange(year, match(window, unsup_windows))
    unsup_selected_dir <- candidate_dir
    break
  }
}

if (is.null(unsup_catalog)) {
  found_files <- unlist(lapply(unsup_input_dirs, function(d) {
    list.files(d, pattern = "\\.(tif|tiff)$", full.names = TRUE, ignore.case = TRUE)
  }), use.names = FALSE)
  cat("GeoTIFF search folders:\n", paste(unsup_input_dirs, collapse = "\n"), "\n\n")
  cat("GeoTIFF files found:\n", if (length(found_files)) paste(basename(found_files), collapse = "\n") else "(none)", "\n")
  warning(
    "Section 38 has been skipped because the 16 seasonal GeoTIFFs are not all available. ",
    "If needed, run Section 36 interactively, download the completed seasonal exports, ",
    "place them in 01_data/landsat/qgis_input, then rerun the document."
  )
} else {

cat("Using seasonal imagery from:", normalizePath(unsup_selected_dir, winslash = "/"), "\n")
print(unsup_catalog, n = Inf)
readr::write_csv(unsup_catalog, file.path(unsup_out_dir, "Tharaka_Unsupervised_Input_Inventory.csv"))

# Use the first composite to define a common raster grid. Every other scene
# is resampled to this grid if needed. QGIS already created the study-area clips;
# when a repaired boundary is available, apply it as an additional mask.
unsup_reference <- terra::rast(unsup_catalog$file[1])[[1]]
unsup_boundary_path <- c(
  "01_data/boundary/qgisprocess/Tharaka_SubCounty_QGIS_fixed.gpkg",
  "01_data/boundary/Tharaka_SubCounty.shp"
)
unsup_boundary_path <- unsup_boundary_path[file.exists(unsup_boundary_path)]
unsup_mask <- NULL
if (length(unsup_boundary_path) > 0L) {
  unsup_mask <- terra::vect(unsup_boundary_path[1])
  if (!terra::same.crs(unsup_mask, unsup_reference)) {
    unsup_mask <- terra::project(unsup_mask, terra::crs(unsup_reference))
  }
}

unsup_read_aligned <- function(path) {
  x <- terra::rast(path)
  if (terra::nlyr(x) < 6L) {
    stop("Expected at least six Landsat reflectance bands in: ", basename(path))
  }
  # The exports should follow this order: blue, green, red, nir, swir1, swir2.
  # If your export used another band order, correct it here before proceeding.
  x <- x[[1:6]]
  if (!terra::same.crs(x, unsup_reference)) {
    x <- terra::project(x, unsup_reference, method = "bilinear")
  } else if (!isTRUE(terra::compareGeom(x[[1]], unsup_reference, stopOnError = FALSE))) {
    x <- terra::resample(x, unsup_reference, method = "bilinear")
  }
  names(x) <- unsup_bands
  if (!is.null(unsup_mask)) x <- terra::mask(x, unsup_mask)
  x
}

# Create 36 predictors per year: six reflectance bands and three indices for
# each of the four seasonal windows. Ratio indices use explicit arithmetic.
unsup_feature_names <- character()
unsup_feature_files <- setNames(character(length(unsup_years)), as.character(unsup_years))
for (yr in unsup_years) {
  feature_parts <- list()
  for (win in unsup_windows) {
    scene_file <- unsup_catalog %>% filter(year == yr, window == win) %>% pull(file)
    reflectance <- unsup_read_aligned(scene_file)
    names(reflectance) <- paste0(unsup_bands, "_", win)

    calc_index <- function(a, b, index_name) {
      numerator <- a - b
      denominator <- a + b
      idx <- terra::ifel(abs(denominator) < 1e-6, NA, numerator / denominator)
      names(idx) <- paste0(index_name, "_", win)
      idx
    }
    ndvi <- calc_index(reflectance[[paste0("nir_", win)]], reflectance[[paste0("red_", win)]], "NDVI")
    ndwi <- calc_index(reflectance[[paste0("green_", win)]], reflectance[[paste0("nir_", win)]], "NDWI")
    ndbi <- calc_index(reflectance[[paste0("swir1_", win)]], reflectance[[paste0("nir_", win)]], "NDBI")
    feature_parts[[win]] <- c(reflectance, ndvi, ndwi, ndbi)
  }

  feature_stack <- do.call(c, feature_parts)
  names(feature_stack) <- unlist(lapply(unsup_windows, function(win) c(
    paste0(unsup_bands, "_", win), paste0("NDVI_", win),
    paste0("NDWI_", win), paste0("NDBI_", win)
  )), use.names = FALSE)
  if (length(unsup_feature_names) == 0L) unsup_feature_names <- names(feature_stack)

  feature_path <- file.path(unsup_feature_dir, paste0("Tharaka_Features_", yr, ".tif"))
  terra::writeRaster(
    feature_stack, feature_path, overwrite = TRUE,
    wopt = list(datatype = "FLT4S", gdal = c("COMPRESS=LZW", "TILED=YES"))
  )
  unsup_feature_files[as.character(yr)] <- feature_path
  message("Prepared aligned seasonal feature stack for ", yr)
  rm(feature_stack, feature_parts)
  gc(verbose = FALSE)
}

# Sample from all four years and fit one pooled k-means model. Scaling prevents
# raw reflectance bands from dominating the bounded vegetation/water indices.
unsup_samples_list <- lapply(unsup_years, function(yr) {
  x <- terra::rast(unsup_feature_files[[as.character(yr)]])
  s <- terra::spatSample(
    x, size = unsup_sample_per_year, method = "random",
    na.rm = TRUE, as.df = TRUE, values = TRUE
  )
  s <- as.data.frame(s)
  s$sample_year <- yr
  s
})
unsup_samples <- dplyr::bind_rows(unsup_samples_list)
unsup_samples <- unsup_samples %>% filter(if_all(all_of(unsup_feature_names), ~ is.finite(.x)))
if (nrow(unsup_samples) < unsup_k * 20L) {
  stop("Too few valid multi-season pixel samples for clustering. Check cloud masks and input imagery.")
}

unsup_sample_matrix <- as.matrix(unsup_samples[, unsup_feature_names, drop = FALSE])
unsup_sd <- apply(unsup_sample_matrix, 2, sd, na.rm = TRUE)
unsup_keep <- is.finite(unsup_sd) & unsup_sd > 1e-8
if (sum(unsup_keep) < 3L) stop("Too few variable image predictors remain after quality checks.")
unsup_used_features <- unsup_feature_names[unsup_keep]
unsup_sample_matrix <- unsup_sample_matrix[, unsup_keep, drop = FALSE]
unsup_centres <- colMeans(unsup_sample_matrix, na.rm = TRUE)
unsup_scales <- apply(unsup_sample_matrix, 2, sd, na.rm = TRUE)
unsup_scaled_samples <- scale(unsup_sample_matrix, center = unsup_centres, scale = unsup_scales)

set.seed(20261011)
unsup_kmeans <- stats::kmeans(
  unsup_scaled_samples,
  centers = unsup_k,
  iter.max = 50,
  nstart = 3
)

# Save original-scale cluster signatures. These are the evidence used to review
# and name clusters; the cluster IDs should not be called land-cover classes yet.
unsup_signature_data <- as.data.frame(unsup_sample_matrix)
unsup_signature_data$cluster <- factor(unsup_kmeans$cluster, levels = seq_len(unsup_k))
unsup_signatures <- unsup_signature_data %>%
  group_by(cluster) %>%
  summarise(across(all_of(unsup_used_features), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
readr::write_csv(unsup_signatures, file.path(unsup_out_dir, "Tharaka_Unsupervised_Cluster_Signatures.csv"))

# Cluster assignment function used by terra::predict() on raster blocks.
unsup_model <- list(
  centers = unsup_kmeans$centers,
  center = unsup_centres,
  scale = unsup_scales,
  feature_names = unsup_used_features
)
unsup_predict_cluster <- function(model, data, ...) {
  data <- as.data.frame(data)
  x <- as.matrix(data[, model$feature_names, drop = FALSE])
  result <- rep(NA_integer_, nrow(x))
  valid <- if (nrow(x) == 0L) logical(0) else complete.cases(x) & apply(x, 1, function(z) all(is.finite(z)))
  if (any(valid)) {
    z <- scale(x[valid, , drop = FALSE], center = model$center, scale = model$scale)
    d2 <- matrix(NA_real_, nrow = nrow(z), ncol = nrow(model$centers))
    for (j in seq_len(nrow(model$centers))) {
      delta <- sweep(z, 2, model$centers[j, ], FUN = "-")
      d2[, j] <- rowSums(delta * delta)
    }
    result[valid] <- max.col(-d2, ties.method = "first")
  }
  data.frame(cluster = result)
}

# Map all years with the same pooled model, then estimate area by cluster.
unsup_cluster_map_files <- setNames(character(length(unsup_years)), as.character(unsup_years))
unsup_area_by_cluster <- list()
unsup_map_colours <- grDevices::hcl.colors(unsup_k, palette = "Dynamic")
for (yr in unsup_years) {
  feature_stack <- terra::rast(unsup_feature_files[[as.character(yr)]])[[unsup_used_features]]
  cluster_path <- file.path(unsup_map_dir, paste0("Tharaka_Unsupervised_Clusters_", yr, ".tif"))
  cluster_map <- terra::predict(
    feature_stack, unsup_model, fun = unsup_predict_cluster,
    na.rm = TRUE, filename = cluster_path, overwrite = TRUE,
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW", "TILED=YES"))
  )
  unsup_cluster_map_files[as.character(yr)] <- cluster_path

  png(file.path(unsup_map_dir, paste0("Tharaka_Unsupervised_Clusters_", yr, ".png")),
      width = 1600, height = 1200, res = 150)
  terra::plot(terra::as.factor(cluster_map), col = unsup_map_colours,
               main = paste("Tharaka Sub-county — image-derived clusters", yr),
               axes = TRUE)
  dev.off()

  area_raster <- terra::cellSize(cluster_map, unit = "ha")
  z <- terra::zonal(area_raster, cluster_map, fun = "sum", na.rm = TRUE)
  z <- as.data.frame(z)
  if (ncol(z) >= 2L) {
    names(z)[1:2] <- c("cluster", "area_ha")
    unsup_area_by_cluster[[as.character(yr)]] <- tibble::as_tibble(z) %>%
      mutate(year = yr, cluster = as.integer(cluster)) %>%
      select(year, cluster, area_ha)
  }
  rm(feature_stack, cluster_map, area_raster, z)
  gc(verbose = FALSE)
}

unsup_cluster_area_raw <- dplyr::bind_rows(unsup_area_by_cluster) %>%
  group_by(year, cluster) %>%
  summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop")
unsup_cluster_area <- tidyr::crossing(year = unsup_years, cluster = seq_len(unsup_k)) %>%
  left_join(unsup_cluster_area_raw, by = c("year", "cluster")) %>%
  mutate(area_ha = tidyr::replace_na(area_ha, 0)) %>%
  group_by(year) %>%
  mutate(
    mapped_area_ha = sum(area_ha, na.rm = TRUE),
    share_percent = if_else(mapped_area_ha > 0, 100 * area_ha / mapped_area_ha, NA_real_)
  ) %>%
  ungroup() %>%
  arrange(year, cluster)
readr::write_csv(unsup_cluster_area, file.path(unsup_out_dir, "Tharaka_Unsupervised_Cluster_Area_Trends.csv"))

p_unsup_cluster_area <- ggplot(unsup_cluster_area, aes(x = year, y = area_ha, colour = factor(cluster), group = cluster)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  facet_wrap(~ cluster, scales = "free_y", ncol = 4) +
  scale_x_continuous(breaks = unsup_years) +
  labs(title = "Image-derived cluster area trends in Tharaka Sub-county",
       subtitle = paste0("Pooled k-means on four seasonal Landsat composites; k = ", unsup_k),
       x = "Year", y = "Area (ha)", colour = "Cluster") +
  theme_minimal(base_size = 12) + theme(legend.position = "none")
ggsave(file.path(unsup_out_dir, "Tharaka_Unsupervised_Cluster_Area_Trends.png"), p_unsup_cluster_area, width = 12, height = 8, dpi = 300, bg = "white")
print(p_unsup_cluster_area)

p_unsup_cluster_share <- ggplot(unsup_cluster_area, aes(x = year, y = share_percent, colour = factor(cluster), group = cluster)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2) +
  facet_wrap(~ cluster, scales = "free_y", ncol = 4) +
  scale_x_continuous(breaks = unsup_years) +
  labs(title = "Share of mapped pixels by image-derived cluster",
       subtitle = "Shares use the valid mapped area in each year as the denominator",
       x = "Year", y = "Mapped area (%)", colour = "Cluster") +
  theme_minimal(base_size = 12) + theme(legend.position = "none")
ggsave(file.path(unsup_out_dir, "Tharaka_Unsupervised_Cluster_Share_Trends.png"), p_unsup_cluster_share, width = 12, height = 8, dpi = 300, bg = "white")
print(p_unsup_cluster_share)

# Seasonal vegetation-index profile is especially useful for interpreting
# woody vegetation, grassland and cultivated areas with seasonal growth.
ndvi_signature_cols <- grep("^NDVI_", names(unsup_signatures), value = TRUE)
if (length(ndvi_signature_cols) > 0L) {
  unsup_ndvi_profile <- unsup_signatures %>%
    select(cluster, all_of(ndvi_signature_cols)) %>%
    pivot_longer(-cluster, names_to = "feature", values_to = "mean_ndvi") %>%
    mutate(window = stringr::str_remove(feature, "^NDVI_"),
           window = factor(window, levels = unsup_windows))
  p_unsup_ndvi <- ggplot(unsup_ndvi_profile, aes(x = window, y = mean_ndvi, colour = factor(cluster), group = cluster)) +
    geom_line(linewidth = 0.9) + geom_point(size = 2) +
    labs(title = "Seasonal NDVI signatures by cluster",
         subtitle = "Use alongside the cluster maps and reflectance signatures when naming clusters",
         x = "Seasonal window", y = "Mean NDVI", colour = "Cluster") +
    theme_minimal(base_size = 12)
  ggsave(file.path(unsup_out_dir, "Tharaka_Unsupervised_Cluster_NDVI_Signatures.png"), p_unsup_ndvi, width = 11, height = 6, dpi = 300, bg = "white")
  print(p_unsup_ndvi)
}

# Create a reviewable crosswalk once. The script will not overwrite your edits.
unsup_crosswalk_file <- file.path(unsup_out_dir, "Tharaka_Cluster_to_LandCover_Crosswalk.csv")
if (!file.exists(unsup_crosswalk_file)) {
  readr::write_csv(
    tibble::tibble(
      cluster = seq_len(unsup_k),
      land_cover = "",
      interpretation_notes = "Review map + seasonal spectral signatures before assigning"
    ),
    unsup_crosswalk_file
  )
  message("Created blank cluster-to-cover crosswalk: ", unsup_crosswalk_file)
}

unsup_crosswalk <- readr::read_csv(unsup_crosswalk_file, show_col_types = FALSE,
                                   col_types = cols(cluster = col_integer(), land_cover = col_character(), interpretation_notes = col_character()))
allowed_unsup_classes <- c("forest", "bushland", "grassland", "farmland", "settlement", "other_uncertain")
crosswalk_complete <- all(c("cluster", "land_cover") %in% names(unsup_crosswalk)) &&
  setequal(unsup_crosswalk$cluster, seq_len(unsup_k)) &&
  all(!is.na(unsup_crosswalk$land_cover)) &&
  all(nzchar(trimws(unsup_crosswalk$land_cover))) &&
  all(tolower(trimws(unsup_crosswalk$land_cover)) %in% allowed_unsup_classes)

if (crosswalk_complete) {
  unsup_crosswalk <- unsup_crosswalk %>% mutate(land_cover = tolower(trimws(land_cover)))
  unsup_named_area <- unsup_cluster_area %>%
    left_join(unsup_crosswalk %>% select(cluster, land_cover), by = "cluster") %>%
    group_by(year, land_cover) %>%
    summarise(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop")
  named_classes <- sort(unique(unsup_crosswalk$land_cover))
  unsup_named_area <- tidyr::crossing(year = unsup_years, land_cover = named_classes) %>%
    left_join(unsup_named_area, by = c("year", "land_cover")) %>%
    mutate(area_ha = tidyr::replace_na(area_ha, 0)) %>%
    group_by(year) %>%
    mutate(mapped_area_ha = sum(area_ha), share_percent = if_else(mapped_area_ha > 0, 100 * area_ha / mapped_area_ha, NA_real_)) %>%
    ungroup() %>% arrange(year, land_cover)

  readr::write_csv(unsup_named_area, file.path(unsup_out_dir, "Tharaka_Image_Learned_LandCover_Area_Trends.csv"))
  p_unsup_named_area <- ggplot(unsup_named_area, aes(x = year, y = area_ha, colour = land_cover, group = land_cover)) +
    geom_line(linewidth = 1) + geom_point(size = 2.5) +
    facet_wrap(~ land_cover, scales = "free_y", ncol = 3) +
    scale_x_continuous(breaks = unsup_years) +
    labs(title = "Image-learned land-cover area trends in Tharaka Sub-county",
         subtitle = "Cluster names were assigned through the reviewed cluster crosswalk",
         x = "Year", y = "Area (ha)", colour = "Land cover") +
    theme_minimal(base_size = 12) + theme(legend.position = "none")
  ggsave(file.path(unsup_out_dir, "Tharaka_Image_Learned_LandCover_Area_Trends.png"), p_unsup_named_area, width = 12, height = 8, dpi = 300, bg = "white")
  print(p_unsup_named_area)

  p_unsup_named_share <- ggplot(unsup_named_area, aes(x = year, y = share_percent, colour = land_cover, group = land_cover)) +
    geom_line(linewidth = 1) + geom_point(size = 2.5) +
    facet_wrap(~ land_cover, scales = "free_y", ncol = 3) +
    scale_x_continuous(breaks = unsup_years) +
    labs(title = "Image-learned land-cover composition trends in Tharaka Sub-county",
         subtitle = "Percentages are relative to each year's valid mapped area",
         x = "Year", y = "Mapped area (%)", colour = "Land cover") +
    theme_minimal(base_size = 12) + theme(legend.position = "none")
  ggsave(file.path(unsup_out_dir, "Tharaka_Image_Learned_LandCover_Share_Trends.png"), p_unsup_named_share, width = 12, height = 8, dpi = 300, bg = "white")
  print(p_unsup_named_share)
  message("Named class trends completed using the reviewed crosswalk.")
} else {
  message(
    "Cluster-area and cluster-share plots are ready. To produce named land-cover trends, open ",
    unsup_crosswalk_file,
    ", assign each cluster one of: ", paste(allowed_unsup_classes, collapse = ", "),
    ", save it, and rerun Section 38. Multiple clusters may share the same land-cover label."
  )
}

cat("\nImage-led clustering outputs saved under: ", normalizePath(unsup_out_dir, winslash = "/"), "\n", sep = "")
cat("Cluster area table: ", normalizePath(file.path(unsup_out_dir, "Tharaka_Unsupervised_Cluster_Area_Trends.csv"), winslash = "/"), "\n", sep = "")
cat("Crosswalk to review: ", normalizePath(unsup_crosswalk_file, winslash = "/"), "\n", sep = "")
}

```

## Interpretation and validation note

This is an exploratory, image-derived classification. Cluster trends are quantitative patterns in spectral/seasonal groups; they become named land-cover trends only after the crosswalk has been reviewed against the imagery. A crosswalk reviewed by the analyst is not an independent accuracy assessment. For stronger reporting, validate mapped classes with a separate, year-appropriate sample and report class-specific accuracy and area uncertainty. Compare scene quality, cloud/no-data coverage and mapped-area totals before interpreting a change as real land-cover conversion.
