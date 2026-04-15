# Required Libraries
library(lme4)        # For mixed models
library(tidyverse)   # Data manipulation and visualization
library(vegan)       # Multivariate analyses
library(mgcv)        # GAMs for temporal analysis
library(spatstat)    # Spatial statistics
library(spdep)       # Spatial dependence
library(phenology)   # Phenological analyses

#------------------------------------------------------------------------------
# 1. DISTRIBUTION PATTERNS
#------------------------------------------------------------------------------

# Required Data Structure:
# surveys_df structure:
# site_id: character - unique identifier for each site
# date: Date - survey date
# time: numeric - time of survey (24hr format)
# elevation: numeric - meters above sea level
# slope: numeric - percent slope
# aspect: numeric - degrees from north
# habitat_type: factor - CSS, RIP, GRS, or WDE
# temperature: numeric - Celsius
# wind_speed: numeric - mph
# cloud_cover: numeric - percent
# bee_count: numeric - number of B. crotchii observed
# caste: factor - queen, worker, male
# survey_id: character - unique survey identifier

# 1.1 GLMM for Bee Abundance
abundance_model <- function(surveys_df) {
  # Convert aspect to northness and eastness
  surveys_df <- surveys_df %>%
    mutate(
      northness = cos(aspect * pi/180),
      eastness = sin(aspect * pi/180)
    )
  
  # Fit GLMM with Poisson distribution
  model_abundance <- glmer(bee_count ~ elevation + slope + northness + eastness +
                             habitat_type + temperature + wind_speed +
                             (1|site_id) + (1|date),
                           family = poisson,
                           data = surveys_df)
  
  return(model_abundance)
}

# 1.2 Habitat Type Analysis
habitat_analysis <- function(surveys_df) {
  # Aggregate data by habitat type
  habitat_summary <- surveys_df %>%
    group_by(habitat_type, site_id) %>%
    summarize(
      mean_abundance = mean(bee_count),
      sd_abundance = sd(bee_count),
      total_observations = n()
    )
  
  # ANOVA for habitat differences
  habitat_model <- aov(mean_abundance ~ habitat_type, data = habitat_summary)
  
  return(list(summary = habitat_summary, model = habitat_model))
}

#------------------------------------------------------------------------------
# 2. PHENOLOGICAL RELATIONSHIPS
#------------------------------------------------------------------------------

# Required Data Structure:
# phenology_df structure:
# site_id: character - unique identifier for each site
# date: Date - survey date
# elevation_band: factor - low, mid, high
# species: character - plant species name
# flower_count: numeric - number of flowers
# bee_visits: numeric - number of bee visits to this species
# temperature: numeric - Celsius

# 2.1 Phenological Matching Analysis
phenology_analysis <- function(phenology_df) {
  # Create daily summaries
  daily_activity <- phenology_df %>%
    group_by(date, elevation_band) %>%
    summarize(
      total_flowers = sum(flower_count),
      total_bees = sum(bee_visits)
    )
  
  # Fit GAM to examine temporal patterns
  gam_model <- gam(total_bees ~ s(as.numeric(date)) + 
                     elevation_band + s(total_flowers),
                   family = poisson,
                   data = daily_activity)
  
  return(gam_model)
}

# 2.2 Elevation Band Cross-correlation
elevation_correlation <- function(phenology_df) {
  # Create time series by elevation band
  ts_by_elevation <- phenology_df %>%
    group_by(date, elevation_band) %>%
    summarize(daily_bees = sum(bee_visits)) %>%
    pivot_wider(names_from = elevation_band,
                values_from = daily_bees)
  
  # Calculate cross-correlations
  ccf_low_mid <- ccf(ts_by_elevation$low, ts_by_elevation$mid, lag.max = 30)
  ccf_mid_high <- ccf(ts_by_elevation$mid, ts_by_elevation$high, lag.max = 30)
  
  return(list(low_mid = ccf_low_mid, mid_high = ccf_mid_high))
}

#------------------------------------------------------------------------------
# 3. SPATIAL ANALYSES
#------------------------------------------------------------------------------

# Required Data Structure:
# spatial_df structure:
# site_id: character - unique identifier for each site
# longitude: numeric - decimal degrees
# latitude: numeric - decimal degrees
# elevation: numeric - meters above sea level
# slope: numeric - percent
# aspect: numeric - degrees from north
# habitat_type: factor - CSS, RIP, GRS, or WDE
# total_bees: numeric - total B. crotchii observed
# flower_richness: numeric - number of flowering species

# # 3.1 Spatial Autocorrelation
spatial_autocorr <- function(spatial_df) {
  # Create spatial weights matrix
  coords <- spatial_df %>%
    dplyr::select(longitude, latitude) %>%
    as.matrix()

  nb <- dnearneigh(coords, 0, 1000)  # Neighbors within 1km
  weights <- nb2listw(nb, style = "W")

  # Calculate Moran's I
  moran_test <- moran.test(spatial_df$total_bees, weights)

  return(moran_test)
}
# 
# # 3.2 Species Distribution Model
distribution_model <- function(spatial_df) {
  # Prepare environmental variables
  spatial_df <- spatial_df %>%
    mutate(
      northness = cos(aspect * pi/180),
      eastness = sin(aspect * pi/180)
    )

  # Fit GAM for species distribution
  sdm <- gam(total_bees ~ s(elevation, k=5) +
               s(slope, k=5) +
               s(northness, eastness, k=5) +
               habitat_type +
               s(flower_richness, k=5),
             family = poisson,
             data = spatial_df)

  return(sdm)
}

#------------------------------------------------------------------------------
# 4. VISUALIZATION FUNCTIONS
#------------------------------------------------------------------------------

# 4.1 Plot temporal patterns by elevation
plot_temporal_patterns <- function(phenology_df) {
  ggplot(phenology_df, aes(x = date, y = bee_visits, color = elevation_band)) +
    geom_smooth(method = "gam") +
    geom_point(alpha = 0.3) +
    facet_wrap(~species, scales = "free_y") +
    theme_bw() +
    labs(x = "Date", y = "Number of Bee Visits",
         color = "Elevation Band")
}

# 4.2 Plot spatial distribution
plot_spatial_distribution <- function(spatial_df) {
  ggplot(spatial_df, aes(x = longitude, y = latitude)) +
    geom_point(aes(size = total_bees, color = elevation)) +
    facet_wrap(~habitat_type) +
    scale_color_viridis_c() +
    theme_bw() +
    labs(size = "Total Bees",
         color = "Elevation (m)")
}

#------------------------------------------------------------------------------
# 5. DATA VALIDATION FUNCTIONS
#------------------------------------------------------------------------------

validate_survey_data <- function(surveys_df) {
  # Check for missing values
  missing_data <- colSums(is.na(surveys_df))
  
  # Check for impossible values
  impossible_values <- surveys_df %>%
    filter(
      temperature < 0 | temperature > 45 |
        wind_speed < 0 | wind_speed > 50 |
        cloud_cover < 0 | cloud_cover > 100 |
        elevation < 0 | elevation > 200 |
        slope < 0 | slope > 100 |
        aspect < 0 | aspect > 360
    )
  
  # Check for duplicate surveys
  duplicates <- surveys_df %>%
    group_by(site_id, date) %>%
    filter(n() > 1)
  
  return(list(
    missing = missing_data,
    impossible = impossible_values,
    duplicates = duplicates
  ))
}