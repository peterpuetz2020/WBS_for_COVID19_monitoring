# read in data
df <- readRDS(here(data_here, "df_ready.rds"))

# filter time series data such that all series have no missing values
filtereddata <- filtering_and_selecting(
  df,
  same_time_frame = TRUE,
  starting_date = lubridate::dmy("01072022"),
  interpolate = FALSE
)

# filter time series data such that all series have no missing values and
# interpolate such that daily data is available (necessary for computation of
# hypothetical prevalence data from shedding load distribution)
filtereddata_interpolated <- filtering_and_selecting(
  df,
  same_time_frame = TRUE,
  starting_date = lubridate::dmy("01072022"),
  interpolate = TRUE
)

# compute shedding load distribution
sld_data <- compute_sld()

# calculate hypothetical prevalance data
der_prevalence_data <- calc_prev(filtereddata_interpolated, weights = sld_data %>%
                                         drop_na() %>%
                                         select(weights = gam_value) %>%
                                         pull(weights) )

# compute translation factors for original time series
tf_data <- prepare_translation_factor_data(
  filtereddata,
  samples_removed = 5)

# compute translation factors for changes of smoothed time series
tf_data_fd <- prepare_translation_factor_data(
  filtereddata,
  changes = TRUE,
  samples_removed = 5
)

# compute categorized trends
trend_data <- prepare_trend_data(
  df = tf_data,
  perc_change_threshold = perc_change_threshold
)

# compute retrospective agreement of changes of smoothed data
historical_alignment_data <- tab_full(df = trend_data, vars = c("viruslast_loess", "truth"))

# compute machine learning models (takes some time, you can also skip this step)
ml_results <- compute_ml_models(df = trend_data,
                  form = as.formula(truth_compact ~ viruslast + viruslast_lag_1 +
                                      viruslast_lag_2 +
                                      viruslast_ma_2 +
                                      viruslast_ma_2_lag_1+
                                      viruslast_ma_2_lag_2),
                  outer_folds = 10,
                  inner_folds = 10,
                  avoid_overlap = 4,
                  seed = 1)

# save ml results (do this only of you have executed the previous command)
saveRDS(ml_results, here(data_here, "ml_results.rds"))
