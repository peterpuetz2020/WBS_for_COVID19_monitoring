df <- readRDS(here(data_here, "df_ready.rds"))

filtereddata <- filtering_and_selecting(
  df,
  # if different number of observations, you can set this to true
  # to drop rows with NAs - then we have the same observation period
  # for all surveillance syytems
  same_time_frame = TRUE,
  starting_date = lubridate::dmy("01072022"),
  interpolate = FALSE
)

filtereddata_interpolated <- filtering_and_selecting(
  df,
  # if different number of observations, you can set this to true
  # to drop rows with NAs - then we have the same observation period
  # for all surveillance syytems
  same_time_frame = TRUE,
  starting_date = lubridate::dmy("01072022"),
  interpolate = TRUE
)

# compute shedding load distribution
sld_data <- compute_sld()

# calculate derived prevalance data
der_prevalence_data <- calc_prev(filtereddata_interpolated, weights = sld_data %>%
                                         drop_na() %>%
                                         select(weights = gam_value) %>%
                                         pull(weights) )

# compute translation factors
tf_data <- prepare_translation_factor_data(
  filtereddata,
  samples_removed = 5)

tf_data_fd <- prepare_translation_factor_data(
  filtereddata,
  changes = TRUE,
  samples_removed = 5
)

trend_data <- prepare_trend_data(
  df = tf_data,
  perc_change_threshold = perc_change_threshold
)

# compute retrospective alignment data
historical_alignment_data <- tab_full(df = trend_data, vars = c("viruslast_loess", "truth"))

# compute real-time alignment data
real_time_alignment_data <- tab_full(df = trend_data, vars = c("viruslast_ma_2", "truth"))

# compute ml models
ml_results <- compute_ml_models(df = trend_data,
                  form = as.formula(truth_compact ~ viruslast + viruslast_lag_1 +
                                      viruslast_lag_2 +
                                      viruslast_ma_2 +
                                      viruslast_ma_2_lag_1+
                                      viruslast_ma_2_lag_2),
                  models = c("multinom", "svm", "rf"),
                  outer_folds = 10,
                  inner_folds = 10,
                  avoid_overlap = 4,
                  metric_used = "Accuracy",
                  seed = 1)

prediction_data <- trend_data %>%
  add_column(ml_prediction = ml_results$predictions_results$multinom)




