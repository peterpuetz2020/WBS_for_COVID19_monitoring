# create single curves in one plot (Figure 1)
plot_indicators(tf_data)

# plot cross correlations for raw data (Figure 2)
plot_cc_pearson(filtereddata,
                changes = FALSE,
                # set range of lags for cross-correlations
                range = 4,
                # set data_type only for data file name
                data_type = "original_data")

# plot shedding load distribution (Figure 3)
plot_sld(sld_data)

# plot derived prevalence curves (Figure 4 and Appendix Figures A1, A2, A3)
plot_der_prevalence(df = der_prevalence_data)

# plot cross-correlations for derived prevalence data (Appendix Figure A4)
plot_cc_pearson(der_prevalence_data  %>%
                  select(-all_of(neat_names_no_ww)) %>%
                  rename_with(~ gsub("_vl", "", .)) %>%
                  select(kalenderwoche = Date, everything()),
                data_type = "derived_prev_data")

# plot translation factors (Figure 5)
create_translation_factor_graph(tf_data)

# plot regression graph (Figure 6)
create_regression_graph(df = filtereddata)

# plot changes in smoothed values (Figure 7)
plot_changes(tf_data_fd)

# plot cross correlations for changes of smoothed data (Figure 8)
plot_cc_pearson(tf_data,
                changes = TRUE,
                # set range of lags for cross-correlations
                range = 4,
                # set data_type only for data file name
                data_type = "smoothed_data")

# retrospective contingency table (Table 1)
save_confusion_matrix(df = historical_alignment_data, name = "table_1")

# plot alignment over time (Figure 9)
plot_correspondence_over_time(df = trend_data)

# read in ml results
ml_results <- readRDS(here(data_here, "ml_results.rds"))

# inner cross-validation results for model selection (Table A3)
save_inner_cv_results(results = ml_results)

# contingency table for best ML model: use ml_results and save_confusion_matrix function (Table 2)
save_outer_cv_results(results = ml_results)

