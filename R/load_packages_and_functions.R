# install (if necessary) and load packages
packages_to_load <- c(
  "cli",
  "rlang",
  "dplyr",
  "tidyr",
  "tibble",
  "lubridate",
  "stringr",
  "mgcv",
  "rio",
  "purrr",
  "padr",
  "cowplot",
  "flextable",
  "matrixStats",
  "zoo",
  "kableExtra",
  "xtable",
  "here",
  "glmnet",
  "caret",
  "ranger",
  "ggplotify",
  "extrafont",
  "ggplot2",
  "kernlab"
)
install_and_load(packages_to_load)

# do only once, only necessary for nice plots, but code runs also
# without executing this command
# font_import(prompt = FALSE)
loadfonts(device = "all")

# change the months on the x-axis to english months
Sys.setlocale("LC_TIME", "C")

# defining plotting themes
theme_trend <- function(base_size = 12,
                        base_family = "Times New Roman") {
  theme(
    strip.background = element_blank(),
    strip.text = element_text(family = base_family, size = base_size),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = 'white'),
    text = element_text(family = base_family, size = base_size),
    axis.text = element_text(family = base_family, size = base_size),
    axis.title = element_text(family = base_family, size = base_size),
    plot.title = element_text(family = base_family, size = base_size),
    legend.text = element_text(family = base_family, size = base_size)
  )
}

# set untidy names as given in dataset
ugly_names <- c("gw_sr",
                "gw_vpr",
                "viruslast",
                "seed_covidare",
                "inzidenz_7_tage")

# set tidy names
neat_names_no_ww <- c("GNS-I", "PC-COVID-ARI-I", "PS-SR-I", "PS-VPR-I")
neat_names <- c("GNS-I", "PC-COVID-ARI-I", "PS-SR-I", "PS-VPR-I",  "WW-VL")

# assign colors for neat names
colors_used_neat_names <- c(
  "GNS-I"  = "#CC79A7",
  "PC-COVID-ARI-I" = "#F0E442",
  "PS-SR-I" = "#56B4E9",
  "PS-VPR-I" = "#009E73",
  "WW-VL" = "#E69F00"
)
add_color_manual_neat_names <- function() {
  scale_color_manual(values = colors_used_neat_names)
}
add_color_manual_neat_names_no_ww <- function() {
  scale_fill_manual(values = colors_used_neat_names[neat_names_no_ww])
}

# same for untidy names
colors_used_ugly_names <- c(
  "inzidenz_7_tage"  = "#CC79A7",
  "seed_covidare" = "#F0E442",
  "gw_sr" = "#56B4E9",
  "gw_vpr" = "#009E73",
  "viruslast" = "#E69F00"
)
add_color_manual_ugly_names <- function() {
  scale_color_manual(
    values = colors_used_ugly_names,
    breaks = names(colors_used_ugly_names),
    labels =
      neat_names
  )
}

# similar for colors without wastewater
colors_prev_plot <- rbind(colors_used_neat_names[-5],
                          c("#7A3E5B", "#A68F1B", "#2A6A9A", "#005B4D"))

add_color_manual_uef <- function() {
  scale_color_manual(
    values = as.character(colors_used_ugly_names)[1:4],
    breaks =  c(
      "UEF_WW_inzidenz_7_tage_SM",
      "UEF_WW_COVARE_SM",
      "UEF_WW_GW_SR_SM",
      "UEF_WW_GW_VPR_SM"
    ),
    labels =  neat_names_no_ww,
  )
}

## Templates Setup
# correlation table default font
set_flextable_defaults(font.family = "Times New Roman",
                       font.size = 7,
                       width = .9)

# change category definition: how much percentage points increase/decrease required?
perc_change_threshold <- 0.05

## Setting up functions
# function that changes untidy to tidy variable names. df: input data
rename_neat <- function(df = NULL)
{
  # check if untidy variable names available
  if ("gw_vpr" %in% names(df))
    df %>%
    # rename variable names to neat ones
    rename_at(vars(all_of(
      c(
        "inzidenz_7_tage",
        "seed_covidare",
        "gw_sr",
        "gw_vpr",
        "viruslast"
      )
    )), ~   neat_names)
  else
    df
}


# contingency table. df: input data, vars: two variables to be compared
tab_full <- function(df = NULL,
                     vars = c("viruslast_ma_2", "truth")) {
  # Define the common levels for both variables
  common_levels <- c("decreasing", "increasing", "no_change", "unclear")
  # Create a contingency table
  contingency_table <- table(
    # Rows
    factor(df %>% pull(vars[1]), levels = common_levels),
    # Columns
    factor(df %>% pull(vars[2]), levels = common_levels)
  )
  # Convert to a matrix for better formatting
  contingency_table <- as.matrix(contingency_table)
  # Add row and column names explicitly
  rownames(contingency_table) <- common_levels
  colnames(contingency_table) <- common_levels
  # Remove rows with a sum of zero
  contingency_table <- contingency_table[rowSums(contingency_table) != 0, ]
  # View the transposed table
  return(t(contingency_table))
}

# block sampling. data: input data, n_splits: number of blocks,
# avoid_overlap: distance/removed values between training and test/validation set
block_cv <- function(data, n_splits, avoid_overlap = 0) {
  # Get full sequence of indices
  max_index <- nrow(data)
  all_indices <- as.integer(seq(1, max_index, 1))
  # Split sequence all_indices in n_splits equal parts
  list_validation <- split(all_indices, cut(seq_along(all_indices), n_splits, labels = FALSE))
  # Add gaps accounting for overlap to validation indices, i.e. extend indices
  extend_func <- function(i, test = list_validation) {
    as.integer(seq(max(c(
      1, min(test[[i]]) - avoid_overlap
    )), min(c(
      max_index, max(test[[i]]) + avoid_overlap
    )), 1))
  }
  list_extend <- lapply(1:n_splits, extend_func)
  # Drop extended indices from sequence of all indices to get training sets
  # without overlap with validation sets
  dropping_func <- function(i, list_ext = list_extend) {
    as.integer(all_indices[!all_indices %in% list_ext[[i]]])
  }
  list_train <- lapply(1:n_splits, dropping_func)
  # Generate and return list containing validation and training indices
  final_list <- list("validation" = list_validation, "training" = list_train)
  return(final_list)
}

# function to filter data and select variables. df: input data,
# starting_date: starting date of analysis, same_time_frame: drops rows with at
# least one NA, interpolate: should time series be interpolated to obtain daily data
filtering_and_selecting <- function(df = NULL,
                                    starting_date = lubridate::dmy("01072022"),
                                    same_time_frame = FALSE,
                                    interpolate = TRUE) {
  df <- df %>%
    # Set beginning of analysis period
    dplyr::filter(kalenderwoche  > starting_date) %>%
    # Select relevant variables
    select(kalenderwoche, all_of(ugly_names))
  
  # interpolate if required
  if (interpolate)
    df <- df %>%
      pad(by = "kalenderwoche", interval = "day") %>%
      mutate(across(where(is.numeric), ~ exp(na.approx(log(
        .
      ), na.rm = FALSE))))
  
  # drop NA rows if required to get same no. of observations for all time series
  if (same_time_frame)
    df %>%
      drop_na()
  else
    df
}

# function to generate the data with week-to-weeks changes. Also, input
# variables (features) for the machine learning task are generated.
# df: input data, perc_change_threshold: how much percentage points increase/decrease
# required for categorization of week-to-week changes?
prepare_trend_data <- function(df = NULL,
                               perc_change_threshold = 0.05) {
  df_change <- df %>%
    # drop irrelevant variables
    select(-contains("UEF")) %>%
    # drop NA data between measurements
    filter(!is.na(viruslast)) %>%
    # add moving average for wastewater viral load
    mutate(viruslast_ma_2 = rollapply(
      viruslast,
      width = 2,
      FUN = function(x)
        sum(x * c(1 / 2, 1 / 2)),
      fill = NA,
      align = "right"
    )) %>%
    # save smoothed viral load under different name
    mutate(viruslast_smoothed = viruslast_loess) %>%
    # compute changes for gam smoothed data and moving average smoothed ww data
    mutate(across(contains("loess") |
                    contains("_ma") , ~ . / lag(.))) %>%
    # add lags for (unsmoothed and smoothed) wastewater data
    mutate(
      viruslast_lag_1 = lag(viruslast),
      viruslast_lag_2 = lag(viruslast, 2),
      viruslast_ma_2_lag_1 = lag(viruslast_ma_2, 1),
      viruslast_ma_2_lag_2 = lag(viruslast_ma_2, 2),
    ) %>%
    # drop NA values generated by lags
    drop_na()
  
  # build trends
  df_trend <- df_change  %>%
    mutate(across(
      c(contains("loess"), contains("_ma")),
      ~ case_when
      (
        between(., 1 - perc_change_threshold, 1 + perc_change_threshold) ~ "no_change",
        . < 1 - perc_change_threshold ~ "decreasing",
        . > 1 + perc_change_threshold ~ "increasing"
      )
    ))
  
  # count trends over incidence systems
  correspondence <- df_trend  %>%
    # drop other variables
    select(-contains("viruslast")) %>%
    # compute number of agreeing systems
    rowwise() %>%
    mutate(same_count = max(table(c_across(
      where(is.character)
    )), na.rm = TRUE),
    # store category where most systems agree on
    major_cat = names(sort(table(c_across(
      where(is.character)
    )), decreasing = TRUE)[1])) %>%
    ungroup() %>%
    # set category to "unclear" if less than 3 systems agree
    mutate(
      truth = case_when(same_count >= 3 ~ major_cat, .default = "unclear"),
      # combine "unclear" and "no_change" to "other"
      truth_compact = ifelse(truth == "no_change" |
                               truth == "unclear", "other", truth)
    )
  
  # merge this data with trend data
  temp <- correspondence %>%
    left_join(df_trend %>% select(kalenderwoche, viruslast, contains("viruslast"))) %>%
    # drop NAs
    drop_na() %>%
    # change categorical character variables to factor
    mutate(across(contains("truth"), ~ as.factor(.)))
  # return data
  temp
}

# function to plot pearson cross correlation
# df: input data, changes: should weekly changes be considered,
# range: how many lags should be shown,
# data_type: what kind if input data is provided
plot_cc_pearson <- function(df = NULL,
                            changes = FALSE,
                            range = 4,
                            data_type = c("original_data", "derived_prev_data", "smoothed_data")) {
  # if smoothed data should be used, select the corresponding columns and rename them
  if (data_type == "smoothed_data")
  {
    df <- df %>%
      select(kalenderwoche, contains("loess")) %>%
      rename_with(~ gsub("_loess", "", .))
  }
  
  # set name of file that will be stored below
  if (data_type == "smoothed_data")
    figure_name <- "figure_8"
  else
    if (data_type == "original_data")
      figure_name <- "figure_2"
    else
      if (data_type == "derived_prev_data")
        figure_name <- "figure_A4"
      
      # select only one value per week to ensure weekly lag calculation
      df <-  df %>%
        filter(kalenderwoche == min(kalenderwoche) |
                 as.numeric((kalenderwoche - min(kalenderwoche))) %% 7 == 0)
      
      # select only numeric variables, i.e. the indicators
      df_num <- df %>%
        select_if(is.numeric) %>%
        # sort alphabetically
        select(order(names(.)))
      
      # should first differences be calculated
      if (changes)
      {
        df_num <- df_num %>%
          # compute changes
          mutate_all(~ . / lag(.)) %>%
          # Drop rows where all values are NA
          filter(!if_all(everything(), is.na))
      }
      
      # rename variable names
      df_num <- df_num %>%
        rename_neat()
      
      # extract the new variable names
      varnames_neat <- df_num %>%
        colnames()
      
      # List of series to compare with with wastewater viral load
      series_to_compare <- varnames_neat[varnames_neat != "WW-VL"]
      
      # Function to compute cross-correlation with wastewater viral load that returns a data frame
      compute_ccf <- function(series_name) {
        ccf_result <- ccf(df_num[, "WW-VL"], df_num[[series_name]], plot = FALSE, na.action = na.pass)
        
        # Create a data frame with lags, correlations and series which is correlated with wastewater
        data.frame(lag = ccf_result$lag,
                   correlation = ccf_result$acf,
                   series = series_name) %>%
          # arrange by series name
          arrange(series)
      }
      
      # Compute cross-correlations for all series and bind results into one data frame
      ccf_results <- map_dfr(series_to_compare, compute_ccf)
      # show only a certain number weeks / days (set as function input)
      ccf_results <- ccf_results %>%
        filter(between(lag, -range, range))
      
      # Identify the maximum correlation and corresponding lag for each series
      max_ccf <- ccf_results %>%
        group_by(series) %>%
        dplyr::slice(which.max(correlation)) %>%
        ungroup()
      
      # extract the correlation values
      y_vals_in_range <- ccf_results %>%
        pull(correlation)
      
      # extract the lags
      valid_lags <- ccf_results %>% pull(lag)
      
      # rounding functions such that unpretty values on the y-axis are avoided
      round_down <- function(x, base = 0.2)
        base * floor(x / base)
      round_up <- function(x, base = 0.2)
        base * ceiling(x / base)
      y_min_valid <- round_down(min(y_vals_in_range) - 0.05)
      y_max_valid <- round_up(ceiling(max(y_vals_in_range)))
      
      # Create a bar plot of cross-correlations
      cor_plot <- ggplot(ccf_results, aes(x = factor(lag), y = correlation, fill = series)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Lag in weeks", y = "Cross-Correlation") +
        theme_trend() +
        scale_x_discrete(breaks = levels(factor(ccf_results$lag))[seq(1, length(levels(factor(ccf_results$lag))), by = 1)]) +
        facet_wrap(~ series, ncol = 2, scales = "fixed") +
        add_color_manual_neat_names_no_ww() +
        geom_text(
          data = max_ccf,
          aes(label = round(correlation, 2), y = correlation + 0.1),
          size = 4.2175,
          color = "black",
          family = "Times New Roman"
        ) +
        scale_y_continuous(
          breaks = seq(y_min_valid, y_max_valid, by = 0.2),
          expand = c(0.025, 0.035)
        )
      print(cor_plot)
      
      # save plot
      ggsave(
        here(results_here_plots, paste0(figure_name, ".svg")),
        width = 16,
        height = 10,
        units = "cm", create.dir = TRUE
      )
}

# function to compute shedding load distribution
compute_sld <- function() {
  # read in data
  df <- import(here(data_here, "sld.xlsx"),
               sheet = "pooled",
               setclass = "tibble")
  
  # combine wannigama data for different variants
  df <- df %>%
    group_by(study, days) %>%
    # build weighted median over different variants
    mutate(value = weightedMedian(Median, w = N), n = sum(N)) %>%
    ungroup() %>%
    # drop redundant rows for wannigama
    distinct(study, days, .keep_all = T) %>%
    # determine same time frame
    filter(between(days, 0, 21)) %>%
    # drop irrelavant variables
    select(-type, -Median)
  
  # interpolate wannigama data
  df_int <- df %>%
    group_by(study) %>%
    # first extend data for the days without measurements
    complete(days = seq(min(days), max(days), by = 1)) %>%
    fill(n, N, .direction = "downup") %>%
    # exponential interpolation
    mutate(value = exp(na.approx(log(value)))) %>%
    # normalize data to make them comparable between studies
    mutate(across(contains("value"), ~ (. - min(., na.rm = TRUE)) /
                    (max(., na.rm = TRUE) - min(., na.rm = TRUE)))) %>%
    ungroup()
  
  # aggregate over studies
  df_agg <- df_int %>%
    group_by(days) %>%
    # compute weighted median over studies
    summarise(across(
      contains("value"),
      .fns = list(w_agg = ~ weightedMedian(., w = n)),
      .names = "{fn}_{col}"
    )) %>%
    # scale data
    mutate(across(contains("value"), ~ . * 100))
  
  # compute gam smoother
  mod <- gam(w_agg_value ~ s(days, k = 20), data = df_agg)
  
  # compute intermediate values (for plotting purposes only)
  # set sequence with small steps
  temp <- data.frame(days = seq(min(df_agg$days), max(df_agg$days), by = 0.001))
  # compute predictions for this sequence
  df_gam <- cbind(temp, gam_value = predict(mod, newdata = temp)) %>%
    as_tibble() %>%
    # negative values not possible - ensure values >= 0
    mutate(gam_value = ifelse(gam_value < 0, 0, gam_value)) %>%
    # merge with initial data
    left_join(df_agg)
  
  # return data
  df_gam
}

# function to plot shedding load distribution
# df: input shedding load distribution
plot_sld <- function(df = NULL) {
  # plot shedding load distribution
  sld_plot <- df %>%
    ggplot() +
    geom_point(aes(days, w_agg_value)) +
    geom_line(aes(days, gam_value), linewidth = 1) +
    theme_trend() +
    lims(y = c(0, 80)) +
    labs(x = "Days post symptom onset" , y = "Standardized viral load (in %)")
  print(sld_plot)
  
  #save plot
  ggsave(
    here(results_here_plots, "figure_3.svg"),
    width = 16,
    height = 10,
    units = "cm", create.dir = TRUE
  )
}

# function to calculate hypothetical prevalence data from incidence
# df: input data, weights: weights derived from shedding load distribution
calc_prev <- function(df = NULL, weights = NULL) {
  # store length of weights (days after symptom onset with viral load in stool)
  # and no. of rows of data set
  m <- length(weights)
  n_rows <- nrow(df)
  
  # rename variable names to tidy names
  df <- df %>%
    rename_neat()
  
  # placeholder tibble for results
  dat_vl <-
    df %>% select(kalenderwoche)
  
  # for each incidence indicator
  for (var_name in neat_names_no_ww)
  {
    # store data frame
    data <- df
    # for each day of viral load in stool
    for (i in 0:(m - 1))
      data  <- data  %>%
        # build lag variables
        mutate(!!paste0(var_name, "_lag_", i) := lag(get(var_name), i))
    
    # create new variable that is the cross-product of the lag variables
    # and the corresponding weights
    dat_vl  <- dat_vl  %>%  add_column(
      !!paste0(var_name, "_vl") := data %>%
        select(contains("_lag_")) %>%
        as.matrix() %>%
        t() %>%
        crossprod(weights) %>%
        as.numeric()
    )
  }
  
  # merge with interpolated data
  plot_data <- dat_vl %>%
    left_join(df) %>%
    mutate_if(is.numeric, ~ as.numeric(scale(., scale = T))) %>%
    rename(Date = "kalenderwoche")
  
  # return data
  plot_data
}

# function to plot hypothetical prevalences
plot_der_prevalence <- function(df = NULL)
{
  # for each incidence indicator
  for (var_name in neat_names_no_ww)
  {
    # plot hypothetical prevalence curve
    p <- df  %>%
      select(Date, contains(var_name), `WW-VL`) %>%
      gather(Indicator, Value, -Date) %>%
      ggplot(aes(Date, Value, color = Indicator)) +
      geom_line(linewidth = 1.2) +
      scale_color_manual(
        values = c(colors_prev_plot[, var_name], "#E69F00"),
        name = "",
        breaks = c(var_name, paste0(var_name, "_vl"), "WW-VL"),
        labels = c(var_name, paste0(var_name, " derived prevalence"), "WW-VL")
      ) +
      theme_trend() +
      scale_x_date(date_breaks = "2 month", date_labels = "%b\n%y") +
      labs(y =  "Standardized value")
    print(p)
    
    # set figure name
    if (var_name == "GNS-I")
      figure_name <- "A1"
    else
      if (var_name == "PS-SR-I")
        figure_name <- "4"
    else
      if (var_name == "PS-VPR-I")
        figure_name <- "A2"
    else
      if (var_name == "PC-COVID-ARI-I")
        figure_name <- "A3"
    
    # save plot
    ggsave(
      here(
        results_here_plots,
        paste0("figure_", figure_name, ".svg")
      ),
      width = 16,
      height = 10,
      units = "cm", create.dir = TRUE
    )
  }
}

# function to smooth time series and compute translation factors
# df: input data, changes: should changes of smoothed time series be considered,
# sampled_removed: how many values at the boundaries of the time scale are removed
# (due to smoothing and inaccuracies of trends at the boundaries)
prepare_translation_factor_data <- function(df = NULL,
                                            changes = FALSE,
                                            samples_removed = 0) {
  df <- df %>%
    # add days between weekly measurements
    pad(interval = "day") %>%
    # create new time variables and an index variable
    mutate(
      woche = week(kalenderwoche),
      jahr = year(kalenderwoche),
      obs = row_number()
    )
  
  df <- df %>%
    # log-transform data
    mutate(
      across(all_of(
        c(
          "gw_sr",
          "gw_vpr",
          "viruslast",
          "seed_covidare",
          "inzidenz_7_tage"
        )
      ), ~ log10(.)),
      # compute gam smoothing
      across(all_of(
        c(
          "gw_sr",
          "gw_vpr",
          "viruslast",
          "seed_covidare",
          "inzidenz_7_tage"
        )
      ), ~ {
        model <- gam(as.formula(paste(
          cur_column(), "~ s(obs,
                                                               bs = 'ad', k = 60)"
        )))
        10 ^ predict(model, newdata = data.frame(obs = df$obs))
      }, .names = "{col}_loess"),
      # backtransform to original scale
      across(all_of(
        c(
          "gw_sr",
          "gw_vpr",
          "viruslast",
          "seed_covidare",
          "inzidenz_7_tage"
        )
      ), ~ 10 ^ (.))
    )
  
  df <- df %>%
    # avoid negative values
    mutate(across(all_of(matches("loess")), ~ ifelse(. <= 0, 1, .))) %>%
    # compute translation factors for smoothed data
    mutate(
      UEF_WW_GW_SR_SM = gw_sr_loess / viruslast_loess,
      UEF_WW_GW_VPR_SM = gw_vpr_loess / viruslast_loess ,
      UEF_WW_COVARE_SM = seed_covidare_loess / viruslast_loess,
      UEF_WW_inzidenz_7_tage_SM = inzidenz_7_tage_loess / viruslast_loess
    ) %>%
    dplyr::select(-obs)
  
  # should first differences be calculated
  if (changes)
  {
    # do this for the raw values
    df_raw_change <- df %>%
      # drop irrelevant variables and rows containing NAs
      select(-contains("loess"), -contains("UEF"), -jahr) %>%
      drop_na() %>%
      # replace zeros by minimum
      mutate_if(is.numeric, ~ ifelse(. == 0, min(., na.rm = T), .)) %>%
      # standardize first
      mutate_if(is.numeric, ~ . / max(., na.rm = T)) %>%
      # compute week-to-week changes
      mutate_if(is.numeric, ~ . / lag(.)) %>%
      # Drop rows where all values are NA
      filter(!if_all(everything(), is.na))
    # and for the smoothed values and UEFs
    df_smooth_change <- df %>%
      # drop irrelevant variables
      select(kalenderwoche, contains("loess"), contains("UEF")) %>%
      # standardize first
      mutate_if(is.numeric, ~ . / max(., na.rm = T)) %>%
      # compute week-to-week changes
      mutate_if(is.numeric, ~ . / lag(.)) %>%
      # Drop rows where all values are NA
      filter(!if_all(everything(), is.na))
    # combine both data sets
    df <- df_smooth_change %>%
      left_join(df_raw_change) %>%
      # add year again
      mutate(woche = week(kalenderwoche),
             jahr = year(kalenderwoche))
  }
  
  # remove boundary values as smootheing and trends are sometimes not as accurate there
  df <- df %>%
    arrange(desc(kalenderwoche)) %>%
    dplyr::slice((samples_removed * 7 + 1):n()) %>%
    arrange(kalenderwoche) %>%
    dplyr::slice((samples_removed * 7 + 1):n())
  
  # return data
  df
  
}

# function to plot regressions between wastewater and incidence
# df: input data
create_regression_graph <- function(df = NULL) {
  # divide ww viral laod by 10000 for nicer graphic
  scaling_factor = 10000
  df <-
    df %>%  mutate_at(vars(contains("viruslast")), ~ . / scaling_factor) %>%
    rename_neat()
  
  df_long <- df %>%
    # select relevant variables
    dplyr::select(all_of(neat_names)) %>%
    # drop rows with NAs
    drop_na() %>%
    # reformat data
    pivot_longer(cols = -`WW-VL`,
                 names_to = "system",
                 values_to = "value")
  
  # plot regression curves
  p <- ggplot(df_long, aes(x = `WW-VL`, y = value, color = system)) +
    geom_point() +
    geom_smooth(se = F,
                formula = y ~ x + 0,
                method = "lm")  +
    facet_wrap(~ system) +
    scale_color_manual(values = colors_used_neat_names[neat_names_no_ww]) +
    labs(x = "SARS-CoV-2 viral load in wastewater\nin 10,000 gene copies per liter", y = "COVID-19 cases\n per 100,000 inhabitants") +
    theme_trend() +
    theme(legend.position = "none")
  
  # Calculate slopes and add annotations
  slope_data <- df_long %>%
    group_by(system) %>%
    summarize(slope = coef(lm(value ~ `WW-VL` + 0, data = pick(value, `WW-VL`)))[1], r_2 = summary(lm(value ~ `WW-VL` + 0, data = pick(value, `WW-VL`)))$r.squared)  # Calculate slope through the origin
  
  # Add slope annotations to the plot
  p <- p  +   geom_text(
    data = slope_data,
    aes(
      x = 42,
      y = 450,
      label = paste("Slope:", round(slope, 2))
    ),
    hjust = .1,
    vjust = -3.3,
    size = 3,
    color = "black"
    #color = colors_used_neat_names[neat_names_no_ww]
  ) +
    geom_text(
      data = slope_data,
      aes(
        x = 42,
        y = 350,
        label = paste("R²:", round(r_2, 2))
      ),
      hjust = .15,
      vjust = -1.8,
      size = 3,
      color = "black"
      #color = colors_used_neat_names[neat_names_no_ww]
    )
  print(p)
  
  # save plot
  ggsave(
    plot = p,
    here(results_here_plots, paste0("figure_6.svg")),
    width = 16,
    height = 10,
    units = "cm", create.dir = TRUE
  )
}

# function to plot time series together in one plot
# df: input data
plot_indicators <- function(df = NULL) {
  # divide ww viral laod by 10000 for nicer graphic
  scaling_factor = 10000
  df <-
    # make variable names tidy
    df %>%  mutate_at(vars(contains("viruslast")), ~ . / scaling_factor)
  
  # get variables that determine the two axes for plot A
  x = pull(df, gw_sr)
  z = pull(df, viruslast)
  
  # compute transformation of axes (for plotting purposes)
  ylim.prim = c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
  ylim.sec = c(min(z, na.rm = TRUE), max(z, na.rm = TRUE))
  beta <- diff(ylim.sec) / diff(ylim.prim)
  alpha = ylim.sec[1] - beta * ylim.prim[1]
  
  # generate plot
  plot_1 <- df %>%
    ggplot() +
    scale_y_continuous(
      expand = c(0.02, 0),
      sec.axis = sec_axis(~ . * beta + alpha, name = "SARS-CoV-2 viral load in wastewater\nin 10,000 gene copies per liter")
    ) +
    theme_trend() +
    scale_x_date(
      date_breaks = "2 month",
      date_labels = "%b\n%y",
      expand = c(0, 0)
    ) +
    labs(x = "Date", y = "COVID-19 cases\n per 100,000 inhabitants") +
    geom_point(aes(kalenderwoche, inzidenz_7_tage, color = "GNS-I"),
               size = .8) +
    geom_point(aes(kalenderwoche, gw_sr, color = "PS-SR-I"),
               size = .8) +
    geom_point(aes(kalenderwoche, gw_vpr , color = "PS-VPR-I"),
               size = .8) +
    geom_point(aes(kalenderwoche, seed_covidare, color = "PC-COVID-ARI-I"),
               size = .8) +
    geom_point(aes(kalenderwoche, (viruslast - alpha) / beta, color = "WW-VL"),
                   size = .8) +
    geom_line(aes(kalenderwoche, inzidenz_7_tage_loess, color =   "GNS-I"),
              linewidth = .8) +
    geom_line(aes(kalenderwoche, gw_sr_loess, color = "PS-SR-I"),
              linewidth = .8) +
    geom_line(aes(kalenderwoche, gw_vpr_loess, color = "PS-VPR-I"),
              linewidth = .8) +
    geom_line(aes(kalenderwoche, seed_covidare_loess, color =   "PC-COVID-ARI-I"),
              linewidth = .8) +
    geom_line(aes(kalenderwoche, (viruslast_loess - alpha) / beta, color = "WW-VL"),
              linewidth = 1.6) +
    scale_color_manual(name = "",
                       values = as.character(colors_used_neat_names)) +
    guides(color = guide_legend(title = "", override.aes =
                                  list(lwd = rep(0.8, 5))))
  
  print(plot_1)
  
  # save plot
  ggsave(
    plot = plot_1,
    here(results_here_plots, paste0("figure_1.svg")),
    width = 16,
    height = 10,
    units = "cm", create.dir = TRUE
  )
}

# function to plot changes of smoothes time series together in one plot
# df: input data
plot_changes <- function(df = NULL) {
  # make variable names tidy
  df <-
    df %>%
    rename_neat()
  
  plot_ch <- df %>%
    # transform to percentage change
    mutate(across(where(is.numeric), ~ (. - 1))) %>%
    # select relevant variables
    select(contains("loess"), kalenderwoche) %>%
    # rename variables
    rename_with(~ str_replace(., "_loess", ""), contains("_loess")) %>%
    # reshape data
    gather(Indicator, FD, -kalenderwoche) %>%
    # drop NAs
    drop_na() %>%
    # plot changes
    ggplot() +
    theme_trend() +
    scale_x_date(breaks = "2 month",
                 expand = c(0, 0),
                 date_labels = "%b\n%y") +
    labs(x = "Date", y = "Changes in smoothed values") +
    geom_line(aes(kalenderwoche, FD, color = Indicator, size = Indicator)) +
    scale_size_manual(values = c(rep(0.8,4), 1.6)) +
    add_color_manual_ugly_names() +
    scale_y_continuous(labels = scales::percent) +
    guides(size = "none", color = guide_legend(title = "", override.aes =
                                                 list(lwd = c(rep(0.8,5)))))
  
  print(plot_ch)
  
  # save plot
  ggsave(
    plot = plot_ch,
    here(results_here_plots, "figure_7.svg"),
    width = 16,
    height = 10,
    units = "cm", create.dir = TRUE
  )
}


# df: input data
create_translation_factor_graph <- function(df = NULL) {
  # set scaling factor for nicer graphic
  scaling_factor = 10000
  
  plot_data <-
    df %>%
    # select relevant variables
    dplyr::select(jahr, woche, contains("UEF"), kalenderwoche) %>%
    # reshape and scale data
    gather(criterion, UEF, -jahr, -woche, -kalenderwoche) %>%
    #  divide ww viral laod by 10000 / multiply UEF by 10000 for nicer graphic
    mutate(UEF = UEF * scaling_factor)
  
  # generate translation factor plot for all indicators
  plot_all <- plot_data %>%
    ggplot() +
    geom_line(
      aes(kalenderwoche, UEF, color = criterion),
      linewidth = 1.0,
      data = plot_data %>% filter(
        criterion %in%
          c(
            "UEF_WW_GW_VPR_SM",
            "UEF_WW_inzidenz_7_tage_SM",
            "UEF_WW_GW_SR_SM",
            "UEF_WW_COVARE_SM"
          )
      )
    ) +
    scale_x_date(
      date_breaks = "4 month",
      date_labels = "%b\n%y",
      expand = c(0, 0)
    ) +
    theme_trend() +
    add_color_manual_uef() +
    theme(legend.position = "bottom") +
    labs(x = "Date", y = "Translation factor") +
    scale_y_continuous(
      trans = 'log10',
      expand = c(0.02, 0),
      labels = ~ format(.x, scientific = FALSE)
    )
  
  # do the same only for PS-SR_I
  plot_gw_sr <- plot_data %>% filter(criterion ==
                                       "UEF_WW_GW_SR_SM") %>%
    ggplot() +
    geom_line(aes(kalenderwoche, UEF),
              color = colors_used_ugly_names["gw_sr"],
              linewidth = 1.0,
    )  +
    scale_x_date(
      date_breaks = "4 month",
      date_labels = "%b\n%y",
      expand = c(0, 0)
    ) +
    theme_trend() +
    labs(x = "Date", y = "Translation factor") +
    scale_y_continuous(trans = 'log10', expand = c(0.02, 0))
  
  legend <- ggpubr::get_legend(plot_all)
  legend <- as.ggplot(legend)
  plot_all <- plot_all + theme(legend.position = "none") +
    theme(axis.title.y = element_text(margin = margin(r = 20)))
  plot_gw_sr <- plot_gw_sr +
    theme(axis.title.y = element_text(margin = margin(r = 20)))
  
  # combine plots
  combined_plots <- plot_grid(plot_all, plot_gw_sr, ncol = 2, align = "hv")
  
  # add legend
  final_plot <- plot_grid(combined_plots,
                          legend,
                          ncol = 1,
                          rel_heights = c(1, 0.1))
  
  print(final_plot)
  
  # save plot
  ggsave(
    plot =  final_plot,
    here(results_here_plots, "figure_5.svg"),
    width = 16,
    height = 10,
    units = "cm", create.dir = TRUE
  )
}

# function to plot correspondence over time between viral load and case-based systems
# df: input data, comparison_var: variable to plot over time
plot_correspondence_over_time <- function(df = NULL,
                                          comparison_var = "viruslast_loess") {
  # indicate when comparison variable agrees with category indicated by majority
  # of case-based surveillance systems
  df <- df %>%
    mutate(
      Alignment = ifelse(
        !!sym(comparison_var) == as.character(truth),
        "agreement",
        "no agreement"
      ),
      # set "unclear if case-based surveillance systems show "unclear" trend
      Alignment = ifelse(truth == "unclear", "unclear", Alignment),
      # scale viral load for plotting purposes
      viruslast = viruslast_smoothed / 10000
    )
  
  # plot
  rect_data <- df %>%
    select(kalenderwoche, Alignment) %>%
    arrange(kalenderwoche) %>%
    mutate(xmin = kalenderwoche - diff(range(kalenderwoche)) / n() / 2,
           xmax = kalenderwoche + diff(range(kalenderwoche)) / n() / 2)
  
  p <- df %>%
    ggplot() +
    geom_rect(
      data = rect_data,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = -Inf,
        ymax = Inf,
        fill = Alignment
      ),
      alpha = 0.4
    ) +
    scale_fill_manual(
      values = c(
        "agreement" = "#619CFF",
        "no agreement" = "#F8766D",
        "unclear" = "white"
      ),
      breaks =  c("agreement", "no agreement"),
      labels =  c("agreement", "no agreement")
    ) +
    geom_line(aes(kalenderwoche, viruslast), linewidth = 1.0) +
    theme_trend() +
    labs(x = "Date", y = "SARS-CoV-2 viral load in wastewater\nin 10,000 gene copies per liter") +
    scale_x_date(
      date_breaks = "2 month",
      date_labels = "%b\n%y",
      expand = c(0, 0)
    )
  print(p)
  
  # save plot
  ggsave(
    plot = p,
    here(results_here_plots, "figure_9.svg"),
    width = 16,
    height = 10,
    units = "cm", create.dir = TRUE
  )
}

# function to calculate prediction measures.
# ta: input contingency table.
pred_measures <- function(ta = NULL) {
  # the diagonal contains correct predictions / agreements
  hits <- sum(diag(ta))
  # overall accuracy = correct predictions / all observations
  overall_acc = hits / sum(ta)
  # specificity, sensitivity and positive / negative predictive value for decreases
  TP <- diag(ta)["decreasing"]
  FP <- sum(ta[, "decreasing"]) - TP
  FN <- sum(ta["decreasing", ]) - TP
  TN <- sum(ta) - TP - FP - FN
  
  decr_spec = TN / (TN + FP)
  decr_sens = TP / (TP + FN)
  decr_ppv = TP / (TP + FP)
  decr_npv = TN / (TN + FN)
  
  #  specificity, sensitivity and positive / negative predictive value for increases
  TP <- diag(ta)["increasing"]
  FP <- sum(ta[, "increasing"]) - TP
  FN <- sum(ta["increasing", ]) - TP
  TN <- sum(ta) - TP - FP - FN
  
  incr_spec = TN / (TN + FP)
  incr_sens = TP / (TP + FN)
  incr_ppv = TP / (TP + FP)
  incr_npv = TN / (TN + FN)
  
  # return results
  return(
    data.frame(
      overall_acc,
      decr_sens,
      decr_spec,
      decr_ppv,
      decr_npv,
      incr_sens,
      incr_spec,
      incr_ppv,
      incr_npv,
      row.names = ""
    ) %>% t() %>% round(2)
  )
}

# function to save confusion matrix for a given contingency table
# df: input contingency table
# name: name of output file
save_confusion_matrix <- function(df = NULL, name = NULL) {
  # calculate prediction measures
  measures <- pred_measures(df)
  # Define a function to replace certain words
  replace_words <- function(words) {
    # Define the words to replace and their replacements
    replacements <- list(
      "no_change" = "No Change",
      "decreasing" = "Decrease",
      "increasing" = "Increase",
      "unclear" = "Unclear",
      "other" = "Other"
    )
    # Replace words in the input list
    for (i in seq_along(words)) {
      if (words[i] %in% names(replacements)) {
        words[i] <- replacements[[words[i]]]
      }
    }
    return(words)
  }
  # Apply the function to each element of the confusion matrix
  dimnames(df) <- lapply(dimnames(df), replace_words)
  # Convert to data frame for labeling
  tab_df <- as.data.frame.matrix(df)
  # add row names as an extra column
  tab_df <- cbind(XXX = rownames(tab_df), tab_df)
  # remove initial rownames
  rownames(tab_df) <- NULL
  # make nice latex table
  latex_conf_matrix <- kable(tab_df,
                             format = "latex",
                             booktabs = TRUE,
                             align = "lrrr") %>%
    add_header_above(c(" " = 1, "YYY" = 3)) %>%
    kable_styling(latex_options = c("hold_position"))
  
  # save table
  sink(here(results_here_tables, paste0(name, "_part_1", ".tex")))
  print(latex_conf_matrix,
        type = "latex",
        include.rownames = TRUE)
  sink()
  # add prediction measures below
  summary_stats <- data.frame(
    Metric = c(
      "Accuracy",
      "Decrease: Specificity",
      "Decrease: Sensitivity",
      "Decrease: Positive Predictive Value",
      "Decrease: Negative Predictive Value",
      "Increase: Specificity",
      "Increase: Sensitivity",
      "Increase: Positive Predictive Value",
      "Increase: Negative Predictive Value"
    ),
    Value = c(
      round(measures["overall_acc", ], 2),
      round(measures["decr_spec", ], 2),
      round(measures["decr_sens", ], 2),
      round(measures["decr_ppv", ], 2),
      round(measures["decr_npv", ], 2),
      round(measures["incr_spec", ], 2),
      round(measures["incr_sens", ], 2),
      round(measures["incr_ppv", ], 2),
      round(measures["incr_npv", ], 2)
    )
  )
  colnames(summary_stats) <- c("", "")
  latex_sum_stats <- xtable(summary_stats, include.rownames = FALSE)
  sink(here(results_here_tables, paste0(name, "_part_2", ".tex")))
  print(latex_sum_stats,
        type = "latex",
        include.rownames = FALSE)
  sink()
}

# function to calculate outer loop accuracy
# preds: predicted values, true_labels: observed values
calculate_outer_metric <- function(preds, true_labels) {
  return(mean(preds == true_labels))
}

# function to calculate ML models results
# df: input data, form: formula used for the ML algorithms,
# outer_folds: no. of folds for outer loop,
# inner_folds: no. of folds for innter loop,
# tuneLength:  amount of granularity in the tuning parameter grid for Ml algorithms,
# avoid_overlap: no. of last values dropped from validation / test sets
# to avoid overlap with training sets,
# seed: seed for replicability
compute_ml_models <- function(df = NULL,
                              form = NULL,
                              outer_folds = 10,
                              inner_folds = 10,
                              tuneLength = 10,
                              avoid_overlap = 0,
                              seed = 22) {
  # set model names
  models = c("multinom", "svm", "rf")
  # set seed
  set.seed(seed)
  # store true target values
  true_y <- df %>% pull(as.character(form)[2])
  # store true target variable name
  target <- as.character(form)[2]
  # store number of rows of dataset
  n_rows = nrow(df)
  
  # nested cross-validation
  # use outer cv for estimation of generalization error
  # and inner cv for hyperparameter tuning and model selection
  # list/container for the outer and inner loop results
  outer_results <- list()
  inner_results <- list()
  # container for the final predictions made by the algorithms
  predictions_results <- list()
  
  # set correct data type for the containers
  for (mod in models) {
    outer_results[[mod]] <- data.frame()
    inner_results[[mod]] <- c()
    predictions_results[[mod]] <- factor(c(), levels = levels(true_y))
  }
  
  # Perform outer cross-validation
  for (i in 1:outer_folds) {
    # print current loop number to follow progress of the computation
    print("outer fold: ")
    print(i)
    # Create indices for the outer fold
    start_index <- ceiling(n_rows * (i - 1) / outer_folds) + 1
    end_index <- ceiling(n_rows * (i) / outer_folds)
    # Create tte corresponding training and test set while avoiding potential overlap
    test_outer <- df[start_index:end_index, ]
    train_outer <- df[-(max(c(1, start_index - avoid_overlap)):min(c(end_index +
                                                                       avoid_overlap), n_rows)), ]
    # Inner cross-validation setup
    block <- block_cv(train_outer,
                      n_splits = inner_folds,
                      avoid_overlap = avoid_overlap)
    inner_control <- trainControl(
      "cv",
      index = block[['training']],
      indexOut = block[['validation']],
      summaryFunction = multiClassSummary
    )
    # Perform inner cross-validation for Lasso regression
    model_multinom <- train(
      form,
      data =  train_outer,
      method = "glmnet",
      preProcess = c("center", "scale"),
      trControl = inner_control,
      metric = "Accuracy",
      tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 0.3, by = 0.0005)),
      verbose = F
    )
    # output supervision
    print("best lambda for Lasso:")
    print(model_multinom$bestTune$lambda)
    
    # Perform inner cross-validation for SVM
    model_svm <- train(
      form,
      data =  train_outer,
      method = "svmRadial",
      metric = "Accuracy",
      preProcess = c("center", "scale"),
      trControl = inner_control,
      tuneLength = tuneLength,
      verbose = F
    )
    # output supervision
    print("best parameters for SVM:")
    print(model_svm$bestTune)
    
    # Perform inner cross-validation for Random Forest
    model_rf <- train(
      form,
      data =  train_outer,
      method = "ranger",
      trControl = inner_control,
      metric = "Accuracy",
      preProcess = c("center", "scale"),
      tuneGrid = expand.grid(
        mtry = c(1, 2, 3, 4, 5),
        splitrule = c("gini", "extratrees"),
        min.node.size = 1:15
      ),
      verbose = F
    )
    # output supervision
    print("best parameters for RF:")
    print(model_rf$bestTune)
    
    # Store best inner CV results
    inner_results[["multinom"]][i] <- max(model_multinom$results[["Accuracy"]])
    inner_results[["svm"]][i] <- max(model_svm$results[["Accuracy"]])
    inner_results[["rf"]][i] <- max(model_rf$results[["Accuracy"]])
    
    # Outer test set predictions
    predictions_multinom <- predict(model_multinom, newdata = test_outer)
    predictions_svm <- predict(model_svm, newdata = test_outer)
    predictions_rf <- predict(model_rf, newdata = test_outer)
    
    # store outer test set accuracy
    metric_multinom <- calculate_outer_metric(predictions_multinom, test_outer[[target]])
    metric_svm <- calculate_outer_metric(predictions_svm, test_outer[[target]])
    metric_rf <- calculate_outer_metric(predictions_rf, test_outer[[target]])
    
    # Store predictions
    predictions_results[["multinom"]] <- c(predictions_results[["multinom"]], predictions_multinom)
    predictions_results[["svm"]] <- c(predictions_results[["svm"]], predictions_svm)
    predictions_results[["rf"]] <- c(predictions_results[["rf"]], predictions_rf)
    
    # Store outer results
    outer_results[["multinom"]] <- rbind(outer_results[["multinom"]],
                                         data.frame(Fold = i, Metric = metric_multinom))
    outer_results[["svm"]] <- rbind(outer_results[["svm"]], data.frame(Fold = i, Metric = metric_svm))
    outer_results[["rf"]] <- rbind(outer_results[["rf"]], data.frame(Fold = i, Metric = metric_rf))
    
  }
  # supervise inner results
  print("inner results: multinom")
  print(summary(inner_results[["multinom"]]))
  print("inner results: svm")
  print(summary(inner_results[["svm"]]))
  print("inner results: rf")
  print(summary(inner_results[["rf"]]))
  
  # store and return results
  results <- list(
    inner_results = inner_results,
    predictions_results = predictions_results,
    outer_results = outer_results,
    true_y = true_y
  )
  
  return(results)
}

# function to save table for inner loop results from cross-validation
# results: list of results data from machine learning computations
save_inner_cv_results <- function(results = NULL) {
  # access inner loop results from list
  tab <- results$inner_results %>%
    map_df(summary, .id = "model") %>%
    mutate_if(is.numeric, ~ round(., 2)) %>%
    # make algorithm names nicer
    mutate(
      model = case_when(
        model == "multinom" ~ "LASSO",
        model == "svm" ~ "Support Vector Machine",
        model == "rf" ~ "Random Forest"
      )
    )
  
  # save inner loop results in latex table
  sink(here(results_here_tables, paste0("table_a3.tex")))
  print(kable(tab, format = "latex", booktabs = TRUE))
  sink()
}

# function to save table for outer loop results from best ML algorithm
# (assessed regarding inner loop results)
# results: list of results data from machine learning computations
save_outer_cv_results <- function(results = NULL) {
  # select best model by inner loop results
  best_mod <- results$inner_results %>%
    map_df(mean) %>%
    pivot_longer(everything(), names_to = "column", values_to = "value") %>%
    filter(value == max(value)) %>%
    pull(column)
  
  # build confusion matrix for outer model
  conf_mat <- confusionMatrix(as.factor(results$true_y), results$predictions_results[[best_mod]])
  
  # save results in latex table
  save_confusion_matrix(df = conf_mat$table, name = "table_2")
}
