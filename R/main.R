#' Pre-processing data before training with DBN
#'
#' @param data A data frame which rows are financial reports and columns is KPI.
#' @param type Data will preprocess as discrete or continuous
#' @param index_column Column name of company id
#' @param time_column Column name of time value
#' @param time_values Time values by year
#' @param continuous_kpi_names Column names of continuous dynamic KPI
#' @param continuous_static_kpi_names Column names of continuous static (un-change over time) KPI
#' @param discrete_static_kpi_names Columns names of discrete static (un-change over time) KPI
#' @param current_layers Maximum number of layers in bayesian network, equal number of time values
#' @param desire_layers Number of layers for training bayesian network, less than or equal current layers
#' @param quantile_number For type is "discrete", number of quantile for each continuous variable
#' @param normalize_type Normalize type, possible values are: mean_normalization, min_max or standardisation
#' @param na_omit Is omit NA or not
#' @param debug Debug mode
#' @return A processed data frame with pre-process params
#' @examples
#' library(tfdbn)
#' data("data_small")
#' type <- "continuous"
#' index_column <- "MST"
#' time_column <- "NAM"
#' time_values <- 2017:2019
#' continuous_kpi_names <- paste("K_C_V0", 1:9, sep = "")
#' continuous_static_kpi_names <- c("TUOI", "VON")
#' discrete_static_kpi_names <- c("NGANHKT")
#' current_layers = 3
#' desire_layers = 2
#' quantile_number = -1
#'
#' head(data_small)
#'
#' preprocessed <- tfdbn::preprocess_training_data(data_small, type, index_column, time_column, time_values,
#'                                                 continuous_kpi_names, continuous_static_kpi_names,
#'                                                 discrete_static_kpi_names, current_layers, desire_layers, "min_max", quantile_number,debug = TRUE)
preprocess_training_data <- function(data, type, index_column, time_column, time_values,
                                   continuous_kpi_names, continuous_static_kpi_names, discrete_static_kpi_names, current_layers,
                                   desire_layers, normalize_type = NULL, quantile_number = -1, na_omit = TRUE, debug = FALSE) {


  #Get all necessary columns
  data <- data[, c(index_column, time_column, continuous_kpi_names, continuous_static_kpi_names, discrete_static_kpi_names)]
  index_values <- unique(data[, index_column])

  #Reconstruct data to one row
  data_constructed <- reconstruct_report_data(data, index_column, index_values, time_column, time_values, current_layers, debug)
  data_constructed <- convert_data_type(data_constructed, c(continuous_kpi_names, continuous_static_kpi_names), discrete_static_kpi_names)

  #Generate continuous columns with time notation
  continuous_dynamic_variables <- c()
  for(name in c(continuous_kpi_names)) {
    continuous_dynamic_variables <- c(continuous_dynamic_variables, c(paste(name, 1:current_layers, sep = "_")))
  }

  continuous_static_variables <- c()
  for(name in c(continuous_static_kpi_names)) {
    continuous_static_variables <- c(continuous_static_variables, c(paste(name, 1:current_layers, sep = "_")))
  }

  continuous_variables <- c(continuous_dynamic_variables, continuous_static_variables)

  #Generate discrete columns with time notation
  discrete_variables <- c()
  for(name in discrete_static_kpi_names) {
    discrete_variables <- c(discrete_variables, c(paste(name, 1:current_layers, sep = "_")))
  }

  #Exclude index and time columns
  data_constructed <- data_constructed[, c(continuous_variables, discrete_variables)]

  #Generate continuous dynamic variables
  continuous_dynamic_desire_variables <- c()
  for(name in c(continuous_kpi_names)) {
    continuous_dynamic_desire_variables <- c(continuous_dynamic_desire_variables, c(paste(name, 1:desire_layers, sep = "_")))
  }

  #Reconstruct data frame to desire layers
  data_constructed <- prepare_report_for_dbn(data_constructed, current_layers, desire_layers, discrete_variables, continuous_variables, na_omit, debug)
  data_constructed <- remove_redundant_variables(data_constructed, c(continuous_static_kpi_names, discrete_static_kpi_names), debug)
  #Normalize data
  normalizers <- NULL
  if(!is.null(normalize_type)) {
    data_normalized <- normalize_data(data_constructed, normalize_type)
    data_constructed <- data_normalized$data
    normalizers <- data_normalized$normalizers
  }

  result <- list()
  result[["continuous_dynamic_variables"]] <- continuous_dynamic_desire_variables
  result[["continuous_kpi_names"]] <- continuous_kpi_names
  result[["continuous_static_variables"]] <- continuous_static_kpi_names
  result[["discrete_static_variables"]] <- discrete_static_kpi_names
  result[["quantile_number"]] <- quantile_number
  result[["desire_layers"]] <- desire_layers
  result[["current_layers"]] <- current_layers
  result[["index_column"]] <- index_column
  result[["time_column"]] <- time_column
  result[["time_values"]] <- time_values
  result[["type"]] <- type
  result[["normalizers"]] <- normalizers
  result[["normalize_tye"]] <- normalize_type
  if(type == "discrete") {
    quantiled_variables <- c()

    if(quantile_number > 1) {
      quantile_numbers <- rep(quantile_number, length(c(continuous_kpi_names, continuous_static_kpi_names)) * desire_layers)
      for(kpi in c(continuous_kpi_names, continuous_static_kpi_names)) {
        quantiled_variables <- c(quantiled_variables, paste(kpi, 1:desire_layers, sep = "_"))
      }
    }

    data_discrete_dbn <- prepare_constructed_report_data(data_constructed, current_layers, desire_layers,
                                                         discrete_variables, continuous_variables, quantiled_variables,
                                                         quantile_numbers, na_omitm, debug)

    if(quantile_number > 1) {

      result[["data"]] <- data_discrete_dbn[[1]]
      result[["break_list"]] <- data_discrete_dbn[[2]]
    } else {
      result[["data"]] <- data_discrete_dbn[[1]]
    }

    return(result)

  } else if(type == "continuous"){
    result[["data"]] <- data_constructed
    return(result)
  } else {
    result <- list()
  }
}

#' Pre-processing data before calculate score
#'
#' @param data A data frame which rows are financial reports and columns is KPI.
#' @param type Data will preprocess as discrete or continuous
#' @param index_column Column name of company id
#' @param time_column Column name of time value
#' @param time_values Time values by year
#' @param continuous_kpi_names Column names of continuous dynamic KPI
#' @param continuous_static_kpi_names Column names of continuous static (un-change over time) KPI
#' @param discrete_static_kpi_names Columns names of discrete static (un-change over time) KPI
#' @param current_layers Maximum number of layers in bayesian network, equal number of time values
#' @param desire_layers Number of layers for training bayesian network, less than or equal current layers
#' @param normalize_type Normalize type, possible values are: mean_normalization, min_max or standardisation
#' @param normalizers Normalizer for each variable
#' @param quantile_number For type is "discrete", number of quantile for each continuous variable
#' @param debug Debug mode
#' @return A processed data frame with pre-process params
#' @examples
#' library(tfdbn)
#' type <- preprocessed$type
#' normalize_type <- preprocessed$normalize_tye
#' normalizers <- preprocessed$normalizers
#' index_column <- preprocessed$index_column
#' time_column <- preprocessed$time_column
#' time_values <- preprocessed$time_values
#' continuous_kpi_names <- preprocessed$continuous_kpi_names
#' continuous_static_kpi_names <- preprocessed$continuous_static_variables
#' discrete_static_kpi_names <- preprocessed$discrete_static_variables
#' current_layers <- preprocessed$current_layers
#' desire_layers <- preprocessed$desire_layers
#' quantile_number <- -1
#'
#' data_test <- tfdbn::preprocess_test_data(data_small, type, index_column, time_column, time_values,
#'                                          continuous_kpi_names, continuous_static_kpi_names,
#'                                          discrete_static_kpi_names, current_layers, desire_layers, normalize_type, normalizers, quantile_number, debug = FALSE)
preprocess_test_data <- function(data, type, index_column, time_column, time_values,
                                  continuous_kpi_names, continuous_static_kpi_names,
                                  discrete_static_kpi_names, current_layers, desire_layers,
                                  normalize_type = NULL, normalizers = NULL,
                                  quantile_number = -1, debug = FALSE) {

  if(debug) {
    cat("Preprocess data test with dim", dim(data), "\n")
  }

  data <- data[, c(index_column, time_column, continuous_kpi_names, continuous_static_kpi_names, discrete_static_kpi_names)]

  all_id <- unique(data[, index_column])

  reports <- list()
  for(id in all_id) {
    if(debug) {
      cat("Preprocess data test for", id, "\n")
    }

    layers <- current_layers - desire_layers + 1
    for(i in 1:layers) {
      if(debug) {
        cat("Preprocess data test for", id, "in layer", i, "\n")
      }

      layer_times <- time_values[i:(i + desire_layers - 1)]
      a_report <- data[data[, index_column] == id & data[, time_column] %in% layer_times, ]

      a_report_preprocessed <- reconstruct_test_data(a_report, type, index_column, time_column, layer_times,
                                                           continuous_kpi_names, continuous_static_kpi_names,
                                                           discrete_static_kpi_names,
                                                           desire_layers,
                                                           normalize_type, normalizers, quantile_number, debug = debug)
      a_report_preprocessed <- convert_data_type(a_report_preprocessed, c(continuous_kpi_names, continuous_static_kpi_names), discrete_static_kpi_names)
      a_report_preprocessed <- remove_redundant_variables(a_report_preprocessed, c(continuous_static_kpi_names, discrete_static_kpi_names))

      if(!is.null(normalize_type) & !is.null(normalizers)) {
        a_report_preprocessed <- normalize_data(a_report_preprocessed, normalize_type, normalizers)$data
      }
      if(debug) {
        cat("Preprocess data test for", id, "in layer", i, "completed", "\n")
      }

      reports[[paste(paste(layer_times, collapse = "_"), as.character(id), sep = "_")]] <- a_report_preprocessed
    }
    if(debug) {
      cat("Preprocess data test for", id, "completed", "\n")
    }


  }

  if(debug) {
    cat("Preprocess data test with dim", dim(data), "completed", "\n")
  }

  return(reports)
}

#' Filter blacklist and whitelist before training bayesian network
#'
#' @param continuous_data A data frame which rows are financial reports and columns is KPI pre-processed
#' @param desire_layers Number of layers for training bayesian network, less than or equal current layers
#' @param quantile_number For type is "discrete", number of quantile for each continuous variable
#' @param continuous_dynamic_variables Column names of continuous dynamic KPI
#' @param continuous_static_variables Column names of continuous static (un-change over time) KPI
#' @param discrete_static_variables Columns names of discrete static (un-change over time) KPI
#' @param known_structure Relation between KPI specific by user
#' @param corr_threshold Threshold for filter arcs with low correlation
#' @param is_blacklist_internal Is blacklist all internal arcs in n-i layers (n is number layers, i = 1..n-1)
#' @param is_variable_only Is known_structure is KPI only, without time notation
#' @param is_blacklist_other Is blacklist all except whitelist
#' @param custom_blacklist Blacklist specific by user
#' @param custom_whitelist Whitelist sepcific by user
#' @return An object with blacklist and whitelist of network
#' @examples
#' data("preprocessed")
#' data("data_small")
#' continuous_data <- preprocessed$data
#' desire_layers <- preprocessed$desire_layers
#' quantile_number <- preprocessed$quantile_number
#' continuous_dynamic_variables <- preprocessed$continuous_dynamic_variables
#' continuous_static_variables <- preprocessed$continuous_static_variables
#' discrete_static_variables <- preprocessed$discrete_static_variables
#' known_structure <- NULL
#' corr_threshold <- 0.4
#' is_blacklist_internal <- TRUE
#' is_variable_only <- TRUE
#' is_blacklist_other <- FALSE
#' custom_blacklist <- NULL
#' custom_whitelist <- NULL

#' bl_wl <- tfdbn::get_continuous_structure_filter(continuous_data, desire_layers, quantile_number, continuous_dynamic_variables,
#'                                                 continuous_static_variables, discrete_static_variables,
#'                                                 known_structure, corr_threshold, is_blacklist_internal,
#'                                                 is_variable_only, is_blacklist_other,
#'                                                 custom_blacklist, custom_whitelist)
get_continuous_structure_filter <- function(continuous_data, desire_layers, quantile_number, continuous_dynamic_variables,
                                            continuous_static_variables, discrete_static_variables,
                                            known_structure, corr_threshold, is_blacklist_internal,
                                            is_variable_only, is_blacklist_other,
                                            custom_blacklist, custom_whitelist){


    continuous_data <- continuous_data[, c(continuous_dynamic_variables, continuous_static_variables, discrete_static_variables)]

    rho <- abs(cor(convert_variables_to_factor(continuous_data)$data))
    rho <- as.matrix(rho)
    arc_structures <- structure_filter(known_structure, desire_layers, continuous_dynamic_variables,
                                       c(continuous_static_variables, discrete_static_variables),
                                       is_blacklist_internal, is_blacklist_other, is_variable_only,
                                       rho, corr_threshold,
                                       custom_blacklist, custom_whitelist)

    return(arc_structures)
}


#' Filter blacklist and whitelist before training bayesian network
#'
#' @param continuous_data A data frame which rows are financial reports and columns is KPI pre-processed
#' @param desire_layers Number of layers for training bayesian network, less than or equal current layers
#' @param quantile_number For type is "discrete", number of quantile for each continuous variable
#' @param continuous_dynamic_variables Column names of continuous dynamic KPI
#' @param continuous_static_variables Column names of continuous static (un-change over time) KPI
#' @param discrete_static_variables Columns names of discrete static (un-change over time) KPI
#' @param known_structure Relation between KPI specific by user
#' @param corr_threshold Threshold for filter arcs with low correlation
#' @param is_blacklist_internal Is blacklist all internal arcs in n-i layers (n is number layers, i = 1..n-1)
#' @param is_variable_only Is known_structure is KPI only, without time notation
#' @param is_blacklist_other Is blacklist all except whitelist
#' @param custom_blacklist Blacklist specific by user
#' @param custom_whitelist Whitelist sepcific by user
#' @return An object with blacklist and whitelist of network
#' @examples
#' data("preprocessed")
#' data("data_small")
#' continuous_data <- preprocessed$data
#' desire_layers <- preprocessed$desire_layers
#' quantile_number <- preprocessed$quantile_number
#' continuous_dynamic_variables <- preprocessed$continuous_dynamic_variables
#' continuous_static_variables <- preprocessed$continuous_static_variables
#' discrete_static_variables <- preprocessed$discrete_static_variables
#' known_structure <- NULL
#' corr_threshold <- 0.4
#' is_blacklist_internal <- TRUE
#' is_variable_only <- TRUE
#' is_blacklist_other <- FALSE
#' custom_blacklist <- NULL
#' custom_whitelist <- NULL

#' bl_wl <- tfdbn::get_continuous_structure_filter(continuous_data, desire_layers, quantile_number, continuous_dynamic_variables,
#'                                                 continuous_static_variables, discrete_static_variables,
#'                                                 known_structure, corr_threshold, is_blacklist_internal,
#'                                                 is_variable_only, is_blacklist_other,
#'                                                 custom_blacklist, custom_whitelist)
get_discrete_structure_filter <- function(discrete_data, desire_layers, quantile_number, continuous_dynamic_variables,
                                            continuous_static_variables, discrete_static_variables,
                                            known_structure, corr_threshold, is_blacklist_internal,
                                            is_variable_only, is_blacklist_other,
                                            custom_blacklist, custom_whitelist, debug = FALSE){


  discrete_data <- discrete_data[, c(continuous_dynamic_variables, continuous_static_variables, discrete_static_variables)]

  rho <- abs(cor(convert_variables_to_factor(discrete_data)$data))
  rho <- as.matrix(rho)

  arc_structures <- structure_filter(known_structure, desire_layers, continuous_dynamic_variables,
                                     c(continuous_static_variables, discrete_static_variables),
                                     is_blacklist_internal, is_blacklist_other, is_variable_only,
                                     rho, corr_threshold,
                                     custom_blacklist, custom_whitelist)

  return(arc_structures)
}

#' Training bayesian network
#'
#' @param training_type Training type, discrete or continuous
#' @param data Data for training
#' @param number_layers Number of bayesian network layer
#' @param bl List of blacklist
#' @param wl List of whitelist
#' @param n_cluster Number of core for training
#' @param algorithms Algorithms for learning network structure and parameters
#' @param number_bootstrap Number of bootstrap
#' @param debug Debug mode
#' @return An object list of trained model and training parameters
#' @examples
#' data("data_small")
#' data("preprocessed")
#' continuous_data <- preprocessed$data
#' desire_layers <- preprocessed$desire_layers
#' bl <- bl_wl$blacklist
#' wl <- bl_wl$whitelist
#' n_cluster = 4
#' algorithms <- c("tabu", "hc")
#' number_bootstrap = 100

#' trained <- training_model("continuous", continuous_data, number_layers, bl, wl, n_cluster, algorithms, number_bootstrap)
training_model <- function(training_type, data, number_layers, bl, wl, n_cluster, algorithms, number_bootstrap = 100, debug = FALSE) {
  if(debug) {
    cat("Training model ", training_type, "\n")
  }

  cluster <- parallel::makeCluster(n_cluster)

  data_converted <- convert_variables_to_factor(data)

  data_factors <- data_converted$data_factors
  data <- data_converted$data

  bl <- remove_unknown_arc(bl, colnames(data))
  wl <- remove_unknown_arc(wl, colnames(data))
  results <- list()
  results$data_factors <- data_factors
  for(algor in algorithms) {

    if(debug) {
      cat("Training model by algorithm ", algor, "\n")
    }

    begin <- Sys.time()
    trained <- structure_learning(data, desire_layers, bl, wl, slearning_algo = algor, number_bootstrap = number_bootstrap, cluster, debug)
    end <- Sys.time()
    trained$time <- begin - end
    trained$date <- date()
    trained$blacklist <- bl
    trained$whitelist <- wl

    if(debug) {
      cat("Training model by algorithm ", algor, "compeleted", "\n")
    }

    results[[algor]] <- trained

  }

  parallel::stopCluster(cluster)

  if(debug) {
    cat("Training model", training_type, "compeleted", "\n")
  }


  return(results)
}

#' Get sector profile
#'
#' @param fitted Training type, discrete or continuous
#' @param sector_value Data for training
#' @param sector_variable Number of bayesian network layer
#' @param target_variables List of blacklist
#' @param n_samples List of whitelist
#' @param max_times Number of core for training
#' @param n_cluster Algorithms for learning network structure and parameters
#' @param number_bootstrap Number of bootstrap
#' @param debug Debug mode
#' @return An profile of KPIs for specific sector
#' @examples
#' data_factor <- trained$data_factors
#' fitted <- trained$tabu$fitted
#' data <- preprocessed$data
#' n_cluster <- 4
#' sector_variable <- "NGANHKT"
#' all_variables <- c(preprocessed$continuous_dynamic_variables,
#'                    preprocessed$continuous_static_variables,
#'                    preprocessed$discrete_static_variables)
#'
#' target_variables <- setdiff(all_variables, c(sector_variable))
#' sector_profiles <- list()
#' for(sector in data_factor[[sector_variable]]) {
#'   sector_id <- which(data_factor[[sector_variable]] %in% sector)
#'
#'   sector_profiles[[sector]] <- tfdbn::get_sector_normal(fitted, sector_id, sector_variable, target_variables,
#'                                                         1000, 10, n_cluster)
#' }
get_sector_normal <- function(fitted, sector_value, sector_variable,
                              target_variables, n_samples, max_times, n_cluster, debug = FALSE) {
  if(debug) {
    cat("Get sector distribution of sector", sector_value,
        "for targets", target_variables, "\n")
  }

  cl <- parallel::makeCluster(n_cluster)
  dist_total <- NULL
  nth_sample <- 0
  while(TRUE) {
    nodes <- generate_dist_node(target_variables)
    evidence_equation <- generate_sector_equation(sector_value, sector_variable, 0.5)
    dist_equation <- paste(paste("bnlearn::cpdist(fitted, ", nodes, ", ", evidence_equation, ", cluster=cl)", sep = ""))

    dist <- eval(parse(text=dist_equation))

    if(is.null(dist_total)) {
      dist_total <- dist
    } else {
      dist_total <- rbind(dist_total, dist)
    }

    nth_sample <- nth_sample + 1

    if((!is.null(dist_total) & nrow(dist_total) >= n_samples)
       | nth_sample > max_times) {
      break
    }
  }

  parallel::stopCluster(cl)

  if(debug) {
    cat("Get sector distribution of sector", sector_value,
        "for targets", target_variables, "completed with", nrow(dist_total), "sample", "\n")
  }


  return(dist_total)
}

get_company_profiles <- function(fitted, evidence_data, target_variables,
                                 n_samples, max_times, n_cluster, debug = FALSE) {
  if(debug) {
    cat("Get company profiles by evidence from", names(evidence_data), "for targets", target_variables, "\n")
  }

  cl <- parallel::makeCluster(n_cluster)
  dist_total <- NULL
  nth_sample <- 0
  for(i in 1:max_times) {
    dist <- bnlearn::cpdist(fitted, nodes = target_variables, evidence = as.list(evidence_data), method = "lw")

    if(is.null(dist_total)) {
      dist_total <- dist
    } else {
      dist_total <- rbind(dist_total, dist)
    }

    nth_sample <- nth_sample + 1

    if((!is.null(dist_total) & nrow(dist_total) >= n_samples)
       | nth_sample > max_times) {
      break
    }
  }

  parallel::stopCluster(cl)

  if(debug) {
    cat("Get company profiles by evidence from", colnames(evidence_data),
        "for targets", target_variables, "completed with", nrow(dist_total), "sample", "\n")
  }


  return(dist_total)
}


#' Get KPI Score
#'
#' @param data A list of data to calculate score, each item present a data frame of KPI values
#' @param sector_variable Sector variable name
#' @param sector_profiles Sector profile
#' @param probs List of quantiles, default is probs = c(0, 0.25, 0.50, 0.75, 1)
#' @param score_table List of score table corresponding with probs, default is score_table = c(0, 5, 20, 20, 5, 0)
#' @param debug Debug mode
#' @return A list of data scores, each item present a data frame of KPI scores
#' @examples
#' sector_variable <- "NGANHKT"
#' probs <- c(0, 0.05, 0.95, 1)
#' score_table <- c(0, 5, 50, 5)
#' discrete_static_kpi_names <- preprocessed$discrete_static_variables
#' data_scores <- tfdbn::calculate_kpi_score(test_data, sector_variable, sector_profiles, probs, score_table)
#'
#' data_id <- names(data_scores)
#' for(id in data_id) {
#'   anomally_kpi <- get_anomally_kpi(data_scores[[id]], 5)
#' }
calculate_kpi_score <- function(data, sector_variable, sector_profiles,
                                probs = c(0, 0.25, 0.50, 0.75, 1),
                                score_table = c(0, 5, 20, 20, 5, 0),
                                debug = FALSE) {
  #TODO: validate length probs and scrore table
  if(debug) {
    cat(file = stdout(), "Calculate kpi score", "\n")
  }

  data_scores <- list()
  all_id <- names(data)
  for(id in all_id) {
    if(debug) {
      cat(file = stdout(), "Calculate kpi score for", id, "\n")
    }

    data_id <- data[[id]]
    sector_id <- data_id[, sector_variable][1]
    sector_profile <- sector_profiles[[sector_id]]
    sector_kpi <- colnames(sector_profile)

    kpi_scores <- c()
    for(kpi in sector_kpi) {
      kpi_profile <- sector_profile[, kpi]
      kpi_quantile <- quantile(kpi_profile, probs)

      kpi_value <- data_id[, kpi]

      kpi_score <- get_kpi_score(kpi_value, kpi_quantile, score_table)

      kpi_scores <- c(kpi_scores, kpi_score)
    }
    data_score <- rbind(sector_kpi, kpi_scores)
    data_score <- data.frame(data_score)
    data_score <- data_score[-1, ]
    colnames(data_score) <- sector_kpi

    data_scores[[id]] <- data_score
    if(debug) {
      cat(file = stdout(), "Calculate kpi score for", id, "completed", "\n")
    }

  }

  if(debug) {
    cat(file = stdout(), "Calculate kpi score", "completed", "\n")
  }


  return(data_scores)
}

calculate_company_kpi_score <- function(actual_data, profile_data,
                                probs = c(0, 0.25, 0.50, 0.75, 1),
                                score_table = c(0, 5, 20, 20, 5, 0),
                                debug = FALSE) {
  if(length(probs) != length(score_table)) {
    cat(file = stderr(), "Probs and score length is not match", "\n")
    return(NULL)
  }
  if(debug) {
    cat(file = stdout(), "Calculate company kpi score", "\n")
  }

  data_scores <- list()
  all_id <- names(actual_data)
  for(id in all_id) {
    if(debug) {
      cat(file = stdout(), "Calculate company kpi score for", id, "\n")
    }

    company_data <- actual_data[[id]]
    company_profile <- profile_data[[id]]
    if(is.null(company_profile)) {
      break
    }
    profile_kpi <- colnames(company_profile)

    kpi_scores <- c()
    for(kpi in profile_kpi) {
      kpi_profile <- company_profile[, kpi]
      kpi_quantile <- quantile(kpi_profile, probs)

      kpi_value <- company_data[, kpi]

      kpi_score <- get_kpi_score(kpi_value, kpi_quantile, score_table)

      kpi_scores <- c(kpi_scores, kpi_score)
    }
    data_score <- rbind(profile_kpi, kpi_scores)
    data_score <- data.frame(data_score)
    data_score <- data_score[-1, ]
    colnames(data_score) <- profile_kpi

    data_scores[[id]] <- data_score
    if(debug) {
      cat(file = stdout(), "Calculate kpi score for", id, "completed", "\n")
    }

  }

  if(debug) {
    cat(file = stdout(), "Calculate kpi score", "completed", "\n")
  }


  return(data_scores)
}

#' Get anomaly KPI probabilities
#'
#' @param fitted A list of data to calculate score, each item present a data frame of KPI values
#' @param data_test A list of data to calculate score, each item present a data frame of KPI values
#' @param data_score A list of data to calculate score, each item present a data frame of KPI scores
#' @param sector_variable Sector variable name
#' @param except_variables List of variables to ignore
#' @param sector_factor Sector in string
#' @param sector_profile Sector KPI profile
#' @param kpi_score_threshold Threshold
#' @param probs List of quantiles, default is probs = c(0, 0.25, 0.50, 0.75, 1)
#' @param n_cluster Number of CPU to calculate
#' @param n_times Number of calculation
#' @param debug Debug mode
#' @return A list of data scores, each item present a data frame of KPI scores
#' @examples
#' sector_variable <- "NGANHKT"
#'
#' data_probs <- tfdbn::calculate_prob_score(fitted, test_data, data_scores, sector_variable, c("VON", "TUOI"), data_factor[[sector_variable]], sector_profiles, 5, c(0, 0.05, 0.95, 1), 4, 5)
calculate_prob_score <- function(fitted, data_test, data_score, sector_variable, except_variables,
                                 sector_factor, sector_profile, kpi_score_threshold, probs,
                                 n_cluster = 4, n_times = 10, debug = FALSE) {
  data_prob <- list()
  cluster <- parallel::makeCluster(n_cluster)
  all_id <- names(data_test)
  for(id in all_id) {

    data_id <- data_test[[id]]

    sector <- as.character(data_id[, sector_variable])
    sector_id <- which(sector_factor %in% sector)

    if(length(sector_id) != 0) {
      anomally_kpi <- get_anomally_kpi(data_scores[[id]], kpi_score_threshold)
      anomally_kpi <- setdiff(anomally_kpi, except_variables)
      if(length(anomally_kpi) > 0) {
        kpi_ranges <- list()
        lower <- c()

        for(kpi in anomally_kpi) {

          kpi_value <- data_id[, kpi]

          kpi_profile <- sector_profile[[sector]][, kpi]

          kpi_range <- get_kpi_range_around_value(kpi_value, kpi_profile, probs)

          if(!is.null(kpi_range)) {
            kpi_ranges[[kpi]] <- kpi_range
          }

        }

        event_equation <- generate_continuous_equation(anomally_kpi, "event", kpi_ranges)

        evidence_ranges <- list()
        evidence_ranges[[sector_variable]] <- c(sector_id - 0.5, sector_id + 0.5)
        evidence_equation <- generate_continuous_equation(c(sector_variable), "evidence", evidence_ranges)
        query <- paste(paste("bnlearn::cpquery(fitted, ", event_equation, ", ", evidence_equation, ", cluster=cluster, n = 1e6)", sep = ""))
        prob_all <- c()
        for(i in 1:n_times) {
          prob <- eval(parse(text=query))
          prob_all <- c(prob_all, prob)
        }


        data_prob[[id]] <- mean(prob_all)
      } else {
        data_prob[[id]] <- 1
      }

    }

  }

  parallel::stopCluster(cluster)
  return(data_prob)
}

calculate_company_prob_score <- function(fitted, actual_data, event_variables, evidence_variables,
                                 n_cluster = 4, n_times = 10, debug = FALSE) {
  if(debug) {
    cat(file = stdout(), "Calculate company probability score by event", event_variables,
        "and evidence variables", evidence_variables, "\n")
  }
  data_prob <- list()
  cluster <- parallel::makeCluster(n_cluster)
  all_id <- names(actual_data)
  for(id in all_id) {
    if(debug) {
      cat(file = stdout(), "Calculate company probability for company", id, "\n")
    }
    company_data <- actual_data[[id]]
    company_prob_scores <- c(event_variables)

    evidence_data <- list()
    for(evidence_variable in evidence_variables) {
      evidence_data[evidence_variable] <- company_data[, evidence_variable]
    }

    event_probs <- c()
    for(event_variable in event_variables) {
      if(debug) {
        cat(file = stdout(), "Calculate company probability for company", id, "with event", event_variable, "\n")
      }
      if(is.na(company_data[, event_variable])) {
        event_probs <- c(event_probs, -1)
      } else {
        event_data <- list()
        event_data[event_variable] <- company_data[, event_variable]
        event_equation <- generate_continuous_equation_2(event_variable, "event", company_data[, event_variable])
        evidence_equation <- generate_continuous_equation_2(evidence_variables, "evidence", as.numeric(company_data[, evidence_variables]))
        query <- paste("bnlearn::cpquery(fitted, ", event_equation, ", ", evidence_equation, ", cluster=cluster, n = 1e4)", sep = "")

        prob_all <- c()
        for(i in 1:n_times) {
          prob <- eval(parse(text=query))
          prob_all <- c(prob_all, prob)
        }
        event_probs <- c(event_probs, round(mean(prob), 3))
      }
      if(debug) {
        cat(file = stdout(), "Calculate company probability for company", id, "with event", event_variable, "completed", "\n")
      }
    }

    company_prob_scores <- rbind(company_prob_scores, event_probs)
    company_prob_scores <- as.data.frame(company_prob_scores)
    company_prob_scores <- company_prob_scores[-1, ]
    colnames(company_prob_scores) <- event_variables
    data_prob[[id]] <- company_prob_scores
    if(debug) {
      cat(file = stdout(), "Calculate company probability for company", id, "completed", "\n")
    }
  }

  parallel::stopCluster(cluster)
  return(data_prob)
}

calculate_company_fraud_prob <- function(fitted, actual_data, event_variables, evidence_variables,
                                         n_cluster = 4, n_times = 10, debug = FALSE) {
  if(debug) {
    cat(file = stdout(), "Calculate fraud probability score by event", event_variables,
        "and evidence variables", evidence_variables, "\n")
  }
  data_prob <- 1:2
  cluster <- parallel::makeCluster(n_cluster)
  all_id <- names(actual_data)
  for(id in all_id) {
    if(debug) {
      cat(file = stdout(), "Calculate fraud probability for company", id, "\n")
    }
    company_data <- actual_data[[id]]

    evidence_data <- list()
    for(evidence_variable in evidence_variables) {
      evidence_data[evidence_variable] <- company_data[, evidence_variable]
    }

    event_around_prob <- 1
    event_upper_prob <- 1
    event_lower_prob <- 1
    if(sum(is.na(company_data[, event_variables])) > 0) {
      event_prob <- -1
    } else {
      event_data <- list()
      event_data[event_variables] <- company_data[, event_variables]
      event_around_equation <- generate_continuous_equation_2(event_variables, "event", as.numeric(company_data[, event_variables]))
      event_upper_equation <- generate_continuous_equation_2(event_variables, "event", as.numeric(company_data[, event_variables]), "upper")
      event_lower_equation <- generate_continuous_equation_2(event_variables, "event", as.numeric(company_data[, event_variables]), "lower")

      evidence_equation <- generate_continuous_equation_2(evidence_variables, "evidence", as.numeric(company_data[, evidence_variables]), " | ")
      query_around <- paste("bnlearn::cpquery(fitted, ", event_around_equation, ", ", evidence_equation, ", cluster=cluster, n = 1e4)", sep = "")
      query_upper <- paste("bnlearn::cpquery(fitted, ", event_upper_equation, ", ", evidence_equation, ", cluster=cluster, n = 1e4)", sep = "")
      query_lower <- paste("bnlearn::cpquery(fitted, ", event_lower_equation, ", ", evidence_equation, ", cluster=cluster, n = 1e4)", sep = "")
      prob_around_all <- c()
      prob_upper_all <- c()
      prob_lower_all <- c()

      for(i in 1:n_times) {
        prob_around <- eval(parse(text=query_around))
        prob_upper <- eval(parse(text=query_upper))
        prob_lower <- eval(parse(text=query_lower))

        prob_around_all <- c(prob_around_all, prob_around)
        prob_upper_all <- c(prob_around_all, prob_upper)
        prob_lower_all <- c(prob_around_all, prob_lower)
      }
      event_around_prob <- round(mean(prob_around_all), 3)
      event_upper_prob <- round(mean(prob_upper_all), 3)
      event_lower_prob <- round(mean(prob_lower_all), 3)
    }
    if(debug) {
      cat(file = stdout(), "Calculate fraud probability for company", id, "with event", event_variable, "completed", "\n")
    }

    data_prob <- rbind(data_prob, c(id, event_around_prob, event_upper_prob, event_lower_prob))
    if(debug) {
      cat(file = stdout(), "Calculate fraud probability for company", id, "completed", "\n")
    }
  }
  data_prob <- as.data.frame(data_prob)
  data_prob <- data_prob[-1, ]
  colnames(data_prob) <- c("report_id", "fraud_prob", "fraud_upper_prob", "fraud_lower_prob")

  parallel::stopCluster(cluster)
  return(data_prob)
}

calculate_company_target_likelihood <- function(fitted, actual_data, evidence_variables, target_variables,
                                         n_cluster = 4, n_times = 10, debug = FALSE) {
  if(debug) {
    cat(file = stdout(), "Calculate target likelihood for with event", evidence_variables, "\n")
  }
  keys <- names(actual_data)
  predict_target_data <- 1:(length(target_variables) * 2 + 1)
  for(key in keys) {
    if(debug) {
      cat(file = stdout(), "Calculate target likelihood for company", id, "with event", evidence_variables, "\n")
    }
    evidence_data <- list()
    values <- actual_data[[key]]
    target_actuals <- values[, target_variables]
    for(evidence_variable in evidence_variables) {
      evidence_value <- values[1, evidence_variable]
      if(!is.na(evidence_value)) {
        evidence_data[evidence_variable] <- evidence_value
      }
    }

    target_dist <- get_company_profiles(fitted, evidence_data, target_variables, n_samples, max_times, n_cluster, debug = FALSE)
    predict_target_data <- rbind(predict_target_data, c(key, rowMeans(target_dist), target_actuals))
    if(debug) {
      cat(file = stdout(), "Calculate target likelihood for company", id, "with event", evidence_variables, "completed", "\n")
    }
  }
  predict_target_data <- data.frame(predict_target_data)
  row.names(predict_target_data) <- NULL
  predict_target_data <- predict_target_data[-1, ]
  colnames(predict_target_data) <- c("id", paste(target_variables, "mean", sep = "_"), paste(target_variables, "actual", sep = "_"))
  sorted_colnames <- sort(colnames(predict_target_data))
  predict_target_data <- predict_target_data[, sorted_colnames]

  if(debug) {
    cat(file = stdout(), "Calculate target likelihood with event", evidence_variables, "completed", "\n")
  }
  return(predict_target_data)
}

get_model_kpi <- function(model, target_variable){
  results <- list()
  fitted <- model$fitted
  target_residuals <- fitted[[target_variable]]$residuals

  results['residual_mean'] <- mean(target_residuals)
  results['residual_sd'] <- sd(target_residuals)

  arcs <- model$dyn.avg$arcs
  arcs_strength_all <- model$bootstrap
  arcs_strength <- merge(x = arcs, arcs_strength_all, by = c("from", "to"))

  target_arcs <- tfdbn::filter_graph_by_target("K_C_V18_2", arcs)
  target_arcs_strength <- merge(x = target_arcs, arcs_strength_all, by = c("from", "to"))

  results['total_arc'] <- nrow(arcs)
  results['arc_strength_mean'] <- mean(arcs_strength[, "strength"])

  results['total_target_arc'] <- nrow(target_arcs)
  results['arc_strength_mean'] <-  mean(target_arcs_strength[, "strength"])

  nodes <- model$dyn.avg$nodes
  results['avg_mb_size'] <- get_mb_size(nodes)
  results['avg_nbr_size'] <- get_nbr_size(nodes)
  results['arc_threshold'] <- model$dyn.avg$learning$args$threshold

  return(results)
}






