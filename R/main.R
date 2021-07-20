preprocess_training_data <- function(data, type, index_column, time_column, time_values,
                                   continuous_kpi_names, continuous_static_kpi_names, discrete_static_kpi_names, current_layers,
                                   desire_layers, quantile_number = -1, na_omit = TRUE, debug = FALSE) {


  data <- data[, c(index_column, time_column, continuous_kpi_names, continuous_static_kpi_names, discrete_static_kpi_names)]
  index_values <- unique(data[, index_column])
  data_constructed <- reconstruct_report_data(data, index_column, index_values, time_column, time_values, current_layers, debug)

  continuous_dynamic_variables <- c()
  for(name in c(continuous_kpi_names)) {
    continuous_dynamic_variables <- c(continuous_dynamic_variables, c(paste(name, 1:current_layers, sep = "_")))
  }

  continuous_static_variables <- c()
  for(name in c(continuous_static_kpi_names)) {
    continuous_static_variables <- c(continuous_static_variables, c(paste(name, 1:current_layers, sep = "_")))
  }

  continuous_variables <- c(continuous_dynamic_variables, continuous_static_variables)

  discrete_variables <- c()
  for(name in discrete_static_kpi_names) {
    discrete_variables <- c(discrete_variables, c(paste(name, 1:current_layers, sep = "_")))
  }

  data_constructed <- data_constructed[, c(continuous_variables, discrete_variables)]

  continuous_dynamic_desire_variables <- c()
  for(name in c(continuous_kpi_names)) {
    continuous_dynamic_desire_variables <- c(continuous_dynamic_desire_variables, c(paste(name, 1:desire_layers, sep = "_")))
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

      result[["data"]] <- remove_redundant_variables(data_discrete_dbn[[1]], c(continuous_static_kpi_names, discrete_static_kpi_names), debug)
      result[["break_list"]] <- data_discrete_dbn[[2]]
    } else {
      result[["data"]] <- remove_redundant_variables(data_discrete_dbn[[1]], c(continuous_static_kpi_names, discrete_static_kpi_names))
    }

    return(result)

  } else if(type == "continuous"){
    data_continuous_dbn <- prepare_constructed_report_data(data_constructed, current_layers, desire_layers,
                                                           discrete_variables, continuous_variables, c(), quantile_number, na_omit, debug)
    result[["data"]] <- remove_redundant_variables(data_continuous_dbn, c(continuous_static_kpi_names, discrete_static_kpi_names))
    return(result)
  } else {
    result <- list()
  }
}

preprocess_test_data <- function(data, type, index_column, time_column, time_values,
                                  continuous_kpi_names, continuous_static_kpi_names,
                                  discrete_static_kpi_names, current_layers, desire_layers,
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

      a_report_preprocessed <- tfdbn::preprocess_training_data(a_report, type, index_column, time_column, layer_times,
                                                               continuous_kpi_names, continuous_static_kpi_names,
                                                               discrete_static_kpi_names,
                                                               desire_layers, desire_layers, quantile_number, FALSE)
      if(debug) {
        cat("Preprocess data test for", id, "in layer", i, "completed", "\n")
      }

      reports[[paste(paste(layer_times, collapse = "_"), as.character(id), sep = "_")]] <- a_report_preprocessed$data
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
    #print(dist_equation)
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

calculate_prob_score <- function(fitted, data_test, data_score, sector_variable, except_variables,
                                 sector_factor, sector_profile, kpi_score_threshold, probs,
                                 n_cluster = 4, n_times = 10, debug = FALSE) {
  data_prob <- list()
  cluster <- parallel::makeCluster(cluster)
  all_id <- names(data_test)
  for(id in all_id) {
    print(id)
    data_id <- data_test[[id]]

    sector <- as.character(data_id[, sector_variable])
    sector_id <- which(sector_factor %in% sector)
    print(sector_id)
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

          kpi_ranges[[kpi]] <- kpi_range
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
      }

    }

  }

  parallel::stopCluster(cluster)
  return(data_prob)
}






