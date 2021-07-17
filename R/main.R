preprocess_report_data <- function(data, type, index_column, time_column, time_values,
                                   continuous_kpi_names, continuous_static_kpi_names, discrete_static_kpi_names, current_layers, desire_layers, quantile_number = -1) {


  data <- data[, c(index_column, time_column, continuous_kpi_names, continuous_static_kpi_names, discrete_static_kpi_names)]
  index_values <- unique(data[, index_column])
  data_constructed <- reconstruct_report_data(data, index_column, index_values, time_column, time_values, current_layers)

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
  result[["continuous_static_variables"]] <- continuous_static_kpi_names
  result[["discrete_static_variables"]] <- discrete_static_kpi_names
  result[["quantile_number"]] <- quantile_number
  result[["desire_layers"]] <- desire_layers

  if(type == "discrete") {
    quantiled_variables <- c()

    if(quantile_number > 1) {
      quantile_numbers <- rep(quantile_number, length(c(continuous_kpi_names, continuous_static_kpi_names)) * desire_layers)
      for(kpi in c(continuous_kpi_names, continuous_static_kpi_names)) {
        quantiled_variables <- c(quantiled_variables, paste(kpi, 1:desire_layers, sep = "_"))
      }
    }

    data_discrete_dbn <- prepare_constructed_report_data(data_constructed, current_layers, desire_layers, discrete_variables, continuous_variables, quantiled_variables, quantile_numbers)

    if(quantile_number > 1) {

      result[["discrete"]] <- remove_redundant_variables(data_discrete_dbn[[1]], c(continuous_static_kpi_names, discrete_static_kpi_names))
      result[["break_list"]] <- data_discrete_dbn[[2]]
    } else {
      result[["discrete"]] <- remove_redundant_variables(data_discrete_dbn[[1]], c(continuous_static_kpi_names, discrete_static_kpi_names))
    }

    return(result)

  } else if(type == "continuous"){
    data_continuous_dbn <- prepare_constructed_report_data(data_constructed, current_layers, desire_layers, discrete_variables, continuous_variables)
    result[["continuous"]] <- remove_redundant_variables(data_continuous_dbn, c(continuous_static_kpi_names, discrete_static_kpi_names))
    return(result)
  } else {
    result <- list()
  }
}

get_continuous_structure_filter <- function(continuous_data, desire_layers, quantile_number, continuous_dynamic_variables,
                                            continuous_static_variables, discrete_static_variables,
                                            known_structure, corr_threshold, is_blacklist_internal,
                                            is_variable_only, is_blacklist_other,
                                            custom_blacklist, custom_whitelist){


    continuous_data <- continuous_data[, c(continuous_dynamic_variables, continuous_static_variables, discrete_static_variables)]

    rho <- abs(cor(convert_variables_to_factor(continuous_data)))
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
                                            custom_blacklist, custom_whitelist){


  discrete_data <- discrete_data[, c(continuous_dynamic_variables, continuous_static_variables, discrete_static_variables)]

  rho <- abs(cor(convert_variables_to_factor(discrete_data)))
  rho <- as.matrix(rho)

  arc_structures <- structure_filter(known_structure, desire_layers, continuous_dynamic_variables,
                                     c(continuous_static_variables, discrete_static_variables),
                                     is_blacklist_internal, is_blacklist_other, is_variable_only,
                                     rho, corr_threshold,
                                     custom_blacklist, custom_whitelist)

  return(arc_structures)
}





