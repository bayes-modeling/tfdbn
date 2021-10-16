# Title     : TODO
# Objective : TODO
# Created by: ADMIN
# Created on: 6/19/2021

remove_redundant_variables <- function(data, variables, debug = FALSE) {

  if(debug) {
    cat("Remove redundant variables of dim", dim(data), "\n")
  }

  for(variable in variables) {
    if(debug) {
      cat("Remove redundant variable", variable, "\n")
    }

    all_variables <- colnames(data)
    others <- grep(variable, all_variables, ignore.case = TRUE)
    data[, variable] <- data[, others[1]]
    data <- data[, -others]
  }
  if(debug) {
    cat("Remove redundant variables of dim", dim(data), "completed", "\n")
  }

  return(data)
}

reconstruct_test_data <- function(data, type, index_column, time_column, time_values,
                                     continuous_kpi_names, continuous_static_kpi_names, discrete_static_kpi_names, layers,
                                     normalize_type = NULL, normalizers = NULL, factors = NULL, debug = FALSE) {


  if(debug)
    cat("Reconstruct data with dim", dim(data), "and normalize type", normalize_type, "\n")
  data <- data[, c(index_column, time_column, continuous_kpi_names, continuous_static_kpi_names, discrete_static_kpi_names)]
  index_values <- unique(data[, index_column])
  data_constructed <- reconstruct_report_data(data, index_column, index_values, time_column, time_values, layers, debug)

  if(debug)
    cat("Reconstruct data with dim", dim(data), "and normalize type", normalize_type, "completed", "\n")
  if(is.null(normalizers) | is.null(normalize_type)) {
    return(data_constructed)
  } else {
    return(normalize_data(data_constructed, normalize_type, normalizers)$data)
  }
}

reconstruct_report_data <- function(data, index, index_values, time_column, time_values, current_layers, debug = FALSE) {
  cols <- colnames(data)

  new_cols <- c()
  for(i in 1:current_layers) {
    new_cols <- c(new_cols, paste(cols, i, sep = "_"))
  }

  f <- new_cols

  for(i in 1:length(index_values)) {

    index_value <- index_values[i]
    index_data <- data[data[, index] == index_value, ]

    row <- c()

    for (time_value in time_values) {
      time_data <- index_data[index_data[, time_column] == time_value, ]

      if(nrow(time_data) == 0) {
        row <- c(row, rep(NA, ncol(index_data)))
      } else {
        row <- c(row, as.character(index_data[index_data[, time_column] == time_value, ]))
      }

    }

    f <- rbind(f, row)

  }

  f <- as.data.frame(f)
  f <- f[-1, ]
  colnames(f) <- new_cols
  return(as.data.frame(f))
}

prepare_constructed_report_data <- function(data, current_layers, desire_layers,
                                            discrete_variables, continuous_variables,
                                            quantiled_variables = c(), quantile = -1,
                                            normalize_type = NULL,
                                            na_omit = TRUE, debug = FALSE) {
  if(length(quantile) == 1 && quantile != -1) {
    if(debug) {
      cat("Quantile binary for data dim", dim(data))
    }
    data = quantile_report_binary(data, quantiled_variables)
  } else if(length(quantile) > 1) {
    if(debug) {
      cat("Quantile to", quantile[1], "levels for data dim", dim(data))
    }
    data <- quantile_report_composite(data, quantiled_variables, quantile)
  } else {
    if(na_omit) {
      data <- na.omit(data)
    }
  }
  return(data)
}

prepare_report_for_dbn <- function(data_shifted, current_layers, desire_layers, discrete_variables,
                                   continuous_variables, na_omit, debug = FALSE) {

  if(debug) {
    cat("Prepare data for dbn with dim", dim(data_shifted), "\n")
  }

  if(length(discrete_variables) != 0) {
    if(debug) {
      cat("Prepare data for dbn for discrete variables", discrete_variables, "\n")
    }

    for(discrete_variable in discrete_variables) {
      data_shifted[, discrete_variable] <- as.character(data_shifted[, discrete_variable])
    }
  }

  if(length(continuous_variables) != 0) {
    if(debug) {
      cat("Prepare data for dbn for continuous variables", continuous_variables, "\n")
    }

    for(continuous_variable in continuous_variables) {
      data_shifted[, continuous_variable] <- as.numeric(as.character(data_shifted[, continuous_variable]))
    }
  }

  kpi_names <- sort(colnames(data_shifted))

  data <- prepare_report_data(data_shifted, kpi_names, current_layers, desire_layers)
  if(na_omit) {
    data <- na.omit(data)
  }

  if(debug) {
    cat("Prepare data for dbn with dim", dim(data), "completed", "\n")
  }

  return(data)
}

prepare_report_data <- function(data, total_variables, current_layers, desire_layers, debug=FALSE) {
  if(debug) {
    cat("Prepare data with dim", dim(data), "\n")
  }

  if(desire_layers == current_layers) {
    if(debug) {
      cat("The desire layers equals current layers\n")
    }

    return(data)
  } else if(desire_layers < current_layers) {
    i <- 1
    new_frame <- NULL
    while(i < length(total_variables)) {
      variables <- total_variables[i:(i-1+current_layers)]

      new_data <- data[, variables[1:desire_layers]]

      for(j in 2:(current_layers - desire_layers + 1)) {
        new_values <- data[, variables[j:(j + desire_layers - 1)]]
        colnames(new_values) <- variables[1:desire_layers]
        new_data <- rbind(new_data, new_values)
      }

      if(is.null(new_frame)) {
        new_frame = data.frame(new_data)
      } else {
        new_frame <- cbind(new_frame, new_data)
      }

      i <- i + current_layers
    }

    if(debug) {
      cat("Prepare data with dim", dim(data), "completed", "\n")
    }

    return(new_frame)

  } else {
    return(NULL)
  }
}

quantile_report_binary <- function(data, continuous_variables) {
  data_quantile <- data
  cols <- colnames(data)
  for(i in 1:length(data[1, ])) {
    if(cols[i] %in% continuous_variables) {
      values <- data[, i]
      na_index <- which(values == 1e10)
      zero_index <- which(values == 0)
      negative_index <- which(values < 0)
      positive_index <- which(values > 0 & values < 1e10)
      values[na_index] <- "NAA"
      values[negative_index] <- "NEG"
      values[positive_index] <- "POS"
      values[zero_index] <- "ZER"
      data_quantile[, i] <- factor(values)
    }

  }
  write.csv(data_quantile, "data_binary_quantile_tmp.csv", row.names = FALSE)
  data_quantile <- read.csv("data_binary_quantile_tmp.csv")
  for(i in 1:length(data_quantile[1, ])) {
    data_quantile[, i] <- factor(data_quantile[, i])
  }
  return(data_quantile)
}

quantile_report_composite <- function(data, continuous_variables, node_quantile) {
  data_quantile <- data
  cols <- colnames(data)
  breaks_list <- list()
  #TODO: check length of continuous_variables and node_quantile
  for(k in 1: length(node_quantile)) {

    variable <- continuous_variables[k]
    values <- data[, variable]
    na_index <- which(values == -1010101010.0)
    zero_index <- which(values == 0)
    negative_index <- which(values < 0)

    values_non_zeros <- values[values > 0]

    if(length(values_non_zeros) > 0) {
      if(length(unique(values_non_zeros)) > node_quantile[k]) {
        values_non_zeros <- scale_variables(values_non_zeros)
        breaks <- c(unique(quantile(values_non_zeros, seq(0, 1, 1/node_quantile[k]))))

        value_max <- max(values[values > 0])
        value_min <- min(values[values > 0])

        breaks_list[[variable]] <- list(breaks, c(value_max, value_min))
        for(j in 1:length(values)) {
          value = as.numeric(values[j])

          if(value > 0) {

            value_scale = (value - value_min) / (value_max - value_min)
            q = 0
            for(i in 1:(length(breaks) - 1)) {
              lower = breaks[i]
              upper = breaks[i + 1]

              if(value_scale <= upper) {
                q = i
                break
              }
            }

            values[j] <- paste("Q", q, sep = "")
          }
        }
      } else {
        for(j in 1:length(values)) {
          value = as.numeric(values[j])

          if(value > 0) {
            values[j] <- "OTHER"
          }
        }
      }

    }
    values[na_index] <- "NAA"
    values[zero_index] <- "ZER"
    values[negative_index] <- "NEG"

    data_quantile[, variable] <- values
  }


  write.csv(data_quantile, "data_quantile_tmp.csv", row.names = FALSE)
  data_quantile <- read.csv("data_quantile_tmp.csv")
  file.remove("data_quantile_tmp.csv")

  for(i in 1:length(continuous_variables)) {
    data_quantile[, continuous_variables[i]] <- factor(data_quantile[, continuous_variables[i]])
  }
  return(list(data_quantile, breaks_list))
}
