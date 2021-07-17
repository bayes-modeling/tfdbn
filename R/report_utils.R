# Title     : TODO
# Objective : TODO
# Created by: ADMIN
# Created on: 6/19/2021

preprocess_report_data <- function(file_data, index_column, time_column, time_values, filter_columns, filter_values, filter_operations,
                                   continuous_kpi_names, continuous_unchange_kpi_names, discrete_kpi_names, current_layers, desire_layers, quantile_number = -1) {

  data <- data.frame(fread(file_data, sep = ",", header = TRUE))
  #Filter by super sector
  data <- filter_data(data, filter_columns, filter_values, filter_operations)
  data <- data[, c(index_column, time_column, continuous_kpi_names, continuous_unchange_kpi_names, discrete_kpi_names)]
  print(dim(data))
  index_values <- unique(data[, index_column])
  data_constructed <- reconstruct_report_data(data, index_column, index_values, time_column, time_values, current_layers)
  print(dim(data_constructed))
  print(colnames(data_constructed))
  print(head(data_constructed))
  continuous_variables <- c()
  for(name in c(continuous_kpi_names, continuous_unchange_kpi_names)) {
    continuous_variables <- c(continuous_variables, c(paste(name, 1:current_layers, sep = "_")))
  }

  discrete_variables <- c()
  for(name in discrete_kpi_names) {
    discrete_variables <- c(discrete_variables, c(paste(name, 1:current_layers, sep = "_")))
  }

  data_constructed <- data_constructed[, c(continuous_variables, discrete_variables)]

  quantiled_variables <- c()

  if(quantile_number > 1) {
    quantile_number <- rep(quantile_number, length(continuous_kpi_names) * desire_layers)
    for(kpi in continuous_kpi_names) {
      quantiled_variables <- c(quantiled_variables, paste(kpi, 1:desire_layers, sep = "_"))
    }
  }

  print(head(data_constructed))

  data_discrete_dbn <- prepare_constructed_report_data(data_constructed, current_layers, desire_layers, discrete_variables, continuous_variables, quantiled_variables, quantile_number)
  data_continuous_dbn <- prepare_constructed_report_data(data_constructed, current_layers, desire_layers, discrete_variables, continuous_variables)

  result <- list()
  if(quantile_number > 1) {
    result[["discrete"]] <- remove_redundant_variables(data_discrete_dbn[[1]], c(continuous_unchange_kpi_names, discrete_kpi_names))
    result[["break_list"]] <- data_discrete_dbn[[2]]
  } else {
    result[["discrete"]] <- remove_redundant_variables(data_discrete_dbn[[1]], c(continuous_unchange_kpi_names, discrete_kpi_names))
  }

  result[["continuous"]] <- remove_redundant_variables(data_continuous_dbn, c(continuous_unchange_kpi_names, discrete_kpi_names))
  return(result)
}

remove_redundant_variables <- function(data, variables) {
  
  for(variable in variables) {
    all_variables <- colnames(data)
    others <- grep(variable, all_variables, ignore.case = TRUE)
    print(variable)
    print(others)
    data[, variable] <- data[, others[1]]
    data <- data[, -others]
  }
  
  return(data)
}

reconstruct_report_data <- function(data, index, index_values, time_column, time_values, current_layers) {
  cols <- colnames(data)

  new_cols <- c()
  for(i in 1:current_layers) {
    new_cols <- c(new_cols, paste(cols, i, sep = "_"))
  }

  f <- new_cols

  for(i in 1:length(index_values)) {
    process = i*100/length(index_values)
    if(i %% 100 == 0) {
      print(process)
    }

    index_value <- index_values[i]
    index_data <- data[data[, index] == index_value, ]

    row <- c()

    for (time_value in time_values) {
      for(j in 1:ncol(index_data)) {
        value <- as.character(index_data[index_data[, time_column] == time_value, j][1])
        if(identical(value, "character(0)") | identical(value, "") | identical(value, "numeric(0)")) {
          #print(value)
          value <- NA
        }
        row <- c(row, value)
      }
    }

    f <- rbind(f, row)

  }
  f <- f[-1, ]
  colnames(f) <- new_cols
  return(as.data.frame(f))
}

prepare_constructed_report_data <- function(data, current_layers, desire_layers, discrete_variables, continuous_variables, quantiled_variables = c(), quantile = -1) {
  data_dbn <- prepare_report_for_dbn(data, current_layers, desire_layers, discrete_variables, continuous_variables)
  print(dim(data_dbn))

  if(quantile == 1) {
    print("binary quantile")
    data_dbn = quantile_report_binary(data_dbn, quantiled_variables)
  } else if(quantile > 1) {
    print("composite quantile")
    data_dbn <- quantile_report_composite(data_dbn, quantiled_variables, quantile)
  } else {
    data_dbn[data_dbn == 1e10] <- NA
    data_dbn <- na.omit(data_dbn)
  }

  return(data_dbn)
}

prepare_report_for_dbn <- function(data_shifted, current_layers, desire_layers, discrete_variables, continuous_variables) {

  if(length(discrete_variables) != 0) {
    for(discrete_variable in discrete_variables) {
      data_shifted[, discrete_variable] <- factor(as.character(data_shifted[, discrete_variable]))
    }
  }

  if(length(continuous_variables) != 0) {
    for(continuous_variable in continuous_variables) {
      data_shifted[, continuous_variable] <- as.numeric(as.character(data_shifted[, continuous_variable]))
    }
  }

  kpi_names <- sort(colnames(data_shifted))

  data <- prepare_report_data(data_shifted, kpi_names, current_layers, desire_layers)
  print(dim(data))
  data <- na.omit(data)
  print(dim(data))
  return(data)
}

prepare_report_data <- function(data, total_variables, current_layers, desire_layers) {
  print(dim(data))
  if(desire_layers == current_layers) {
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
  print(cols)
  print(node_quantile)
  print(continuous_variables)
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

        print(breaks)

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

            if(q == 0) {
              print(value)
              print(value_scale)
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