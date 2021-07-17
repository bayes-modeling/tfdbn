library(data.table)

saveUploadedFile <- function(fileUpload, saveFolder, datasetType) {
  tryCatch (
    {
      # TOD: Validate folder exist
      filePath = paste(saveFolder, datasetType, "_", fileUpload$name, sep = "")
      file.copy(fileUpload$datapath, filePath)
      print("File uploaded")
    },
    error = function(e) {
      print(e)
    }
  )
}

save_arcs <- function(model_path, blacklist, whitelist, type) {
  
  if(!dir.exists(model_path)) {
    dir.create(model_path)
  }
  
  type_path <- paste(model_path, type, sep = "/")
  if(!dir.exists(type_path)) {
    dir.create(type_path)
  }
  
  write.csv(blacklist, paste(model_path, type, "blacklist.csv", sep = "/"))
  write.csv(whitelist, paste(model_path, type, "whitelist.csv", sep = "/"))
}

get_datset_type_from_name <- function(file_name) {
  print(file_name)
  file_names <- strsplit(file_name, "/")[[1]]
  print(file_names)
  file_names <- file_names[length(file_names)]
  print(file_names)
  file_names <- strsplit(file_names, "_")[[1]]
  file_name <- file_names[1]
  print(file_name)
  if(file_name == "Timeseries") {
    return("Timeseries")
  }
  
  return("Report")
}

generate_model_name <- function(processed_file) {
  preprocessed_file <- get_file_name(processed_file)
  model_name <- paste(preprocessed_file, stri_rand_strings(1, 3), sep = "-")
  return(model_name)
}

get_datasets <- function(save_folder) {
  cat(file = stderr(), "Get dataset from", save_folder, "\n")
  files <- list.files(save_folder, full.names = TRUE)
  file_infos <- c("File", "Size", "Record")
  
  file_names <- c()
  file_sizes <- c()
  file_records <- c()
  file_mtimes <- c()
  file_ctimes <- c()
  
  for(file_name in files) {
    cat(file = stderr(), "Load file from", file_name, "\n")
    if(file_test("-f", file_name)) {
      file_data <- fread(file_name, sep = ",", header = TRUE)
      file_info <- file.info(file_name)
      
      file_size = calculate_file_size(file_info$size)
      file_record = nrow(file_data)
      file_created = as.character(file_info$ctime)
      file_mtime = as.character(file_info$mtime)
      
      file_names <- c(file_names, file_name)
      file_sizes <- c(file_sizes, file_size)
      file_records <- c(file_records, file_record)
      file_mtimes <- c(file_mtimes, file_mtime)
      file_ctimes <- c(file_ctimes, file_created)
    }
  }
  
  file_infos <- data.frame(File = file_names, Size = file_sizes, Record = file_records, Modified = file_mtimes, Created = file_ctimes)
  
  cat(file = stderr(), "Total file is", nrow(file_infos), "\n")
  return(file_infos)
}

get_processed_data <- function(saved_folder) {
  cat(file = stderr(), "Get processed dataset from", saved_folder, "\n")
  files <- list.files(saved_folder, full.names = TRUE)
  
  processed_folder <- c()
  for(a_file in files) {
    if(file_test("-d", a_file)) {
      processed_folder <- c(processed_folder, a_file)
    }
  }
  cat(file = stderr(), "Get processed dataset from", saved_folder, "completed \n")
  return(processed_folder)
}

get_data_columns <- function(filePath) {
  cat(file = stderr(), "Get dataset columns from", filePath, "\n")
  data <- data.frame(fread(filePath, sep = ",", header = TRUE))
  datatset_columns <- colnames(data)
  cat(file = stderr(), "Get dataset columns", datatset_columns, "from", filePath, "\n")
  return(datatset_columns)
}

load_processed_data <- function(folder_path) {
  processed_data <- list()
  processed_discrete_path <- get_processed_discrete_path(folder_path)
  processed_continuous_path <- get_processed_continuous_path(folder_path)
  processed_params_path <- get_processed_params_path(folder_path)
  
  print(processed_discrete_path)
  processed_data$discrete <- as.data.frame(fread(processed_discrete_path, header = TRUE, sep = ","))
  processed_data$continuous <- as.data.frame(fread(processed_continuous_path, header = TRUE, sep = ","))
  processed_data$params <- dget(processed_params_path)
  
  return(processed_data)
}

load_structure_constraint <- function(folder_path) {
  cat(file = stderr(), "Load constraint structure from", folder_path, "\n")
  structure <- list()
  
  structure$continuous <- load_continuous_constraint(folder_path)
  structure$discrete <- load_discrete_constraint(folder_path)
  
  cat(file = stderr(), "Load constraint structure from", folder_path, "completed", "\n")
  return(structure)
}

load_discrete_constraint <- function(folder_path) {
  cat(file = stderr(), "Load discrete constraint structure from", folder_path, "\n")
  structure_discrete <- list()
  
  #TODO: Validate file existed + validate form
  structure_discrete$blacklist <- read.csv(paste0(folder_path, "/discrete/blacklist.csv"))[, c("from", "to")]
  structure_discrete$whitelist <- read.csv(paste0(folder_path, "/discrete/whitelist.csv"))[, c("from", "to")]
  
  cat(file = stderr(), "Discrete blacklist structure dim", dim(structure_discrete$blacklist), "\n")
  cat(file = stderr(), "Discrete whitelist structure dim", dim(structure_discrete$whitelist), "\n")
  cat(file = stderr(), "Load discrete constraint structure from", folder_path, "completed", "\n")
  
  return(structure_discrete)
}

load_continuous_constraint <- function(folder_path) {
  cat(file = stderr(), "Load continuous constraint structure from", folder_path, "\n")
  structure_continuous <- list()
  
  #TODO: Validate file existed + validate form
  structure_continuous$blacklist <- read.csv(paste0(folder_path, "/continuous/blacklist.csv"))[, c("from", "to")]
  structure_continuous$whitelist <- read.csv(paste0(folder_path, "/continuous/whitelist.csv"))[, c("from", "to")]
  
  cat(file = stderr(), "Continuous blacklist structure dim", dim(structure_continuous$blacklist), "\n")
  cat(file = stderr(), "Continuous whitelist structure dim", dim(structure_continuous$whitelist), "\n")
  cat(file = stderr(), "Load continuous constraint structure from", folder_path, "completed", "\n")
  return(structure_continuous)
}

get_proccessed_name <- function(processed_path) {
  path_elems <- strsplit(processed_path, "/")[[1]]
  return(path_elems[length(path_elems)])
}

get_processed_discrete_path <- function(proccessed_path) {
  processed_name <- get_proccessed_name(proccessed_path)
  return(paste(proccessed_path, "/", processed_name, "_discrete.csv", sep = ""))
}

get_processed_continuous_path <- function(proccessed_path) {
  processed_name <- get_proccessed_name(proccessed_path)
  return(paste(proccessed_path, "/", processed_name, "_continuous.csv", sep = ""))
}

get_processed_params_path <- function(proccessed_path) {
  processed_name <- get_proccessed_name(proccessed_path)
  return(paste(proccessed_path, "/", processed_name, "_params.R", sep = ""))
}

get_all_discrete_values <- function(data, discrete_variables) {
  return(unique(data[, discrete_variables]))
}

delete_dataset <- function(datasetId, saveFolder) {
  file <- list.files(saveFolder, full.names = TRUE)[datasetId]
  cat(file = stderr(), "Delete dataset", file, " from", saveFolder, "\n")
  file.remove(file)
  cat(file = stderr(), "File", file, "deleted successfully")
}

calculate_file_size <- function(object_size) {
  KB = 1024
  MB = KB * 1024
  GB = MB * 1024
  
  if (object_size > GB) {
    return(paste(round(object_size / GB, 2), "GB"))
  } else if(object_size > MB) {
    return(paste(round(object_size / MB, 2), "MB"))
  } else if(object_size > KB) {
    return(paste(round(object_size / KB, 2), "KB"))
  } else {
    return(paste(object_size, "B"))
  }
}

save_preprocess_data <- function(folder, file_name, processed_data, params) {
  
  if(!dir.exists(folder)) {
    dir.create(folder)
  }
  
  sub_folder = paste(folder, file_name, sep = "/")
  
  if(!dir.exists(sub_folder)) {
    dir.create(sub_folder)
  }
  
  discrete_file <- paste(sub_folder, "/", file_name, "_discrete.csv", sep = "")
  continuous_file <- paste(sub_folder, "/", file_name, "_continuous.csv", sep = "")
  params_file <- paste(sub_folder, "/", file_name, "_params.R", sep = "")
  
  write.table(processed_data$discrete, discrete_file, sep = ",", row.names = FALSE)
  write.table(processed_data$continuous, continuous_file, sep = ",", row.names = FALSE)
  dput(params, params_file)
  
}

scale_variables <- function(values) {
  ma <- max(values)
  mi <- min(values)
  values <- (values - mi)/(ma - mi)
  return(values)
}

get_file_name <- function(file_path) {
  elems <- strsplit(file_path, "/")[[1]]
  file_name <- elems[length(elems)]
  return(tools::file_path_sans_ext(file_name))
}

filter_data <- function(data, filter_columns, filter_values, filter_operation) {
  
  if(length(filter_columns) == 0) return(data)
  for(i in 1:length(filter_columns)) {
    column_name = filter_columns[i]
    column_value = filter_values[i]
    operation = filter_operation[i]
    
    print(column_name)
    if(operation == "==") {
      data <- data[data[, column_name] == column_value, ]
    } else if(operation == "!=") {
      data <- data[data[, column_name] != column_value, ]
    } else if(operation == ">=") {
      data <- data[data[, column_name] >= column_value, ]
    } else if(operation == "<=") {
      data <- data[data[, column_name] <= column_value, ]
    } else if(operation == ">") {
      data <- data[data[, column_name] > column_value, ]
    } else if(operation == "<") {
      data <- data[data[, column_name] < column_value, ]
    } else {
      cat(file = stderr(), "Unsupported operation", operation, "\n")
    }
  }
  return(data)
}

group_levels <- function(data, data_levels) {
  cols <- colnames(data)
  group_level <- matrix(0, nrow = length(data_levels), ncol = length(cols))
  colnames(group_level) <- cols
  rownames(group_level) <- data_levels
  
  for(i in 1:length(cols)) {
    value_levels <- unique(data[, i])
    for(level in value_levels){
      if(level %in% data_levels) {
        count <- sum(data[, i] == level)
        proportion <- round(count / nrow(data), 3) * 100
        group_level[level, i] = proportion
      }
    }
  }
  
  return(group_level)
}

group_continuous <- function(data) {
  group_result <- matrix(0, nrow = ncol(data), ncol = 5)
  colnames(group_result) <- c("Lower 95", "Lower 85", "Mean", "Upper 85", "Upper 95")
  rownames(group_result) <- colnames(data)
  for(i in 1:ncol(data)) {
    values <- data[, i]
    error_95 <- round(qt(0.975,df=length(values)-1)*sd(values)/sqrt(length(values)), 5)
    error_85 <- round(qt(0.925,df=length(values)-1)*sd(values)/sqrt(length(values)), 5)
    
    value_mean <- round(mean(values), 5)
    lower_85 <- value_mean - error_85
    upper_85 <- value_mean + error_85
    
    lower_95 <- value_mean - error_95
    upper_95 <- value_mean + error_95
    
    group_result[i, 1] <- lower_95
    group_result[i, 2] <- lower_85
    group_result[i, 3] <- value_mean
    group_result[i, 4] <- upper_85
    group_result[i, 5] <- upper_95
  }
  
  return(group_result)
}

remove_bias_variables <- function(data, pattern, threshold) {
  data_length = dim(data)[1]
  print(data_length)
  remove_col_index <- c()
  for(col in colnames(data)) {
    number_zeros = sum(as.character(data[, col]) == pattern)
    if((number_zeros / data_length) >= threshold) {
      print(paste("Variables", col, "removed"))
      remove_col_index <- c(remove_col_index, which(col == colnames(data)))
    }
  }
  if(length(remove_col_index) != 0) {
    return(data[, -remove_col_index])
  }
  return(data)
}

convert_to_numeric <- function(discrete_data) {
  col_names <- colnames(discrete_data)
  
  for(col_name in col_names) {
    col_data <- as.double(as.factor(discrete_data[, col_name]))
    discrete_data[, col_name] <- col_data
  }
  
  return(discrete_data)
}

get_dynamic_variables <- function(variables, layers) {
  dynamic_variables <- c()
  
  for(variabel in variables) {
    dynamic_variables <- c(dynamic_variables, paste(variabel, 1:layers, sep = "_"))
  }
  
  return(dynamic_variables)
}

init_blacklist <- function(total_variables, number_of_layer) {
  bl <- data.frame(from = character(), to = character())
  for(i in number_of_layer:1) {
    froms <- grep(paste(i, "$", sep = ""), total_variables, value = TRUE)
    if(i > 1) {
      for(j in 1:i-1) {
        tos <- grep(paste(j, "$", sep = ""), total_variables, value = TRUE)
        empty <- expand.grid(from = froms, to = tos, stringsAsFactors = FALSE)
        bl <- rbind(bl, empty)
      }
    } else {
      empty.t1 <- expand.grid(from = froms, to = froms, stringsAsFactors = FALSE)
      bl <- rbind(bl, empty.t1)
    }
  }
  return(bl)
}

deduplicate_discrete <- function(discrete_data, threshold) {
  discrete_data_numeric <- convert_to_numeric(discrete_data) + 0.01
  discrete_data_numeric <- dedup(discrete_data_numeric, threshold)
  
  return(discrete_data[, colnames(discrete_data_numeric)])
}

deduplicate_continuous <- function(discrete_continuous, threshold) {
  continuous_data_numeric <- discrete_continuous + 0.01
  continuous_data_numeric <- dedup(continuous_data_numeric, threshold)
  
  return(discrete_continuous[, colnames(continuous_data_numeric)])
}

generate_fully_connected_graph <- function(dynamic_variables, static_variables, number_layers) {
  graph <- data.frame(from = character(), to = character())
  for(i in 1:(number_layers - 1)) {
    from_i = grep(paste0(i, "$"), dynamic_variables, value = TRUE)
    for(j in (i+1): number_layers) {
      to_j = grep(paste0(j, "$"), dynamic_variables, value = TRUE)
      graph_from_i <- expand.grid(from = from_i, to = to_j, stringsAsFactors = FALSE)
      graph <- rbind(graph, graph_from_i)
    }
  }
  
  static_graph <- expand.grid(from = static_variables, to = dynamic_variables, stringsAsFactors = FALSE)
  graph <- rbind(graph, static_graph)
  
  colnames(graph) <- c("from", "to")
  return(graph)
}

filter_by_correlation <- function(graph, cor_matrix, threshold) {
  remove_arcs <- c()
  for(i in 1:nrow(graph)) {
    from <- graph[i, 1]
    to <- graph[i, 2]

    if(abs(cor_matrix[from, to]) <= threshold) {
      print(paste(from, to))
      remove_arcs <- c(remove_arcs, i)
    }
  }
  if(length(remove_arcs) > 0) {
    graph <- graph[-remove_arcs, ]
  }
  
  return(graph)
}

filter_by_blacklist_internal <- function(graph, variables, number_layers) {
  print(dim(graph))
  for(i in 1:(number_layers - 1)) {
    layer_variables <- grep(paste0(i, "$"), variables, value = TRUE)
    internal_arcs <- generate_internal_arcs(layer_variables)
    graph <- get_graph_diff(graph, internal_arcs)
    
  }
  
  print(dim(graph))
  
  return(graph)
}

filter_by_custom_blacklist <- function(graph, blacklist_file_path) {
  custom_blacklist <- read.csv(blacklist_file_path)
  
  return(get_graph_diff(graph, custom_blacklist))
}

get_custom_blacklist <- function(blacklist_file_path) {
  return(read.csv(blacklist_file_path))
}

get_static_blacklist <- function(static_variables, dynamic_variables) {
  return(expand.grid(from=dynamic_variables, to=static_variables, stringsAsFactors = FALSE))
}

get_all_internal_arcs <- function(number_layers, variables) {
  internal_arcs <- data.frame(from = character(), to = character())
  for(i in 1:(number_layers - 1)) {
    internal_layer_variables <- grep(paste0(i, "$"), variables, value = TRUE)
    
    internal_layer_arcs <- generate_internal_arcs(internal_layer_variables)
    internal_arcs <- rbind(internal_arcs, internal_layer_arcs)
  }
  
  return(internal_arcs)
}

blacklist_all <- function(variables, whitelist, number_layers) {
  all_arcs <- expand.grid(from = variables, to = variables, stringsAsFactors = FALSE)
  blacklist <- get_graph_diff(all_arcs, whitelist)
  
  return(blacklist)
}

get_reverse_time_arcs <- function(number_layers, variables) {
  bl <- data.frame(from = character(), to = character())
  for(i in number_layers:1) {
    froms <- grep(paste(i, "$", sep = ""), variables, value = TRUE)
    if(i > 1) {
      for(j in 1:i-1) {
        tos <- grep(paste(j, "$", sep = ""), variables, value = TRUE)
        empty <- expand.grid(from = froms, to = tos, stringsAsFactors = FALSE)
        bl <- rbind(bl, empty)
      }
    } 
  }
  return(bl)
}

generate_internal_arcs <- function(layer_variables) {
  layer_graph <- expand.grid(from = layer_variables, to = layer_variables, stringsAsFactors = FALSE)
  return(layer_graph)
}

get_known_structure <- function(number_layers, all_variables, structure_file, is_variable_only) {
  structure <- read.csv(structure_file$datapath)
  
  known_structure <- data.frame(from = character(), to = character())
  
  if(!is.null(structure) && nrow(structure) > 0) {
    if(number_layers > 1 && is_variable_only) {
      froms <- structure$from
      tos <- structure$to
      
      for(i in 1:number_layers) {
        from_i <- paste(froms, i, sep = "_")
        if(i < number_layers) {
          #Generate structure between layers
          for(j in (i + 1):number_layers) {
            
            to_j <- paste(tos, j, sep = "_")
            known_i_to_j <- data.frame(from = from_i, to = to_j)
            known_structure <- rbind(known_structure, known_i_to_j)
          }
        } else {
          #Generate structure within last layer
          to_i <- paste(tos, i, sep = "_")
          known_i_to_i <- data.frame(from = from_i, to = to_i)
          known_structure <- rbind(known_structure, known_i_to_i)
        }
        
      }
      self_structure <- init_self_structure(all_variables, number_layers)
      print(dim(known_structure))
      known_structure <- rbind(known_structure, self_structure)
      
      print(dim(known_structure))
    } else {
      known_structure <- structure
    }
    
  }
  
  return(known_structure)
  
}

get_variable_name <- function(variable) {
  variables <- strsplit(variable, "_")[[1]]
  return(variables[length(variables) - 1])
}

get_variable_layer <- function(variable) {
  variables <- strsplit(variable, "_")[[1]]
  return(as.numeric(variables[length(variables)]))
}


init_self_structure <- function(all_variables, number_layers) {
  wl <- c("from", "to")
  for(variable_1 in all_variables) {
    for(variable_2 in all_variables) {
      if(variable_1 != variable_2) {
        variable_1_name <- get_variable_name(variable_1)
        variable_2_name <- get_variable_name(variable_2)
        
        variable_1_layer <- get_variable_layer(variable_1)
        variable_2_layer <- get_variable_layer(variable_2)
        
        if(variable_1_name == variable_2_name 
           && variable_1_layer < variable_2_layer) {
          wl <- rbind(wl, c(variable_1, variable_2))
        }
      }
    }
  }
  print(wl)
  wl <- wl[-1, ]
  colnames(wl) <- c("from", "to")
  return(wl)
}


get_group <- function(variables) {
  groups <- c()
  for(variable in variables) {
    variable_elems <- strsplit(variable, "_")[[1]]
    group = variable_elems[length(variable_elems)]
    if(length(variable_elems) > 1) {
      groups <- c(groups, group)
    } else {
      groups <- c(groups, 0)
    }
    
  }
  
  return(groups)
}

filter_unknow_arc <- function(graph, all_variables) {
  remove_index <- c()
  for(i in 1:nrow(graph)) {
    from <- graph$from[i]
    to <- graph$to[i]
    print(paste(from, to))
    if((length(which(from %in% all_variables)) == 0 
       || length(which(to %in% all_variables)) == 0) 
       || from == to) {
      remove_index <- c(remove_index, i)
    }
  }
  
  if(length(remove_index) > 0) {
    graph <- graph[-remove_index, ]
  }
  return(graph)
}

add_custom_whitelist <- function(graph, whitelist_file_path) {
  custom_whitelist <- read.csv(whitelist_file_path)
  graph <- rbind(graph, custom_whitelist)
  graph <- graph[-duplicated(graph), ]
  return(graph)
}

get_graph_diff <- function(graph1, graph2) {
  remove_index <- c()
  for(i in 1:nrow(graph1)) {
    from_1 <- graph1[i, "from"]
    to_1 <- graph1[i, "to"]
    for(j in 1:nrow(graph2)) {
      from_2 <- graph2[j, "from"]
      to_2 <- graph2[j, "to"]
      
      if(from_1 == from_2 && to_1 == to_2) {
        remove_index <- c(remove_index, i)
        break
      }
    }
  }
  
  if(length(remove_index) > 0) {
    return(graph1[-remove_index, ])
  }
  
  return(graph1)
}


