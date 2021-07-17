
training_model <- function(training_type, data, number_layers, bl, wl, cluster, algorithms, number_bootstrap = 100) {
  cat(file = stderr(), "Training model ", training_type, "\n")
  data <- convert_variables_to_factor(data)
  bl <- remove_unknown_arc(bl, colnames(data))
  wl <- remove_unknown_arc(wl, colnames(data))
  results <- list()
  for(algor in algorithms) {

    cat(file = stderr(), "Training model by algorithm ", algor, "\n")
    begin <- Sys.time()
    trained <- structure_learning(data, desire_layers, bl, wl, slearning_algo = algor, number_bootstrap = number_bootstrap, cluster)
    end <- Sys.time()
    trained$time <- begin - end
    trained$date <- date()
    trained$blacklist <- bl
    trained$whitelist <- wl

    cat(file = stderr(), "Training model by algorithm ", algor, "compeleted", "\n")
    results[[algor]] <- trained

  }
  cat(file = stderr(), "Training model", training_type, "compeleted", "\n")

  return(trained)
}

structure_learning <- function(data, number_layers, bl, wl, cluster, slearning_algo = "tabu", number_bootstrap = 100) {

  cat(file = stderr(), "Learning structure by algorithm", slearning_algo, ", number bootstrap", number_bootstrap, "\n")

  data_kpi <- data
  #Start learning
  dyn.dag = NULL
  if(slearning_algo == "tabu") {
    dyn.dag = bnlearn::tabu(data_kpi, blacklist = bl, whitelist = wl, max.iter = 10000, optimized = FALSE)
  } else if(slearning_algo == "fast.iamb") {
    dyn.dag = bnlearn::fast.iamb(data_kpi, blacklist = bl, whitelist = wl, cluster = cluster)
  } else if(slearning_algo == "inter.iamb") {
    dyn.dag = bnlearn::fast.iamb(data_kpi, blacklist = bl, whitelist = wl, cluster = cluster)
  } else if(slearning_algo == "iamb") {
    dyn.dag = bnlearn::iamb(data_kpi, blacklist = bl, whitelist = wl, cluster = cluster)
  } else if(slearning_algo == "hc") {
    dyn.dag = bnlearn::hc(data_kpi, blacklist = bl, whitelist = wl, max.iter = 10000, optimized = FALSE)
  } else {
    return(NULL)
  }

  #Bootstrap
  cat(file = stderr(), "Start bootstrap structure by algorithm", slearning_algo, ", number bootstrap", number_bootstrap, "\n")
  dyn.bootstrap = custom_bootstrap(data_kpi, number_bootstrap, slearning_algo, bl, wl, cluster)
  threshold = attr(dyn.bootstrap, "threshold")

  #Pruning arcs
  dyn.avg = bnlearn::averaged.network(dyn.bootstrap, threshold = threshold)

  undirected_arcs <- undirected.arcs(dyn.avg)
  if(dim(undirected_arcs)[1] != 0) {
    for(i in 1:length(undirected_arcs[, 1])) {
      dyn.avg <- drop.arc(dyn.avg, from = undirected_arcs[i, 1], to = undirected_arcs[i, 2])
    }
  }

  #Learning parameters
  fitted <- bn.fit(dyn.avg, data, cluster = cluster)

  result <- list()
  result[["dyn"]] <- dyn.dag
  result[["dyn.avg"]] <- dyn.avg
  result[["fitted"]] <- fitted
  result[["bootstrap"]] <- dyn.bootstrap
  result[["threshold"]] <- threshold
  result[["kpi"]] <- data_kpi
  cat(file = stderr(), "Learning structure by algorithm", slearning_algo, ", number bootstrap", number_bootstrap, "completed", "\n")
  return(result)
}

convert_variables_to_factor <- function(data) {
  data_cols <- colnames(data)
  for(data_col in data_cols) {
    if(is.character(data[, data_col])) {
      data[, data_col] <- as.factor(data[, data_col])
    } else if(is.integer(data[, data_col])) {
      data[, data_col] <- data[, data_col] + 1e6
    }
  }

  return(data)
}

remove_unknown_arc <- function(arcs, variables) {
  remove_index <- c()
  for(i in 1:length(arcs[, 1])) {
    fr <- arcs[i, 1]
    to <- arcs[i, 2]

    if(length(which(fr %in% variables)) == 0 || length(which(to %in% variables)) == 0) {
      remove_index <- c(remove_index, i)
    }
  }
  if(length(remove_index) > 0) {
    arcs <- arcs[-remove_index, ]
  }

  if(length(arcs[, 1]) > 0) {
    rownames(arcs) <- 1:length(arcs[, 1])
    return(arcs)
  } else {
    return(NULL)
  }
}

custom_bootstrap <- function(data, R, algorithm, blacklist, whitelist, cluster) {
  cat(file = stderr(), paste("Boostrap with R = ", R, " and algorithm = ", algorithm), "\n")
  dyn.str = bnlearn::boot.strength(data, cluster = cluster, R = R, algorithm = algorithm, algorithm.args = list(blacklist = blacklist, whitelist = whitelist))
  cat(file = stderr(), "Boostrap with R = ", R, " and algorithm = ", algorithm, " completed", "\n")
  return(dyn.str)
}

