generate_continuous_equation <- function(nodes, equation_type, ranges) {
  equation <- c()
  for(i in 1:length(nodes)) {

    value_ranges <- ranges[[i]]
    value_upper <- value_ranges[1]
    value_lower <- value_ranges[2]

    node <- nodes[i]

    equation = c(equation, paste("(", node, " >= ", value_upper, " & ", node, " <= ", value_lower, ")", sep = ""))
  }

  equation <- paste(equation_type, " = (", paste(equation, collapse = " | "), ")", sep = "")

  return(equation)
}

generate_continuous_equation_2 <- function(nodes, equation_type, values, type = "around", collapse = " & ") {
  equation <- c()
  for(i in 1:length(nodes)) {
    value <- values[i]
    node <- nodes[i]
    if(type == "around") {
      value_upper <- value * 0.95
      value_lower <- value * 1.05

      equation = c(equation, paste("(", node, " >= ", value_upper, " & ", node, " <= ", value_lower, ")", sep = ""))
    } else if (type == "upper") {
      equation = c(equation, paste("(", node, " >= ", value, ")", sep = ""))
    } else {
      equation = c(equation, paste("(", node, " <= ", value, ")", sep = ""))
    }

  }

  equation <- paste(equation_type, " = (", paste(equation, collapse = collapse), ")", sep = "")

  return(equation)
}

generate_a_continuous_equation <- function(node, equation_type, upper, lower) {
  return(paste("(", node, " >= ", value_lower, " & ", node, " <= ", value_upper, ")", sep = ""))
}

get_kpi_range_around_value <- function(kpi_value, kpi_profile, probs) {
  kpi_quantile <- as.numeric(quantile(kpi_profile, probs))

  if(sum(is.na(kpi_quantile)) > 0 | is.na(kpi_value) | is.na(kpi_profile)){
    return(NULL)
  }

  if(kpi_value < min(kpi_profile)) {
    return(c(-Inf, min(kpi_profile)))
  } else if(kpi_value >= max(kpi_profile)) {
    return(c(max(kpi_profile), Inf))
  }
  for(i in 1:(length(kpi_quantile) - 1)) {
    if(kpi_quantile[i] <= kpi_value & kpi_value < kpi_quantile[i + 1]) {
      return(c(kpi_quantile[i], kpi_quantile[i + 1]))
    }
  }

  return(NULL)
}

