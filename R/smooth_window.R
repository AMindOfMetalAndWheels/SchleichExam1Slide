#Fabian Schleich
#23.11.2024
##########
#' @title Smooth function
#' @describeIn  Uses mean function with a window to gnerate "mean" column
#' @param x Data (Index, time, x, y, z) in this order
#' @param window_size positive integer
#' @returns data set with smoothed euclidean vector
#' @example /data/Eucledean_test_file.R

###############
#Smooth window function
smooth_window <- function(data, window_size) {
  #only numeric columns
  numeric_columns <- sapply(data, function(col) {
    is.numeric(col) && !inherits(col, "POSIXt")  #numeric and not POSIXct
  })

  #remove it for the calcultion
  numeric_columns[1] <- FALSE

  #store mean results
  smooth_results_top <- list()
  smooth_results_bottom <- list()

  #Functions runs Top-to-Bottom (1:(nrow-winodw_size))
  for (col in names(data)[numeric_columns]) {
    smooth_results_top[[col]] <- sapply(1:(nrow(data) - window_size + 1), function(i) {
      #window of data
      window <- data[i:(i + window_size - 1), col]
      return(mean(window))
    })

    #Functions runs Bottom to top(1:(nrow-winodw_size))
    smooth_results_bottom[[col]] <- sapply((nrow(data) - window_size + 1):1, function(i) {
      #window of data
      window <- data[i:(i + window_size - 1), col]
      return(mean(window))
    })
  }

  #Combine the results
  for (col in names(smooth_results_top)) {

    smooth_column <- c(rep(0, nrow(data)))
    smooth_column[(window_size):nrow(data)] <- smooth_results_top[[col]]

    # Replace the first `window_size
    smooth_column[1:(window_size - 1)] <- rev(smooth_results_bottom[[col]][1:(window_size - 1)])

    #Adds the column
    data[paste("mean")] <- smooth_column
  }

  return(data)
}
