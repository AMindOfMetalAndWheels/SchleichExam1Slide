##########
#Binary function
#Fabian Schleich
#22.11.2024

##########
#' @title Binary function
#' @describeIn calculates binary result for a given window size
#' @param data First column Index, time (POISXct recommended), ...
#' @param window_size positive integer
#' @returns binary version of eucledian data
#' @example /SchleichExam1Slide/data/test_file.R

##########
#binary window function
binary_window <- function(data, window_size) {
  #only numeric columns
  numeric_columns <- sapply(data, function(col) {
    is.numeric(col) && !inherits(col, "POSIXt")  #numeric and not POSIXct
  })

  #remove it for the calcultion
  numeric_columns[1:2] <- FALSE
  numeric_columns[6:7] <- FALSE
  #store binary results
  binary_results_top <- list()
  binary_results_bottom <- list()

  #Functions runs Top-to-Bottom (1:(nrow-winodw_size))
  for (col in names(data)[numeric_columns]) {
    binary_results_top[[col]] <- sapply(1:(nrow(data) - window_size + 1), function(i) {
      #window of data
      window <- data[i:(i + window_size - 1), col]

      #binary result (diff > 0 -> +1, else -1)
      if (diff(window)[length(diff(window))] > 0) {
        return(1)  #increase
      } else {
        return(-1) #decrease
      }
    })

    #Bottom-to-Top: reverse order to fill empty part of the windows (nrow-window_size):1
    binary_results_bottom[[col]] <- sapply((nrow(data) - window_size + 1):1, function(i) {
      #window of data
      window <- data[i:(i + window_size - 1), col]

      #binary result (diff > 0 -> +1, else -1)
      if (diff(window)[length(diff(window))] > 0) {
        return(1)  #increase
      } else {
        return(-1) #decrease
      }
    })
  }

  # Combine the top-to-bottom and bottom-to-top results
  for (col in names(binary_results_top)) {
    #adds the top data
    binary_column <- c(rep(NA, window_size - 1), binary_results_top[[col]])

    #replaces the empty values with bottom results
    binary_column[1:(window_size - 1)] <- rev(binary_results_bottom[[col]][1:(window_size - 1)])

    #adds to the dataset
    data[paste("binary")] <- binary_column
  }

  return(data)
}

##########




#########
# Threshold function --> I did not get what I was supposed to do, was it related to
# the mean function? and set a threshold there?
