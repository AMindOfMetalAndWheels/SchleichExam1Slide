##########
#combination function
#Fabian Schleich
#23.11.2024
##########
#' @title Combined function
#' @description
#' Combines euclidean, smooth_window and binary_window function and returns one table with Index, time, euclidean, mean and binary
#' @param x Data (Index, time, x, y, z) in this order
#' @param window_size positive integer
#' @returns data set with additional euclidean vector, mean and binary
#' @example /data/Eucledean_test_file.R
#' @export

process_euclidean_data <- function(data, window_size) {
  # Step 1: Calculate Euclidean norm
  euclidean_data <- euclidean(data)

  # Step 2: Apply smoothing window to the "euclidean" column
  smooth_euclidean_data <- smooth_window(euclidean_data, window_size)

  # Step 3: Apply binary window calculation to the "euclidean" column
  binary_euclidean_data <- binary_window(euclidean_data, window_size)

  # Select and save only the required columns
    result <- cbind(smooth_euclidean_data[, c("Index", "time", "euclidean", "mean")],
    binary = binary_euclidean_data[, c("binary")])
  return(result)
}

