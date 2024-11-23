##########
#ggplot2 visualtization
#Fabian Schleich
#22.11.2024

#' @title plot_euclidean_results
#' @description generates ggplot2 file after running process_euclidean_data function
#' @param data (Index, time, euclidean, mean, binary) input
#' @param include_mean set true or false if it should be included
#' @param include_binary set true or false if it should be included
#' @return plots the data
#' @example /SchleichExam1Slide/data/Eucledean_test_file.R
#' @export

##########
#Load libraries
library(ggplot2)

##########
#plot function
plot_euclidean_results <- function(data, include_mean = TRUE, include_binary = TRUE) {
  #Warning for input values
  required_cols <- c("time", "euclidean", "mean", "binary")
  if (!all(required_cols %in% colnames(data))) {
    stop("The data must contain the following columns: 'Index', 'euclidean', 'mean', 'binary'.")
  }

  #euclidean plot
  plot <- ggplot(data, aes(x = time, y = euclidean, color = "euclidean")) +
    geom_line() +
    labs(title = "Visualization of Euclidean Data",
         x = "time",
         y = "Values",
         color = "Metric") +
    theme_minimal()

  #mean vector if true
  if (include_mean) {
    plot <- plot + geom_line(aes(y = mean, color = "mean"), linetype = "dashed")
  }

  #binary vector if true
  if (include_binary) {
    plot <- plot + geom_line(aes(y = binary, color = "binary"), linetype = "dashed")
  }

  #Legend and color
  plot <- plot + scale_color_manual(
    values = c("euclidean" = "blue", "mean" = "green", "binary" = "red"),
    breaks = c("euclidean", "mean", "binary"),
    labels = c("Euclidean", "Mean", "Binary")
  )

  # Return the ggplot object
  return(plot)
}

