##########
#Eucledian function
#Fabian Schleich
#22.11.2024
##########
#' @title euclidean
#' @description calculates euclidean norm based on 3 input columns (poistion 3-5)
#' @param x Data (Index, time, x, y, z) in this order
#' @return data set with additional euclidean vector
#' @example /SchleichExam1Slide/data/Eucledean_test_file.R
#' @export


euclidean <- function(x) {
  #euclidean
  x$euclidean <- sqrt(rowSums(x[3:5]^2))
    return(x)
}
