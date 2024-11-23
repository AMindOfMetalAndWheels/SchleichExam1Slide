library(utils)
library(ggplot2)
#Load data
my_data <- utils::read.csv(system.file("data", "Example_window.csv", package = "SchleichExam1Slide"))
#change column name X to Index
names(my_data)[1]<-paste("Index")
#Change time column
my_data$time <- as.POSIXct(my_data$time)
#subset data set for faster test
my_data2 <- my_data[c(1:250), c(1:5)]

#Test euclidean
SchleichExam1Slide::euclidean(x = my_data)
#Store as data frame
euclidean_data <- SchleichExam1Slide::euclidean(x = my_data)

#Apply smooth_window function on raw data and on euclidean
smooth_data <- SchleichExam1Slide::smooth_window(my_data, window_size = 100)
smooth_euclidean_data <- SchleichExam1Slide::smooth_window(euclidean_data, window_size = 100)

#Apply binary_window function on raw data and on euclidean
binary_data <- SchleichExam1Slide::binary_window(my_data, window_size = 100)
binary_euclidean_data <- SchleichExam1Slide::binary_window(euclidean_data, window_size = 100)
binary__smooth_euclidean_data <- SchleichExam1Slide::binary_window(smooth_euclidean_data, window_size = 100)
#test the combination function
SchleichExam1Slide::process_euclidean_data(my_data, window_size = 100)
my_ggplot_test <- SchleichExam1Slide::process_euclidean_data(my_data, window_size = 1000)
#plot combination function
SchleichExam1Slide::plot_euclidean_results(my_ggplot_test,include_mean = TRUE, include_binary = FALSE)
