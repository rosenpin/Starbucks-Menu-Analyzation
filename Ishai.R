# importing database to dataset
dataset = read.csv(file = "starbucks_drinkMenu_expanded.csv")

##### F #####
beverage <- unique(dataset$Beverage)
# getting only latte beverages
latte_beverages <- c()
for (i in 1:length(beverage)) {
  if (grepl("Latte", beverage[i])) {
    latte_beverages <-  append(latte_beverages, beverage[i])
  }
}

comb_result <- choose(length(latte_beverages), 4)
print(paste("The number of ",4 ," Latte combinations out of ", length(latte_beverages), " is ", comb_result, sep = ""))

##### G #####
# fetching the constant variable
constant_variable <- dataset$Total.Fat..g.
# function for finding the mode value
findmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# function for finding deviation percentage from a given value
find_deviation_percentage <- function(values, c) {
  not_the_number <- 0
  for (i in 1:length(values)) {
    if (values[i] != c) {
      not_the_number <- not_the_number+1
    }
  }
  return((100*not_the_number)/length(values))
}

# getting median, mean and mode values
cv_median <- median(constant_variable)
cv_mean <- mean(constant_variable)
cv_mode <- findmode(constant_variable)
# getting median, mean and mode deviation percentages
cv_median_dev_percent <- find_deviation_percentage(constant_variable, cv_median)
cv_mean_dev_percent <- find_deviation_percentage(constant_variable, cv_mean)
cv_mode_dev_percent <- find_deviation_percentage(constant_variable, cv_mode)

# creating a name vector for the three deviations
s_names <- c("median deviation percentage", "mean deviation percentage", "mode deviation percentage")
# getting highest value of the three deviations
highest_percentage <- s_names[which.max(c(cv_median_dev_percent, cv_mean_dev_percent, cv_mode_dev_percent))]
# getting lowest value of the three deviations
lowest_percentage <- s_names[which.min(c(cv_median_dev_percent, cv_mean_dev_percent, cv_mode_dev_percent))]

# posting highest and lowest values
output_log <- paste("The highest deviation percentage value is the " , highest_percentage , ", and the lowest is the " , lowest_percentage , sep = "")
print(output_log)
