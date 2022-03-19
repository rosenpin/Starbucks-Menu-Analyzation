# libraries
library(ggplot2)

# import the file into a dataset
dataset = read.csv(file = "starbucks_drinkMenu_expanded.csv")


###### A ######
print("###### A ######")

# number of observations, initially 100, in later part of the question we need 20
OBSERVATIONS_COUNT = 100 # 20

# vector multiplication of calories times caffeine
calories_times_caffeine <- dataset$Calories * dataset$Caffeine..mg.

# create empty ranges vector to store results in
ranges <- c()
# repeat 10,000 times
for (i in 1:10000) {
  # generate a random sample of 100 items from our multiplied values (calories*caffeine from before)
  random_sample <- sample(calories_times_caffeine, size=OBSERVATIONS_COUNT, replace = TRUE)
  # get the 25% and 75% percentile
  qts <- quantile(random_sample, probs=c(.25,.75))
  # calculate the diff between the 25% item value and the 75% item value
  range <- unname(qts[2]) - unname(qts[1])
  # store the result in our ranges vector to analyze it later
  ranges <- append(ranges,range)
}

# generate a distribution graph for our ranges
hist(ranges)
# calculate the mean average of our results
avg <- mean(ranges)
# calculate the median average of our results
med <- median(ranges)
# calculate the most common value in our results 
mod <- strtoi(names(sort(table(ranges),decreasing=TRUE)[0:1]),base=0L)

###### B ######
print("###### B ######")

# calculate mean average deviation of sugars
ad <- mad(dataset$Sugars..g.)
# initialize empty vector to add our results to it
ad_ratio <- c()

# repeat 100 times
for (i in 1:100){
  # sample 50 random items
  sampled_sugars = sample(dataset$Sugars..g., size = 50)
  # calculate mean average deviation of sampled sugars
  sampled_sugars_ad <- mad(sampled_sugars)  
  # calculate ratio between previously calculated ad to our sampled ad
  result = ad/sampled_sugars_ad
  # add the result to our vector to analyze later
  ad_ratio <- append(ad_ratio, result)
}
# calculate the variance between the 50 ratios
var(ad_ratio)

###### C ######
print("###### C ######")

# get total fats
total_fats <- dataset$Total.Fat..g.
# get saturated fats
saturated_fats <- dataset$Saturated.Fat..g.
# calculate correlation between total fats and saturated fats
cor_value1 = cor(total_fats, saturated_fats)

# initialize empty vector to store values in later
cor_vector2 <- c()

# get the number of rows in our dataset (total fats and saturated fats have the same count)
dataset_length = length(total_fats)

# given a vector of indexes, return those indexes values from vec
get_values_by_indexes <- function(vec, indexes) {
  # initialize empty vector
  res = c()
  for (i in indexes){
    # add all relevant indexes
    res = append(res, vec[i])
  }
  return(res)
}

# repeat 250 times
for (i in 1:250){
  # which rows we want to sample randomly
  random_indexes = sample(c(1:dataset_length),size=dataset_length*0.7)
  # sample from 70% of items
  random_total_fat = get_values_by_indexes(total_fats,random_indexes)
  random_saturated_fat = get_values_by_indexes(saturated_fats,random_indexes)
  
  # calculate correlation between total fats and saturated fats
  temp_cor_value = cor(random_total_fat, random_saturated_fat)
  # store value to draw it later
  cor_vector2 <- append(cor_vector2, temp_cor_value)
}

# show the graph
hist(cor_vector2)
# draw the line where the original value is 
abline(v=cor_value1)

###### D ######
print("###### D ######")

# generate random normal distributed vector
normal_dist <- rnorm(1000)
# draw it
hist(normal_dist)
# initialize counter
counter = 0
# repeat until stopped
while (TRUE){
  # increase the counter by one
  counter = counter+1
  # sample 20% of the items randomly
  sampled_dist = sample(normal_dist, size = length(normal_dist)*0.2)
  # if mean!=median (absolute difference is larger than 0.01)
  if (abs(mean(sampled_dist)-median(sampled_dist)) > 0.01){
    # stop the loop
    break
  }
}

###### E ######
print("###### E ######")

# import hash library
library(hash)

# fetch beverage categories
categories = dataset$Beverage_category
number_of_items = length(categories)

# fetch beverage prep styles
prep = dataset$Beverage_prep

# define index for iteration
i = 1
# previous will be used to loop through categories while resetting the index for each different category group
previous = 1

# loop until i is the length of the categories
while (i<=length(categories)) {
  # define current category to analyze
  current = categories[i]
  # define a hash map between prep style and the number of its appearances
  conditioned_prep_counts <- hash()
  # reset the current_prep
  current_prep = ""
  # loop until the category changes
  while(categories[i] == current){
    # get the current prep style inside the category
    current_prep = prep[i]
    # if we didn't see this prep style before
    if (is.null(conditioned_prep_counts[[current_prep]])){
      # then update its appearance count to 1
      conditioned_prep_counts[[current_prep]] = 1
    } else {
      # otherwise, increase its appearance count
      conditioned_prep_counts[[current_prep]] = conditioned_prep_counts[[current_prep]]+1
    }
    # increase the index used for the loop
    i = i+1
    # if i exceeds the length of the categories -1 (including the first line of the csv), break this loop
    # this is done because the while will loop will not break for the last item
    if (i > length(categories)){
      break
    }
  }
  # repeat for all the conditioned preps we counted
  for (v in names(conditioned_prep_counts)) {
    # print separation for easier readability
    print("##########")
    
    # index-previous will yield the number of beverages in our current category
    number_of_beverages_in_category = i-previous

    # conditioned probability will be the number of times this prep is shown under the category, divided by the total number of items under the category
    conditioned_probability = conditioned_prep_counts[[v]]/number_of_beverages_in_category
    
    # print the conditioned probabilities for each prep under category
    output_conditioned = paste("P(", v ," | ", current,") = ",conditioned_probability, sep="")
    print(output_conditioned)
    
    p_category = number_of_beverages_in_category/number_of_items
    p_intersection = p_category * conditioned_probability
    # print the intersection probabilities for each prep under category
    output_intersection = paste("P(", v ," ∩ ", current,") = ",p_intersection, sep="")
    print(output_intersection)
  }
  # set the previous to i so we can later calculate the relative position correctly
  previous = i
}

# importing database to dataset
dataset = read.csv(file = "starbucks_drinkMenu_expanded.csv")

##### F #####
print("###### F ######")

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
print("###### G ######")

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

# import the file into a dataset
dataset = read.csv(file = "starbucks_drinkMenu_expanded.csv")

###### H ######
print("###### H ######")

top_rating = 0
top_drink = ""
# go through all drinks
for (i in seq_along(dataset$Beverage)){
  # calculate rating for each drink
  rating = (dataset$Protein..g.[i] * 100) + dataset$Caffeine..mg.[i] - (dataset$Total.Fat..g.[i]*50)
  # if rating is highest we've seen
  if (rating > top_rating){
    # update top rating
    top_rating <- rating
    # update top drink name
    top_drink = paste(dataset$Beverage[i], dataset$Beverage_prep[i])
  }
}
# print the healthiest drink name
print(paste("Top drink is:",top_drink))


###### I ######")
print("###### I ######")

dataset = read.csv(file = "starbucks_drinkMenu_expanded.csv")

library(foreach)

# define what is considered a healthy drink
healthy_rating_threshold = 1000

# go through all our drinks in parallel
healthy_drinks <- foreach(i=seq_along(dataset$Beverage)) %do% {
  # calculate the rating for each drink
  rating = (dataset$Protein..g.[i] * 100) + dataset$Caffeine..mg.[i] - (dataset$Total.Fat..g.[i]*50)
  # get the drink name
  drink_name = paste(dataset$Beverage[i], dataset$Beverage_prep[i])
  # if the drink is healthy
  if(rating > healthy_rating_threshold){
    # yield it back to our healthy drinks
    paste(drink_name)
  }
}
# remove all null objects (that got here from the for each) from our data set
healthy_drinks <- Filter(Negate(is.null), healthy_drinks)