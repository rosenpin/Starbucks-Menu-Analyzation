# import the file into a dataset
dataset = read.csv(file = "starbucks_drinkMenu_expanded.csv")


###### A ######
print("###### A ######")
# vector multiplication of calories times caffieine
calories_times_caffeine <- dataset$Calories * dataset$Caffeine..mg.

# create empty ranges vector to store results in
ranges <- c()
# repeat 10,000 times
for (i in 1:10000) {
  # generate a random sample of 100 items from our multiplied values (calories*caffeine from before)
  random_sample <- sample(calories_times_caffeine, size=20, replace = TRUE)
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
# get total fats sorted from low to high
total_fats <- sort(dataset$Total.Fat..g.)
# get saturated fats sorted from low to high
saturated_fats <- sort(dataset$Saturated.Fat..g.)
# calculate correlation between total fats and saturated fats
cor_value1 = cor(total_fats, saturated_fats)

# initialize empty vector to store values in later
cor_vector2 <- c()

# get the number of rows in our dataset (total fats and saturated fats have the same count)
dataset_length = length(total_fats)

# repeat 250 times
for (i in 1:250){
  # sample from 70% of items
  random_total_fat = sort(sample(total_fats, size = dataset_length*0.7))
  random_saturated_fat = sort(sample(saturated_fats, size = dataset_length*0.7))
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
    print(number_of_beverages_in_category)
  
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