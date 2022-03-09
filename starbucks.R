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
total_fats <- dataset$Total.Fat..g.
saturated_fats <- dataset$Saturated.Fat..g.
cor_value1 = cor(total_fats, saturated_fats)

cor_vector2 <- c()

dataset_length = length(saturated_fats)
for (i in 1:250){
  random_total_fat = sort(sample(total_fats, size = dataset_length))
  random_saturated_fat = sort(sample(saturated_fats, size = dataset_length))
  temp_cor_value = cor(random_total_fat, random_saturated_fat)
  cor_vector2 <- append(cor_vector2, temp_cor_value)
}

hist(cor_vector2)
abline(v=cor_value1)