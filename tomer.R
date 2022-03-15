# import the file into a dataset
dataset = read.csv(file = "starbucks_drinkMenu_expanded.csv")

###### H
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


###### I
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
