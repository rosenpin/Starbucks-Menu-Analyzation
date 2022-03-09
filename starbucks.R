dataset = read.csv(file = "starbucks_drinkMenu_expanded.csv")

calories_times_caffeine <- dataset$Calories * dataset$Caffeine..mg.
#for (i in 10000) {
  random_sample <- sample(calories_times_caffeine, 100, replace = TRUE)
  qts <- quantile(random_sample, probs=c(.25,.75), na.rm = TRUE)
  range <- unname(qts[2]) - unname(qts[1])
  #}