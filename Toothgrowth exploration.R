data("Toothgrowth")
data("TOOTHGROWTH")
View(ToothGrowth)
filtered_tg <- filter(ToothGrowth, dose == 0.5)
View(filtered_tg)
arrange(filtered_tg, len)

## using nested function
arrange(filter(ToothGrowth, dose == 0.5), len)

#Using pipe operators
filtered_toothgrowth <- ToothGrowth %>%
  filter (dose == 0.5) %>%
  arrange(len)

filtered_toothgrowth <- ToothGrowth %>%
  filter (dose == 0.5) %>%
  group_by(supp) %>%
  summarize(mean_len = mean(len, na.rm = T),.group="drop")# tells R what to do with  missing values

  

