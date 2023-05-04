install.packages("tidyverse")
library(tidyverse)
library(skimr)
library(janitor)

#importing data
bookings_df <- read_csv("hotel_bookings.csv")
View(bookings_df)
head(bookings_df)

#Summarizing to preview data
str(bookings_df)
glimpse(bookings_df)
skim_without_charts(bookings_df)

#Checking number of columns 
colnames(bookings_df)

#cleaning the data
#creating a new DF with the required columns

trimmed_df <- bookings_df%>%
  select(hotel, is_canceled, lead_time)

#renaming a column 
trimmed_df %>%
  select(hotel, is_canceled, lead_time) %>%
  rename('hotel_type' = hotel)


#combining data in different columns
#combining the arrival month and the arrival year into one column
combined_df <- bookings_df %>%
  select(arrival_date_year, arrival_date_month) %>%
  unite(arrival_month_year, c("arrival_date_month", "arrival_date_year"), sep = "")
view(combined_df)

#Summing up all adults, children and babies
sum_df <- bookings_df %>%
  mutate(guests = adults + children + babies)


#Calculating the total number of canceled bookings and the average lead time for booking
#making a column called number_canceled to represent the total number canceled,
#and a column called average_lead_time to represent average lead_time
total_df <- bookings_df %>%
  summarize(number_canceled = sum(is_canceled),
            average_lead_time = mean(lead_time))

head(total_df)

#Using the arrange(), max and min()funcs
hotel_bookings_vz <- arrange(hotel_bookings, desc(lead_time))
head(hotel_bookings_vz)
# average lead_time
mean(hotel_bookings_vz$lead_time)
#max lead_time
max(hotel_bookings_vz$lead_time)
#min lead_time
min(hotel_bookings_vz$lead_time)

#using the filter () to find data that contains info about city hotels
hotel_bookings_in_city <- filter(hotel_bookings, hotel_bookings$hotel == "City Hotel")
head(hotel_bookings_in_city)
#The average lead time for city hotels lead time bookings
mean(hotel_bookings_in_city$lead_time)

# Grouping_by and Summarizing the min, max & mean data into one dataset
hotel_summary <-
  hotel_bookings %>%
  group_by(hotel) %>%
  summarise(average_lead_time = mean(lead_time),
            min_lead_time = min(lead_time),
            max_lead_time = max(lead_time))
View(hotel_summary)

#Plotting to test different hypothesis.
#1st Objective: targeting the group of people who book early.
# A) Testing the hypothesis that people with children have to book in advance

ggplot(data = bookings_df) +
  geom_point(mapping = aes(x= lead_time, y = children))

#On the x-axis, the plot shows how far in advance a booking is made, 
  #with the bookings furthest to the right happening the most in advance. 
  #On the y-axis it shows how many children there are in a party.
#The plot reveals that the hypothesis is incorrect. 
  #Instead, many of the advanced bookings are being made by people with 0 children.

#2nd Objective: Increasing the weekend bookings, which is a significant source of revenue.
# B) Testing the hypothesis that guests without children book the most weekend nights

ggplot(data = bookings_df) + 
  geom_point(mapping = aes(x = stays_in_weekend_nights, y = children))

# The plot reveals that the hypothesis is correct.  