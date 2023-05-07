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
  geom_point(mapping = aes(x= lead_time, y = children))+
  labs(title = 'Early_Bookings_Hypothesis_Test: x= lead_time, y = children ')

#On the x-axis, the plot shows how far in advance a booking is made, 
  #with the bookings furthest to the right happening the most in advance. 
  #On the y-axis it shows how many children there are in a party.
#The plot reveals that the hypothesis is incorrect. 
  #Instead, many of the advanced bookings are being made by people with 0 children.

#2nd Objective: Increasing the weekend bookings, which is a significant source of revenue.
# B) Testing the hypothesis that guests without children book the most weekend nights

ggplot(data = bookings_df) + 
  geom_point(mapping = aes(x = stays_in_weekend_nights, y = children))+
  labs(title = "Weekend_Nights_Bookings_hypothesis: x = stays_in_weekend_nights, y = children ")

# The plot reveals that the hypothesis is correct.  

#Bars/facets
# The stakeholder is interested in developing promotions based on different booking distributions, 
# but first they need to know how many of the transactions are occurring for each different distribution type.
# Checking what distribution type has the most number of bookings using a bar_chart
ggplot(data = bookings_df) + 
  geom_bar(mapping = aes(x = distribution_channel, fill= market_segment))+
  labs(title = 'Number of bookings')

#Facets
# Creating separate charts for each deposit type and market segment to help them understand the differences more clearly
# 1) market segment
ggplot(data = bookings_df)+
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~market_segment)+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = 'market_segment_facet')
# 2) deposit type
ggplot(data = bookings_df)+
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~deposit_type)+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = 'deposit_type_facet')
# Exploring the differences by deposit type and market segment in one chart
ggplot(data = bookings_df)+
  geom_bar(mapping = aes(x = distribution_channel)) +
  facet_wrap(~deposit_type~market_segment)+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = 'differences by deposit type and market segment')

#Filtering

# After considering all the data, the stakeholder decides to send the promotion to families that make online bookings for city hotels.
#The online segment is the fastest growing segment, and families tend to spend more at city hotels than other types of guests. 
#The stakeholder asks to create a plot that shows the relationship between lead time and guests traveling with children for online bookings at city hotels. 
#This will give her a better idea of the specific timing for the promotion. 
#Steps involved
# 1) filtering the data; 2) plotting the filtered data. 

online_city_hotels <- filter(bookings_df, (hotel == "City Hotel" &
                                             bookings_df $ market_segment == "Online TA" ))
#or
online_city_hotels_2 <- bookings_df %>%
  filter(hotel == "City Hotel")%>%
  filter(market_segment=="Online TA")
View(online_city_hotels)
View(online_city_hotels_2)

# 2) plotting the filtered data. 

ggplot(data = online_city_hotels_2) +
  geom_point(mapping = aes(x= lead_time, y = children))+
  labs(title= "Online bookings for city hotels")

# Based on the filter, the scatterplot shows data for online bookings for city hotels.
# The plot reveals that bookings with children tend to have a shorter lead time,
#and bookings with 3 children have a significantly shorter lead time (<200 days). 
# So, promotions targeting families can be made closer to the valid booking dates.

mindate <- min(bookings_df$arrival_date_year)
maxdate <- max(bookings_df$arrival_date_year)
# Comparison of market segments by hotel type for hotel bookings
ggplot(data = bookings_df) +
  geom_bar(mapping = aes(x = market_segment)) +
  facet_wrap(~hotel) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Comparison of market segments by hotel type for hotel bookings",
       caption=paste0("Data from: ", mindate, " to ", maxdate),
       x="Market Segment",
       y="Number of Bookings")
ggsave('hotel_booking_chart.png')