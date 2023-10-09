#BAN400 Assignment Git

##############################################################################
#Problem5
##############################################################################
#(Data Transformation in "R for DS")

# set working directory

# load packages
library(nycflights13)
library(tidyverse)

# read data
nycflights13::flights

view(flights)
glimpse(flights)


##################################################################
# Exercise5.2.4
#1 Find all flights that had an arrival delay of two or more hours

flights %>%
  filter(arr_delay>= 120)

#2. Flew to Houston (IAH or HOU)
flights %>% 
  filter(dest %in% c("IAH", "HOU"))

#3. Were operated by United, American, or Delta
airlines #check the abbreviations of careers
# United = "UA", American "AA", Delta "DL"

flights %>% 
  filter(carrier %in% c("UA", "AA", "DL"))

#4. Departed in summer (July, August, and September)
flights %>% 
  filter(month %in% c(7,8,9))

#5.Arrived more than two hours late, but didn’t leave late
flights %>% 
  filter(arr_delay >= 120 & dep_delay <= 0)

#6.Were delayed by at least an hour, but made up over 30 minutes in flight
flights %>% 
  filter(dep_delay >=60,
         arr_delay < dep_delay-30
         )

#7. Departed between midnight and 6am (inclusive)
min(flights$dep_time, na.rm = TRUE)
max(flights$dep_time, na.rm = TRUE)#Max 2400 means 2401 =0001


flights %>% 
  filter(dep_time == "2400" |(dep_time > 0 & dep_time <=600 )) %>% 
  arrange(dep_time)


# Exercise5.3.1

#1. arrange() to sort all missing values to the start
flights %>% 
  arrange(desc(is.na(dep_time)))

#2-1. Sort flights to find the most delayed flights. 
flights %>% 
  arrange(desc(dep_delay),desc(arr_delay))

#2-2. Find the flights that left earliest.
flights %>% 
  arrange(dep_time)

#3. Sort flights to find the fastest (highest speed) flights.
flights %>% 
  head(arrange(air_time))
  
#4-1.Which flights travelled the farthest? 
flights %>% 
  head(arrange(desc(distance)))
  
#4-2. Which travelled the shortest?
flights %>% 
  head(arrange(distance))


# Exercise5.4.1

#1.Brainstorm as many ways as possible to select dep_time, 
# dep_delay, arr_time, and arr_delay from flights.

flights %>% 
  select(dep_time,dep_delay,arr_time,arr_time)
# I don't understand well this question. is there any better way?

#2. What happens if you include the name of a variable 
# multiple times in a select() call?

flights %>% 
  select(dep_time,dep_time,dep_time,dep_time)

# It shows only column "dep_time" one time.

#3. What does the any_of() function do? 
# Why might it be helpful in conjunction with this vector?

vars <- c("year", "month", "day", "dep_delay", "arr_delay")

flights %>% 
  select(any_of(vars))->zz

# this function seesm to return variables in the character vector.

#4.Does the result of running the following code surprise you? 
# How do the select helpers deal with case by default? 
# How can you change that default?

select(flights, contains("TIME"))

# The above is selecting both upper and lower cases of "TIME"
select(flights, contains("TIME", ignore.case = FALSE))


# Exercise5.5.2

#1. Convert dep_time and sched_dep_time 
# to a more convenient representation of number of minutes since midnight

#2.

# Exercise5.6.7
#1. Brainstorm at least 5 different ways to assess 
# the typical delay characteristics of a group of flights.



# Exercise5.7.1

#2. Which plane (tailnum) has the worst on-time record?
flights %>% 
  select(tailnum,dep_delay, arr_delay)
  mutate(
    total_delay = dep_delay+arr_delay
  ) %>% 
  arrange(desc(total_delay)) %>% 
  group_by(tailnum)

#3. What time of day should you fly 
#if you want to avoid delays as much as possible?

  
    
##################################################################
#practices
##################################################################
# destination is IAH, average delay
flights %>% 
  filter(dest== "IAH") %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_arr_delay = mean(arr_delay, na.rm = TRUE)
  )

# find all flights that departed more than 120 minutes (two hours) late:
flights %>% 
  filter(dep_delay >120)

# find flights that departed on January 1

flights %>% 
  filter(month == 1 & day ==1)

# find flights that departed in Jan or Feb

flights %>% 
  filter(month == 1 | month ==2)

flights %>% 
  filter(month %in% c(1,2))

#sort data by year, month, day and departure time
flights %>% 
  arrange(year,month,day, dep_time)

# sorts flights from most to least delayed
flights %>% 
  arrange(desc(dep_delay))

# remove duplicate rows, if any
flights %>% 
  distinct()

# find all unique origin and destination pairs

flights %>% 
  distinct(origin, dest)

# find how many flights with unique origin&destination occurred?
flights %>% 
  count(origin, dest, sort = TRUE)
