# check library path
.libPaths()

# install packages
install.packages("nycflights13")

# load packages
library(tidyverse)
library(dplyr)

# load datasets
library(nycflights13)
library(Lahman)
library(gapminder)

# NOTE: check read-only permission to C:\Users\L137977\R is TURNED OFF

#view data
view(mpg)
view(flights)

# function info
?ggplot
?geom_point

# 3. DATA VISUALISATION

# scatterplot
ggplot(data=mpg) +
  geom_point(mapping = aes (x =displ, y = hwy))

# scatterplot with aesthetics
ggplot (data = mpg) +
  geom_point(mapping = aes (x = displ, y = hwy, colour = class))

# scatterplot with facets
ggplot(data=mpg) +
    geom_point(mapping =aes (x = displ, y = hwy)) +
    facet_wrap(~class, nrow = 2)

ggplot(data=mpg) +
    geom_point(mapping =aes (x = displ, y = hwy)) +
    facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

# geometric objects (geom)
ggplot(data=mpg) +
  geom_point(mapping = aes(x= displ, y = hwy))

# smooth line of scatterplot
ggplot(data=mpg) +
  geom_smooth(mapping = aes(x= displ, y = hwy))

ggplot(data=mpg) +
  geom_smooth(mapping = aes(x= displ, y = hwy, linetype = drv))

# overlay smooth line on points
ggplot(data=mpg, mapping = aes (x=displ, y=hwy)) + 
  geom_point(mapping = aes(colour = class)) + 
  geom_smooth()

#4. WORKFLOW: BASICS

# assignment statement
x <- 3*9
x

r_rocks <- 2^3
r_rocks

filter(mpg, cyl==8)

filter(diamonds, carat > 3)

#5. DATA TRANSFORMATION

# filter
jan1 <- filter(flights, month == 1, day == 1)
jan1
filter(jan1, dep_delay == 2)

sqrt(2)^2==2
near(sqrt(2)^2, 2)

delayed_flights <- filter(flights, !(arr_delay > 120 | dep_delay > 120))
delayed_flights
count(delayed_flights, arr_delay)
count(delayed_flights, dep_delay)

x <- NA
is.na(x)

# create and filter a tibble
df <- tibble(x = c(1, NA, 3), y=c(2, 5, -1))
df
filter(df, x > 1)

# use the following to check if expression evaluates to NA
filter(df, is.na(x) | x > 1)

# find flights with arrival delays > 2 hours and destination either IAH or HOU
filter(flights, arr_delay > 120, dest %in% c("IAH","HOU"))

filter(flights, carrier %in% c("UA", "AA", "DL"))

# find flights that arrived more than two hours late, but didn't leave late
filter(flights, arr_delay > 120, dep_delay <= 0)

# find flights with a missing departure time
filter(flights, is.na(dep_time))

# arranging data (sorting)

arrange(flights, year, month, day)

arrange(flights, desc(dep_delay))

arrange(flights, desc(is.na(dep_delay)))

arrange(flights, distance)


# select function(similar to keep statement)

select(flights, year, month, carrier)

select(flights, year:day)

# select all columns except those from year to day (inclusive)
select(flights, -(year:day))

select(flights, contains("tail"))

rename(flights, tail_num = tailnum)

select(flights, time_hour, air_time, everything())

# create flights_small dataset with key variables
flights_small <- select(flights, 
                         year:day, 
                         ends_with("delay"), 
                         distance, 
                         air_time
                        )

# create gain, speed, hours and gains_per_our variables
mutate(flights_small,
        gain = dep_delay - arr_delay,
        speed = distance / air_time * 60,
        hours = air_time / 60,
        gains_per_hour = gain / hours
       )

# if you only want to keep the new variables use transmute
transmute(flights,
  gain = dep_delay - arr_delay,
  hours = air_time / 60,
  gain_per_hour = gain / hours
)

# split 24 hour time into hour and minutes using integer division (%/%) and remainder 
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
           )

# create lag and lead variables
(x <- 1:10)
lag(x)
lead(x)

# create small dataset and sort, then create cum_delay variable
small <- select(flights, year, month, day, dep_delay)
small_sort <- arrange(small, dep_delay)
mutate(small_sort, cum_delay = cumsum(dep_delay))


# create dep_time_mins which is the departure time in number of minutes since midnight
select(mutate(flights,
              dep_time_mins = (dep_time %/% 100) * 60 + (minutes = dep_time %% 100)),
       dep_time,
       dep_time_mins)

# compare air_time with calculated air time - not sure why these don't match
select(mutate(flights,
              dep_time_mins = dep_time %/% 100 * 60 + dep_time %% 100,
              arr_time_mins = arr_time %/% 100 * 60 + arr_time %% 100,
              air_time_calc = arr_time_mins - dep_time_mins
              ),
              air_time,
              dep_time,
              arr_time,
              dep_time_mins,
              arr_time_mins,
              air_time_calc
              )

# ranking
arrange(select(mutate(flights, arr_delay_new = min_rank(arr_delay)), arr_delay,arr_delay_new), arr_delay_new)

# summarise
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

# summarise with group by
by_month <- group_by(flights, year, month)
summarise(by_month, delay = mean(dep_delay, na.rm = TRUE))

# group flights by destination and calculate mean delay and distance
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
by_dest <- group_by(flights, dest)
summarise(by_dest, count = n(),
          dist = mean(distance, na.rm = TRUE),
           delay = mean(dep_delay, na.rm = TRUE))

# using pipes
# same code as above but using a pipe
delays <- flights %>%
    group_by(dest) %>%
    summarise(
        count = n(),
        dist = mean(distance, na.rm = TRUE),
        delay = mean(arr_delay, na.rm = TRUE)
    ) %>%
filter(count > 20, dest != "HNL")
delays

# calculate mean departure delay by month and year
flights %>%
    group_by(year, month) %>%
    summarise(dep_delay_mean = mean(dep_delay, na.rm = TRUE),
              arr_delay_mean = mean(arr_delay, na.rm = TRUE))

# create dataset of not_cancelled flights (departure and arrival delay not missing)
not_cancelled <- flights %>%
    filter(!is.na(dep_delay), !is.na(arr_delay))

# take not_cancelled dataset and calculate mean departure delay by month
not_cancelled %>%
    group_by(year, month) %>%
    summarise(dep_delay_mean = mean(dep_delay),
              arr_delay_mean = mean(arr_delay))

# create delays dataset from not_cancelled and calculate mean arrival delay by tailnum
delays <- not_cancelled %>%
    group_by(tailnum) %>%
    summarise(
        delay = mean(arr_delay)
        )
arrange(delays, desc(delay))
delays


ggplot(data=delays, mapping=aes(x=delay)) +
    geom_freqpoly(binwidth = 10)

# delays <- not_cancelled %>%
#     group_by(tailnum) %>%
#     summarise(
#         delay = mean(arr_delay, na.rm = TRUE),
#             n = n()
#     )

# delays

# ggplot(data=delays, mapping = aes(x = n, y = delay)) + 
#     geom_point(alpha = 1/10)


# delays %>%
#     filter(n > 25) %>%
#     ggplot(mapping=aes(x = n, y= delay)) +
#         geom_point(alpha = 1/10)


# library(Lahman)

# batting <- as_tibble(Lahman::Batting)

# batting

# batters <- batting %>%
#     group_by (playerID) %>%
#     summarise(
#         ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = "TRUE"),
#         ab = sum(AB, na.rm = TRUE)
#     )


# pipe example
# given these values of x, compute the log of x, return suitably lagged and iterated differences,
# then compute the exponential function and round the result

# x <- c(0.109, 0.359, 0.63, 0.996, 0.515, 0.142, 0.017, 0.829, 0.907)

# x
# log(x)
# diff(log(x))
# exp(diff(log(x)))
# round(exp(diff(log(x))), 1)

# # now using pipe
# x %>%
#     log() %>%
#     diff() %>%
#     exp() %>%
#     round(1)

# batters %>% 
#     filter(ab > 100) %>%
#     ggplot(mapping = aes(x = ab, y = ba)) + 
#     geom_point() +
#     geom_smooth(se = FALSE)

#compute average positive delay on not_cancelled dataset
# not_cancelled %>%
#     group_by(year, month) %>%
#     summarise(
#         avg_delay1 = mean(arr_delay),
#         avg_delay2 = mean(arr_delay[arr_delay > 0]))

#compute mean and standard deviation of distance for each destination
# not_cancelled %>%
#     group_by (dest) %>%
#     summarise(mean = mean(distance),
#               sd = sd(distance)) %>%
# arrange(desc(sd))

#which destinations have the most carriers?
# not_cancelled %>%
#     group_by(dest) %>%
#     summarise(carriers = n_distinct(carrier)) %>%
#     arrange(desc(carriers))



# not_cancelled %>%
#     group_by (carrier, flight) %>%
#     summarise (count = n(),
#                avg_arr_delay = mean(arr_delay),
#                median_arr_delay = median(arr_delay)) %>%
#     filter(count > 20) %>%
#     arrange(median_arr_delay)


#count number of cancelled flights per day
# flights %>%
#     group_by (year, month, day) %>%
#     summarise (count = n(),
#               cancelled = sum(is.na(dep_delay) | is.na(arr_delay)),
#               cancelled_pct = cancelled / count) %>%
#     arrange(desc(cancelled_pct))

# # find the worst members of each group - not sure what this does
# flights %>%
#     select (year, month, day, arr_delay) %>%
#     group_by (year, month, day) %>%
#     filter(rank(desc(arr_delay)) < 10)

# flights %>%
#     group_by (tailnum) %>%
#     summarise(count = n(),
#               mean_arr = mean(arr_delay),
#               mean_dep = mean(dep_delay)) %>%
#     arrange(desc(mean_arr))

#visualising distributions
# ggplot(data = diamonds) +
# geom_bar (mapping = aes (x = cut))

# diamonds %>%
#     count(cut)

# ggplot(data = diamonds) +
#   geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

# diamonds %>% 
#   count(cut_width(carat, 0.5))

# ggplot(data = diamonds) + 
#     geom_freqpoly ( mapping = aes (x = carat, colour = cut), binwidth = 0.1)

# ggplot(diamonds) + 
#   geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
#   coord_cartesian(ylim = c(0, 50))

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = x),binwidth = 0.5)










