##############################
##### Data Visualization #####
##############################
library(tidyverse)
library(dplyr)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg)
dim(mpg)
?mpg
ggplot(data = mpg) + geom_point(mapping = aes(x = hwy, y = cyl))
ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), color = 'blue')

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = 'blue'))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~class, nrow = 2)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ cyl)

ggplot(data = mpg) + geom_point(mapping = aes(x = drv, y = cyl)) + facet_grid(drv ~ cyl)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~.)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(.~ cyl)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~class, nrow = 2)

ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) + geom_smooth(mapping = aes(x = displ, y = hwy, color = drv), show.legend = FALSE)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = drv)) + geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point() + geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point(mapping = aes(color = class)) + geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point(mapping = aes(color = class)) + geom_smooth(data = filter(mpg, class == 'subcompact'), se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + geom_point() + geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point() + geom_smooth()

ggplot() + geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, group = drv)) + geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = drv)) + geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = drv)) + geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + stat_count(mapping = aes(x = cut))

demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) + geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

fair <- sum(diamonds$cut == 'Fair')
good <- sum(diamonds$cut == 'Good')
very_good <- sum(diamonds$cut == 'Very Good')
premium <- sum(diamonds$cut == 'Premium')
ideal <- sum(diamonds$cut == 'Ideal')
ttl <- fair + good + very_good + premium + ideal
prob <- c(fair/ttl, good/ttl, very_good/ttl, premium/ttl, ideal/ttl)
cut <- c('Fair', 'Good', 'Very Good', 'Premium', 'Ideal')
diamond <- data.frame(cut,prob)

ggplot(data = diamond) + geom_bar(mapping = aes(x = cut, y = prob, group = 1), stat = 'identity')

ggplot(data = diamonds) + stat_summary(mapping = aes(x = cut, y = depth), fun.ymin = min, fun.ymax = max, fun.y = median)

ggplot(data = diamonds) + geom_boxplot(mapping = aes(x = cut, y = depth))

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = color, y = ..prop..))

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, y = ..prop..))

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, colour = cut))

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + geom_bar(alpha = 1/5, position = 'identity')

ggplot(data = diamonds, mapping = aes(x = cut, color = clarity)) + geom_bar(fill = NA, position = 'identity')

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill") + ylab('prob')

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge") + ylab('prob')

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), position = 'jitter')

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + geom_point(position = 'jitter') + geom_smooth()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot() + coord_flip()

nz <- map_data("nz")
ggplot(nz, aes(long, lat, group = group)) + geom_polygon(fill = "white", colour = "black")
ggplot(nz, aes(long, lat, group = group)) + geom_polygon(fill = "white", colour = "black") + coord_quickmap()

bar <- ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut, fill = cut), show.legend = FALSE, width = 1) + theme(aspect.ratio = 1) + labs(x = NULL, y = NULL)
bar + coord_flip()
bar + coord_polar()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + geom_point() + geom_abline() + coord_fixed()

##############################
##### Work Flow - Basics #####
##############################
seq(1,10) # 1,2,3,4,5,6,7,8,9,10

seq(1,10,length.out = 5) # 1.00, 3.25, 5.50, 7.75, 10.00

library(tidyverse)
library(ggplot2)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl == 8)
filter(diamonds, carat > 3)

###############################
##### Data Transformation #####
###############################
install.packages('nycflights13')
library(nycflights13)
library(tidyverse)

flights

filter(flights, month == 1, day == 1)

jan1 <- filter(flights, month == 1, day == 1)

(dec25 <- filter(flights, month == 12, day == 25))

filter(flights, month == 1)

sqrt(2) ^ 2 == 2

1/49 * 49 == 1

near(sqrt(2) ^ 2,  2)

near(1 / 49 * 49, 1)

filter(flights, month == 11 | month == 12)

nov_dec <- filter(flights, month %in% c(11, 12))

filter(flights, !(arr_delay > 120 | dep_delay > 120))

filter(flights, arr_delay <= 120 & dep_delay <= 120)

NA > 5 # NA

NA == 10 #NA

NA + 10 #NA

NA / 2

NA == NA

x <- NA

y <- NA

x == y

is.na(x)

df <- tibble(x = c(1, NA, 3))

filter(df, x > 1)

filter(df, is.na(x) | x > 1)

not_na_flights <- flights %>%
  filter(is.na(dep_time))
nrow(not_na_flights)

arrange(flights, year, month, day)

arrange(flights, desc(arr_delay))

df <- tibble(x = c(5, 2, NA))
arrange(df, x) # Missing values are always sorted at the end
arrange(df, desc(x))

most_delay_flight <- flights %>%
  mutate(most_delay = dep_delay + arr_delay) %>%
  arrange(desc(most_delay))

flights %>%
  filter(!is.na(dep_time)) %>%
  filter(dep_time == min(dep_time)) %>%
  select(tailnum)

flights %>%
  filter(!is.na(dep_time)) %>%
  filter(!is.na(arr_time)) %>%
  mutate(longest = arr_time - dep_time) %>%
  filter(longest == min(longest)) %>%
  select(tailnum)

select(flights, year, month, day)

select(flights, year:day)

select(flights, -(year:day)) # starts_with("abc"), ends_with("xyz"), contains("ijk"), matches("(.)\\1"), num_range("x", 1:3): matched x1, x2, x3

rename(flights, tail_num = tailnum) # rename column name

select(flights, time_hour, air_time, everything()) # move air_time to the start of the data frame and everything() shows the remaining columns

flights_sml <- select(flights, year:day, ends_with("delay"), distance, air_time)
mutate(flights_sml, gain = arr_delay - dep_delay, speed = distance / air_time * 60)
mutate(flights_sml, gain = arr_delay - dep_delay, hours = air_time / 60, gain_per_hour = gain / hours)

transmute(flights, gain = arr_delay - dep_delay, hours = air_time / 60, gain_per_hour = gain / hours) # transmute(): only keep the new variables

transmute(flights, dep_time, hour = dep_time %/% 100, minute = dep_time %% 100) # %/%: integer division; %%: remainder

(x <- 1:10)
lag(x) # NA 1 2 3 4 5 6 7 8 9
lead(x) # 1 2 3 4 5 6 7 8 9 NA

x - lag(x) # NA 1 1 1 1 1 1 1 1
cumsum(x) # 1 3 6 10 15 21 28 36 45 55
cummean(x) # 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5

y <- c(1, 2, 2, NA, 3, 4)
min_rank(y) # 1 2 2 NA 4 5
min_rank(desc(y)) # 5 3 3 NA 2 1

row_number(y) # 1  2  3 NA  4  5
dense_rank(y) # 1  2  2 NA  3  4
percent_rank(y) # 0.00 0.25 0.25   NA 0.75 1.00
cume_dist(y) # 0.2 0.6 0.6  NA 0.8 1.0
?cume_dist # proportion of all values less than or equal to the current rank

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest, count = n(), dist = mean(distance, na.rm = TRUE), delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dest != "HNL")

ggplot(data = delay, mapping = aes(x = dist, y = delay)) + geom_point(aes(size = count), alpha = 1/3) + geom_smooth(se = FALSE)

delays <- flights %>% 
  group_by(dest) %>% 
  summarise(count = n(), dist = mean(distance, na.rm = TRUE), delay = mean(arr_delay, na.rm = TRUE)) %>% 
  filter(count > 20, dest != "HNL")

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(delay = mean(arr_delay))

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(delay = mean(arr_delay, na.rm = TRUE), n = n())

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

install.packages('Lahman')
library(Lahman)
batting <- as_tibble(Lahman::Batting)
batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE), ab = sum(AB, na.rm = TRUE))

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)

batters %>% 
  arrange(desc(ba))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(avg_delay1 = mean(arr_delay), avg_delay2 = mean(arr_delay[arr_delay > 0])) # the average positive delay

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(first = min(dep_time), last = max(dep_time))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(first_dep = first(dep_time), last_dep = last(dep_time))

# test <- not_cancelled %>% 
  # group_by(year, month, day) %>% 
  # mutate(r = min_rank(desc(dep_time))) %>% 
  # filter(r %in% range(r))

not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)

not_cancelled %>% 
  count(tailnum, wt = distance) # ???count??? (sum) the total number of miles a plane flew

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500)) # sum(x) gives the number of TRUEs in x, and mean(x) gives the proportion

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))

daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))

(per_month <- summarise(per_day, flights = sum(flights)))

testing <- flights %>%
  group_by(year, month) %>%
  summarise(per_month_test = n())

(per_year  <- summarise(per_month, flights = sum(flights)))

daily %>% 
  ungroup() %>%             # no longer grouped by date
  summarise(flights = n())

daily %>%
  summarise(flights = n())

flights_sml %>% 
  arrange(arr_delay) %>%
  mutate(rank = dense_rank(arr_delay)) %>%
  filter(rank < 10)

flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

#####################################
##### Exploratory Data Analysis #####
#####################################
library(tidyverse)
library(ggplot2)
library(dplyr)
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>% 
  count(cut)

diamonds %>%
  group_by(cut) %>%
  summarize(amount = n())

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>% 
  count(cut_width(carat, 0.5)) # cut_width --> cut range; 0.5 stands for interval

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50)) # coord_cartesian: zoom small values

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)

diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y)) # replace unusual values with NA

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)

library(nycflights13)
flights %>% 
  mutate(cancelled = is.na(dep_time), sched_hour = sched_dep_time %/% 100, sched_min = sched_dep_time %% 100, sched_dep_time = sched_hour + sched_min / 60) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500) # count standardised so that the area under each frequency polygon is one

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) # reorder class based on the median value of hwy

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip() # reverse x-axis and y-axis

ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>%
  group_by(cut) %>%
  summarise(count = n())

diamonds %>% 
  count(color, cut)

diamonds %>%
  count(cut)

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n)) # heatmap

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price)) # geom_bin2d() divides coordinate plane into 2d bins and uses a fill color to display how many points fall into each bin

install.packages('hexbin')
library(hexbin)
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price)) # geom_hex divides coordinate plane into 2d bins and uses a fill color to display how many points fall into each bin

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20))) # 20: number of boxplots

ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))

install.packages('modelr')
library(modelr)
mod <- lm(log(price) ~ log(carat), data = diamonds)
diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))
ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))

ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_freqpoly(binwidth = 0.25)

diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()

library(readr)
ggplot(diamonds, aes(carat, price)) + 
  geom_hex()
ggsave("diamonds.pdf")
write_csv(diamonds, "diamonds.csv")

###################
##### Tibbles #####
###################
as_tibble(iris)

test <- tibble(x = 1:5, y = 1, z = x ^ 2 + y)
class(test) # "tbl_df"     "tbl"        "data.frame"

tb <- tibble(`:)` = "smile", ` ` = "space",`2000` = "number")

tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)

tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

flights %>% 
  print(n = 11, width = Inf) # control the number of rows (n) and the width of the display; width = Inf will display all columns

df <- tibble(
  x = runif(5),
  y = rnorm(5)
)

df$x
df[['x']][1]
df[[1]][1]
df %>% .$x
df %>% .[["x"]]

class(as.data.frame(tb))

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)

annoying$`1`
ggplot(data = annoying, mapping = aes(x = `1`, y = `2`)) + geom_point()
annoying <- annoying %>%
  mutate(`3` = `2` / `1`)
colnames(annoying) <- c('one', 'two', 'three')

#######################
##### Data import #####
#######################
read_csv("a,b,c
1,2,3
4,5,6")

test <- read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3", skip = 2)

read_csv("# A comment I want to skip
  x,y,z
         1,2,3", comment = "#")

read_csv("1,2,3\n4,5,6", col_names = FALSE) # \n: new line

read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))

read_csv("a,b,c\n1,2,.", na = ".")

str(parse_logical(c("TRUE", "FALSE", "NA")))

str(parse_integer(c("1", "2", "3")))

str(parse_date(c("2010-01-01", "1979-10-14")))

parse_integer(c("1", "231", ".", "456"), na = '.')

x <- parse_double(c("123", "345", "1.23", "123.45"))

problems(x) # show the problem

parse_double("1.23")

parse_double("1,23", locale = locale(decimal_mark = ",")) # separate by ,

parse_number("$100")

parse_number("20%")

parse_number("It cost $123.45")

parse_number("$123,456,789")

parse_number("123.456.789", locale = locale(grouping_mark = "."))

parse_number("123'456'789", locale = locale(grouping_mark = "'"))

charToRaw("Hadley") # ASCII

fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit) # parsing is to find the respective character

parse_datetime("2010-10-01T2010")

parse_datetime("20101010")

install.packages('hms')
library(hms)
library(readr)
parse_time("01:10 am")
parse_time("20:10:01") # %Y: 4 digits; %y: 2 digits; %m: 2 digits; %b: like 'Jan'; %B: like 'January'; %d: 2 digits; %H: 0-23 hour; %I: 0-12, must be used with %p; %p: AM/PM indicator; %M: minutes; %S: integer seconds; %OS: real seconds; %Z: Time zone (as name, e.g. America/Chicago); %z: as offset from UTC, e.g. +0800; %.: skips one non-digit character; %*: skips any number of non-digits

parse_date("01/02/15", "%m/%d/%y")
parse_date("01/02/15", "%d/%m/%y")
parse_date("01/02/15", "%y/%m/%d")
parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))

guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TRUE", "FALSE"))
guess_parser(c("1", "5", "9"))
guess_parser(c("12,352,561"))
str(parse_guess("2010-10-10"))

write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")

#####################
##### Tidy data #####
#####################
library(tidyr)
library(dplyr)
country <- c('Afghanistan', 'Afghanistan', 'Brazil', 'Brazil', 'China', 'China')
year <- c(1999,2000,1999,2000,1999,2000)
cases <- c(745,2666,37737,80488,212258,213766)
population <- c(19987071, 20595360, 172006362, 174504898, 1272915272, 1280428583)
table1 <- data.frame(country, year, cases, population)
table1 %>% 
  mutate(rate = cases / population * 10000)

table1 %>% 
  count(year, wt = cases) # count cases in 1999 and 2000

library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

table1 %>%
  group_by(country)
  ggplot(mapping = aes(year, cases)) +
  geom_line(aes(group = country), colour = 'grey50') +
  geom_point(aes(colour = country))

country <- c('Afghanistan', 'Brazil', 'China')
`1999` <- c(745, 37737, 212258)
`2000` <- c(2666, 80488, 213766)
table4a <- data.frame(country, `1999`, `2000`)

tidy4a <- table4a %>% 
  gather('X1999', 'X2000', key = "year", value = "cases")

country <- c('Afghanistan', 'Brazil', 'China')
`1999` <- c(10000, 20000, 30000)
`2000` <- c(40000, 50000, 60000)
table4b <- data.frame(country, `1999`, `2000`)
tidy4b <- table4b %>%
  gather('X1999', 'X2000', key = "year", value = "population")
left_join(tidy4a, tidy4b)

library(tidyr)
country <- c('Afghanistan', 'Afghanistan', 'Afghanistan', 'Afghanistan', 'Brazil', 'Brazil')
year <- c(1999,1999,2000,2000,1999,1999)
type <- c('cases','population','cases','population','cases','population')
count <- c(745,19987071,2666,20595360,37737,172006362)
table2 <- data.frame(country,year,type,count)
spread(table2, key = type, value = count)

country <- c('Afghanistan', 'Afghanistan', 'Brazil', 'Brazil', 'China', 'China')
year <- c(1999,2000,1999,2000,1999,2000)
rate <- c(745/19987071, 2666/20595360, 37737/172006362, 80488/174504898, 212258/1272915272, 213766/1280428583)
table4 <- data.frame(country,year,rate)
table3 %>% 
  separate(rate, into = c("cases", "population"))

table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")

table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)

table3 %>% 
  separate(year, into = c("century", "year"), sep = 2) # separate 1999 as 19 and 99

library(tidyr)
table5 <- tidyr::table5
table5 %>%
  unite(new, century, year, sep = '')

library(tibble)
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks %>% 
  spread(year, return)

stocks %>% 
  spread(year, return) %>% 
  gather(year, return, `2015`:`2016`, na.rm = TRUE)

stocks %>% 
  complete(year, qtr)

treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

treatment %>% 
  fill(person) # replace missing values by last observation carried forward

who <- tidyr::who
who1 <- who %>% 
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)

library(dplyr)
who1 %>%
  count(key)

library(stringr)
who2 <- who1 %>% 
  mutate(key = str_replace(key, "newrel", "new_rel"))

who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")

who3 %>% 
  count(new)

who4 <- who3 %>% 
  select(-new, -iso2, -iso3)

who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1) # split after the first character

who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>% 
  mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
  separate(code, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

###########################
##### Relational data #####
###########################
library(tidyverse)
library(nycflights13)
names(airlines)
names(airports)
names(planes)
names(weather)
names(flights)

library(dplyr)
planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)

flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)

flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")

flights2 %>%
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[carrier %in% airlines$carrier])

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

x %>% 
  inner_join(y, by = "key")

x %>%
  left_join(y, by = 'key') # A left join keeps all observations in x

x %>%
  right_join(y, by = 'key') # A right join keeps all observations in y

x %>%
  full_join(y, by = 'key') # A full join keeps all observations in x and y

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
left_join(x, y, by = "key")

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)

left_join(x, y, by = "key")

flights2 %>% 
  left_join(weather)

flights2 %>% 
  left_join(planes, by = "tailnum")

flights2 %>% 
  left_join(airports, c("dest" = "faa")) # match variable a in table x to variable b in table y. The variables from x will be used in the output.

flights2 %>% 
  left_join(airports, c("origin" = "faa"))

top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)

flights %>% 
  filter(dest %in% top_dest$dest)

flights %>% 
  semi_join(top_dest) # semi_join(x, y) keeps all observations in x that have a match in y.

flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE) # anti_join(x, y) drops all observations in x that have a match in y.

airports %>% count(alt, lon) %>% filter(n > 1)

df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1
)
df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2
)

intersect(df1, df2) # return only observations in both x and y

union(df1, df2) # return unique observations in x and y

setdiff(df1, df2) # return observations in x, but not in y

###################
##### Strings #####
###################
library(tidyverse)
library(stringr)
string1 <- "This is a string"

string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

"This is a string without a closing quote
HELP I'M STUCK"

c("one", "two", "three")

str_length(c("a", "R for data science", NA)) # count length of string: including space

str_c("x", "y") # combine two or more strings

str_c("x", "y", "z")

str_c("x", "y", sep = ", ")

x <- c("abc", NA)
str_c("|-", x, "-|")

str_c("|-", str_replace_na(x), "-|") # Turn NA into "NA"

str_c("prefix-", c("a", "b", "c"), "-suffix")

name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  ".") # "Good morning Hadley."

str_c(c("x", "y", "z"), collapse = ", ") # collapse a vector of strings into a single string

x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3) # str_sub() takes start and end arguments which give the (inclusive) position of the substring

str_sub(x, -3, -1) # negative numbers count backwards from end

str_sub("a", 1, 5) # str_sub() won???t fail if the string is too short

str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))

str_to_lower(x)

x <- c("apple", "eggplant", "banana")

str_sort(x, locale = "en") # alphabet

str_sort(x, locale = "haw")

x <- c("apple", "banana", "pear")
str_view(x, "an") # match exact strings

str_view(x, ".a.") # . matches any character

dot <- "\\."

writeLines(dot) # \.

str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
writeLines(x) # a/b

str_view(x, "\\\\") # a/b

x <- c("apple", "banana", "pear")
str_view(x, "^a") # ^ to match the start of the string

str_view(x, "a$") # $ to match the end of the string

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")

str_view(x, "^apple$")

# \d: matches any digit.
# \s: matches any whitespace (e.g. space, tab, newline).
# [abc]: matches a, b, or c.
# [^abc]: matches anything except a, b, or c.

str_view(c("grey", "gray"), "gr(e|a)y")

# ?: 0 or 1
# +: 1 or more
# *: 0 or more

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?") # first 2 C

str_view(x, "CC+") # all 3 C

str_view(x, 'C[LX]+') # CLXXX

# {n}: exactly n
# {n,}: n or more
# {,m}: at most m
# {n,m}: between n and m

str_view(x, "C{2}") # first 2 C

str_view(x, "C{2,}") # all 3 C

str_view(x, "C{2,3}") # all 3 C

str_view(x, 'C{2,3}?') # first 2 C: matching the shortest string possible by putting a ? after

str_view(x, 'C[LX]+?') # CL

str_view(fruit, "(..)\\1", match = TRUE) # Finds all fruits that have a repeated pair of letters

x <- c("apple", "banana", "pear")
str_detect(x, "e")

# How many common words start with t?
sum(str_detect(words, "^t"))

# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))

# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")

# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")

identical(no_vowels_1, no_vowels_2)

words[str_detect(words, "x$")]

str_subset(words, "x$")

df <- tibble(
  word = words, 
  i = seq_along(word)
)

df %>% 
  filter(str_detect(words, "x$"))

x <- c("apple", "banana", "pear")
str_count(x, "a")

mean(str_count(words, "[aeiou]"))

df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

str_count("abababa", "aba") # count how many 'aba'

str_view_all("abababa", "aba")

library(stringr)
length(sentences)

head(sentences)

colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")

has_colour <- str_subset(sentences, colour_match) # select the sentences that contain a colour

matches <- str_extract(has_colour, colour_match) #  extract the colour
head(matches)

more <- sentences[str_count(sentences, colour_match) > 1] 
str_view_all(more, colour_match)

str_extract(more, colour_match) # str_extract() only extracts the first match

str_extract_all(more, colour_match) # returns a list

str_extract_all(more, colour_match, simplify = TRUE) # returns a matrix with short matches expanded to the same length as the longest

x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE) # with short matches expanded to the same length as the longest

noun <- "(a|the) ([^ ]+)" # word comes after a or the
has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10) # find all the sentences having word comes after a or the

has_noun %>% 
  str_extract(noun) # extract noun

has_noun %>% 
  str_match(noun) # str_match() gives each individual component and returns a matrix

library(readr)
library(tidyr)
library(tibble)
tibble(sentence = sentences) %>% 
  tidyr::extract(sentence, c("article", "noun"), "(a|the) ([^ ]+)", remove = FALSE)

x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-") # only replace the first match
str_replace_all(x, "[aeiou]", "-")

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  head(5) # change the sequence of second and third word

sentences %>%
  head(5) %>% 
  str_split(" ")

"a|b|c|d" %>% 
  str_split("\\|") %>% 
  .[[1]] # the easiest thing is to just extract the first element of the list

sentences %>%
  head(5) %>% 
  str_split(" ", simplify = TRUE)

fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE) # n: request a maximum number of pieces

x <- "This is a sentence.  This is another sentence."
str_view_all(x, boundary("word")) # split up by character, line, sentence and word 

str_split(x, " ")[[1]]

str_split(x, boundary("word"))[[1]]

str_view(fruit, "nana") # Find all the nana

str_view(fruit, regex("nana"))

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")

str_view(bananas, regex("banana", ignore_case = TRUE))

x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line")[[1]]

str_extract_all(x, regex("^Line", multiline = TRUE))[[1]] # multiline = TRUE allows ^ and $ to match the start and end of each line rather than the start and end of the complete string.

###################
##### Factors #####
###################
library(tidyverse)
library(forcats)
library(dplyr)

x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
sort(x1) # Alphabetically

month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
y1 <- factor(x1, levels = month_levels)
sort(y1)

y2 <- factor(x2, levels = month_levels)
sort(y2) # omit NA

y2 <- parse_factor(x2, levels = month_levels)

factor(x1)

f1 <- factor(x1, levels = unique(x1)) # shrink levels to equal unique x1

f2 <- x1 %>%
  factor() %>%
  fct_inorder() # same result as f1
levels(f2)

gss_cat %>%
  count(race)

ggplot(gss_cat, aes(race)) +
  geom_bar()

ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE) # By default, ggplot2 will drop levels that don???t have any values, but can be forced to display

relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(age = mean(age, na.rm = TRUE),
            tvhours = mean(tvhours, na.rm = TRUE),
            n = n())

ggplot(relig_summary, aes(tvhours, relig)) + geom_point()

ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) + geom_point() # fct_reorder(f,x,fun): f --> the factor whose levels you want to modify. x --> a numeric vector that you want to use to reorder the levels. fun --> Optionally, fun is a function that???s used if there are multiple values of x for each value of f. The default value is median

relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()

rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(age = mean(age, na.rm = TRUE),
            tvhours = mean(tvhours, na.rm = TRUE),
            n = n())

ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) + geom_point()

ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) + geom_point() # fct_relevel: pull ???Not applicable??? to the front with the other special levels

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  group_by(age, marital) %>%
  count() %>%
  ungroup() %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)

ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital") # fct_reorder2() reorders the factor by the y values associated with the largest x values.

gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar() # fct_infreq() + fct_rev()to order levels in increasing frequency

gss_cat %>% count(partyid)

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat")) %>%
  count(partyid) # fct_recode: change level names and will leave levels that aren???t explicitly mentioned as is

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat",
                              "Other"                 = "No answer",
                              "Other"                 = "Don't know",
                              "Other"                 = "Other party")) %>%
  count(partyid)

gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat"))) %>%
  count(partyid) # fct_collapse: collapse a lot of levels, indicating that for each new variable, you can provide a vector of old levels

##########################
##### Date and Times #####
##########################
library(tidyverse)
library(lubridate)
library(nycflights13)

today()
now()

ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")
ymd(20170131)
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")
ymd(20170131, tz = "UTC")

flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(dep_time = make_datetime_100(year, month, day, dep_time),
         arr_time = make_datetime_100(year, month, day, arr_time),
         sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
         sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

as_datetime(today())
as_date(now())

as_datetime(60 * 60 * 10) # "1970-01-01 10:00:00 UTC"
as_date(365 * 10 + 2) # "1980-01-01"

datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
month(datetime)
mday(datetime)
yday(datetime) # 190
wday(datetime) # 6

month(datetime, label = TRUE) # Jul
wday(datetime, label = TRUE, abbr = FALSE) # Friday

flights_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>%
  ggplot(aes(x = wday)) +
  geom_bar()

flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE), n = n()) %>% 
  ggplot(aes(minute, avg_delay)) +
  geom_line()

flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE),n = n()) %>%
  ggplot(aes(minute, avg_delay)) +
  geom_line()

flights_dt %>%
  mutate(minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE), n = n()) %>%
  ggplot(aes(minute, n)) +
  geom_line()

flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()

flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line() # floor_date() takes a date-time object and rounds it down to the nearest boundary of the specified time unit.

(datetime <- ymd_hms("2016-07-08 12:34:56"))
year(datetime) <- 2020
month(datetime) <- 01
hour(datetime) <- hour(datetime) + 1

update(datetime, year = 2020, month = 2, mday = 2, hour = 2)

ymd("2015-02-01") %>% 
  update(mday = 30) # If values are too big, they will roll-over

ymd("2015-02-01") %>% 
  update(hour = 400)

flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)

h_age <- today() - ymd(19791014)
as.duration(h_age)

dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)
2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)
tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)

one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
one_pm + ddays(1) # "2016-03-13 14:00:00 EDT": March 12 only has 23 hours
one_pm + days(1)

seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1)
10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)

# A leap year
ymd("2016-01-01") + dyears(1)
ymd("2016-01-01") + years(1)

# Daylight Savings Time
one_pm + ddays(1)
one_pm + days(1)

flights_dt <- flights_dt %>% 
  mutate(overnight = arr_time < dep_time,
         arr_time = arr_time + days(overnight * 1),
         sched_arr_time = sched_arr_time + days(overnight * 1))

flights_dt %>% 
  filter(overnight, arr_time < dep_time)

years(1) / days(1)

Sys.timezone()

(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))
x1 - x2
x1 - x3
x4 <- c(x1, x2, x3)

#################
##### Pipes #####
#################
assign("x", 10)

"x" %>% assign(100)

#####################
##### Functions #####
#####################
library(tibble)
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))

x <- df$a
(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(c(0, 5, 10))
rescale01(c(-10, 0, 10))
rescale01(c(1, 2, 3, NA, 5))

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

x <- c(1:10, Inf)
rescale01(x)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)

has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}

x <- sqrt(2) ^ 2
x == 2 # FALSE

# Compute confidence interval around mean using normal approximation
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}
x <- runif(100)
mean_ci(x)
mean_ci(x, conf = 0.99)

wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}

wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}

wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}

wt_mean(1:6, 1:3)
test <- c(1:6)
testing <- c(1:3)
test * testing

wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}

sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

str_c("a", "b", "c", "d", "e", "f")

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}

mtcars %>% 
  show_missings() %>% 
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings()

###################
##### Vectors #####
###################
library(tidyverse)

typeof(letters) # character

typeof(1:10)

x <- list("a", "b", 1:10)
length(x) # 3

1:10 %% 3 == 0

c(TRUE, TRUE, FALSE, NA)

typeof(1) # double
typeof(1L) # integer
1.5L

x <- sqrt(2) ^ 2
x - 2

c(-1, 0, 1) / 0

library(pryr)

x <- "This is a reasonably long string."
object_size(x) # size of x

y <- rep(x, 1000)
object_size(y) # size of y

x <- sample(20, 100, replace = TRUE) # randomly produce 100 numbers range from 1 to 20
y <- x > 10
sum(y)  # how many are greater than 10?
mean(y) # what proportion are greater than 10?

typeof(c(TRUE, 1L)) # integer > logical
typeof(c(1L, 1.5)) # double > integer
typeof(c(1.5, "a")) # character > double

sample(10) + 100

runif(10) > 0.5

1:10 + 1:2

1:10 + 1:3

tibble(x = 1:4, y = 1:2) # Error: Column `y` must be length 1 or 4, not 2

tibble(x = 1:4, y = rep(1:2, 2))

tibble(x = 1:4, y = rep(1:2, each = 2))

c(x = 1, y = 2, z = 4)

library(purrr)
set_names(1:3, c("a", "b", "c"))

x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]
x[c(1, 1, 5, 5, 5, 2)]
x[c(-1, -3, -5)]
x[c(1, -1)] # Error in x[c(1, -1)] : only 0's may be mixed with negative subscripts

x <- c(10, 3, NA, 5, 8, 1, NA)
x[!is.na(x)]
x[x %% 2 == 0]

x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]

x <- list(1, 2, 3)
str(x)

x_named <- list(a = 1, b = 2, c = 3)
str(x_named)

y <- list("a", 1L, 1.5, TRUE)
str(y)

z <- list(list(1, 2), list(3, 4))
str(z)
z[[2]][1] # 3

x1 <- list(c(1, 2), c(3, 4))
x2 <- list(list(1, 2), list(3, 4))
x3 <- list(1, list(2, list(3)))

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
str(a[1:2])
str(a[4])
str(a[[1]])
str(a[[4]])
a$a
a[["a"]]

x <- 1:10
attr(x, "greeting")
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)

x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
attributes(x)

x <- as.Date("1971-01-01")
unclass(x) # Dates in R are numeric vectors that represent the number of days since 1 January 1970.
typeof(x) # double
attributes(x)

x <- ymd_hm("1970-01-01 01:00")
unclass(x) # 3600
typeof(x) # double
attributes(x)

attr(x, "tzone") <- "US/Pacific"
attr(x, "tzone") <- "US/Eastern"

y <- as.POSIXlt(x)
typeof(y)
attributes(y)

tb <- tibble(x = 1:5, y = 5:1)
typeof(tb) # list
attributes(tb)

df <- data.frame(x = 1:5, y = 5:1)
typeof(df) # list
attributes(df)

#####################
##### Iteration #####
#####################
library(tidyverse)

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence seq_along() --> 1:length(l)
  output[[i]] <- median(df[[i]])      # 3. body
}
output

y <- vector("double", 0)
seq_along(y) # 0 
1:length(y) # 1 0

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}

results <- vector("list", length(df)) # for (x in xs)
names(results) <- names(df) # for (nm in names(xs))

name <- vector('double',ncol(df))
value <- c()
for (i in seq_along(df)) {
  name[[i]] <- names(df)[[i]]
  value[[i]] <- df[[i]]
}
names(value) <- name

means <- c(0, 1, 2)
output <- double()

for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)
str(unlist(out))

flip <- function() sample(c("T", "H"), 1)
flips <- 0
nheads <- 0
while (nheads < 3) {
  if (flip() == "H") {
    nheads <- nheads + 1
  } else {
    nheads <- 0
  }
  flips <- flips + 1
}

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
output <- c()
for(i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output

col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  output
}
col_mean(df)

col_sd <- function(df) {
  output <- vector('double', length(df))
  for(i in seq_along(df)) {
    output[i] <- sd(df[[i]])
  }
  output
}
col_sd(df)

f1 <- function(x) abs(x - mean(x)) ^ 1
f2 <- function(x) abs(x - mean(x)) ^ 2
f3 <- function(x) abs(x - mean(x)) ^ 3

f <- function(x, i) abs(x - mean(x)) ^ i

col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}

col_summary(df, median)
col_summary(df, mean)

# map() makes a list.
# map_lgl() makes a logical vector.
# map_int() makes an integer vector.
# map_dbl() makes a double vector.
# map_chr() makes a character vector.

map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

z <- list(x = 1:3, y = 4:5)
map_int(z, length)

models <- mtcars %>% 
  split(.$cyl) %>% # category according to cyl
  map(function(df) lm(mpg ~ wt, data = df))

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)

models %>% 
  map(summary) %>% 
  map_dbl("r.squared")

x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2) # extract the second element in each list

x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)

x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]

x1 %>% sapply(threshold) %>% str()
x2 %>% sapply(threshold) %>% str()

mu <- list(5, 10, -3)
mu %>% 
  map(rnorm, n = 5) %>% 
  str()

sigma <- list(1, 5, 10)
seq_along(mu) %>% 
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>% 
  str()

map2(mu, sigma, rnorm, n = 5) %>% str() # iterates over two vectors in parallel map2(): rnorm(5, 1, n = 5)

map2 <- function(x, y, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
  out
}

library(purrr)
n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>% 
  str()

args2 <- list(mean = mu, sd = sigma, n = n)
args2 %>% 
  pmap(rnorm) %>% 
  str()

params <- tribble(
  ~mean, ~sd, ~n,
  5,     1,  1,
  10,     5,  3,
  -3,    10,  5
)

params %>% 
  pmap(rnorm)

f <- c("runif", "rnorm", "rpois")

param <- list(
  list(min = -1, max = 1), 
  list(sd = 5), 
  list(lambda = 10)
)

invoke_map(f, param, n = 5) %>% str() # runif(min = -1, max = 1, n = 5) # rnorm(sd = 5, n = 5) # rpois(lambda = 10, n = 5)

sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

sim %>% 
  mutate(sim = invoke_map(f, params, n = 10))

x <- list(1, "a", 3)

x %>% 
  walk(print)

library(ggplot2)
plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths, plots), ggsave, path = tempdir())

x <- list(1:5, letters, list(10))
x %>% 
  some(is_character) # TRUE

x %>% 
  every(is_vector) # TRUE

x <- sample(10)

x %>% 
  detect(~ . > 5) # detect() finds the first element where the predicate is true

x %>% 
  detect_index(~ . > 5)

dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)


dfs %>% reduce(full_join)

vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)

vs %>% reduce(intersect) # find numbers in all three list

x <- sample(10)
x %>% accumulate(`+`)

########################
##### Model basics #####
########################
library(tidyverse)
library(modelr)
options(na.action = na.warn)

ggplot(sim1, aes(x, y)) + 
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point()

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}

model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}

measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )

best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])

sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

grid <- sim1 %>% 
  data_grid(x)

grid <- grid %>% 
  add_predictions(sim1_mod)

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)

sim1 <- sim1 %>% 
  add_residuals(sim1_mod)

ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)

ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() 

df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
)

model_matrix(df, response ~ sex)

ggplot(sim2) + 
  geom_point(aes(x, y))

mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)

ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)

tibble(x = "e") %>% 
  add_predictions(mod2) # error

ggplot(sim3, aes(x1, y)) + 
  geom_point(aes(colour = x2))

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2) # generate predictions from both models simultaneously and add each prediction as a row; spread_predictions(): adds each prediction to a new column

ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)

sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)

seq_range(c(0.0123, 0.923423), n = 5)
seq_range(c(0.0123, 0.923423), n = 5, pretty = TRUE)

x1 <- rcauchy(100)
seq_range(x1, n = 5)
seq_range(x1, n = 5, trim = 0.10) # trim = 0.1 will trim off 10% of the tail values
seq_range(x1, n = 5, trim = 0.25)
seq_range(x1, n = 5, trim = 0.50)

x2 <- c(0, 1)
seq_range(x2, n = 5)
seq_range(x2, n = 5, expand = 0.10) # expand = 0.1 is in some sense the opposite of trim() it expands the range by 10%.
seq_range(x2, n = 5, expand = 0.25)
seq_range(x2, n = 5, expand = 0.50)

ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)

ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~ model)

sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

ggplot(sim5, aes(x, y)) +
  geom_point()

df <- tribble(
  ~x, ~y,
  1, 2.2,
  2, NA,
  3, 3.5,
  4, 8.3,
  NA, 10
)

mod <- lm(y ~ x, data = df)
mod <- lm(y ~ x, data = df, na.action = na.exclude)

##########################
##### Model building #####
##########################
library(nycflights13)
library(lubridate)

ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)

diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))

ggplot(diamonds2, aes(lcarat, lprice)) + 
  geom_hex(bins = 50)

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)

grid <- diamonds2 %>% 
  data_grid(carat = seq_range(carat, 20)) %>% 
  mutate(lcarat = log2(carat)) %>% 
  add_predictions(mod_diamond, "lprice") %>% 
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) + 
  geom_hex(bins = 50) + 
  geom_line(data = grid, colour = "red", size = 1)

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")

ggplot(diamonds2, aes(lcarat, lresid)) + 
  geom_hex(bins = 50)

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)

grid <- diamonds2 %>% 
  data_grid(cut, .model = mod_diamond2) %>% 
  add_predictions(mod_diamond2) # Error in overscope_eval_next(overscope, expr) : object 'G' not found

grid <- diamonds2 %>%
  data_grid(cut, lcarat = -0.515, color = "G", clarity = "SI1") %>%
  add_predictions(mod_diamond2)

ggplot(grid, aes(cut, pred)) + 
  geom_point()

diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")

ggplot(diamonds2, aes(lcarat, lresid2)) + 
  geom_hex(bins = 50)

diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)

daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())

ggplot(daily, aes(date, n)) + 
  geom_line()

daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))

ggplot(daily, aes(wday, n)) + 
  geom_boxplot()

mod <- lm(n ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")

ggplot(daily, aes(wday, n)) + 
  geom_boxplot() +
  geom_point(data = grid, colour = "red", size = 4)

daily <- daily %>% 
  add_residuals(mod)

daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line()

ggplot(daily, aes(date, resid, colour = wday)) + 
  geom_ref_line(h = 0) + 
  geom_line()

daily %>% 
  filter(resid < -100)

daily %>% 
  filter(resid < -100)

daily %>% 
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line(colour = "grey50") + 
  geom_smooth(se = FALSE, span = 0.20)

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n)) + 
  geom_point() + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

term <- function(date) {
  cut(date, 
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall") 
  )
}

daily <- daily %>% 
  mutate(term = term(date)) 

daily %>% 
  filter(wday == "Sat") %>% 
  ggplot(aes(date, n, colour = term)) +
  geom_point(alpha = 1/3) + 
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

daily %>% 
  ggplot(aes(wday, n, colour = term)) +
  geom_boxplot()

mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>% 
  gather_residuals(without_term = mod1, with_term = mod2) %>% 
  ggplot(aes(date, resid, colour = model)) +
  geom_line(alpha = 0.75)

grid <- daily %>% 
  data_grid(wday, term) %>% 
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() + 
  geom_point(data = grid, colour = "red") + 
  facet_wrap(~ term)

compute_vars <- function(data) {
  data %>% 
    mutate(
      term = term(date), 
      wday = wday(date, label = TRUE)
    )
}

#######################
##### Many Models #####
#######################
library(modelr)
library(tidyverse)
library(gapminder)

gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

nz <- filter(gapminder, country == "New Zealand")

nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)

nz %>% 
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")

by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest() # make remaining columns as a list

by_country$data[[1]]

country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

models <- map(by_country$data, country_model)

by_country <- by_country %>% 
  mutate(model = map(data, country_model))

by_country %>% 
  filter(continent == "Europe")

by_country %>% 
  arrange(continent, country)

by_country <- by_country %>% 
  mutate(resids = map2(data, model, add_residuals))

resids <- unnest(by_country, resids)

resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

resids %>% 
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~continent)

broom::glance(nz_mod) # model quality

by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)

glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE) # only remain country, continent and glance

glance %>% 
  arrange(r.squared)

glance %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_jitter(width = 0.5)

bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()

gapminder %>% 
  group_by(country, continent) %>% 
  nest()

gapminder %>% 
  nest(year:gdpPercap)

df <- tribble(
  ~x1,
  "a,b,c", 
  "d,e,f,g"
) 

df %>% 
  mutate(x2 = stringr::str_split(x1, ",")) # x2 as a list

df %>% 
  mutate(x2 = stringr::str_split(x1, ",")) %>% 
  unnest()

sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = -1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)

sim %>%
  mutate(sims = invoke_map(f, params, n = 10))

mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = quantile(mpg)) # Error: Column `q` must be length 1 (a summary value), not 5

mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = list(quantile(mpg))) %>%
  unnest()

probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)

mtcars %>% 
  group_by(cyl) %>% 
  summarise(p = list(probs), q = list(quantile(mpg, probs))) %>% 
  unnest()

x <- list(
  a = 1:5,
  b = 3:4, 
  c = 5:6
) 

df <- enframe(x)

df %>% 
  mutate(smry = map2_chr(name, value, ~ stringr::str_c(.x, ": ", .y[1]))) %>%
  unnest()

df <- tribble(
  ~x,
  letters[1:5],
  1:3,
  runif(5)
)

df %>% mutate(
  type = map_chr(x, typeof),
  length = map_int(x, length))

df <- tribble(
  ~x,
  list(a = 1, b = 2),
  list(a = 2, c = 4)
)

df %>% mutate(
  a = map_dbl(x, "a"),
  b = map_dbl(x, "b", .null = NA_real_)
)

tibble(x = 1:2, y = list(1:4, 1)) %>% unnest(y)

df1 <- tribble(
  ~x, ~y,           ~z,
  1, c("a", "b"), 1:2,
  2, "c",           3
)

df1 %>% unnest(y, z)

df2 <- tribble(
  ~x, ~y,           ~z,
  1, "a",         1:2,  
  2, c("b", "c"),   3
)

df2 %>% unnest(y, z) # All nested columns must have the same number of elements.

######################################
##### Graphics for communication #####
######################################
library(tidyverse)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "Fuel efficiency generally decreases with engine size")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    colour = "Car type" # title of color
  )

df <- tibble(
  x = runif(10),
  y = runif(10)
)

ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i == 1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  ) # special mathematical sign

best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_text(aes(label = model), data = best_in_class)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_label(aes(label = model), data = best_in_class, nudge_y = 2, alpha = 0.5)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  ggrepel::geom_label_repel(aes(label = model), data = best_in_class)

class_avg <- mpg %>%
  group_by(class) %>%
  summarise(
    displ = median(displ),
    hwy = median(hwy)
  )

ggplot(mpg, aes(displ, hwy, colour = class)) +
  ggrepel::geom_label_repel(aes(label = class),
                            data = class_avg,
                            size = 6,
                            label.size = 0,
                            segment.color = NA
  ) +
  geom_point() +
  theme(legend.position = "none")

label <- mpg %>%
  summarise(
    displ = max(displ),
    hwy = max(hwy),
    label = "Increasing engine size is \nrelated to decreasing fuel economy."
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")

label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = "Increasing engine size is \nrelated to decreasing fuel economy."
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")

"Increasing engine size is related to decreasing fuel economy." %>%
  stringr::str_wrap(width = 40) %>%
  writeLines()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_colour_discrete()

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL) # no labels

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date(NULL, breaks = presidential$start, date_labels = "'%y")

base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))

base + theme(legend.position = "left") # change the location of class
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right") # the default

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4)))

ggplot(diamonds, aes(carat, price)) +
  geom_bin2d()

ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d()

ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() + 
  scale_x_log10() + 
  scale_y_log10()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv)) +
  scale_colour_brewer(palette = "Set1")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_colour_brewer(palette = "Set1")

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, colour = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(values = c(Republican = "red", Democratic = "blue"))

df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)

ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis() +
  coord_fixed()

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 7), ylim = c(10, 30))

mpg %>%
  filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>%
  ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact")

ggplot(suv, aes(displ, hwy, colour = drv)) +
  geom_point()

ggplot(compact, aes(displ, hwy, colour = drv)) +
  geom_point()

x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_colour_discrete(limits = unique(mpg$drv))

ggplot(suv, aes(displ, hwy, colour = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

ggplot(compact, aes(displ, hwy, colour = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_bw()

ggplot(mpg, aes(displ, hwy)) + geom_point()

ggsave("my-plot.pdf")
