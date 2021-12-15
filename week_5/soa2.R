library(tidyverse)
library(nycflights13)


airlines
airports
planes
weather


planes %>%
  count(tailnum) %>%
  filter (n > 1)

weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n > 1)


#mutating join

flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2


flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = 'carrier')


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
  left_join(y, by = "key")


x %>%
  inner_join(y, by = "key")


x %>%
  right_join(y, by = "key")


x %>%
  full_join(y, by = "key") %>%
  drop_na()


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
left_join(x, y , by = 'key')
# get all possible combinations


#natural join

flights2 %>%
  left_join(weather)

flights2 %>% 
  left_join(planes, by = "tailnum")


flights2 %>%
  left_join(airports, c("dest" = "faa"))


flights2 %>% 
  left_join(airports, c("origin" = "faa"))


top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
top_dest


flights %>%
  filter(dest %in% top_dest$dest)


flights %>%
  semi_join(top_dest)


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
df1
df2

intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2)
setdiff(df2, df1)
