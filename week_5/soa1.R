library(tidyverse)

table1
table2
table3
table4a
table4b


#These are all representations of the same underlying data, but they are not equally easy to use.

table1 %>%
  mutate(rate = cases / population * 10000)
table1 %>%
  count(year, wt= cases)

library(ggplot2)
ggplot(table1, aes(year, cases)) +
  geom_line(aes(group = country), colour = "grey50") +
  geom_point(aes(colour = country))


tidy4a <- table4a %>%
  pivot_longer(c('1999', '2000'), names_to = "year", values_to = "cases")


tidy4b <- table4b %>%
  pivot_longer(c('1999', '2000'), names_to = 'year', values_to = 'population')


left_join(tidy4a, tidy4b)


table2


table2 %>%
  pivot_wider(names_from = type, values_from = count)




table3


table3 %>%
  separate(rate, into = c("cases", "population"), sep = "/")


table3 %>%
  separate(rate, into = c("cases", "population"), convert = TRUE)



#unite()

table5 %>%
  unite(new, century, year, sep = "")


tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"))

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"))


stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

stocks %>%
  pivot_wider(names_from = year, values_from = return)


stocks %>%
  pivot_wider(names_from = year, values_from = return) %>%
  pivot_longer(
    cols = c('2015', '2016'),
    names_to = "year",
    values_to = "return",
    values_drop_na = TRUE
  )


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
  fill(person)


who1 <- who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = "key",
    values_to = "cases",
    values_drop_na = TRUE
  )
who1


who2 <- who1 %>%
  mutate(names_from = stringr::str_replace(key, "newrel", "new_rel"))
who2


who3 <- who2 %>%
  separate(key, c("new", "type", "sexage"), sep = "_")
who3


who3 %>%
  count(new)


who4 <- who3 %>%
  select(-new, -iso2, -iso3)

who4


who5 <- who4 %>%
  separate(sexage, c("sex", "age"), sep = 1)
who5


who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)


