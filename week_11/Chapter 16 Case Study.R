
library(tidyverse)


vb = read_csv("C:/stat_240/data/volleyball-team-2019.csv")
big_10 = vb %>% 
  filter(Conference == "Big Ten")

big_10



r = big_10 %>%
  summarise(r = cor(Hit_pct, Opp_pct)) %>%
  pull(r)


ggplot(big_10, aes(x=Hit_pct, y=Opp_pct)) +
  geom_point() +
  geom_smooth(se=FALSE, method="lm") +
  ggtitle(paste("r =",round(r,3)))





## Hit_pct and W
r = big_10 %>% 
  summarize(r = cor(Hits_pct,W)) %>% 
  pull(r)

ggplot(big_10, aes(x=Hit_pct, y=W)) +
  geom_point() +
  geom_smooth(se=FALSE, method="lm") +
  ggtitle(paste("r =",round(r,3)))




ex1 = big_10 %>% 
  mutate(kills_per_set = Kills / Sets,
         win_pct = Win_pct,
         team = Team) %>% 
  select(team, kills_per_set, win_pct)




ggplot(ex1, aes(x=kills_per_set, y=win_pct)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")




ex1 %>% 
  summarize(r = cor(kills_per_set,win_pct))



ex1_sum = ex1 %>%
  summarize(mx = mean(kills_per_set),
            sx = sd(kills_per_set),
            my = mean(win_pct),
            sy = sd(win_pct),
            r = cor(kills_per_set, win_pct),
            b1 = r * sy/sx,
            b0 = my - b1*mx)
ex1_sum



ex1_sum %>% pull(b0)
ex1_sum %>% pull(b1)
fit = lm(win_pct ~ kills_per_set, data=ex1)
coef(fit)






vb_matches = read_csv("C:/stat_240/data/vb-division1-2019-all-matches.csv")
vb_matches %>% 
  filter(row_number() == 1) %>% 
  as.data.frame() %>% 
  print()





inv_logistic = function(x) { return ( 1/(1 + exp(-x)) )}
delta = seq(-5,5,length.out=1001)
p = inv_logistic(delta)
df = tibble(delta,p)

ggplot(df, aes(x=delta,y=p)) +
  geom_line(color="blue") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 1, color="red", linetype = "dashed")





inv_logistic = function(x) { return ( 1/(1 + exp(-x)) )}

sim_set = function(delta,target)
{
  p = inv_logistic(delta)
  points = c(0,0)
  repeat
  {
    pt = rbinom(1,1,p)
    if ( pt == 1 )
      points[1] = points[1] + 1
    else
      points[2] = points[2] + 1
    if ( max(points) >= target && abs(diff(points)) >= 2 )
      break
  }
  return ( points )
}



sim_match = function(delta)
{
  tab = matrix(NA,2,5)
  sets = c(0,0)
  index = 1
  repeat
  {
    if ( sum(sets) < 4 )
      result = sim_set(delta,25)
    else
      result = sim_set(delta,15)
    if ( result[1] > result[2] )
      sets[1] = sets[1] + 1
    else
      sets[2] = sets[2] + 1
    tab[,index] = result
    index = index + 1
    if ( max(sets == 3) )
      break
  }
  return ( tab )
}






set.seed(356346)

B = 10
Delta = -0.113

results = list()

for ( i in 1:B )
{
  print(i)
  results[[i]] = sim_match(Delta)
  print( results[[i]] )
  team1 = results[[i]][1,]
  team2 = results[[i]][2,]
  team1 = team1[!is.na(team1)]
  team2 = team2[!is.na(team2)]
  sets1 = sum(team1 > team2)
  sets2 = sum(team1 < team2)
  if ( sets1 > sets2 )
  {
    print(paste("Team 1 wins:", sets1, "to", sets2))
  }
  else
  {
    print(paste("Team 2 wins:", sets2, "to", sets1))
  }
}
