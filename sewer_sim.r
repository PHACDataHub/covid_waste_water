

source(file.path("utils.r"))



packages.get(c(
  "zoo"
))

NUM_PERSONS <- 10^6
NUM_AREAS_OF_CITY <- sample(3:7, 1)

INF_PROB_MIN = 0.001
INF_PROB_MAX = 0.05

TRAVEL_H_MIN = 1
TRAVEL_H_MAX = 4


city_areas <- 
  tibble(per_pop = runif(NUM_AREAS_OF_CITY)) %>%
  mutate(per_pop = per_pop/sum(per_pop)) %>% 
  mutate(area_id = letters[1:NUM_AREAS_OF_CITY]) %>%
  mutate(., prob_inf = runif(n = nrow(.), min = INF_PROB_MIN, max = INF_PROB_MAX)) %>%
  mutate(., trav_time_MIN = rlnorm(n = nrow(.), meanlog = sample(1, TRAVEL_H_MIN:TRAVEL_H_MAX)) * 60) %>%
  mutate(., trav_time_MAX = trav_time_MIN + rlnorm(n = nrow(.), meanlog = 1) * 3 *60)
  






#https://www.healthline.com/health/american-gut-check#Too-much-or-not-enough?-
poop_time <-
  read.csv(text = 
"poop_tod, poop_prob, poop_time_min, poop_time_max
morning,61.3, 6, 12
aternoon,22, 12,18
evening,14.1, 18, 24
night,2.6, 0, 6") %>% 
  mutate(poop_time_min = poop_time_min * 60) %>% 
  mutate(poop_time_max = poop_time_max * 60)


city_areas


persons <- 
tibble(person_id = 1:NUM_PERSONS) %>% 
  mutate(area_id = sample(size = NUM_PERSONS, x = city_areas$area_id, prob = city_areas$per_pop, replace = T)) %>%
  left_join(city_areas, by = "area_id") %>% 
  mutate(inf = runif(n = nrow(.))<= prob_inf) %>% 
  mutate(trav_time = rbeta(n = nrow(.), shape1 = 2 , shape2 = 2) * (trav_time_MAX - trav_time_MIN) + trav_time_MIN) %>% 
  mutate(poop_tod = sample(size = NUM_PERSONS, x = poop_time$poop_tod, prob = poop_time$poop_prob, replace = T))  %>% 
  left_join(poop_time, by = "poop_tod") %>%
  mutate(poop_time = runif(n = nrow(.), min =  poop_time_min , max = poop_time_max)) %>%
  mutate(arrive_time = as.integer((poop_time + trav_time) %% (24*60)))
    



city_str <-
  paste(
  city_areas %>% 
  mutate(kvp = str_kvp(area_id, sprintf("%.0f%%", 100*prob_inf, "%"))) %>% 
  pull(kvp) %>% 
  str_l(qts = "", front = "", back = "", escape = F) %>%
  str_kvp("prob_inf" , .)
,
city_areas %>% 
  mutate(kvp = str_kvp(area_id, sprintf("%.0f%%", 100*per_pop, "%"))) %>% 
  pull(kvp) %>% 
  str_l(qts = "", front = "", back = "", escape = F) %>%
  str_kvp("per_pop" , .)
,
persons %>% 
  group_by(area_id) %>%
  summarise(trav_time_mean = mean(trav_time)) %>% 
  mutate(kvp = str_kvp(area_id, format(trav_time_mean/60.0, digits = 2))) %>% 
  pull(kvp) %>% 
  str_l(qts = "", front = "", back = "", escape = F) %>%
  str_kvp("trav_time" , .)
,
sep = "\n")







persons %>% 
  count(inf, arrive_time) %>% 
  group_by(arrive_time) %>%
  mutate(frac = n/sum(n)) %>%
  filter(inf == TRUE) %>%
  ungroup() %>% 
  arrange(arrive_time) %>% 
  mutate(frac_mean=rollmean(x = frac,k = 120, na.pad=TRUE, align="right")) %>%
  #mutate(frac_mean = rollmean(frac, k = 30)) %>% 
  ggplot(aes(x = arrive_time, y = frac)) + geom_point(alpha = 0.5) + 
  geom_line(aes(y = frac_mean), color = "red", size = 1.2) + 
  labs(title = city_str)
  



persons %>% 
  ggplot(aes(x = arrive_time, fill = inf)) + 
  geom_histogram(alpha = 0.5) +
  facet_grid(rows = vars(inf), scales = "free")




persons %>% 
  ggplot(aes(x = poop_tod)) + geom_histogram(alpha = 0.5, stat="count")

persons %>% 
  ggplot(aes(x = poop_time)) + geom_histogram(alpha = 0.5)

persons %>% 
  ggplot(aes(x = trav_time)) + geom_histogram(alpha = 0.5)


persons %>% 
  count(area_id, inf) %>% 
  #filter(inf == TRUE) %>% 
  ggplot(aes(y = area_id, x = n)) + geom_col(alpha = 0.5) + 
  facet_grid(cols = vars(inf), scales = "free")
  


