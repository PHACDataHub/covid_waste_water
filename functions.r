
if (exists("destroy_environment")) destroy_environment()

  
source(file.path("utils.r"))




packages.get(c(
  "tidyverse",
  "janitor",
  "docstring",
  "ggcorrplot",
  "here",
  "readxl",
  "lubridate"
))

DATA_DIR = "data"


read_ropec_data <- function(
  dir = DATA_DIR,
  path = "OPH - City of Ottawa - COVID-19 in Wastewater 2020.09.24 (002).xlsx",
  sheet = "Data from ROPEC's primary sludg"
){
  #' Read in Ropec Data
  #'
  #'
  read_xlsx(path = file.path(dir, path), sheet = sheet) %>% clean_names() %>%
    mutate(date = as.Date(date)) %>% 
    select(date, n1_avg, n1_stdev, n2_avg, n2_stdev)
}

read_Ottawa_community_data <- function(
  dir = DATA_DIR,
  path = "OPH - City of Ottawa - COVID-19 in Wastewater 2020.09.24 (002).xlsx",
  sheet = "Ottawa community data"
){
  #' Read in Ottawa Testing Data
  #'
  #'
  read_xlsx(path = file.path(dir, path), sheet = sheet) %>% clean_names() %>%
    mutate(date = as.Date(date)) %>% #colnames()
    select(date, non_ltch_tests, non_ltch_positive_tests, ltch_tests, ltch_positive_tests) %>%
    mutate(test = non_ltch_tests + ltch_tests, 
           postitive_tests = non_ltch_positive_tests + ltch_positive_tests )
}





read_HPOC_cases_data <- function(
  dir = "//Ncr-a_irbv2s/IRBV2/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/DATA AND ANALYSIS/SAS_Analysis/Domestic data",
  fn_pattern = "qry_allcases[ ]\\d\\d-\\d\\d-\\d\\d\\d\\d_NEW_FORMAT.xlsx",
  path = sort(list.files(dir,  fn_pattern, full.names = TRUE), decreasing = T)[1],
  pt_use = "on",
  hr_use = "city of ottawa 2251"
){
  #' Read in HPOC cases data
  #'
  df <- get_df_from_url(url = path) %>% 
        clean_names() 
    
  df %>% 
    select(onsetdate, reporteddate, labspecimencollectiondate1, labtestresultdate1, phacreporteddate, healthregion, pt, exposure_cat, age) %>% 
    mutate_if(function(x) inherits(x, "POSIXct") , as.Date) %>%
    filter(pt == pt_use) %>% #count(healthregion, sort = T)
    filter(healthregion == hr_use) 
}





impute_deltas <- function(df, 
                          colforcompare = "onsetdate", 
                          cols_dts = df %>% select_if(is.Date) %>% colnames(),
                          units = "days",
                          min_frac = 0.5){
  #' what are the deltas of all the relevent columns
  
  map_dfc(grep(pattern = colforcompare,  x = cols_dts ,value = T , invert = T), function(curr_col){
    newcolnm <- paste0(colforcompare, "_2_", curr_col)
    
    x = as.integer(difftime(df[[curr_col]], df[[colforcompare]], units = units))
    if (sum(is.na(x))*(1/min_frac) < length(x))
      x = replace(x, is.na(x), values = sample(x = x[!is.na(x)], size = sum(is.na(x)), replace = T))
    else 
      x = x
    
    tibble(col = x
    ) %>%
      rename(!!sym(newcolnm) := col)
  })
}



do_impute_on_col <- function(df = read_HPOC_cases_data(),
                               seed = 2814,
                               ...,
                               m = 5,
                               maxit=5 , 
                               r = sample(x = 1:m, size = 1),
                               col_nm = "onsetdate",
                               if_for_impute_col = is.Date
                               ){
  #' 
  #' Do imuting on df to estimate missing data
  #' 
  #' 
  cols_dts = df %>% select_if(if_for_impute_col) %>% colnames()
  
  #df[pasteo(col_nm, "_imputed")] <-
  df2 <- 
  df %>% 
    bind_cols(., 
              impute_deltas(df = ., colforcompare = col_nm, cols_dts = cols_dts))
  
  
  #get col_name Order for which to use first
  cols_to_use <- 
  map_dfc( df2 %>% select(starts_with(col_nm)) %>% select_if(is.numeric), function(x) {
    quantile(x, probs = c(0.95), na.rm = T) - quantile(x, probs = c(0.05), na.rm = T)
    }) %>% sort() %>% colnames()
  
  # Plot to check things
  # df2 %>% select(starts_with(col_nm)) %>% select_if(is.numeric) %>% 
  #   pivot_longer(., colnames(.)) %>%
  #   ggplot(aes(x = value, fill = name)) + geom_density(alpha = 0.5) +
  #   scale_x_continuous(limits = c(-5, 25))
  
  ifelse(!is.na(df2[[col_nm]]), 
                df2[[col_nm]], 
  ifelse(! is.na(df2[[gsub(paste0(col_nm, "_2_"), "",cols_to_use[1])]]) ,
         df2[[gsub(paste0(col_nm, "_2_"), "",cols_to_use[1])]] - df2[[cols_to_use[1]]],
  ifelse(! is.na(df2[[gsub(paste0(col_nm, "_2_"), "",cols_to_use[2])]]) ,
        df2[[gsub(paste0(col_nm, "_2_"), "",cols_to_use[2])]] - df2[[cols_to_use[2]]],
  ifelse(! is.na(df2[[gsub(paste0(col_nm, "_2_"), "",cols_to_use[3])]]) ,
         df2[[gsub(paste0(col_nm, "_2_"), "",cols_to_use[3])]] - df2[[cols_to_use[3]]],
         FALSE
  )))) %>% as.Date(origin="1970-01-01")
}



do_impute_on_cols <- function(df = read_HPOC_cases_data(),  col_nms = c("onsetdate","labspecimencollectiondate1")){
  #'
  #' Impute multiple columns on the DF
  #' return a slightly wider DF with imputed data
  #'
  #'
  
  
  
  tmp <- map_dfc(col_nms, function(col_nm){ 
    col_nm_imp = paste0(col_nm, "_imputed")
    tibble(tmp = do_impute_on_col(df, col_nm = col_nm)) %>% 
    rename(!!sym(col_nm_imp) := tmp)
    
    })
  
  # # quick plot for ansers
  # tmp %>% ggplot(aes(x = onsetdate_imputed, y = labspecimencollectiondate1_imputed)) + geom_point(alpha = 0.5)
  # df %>% ggplot(aes(x = onsetdate, y = labspecimencollectiondate1)) + geom_point(alpha = 0.5)
    
  
  
  # # quick plot for ansers
  # bind_cols(df, tmp) %>%
  #   select(col_nms, paste0(col_nms, "_imputed")) %>%
  #   pivot_longer(., colnames(.)) %>%
  #   filter(grepl(pattern = "onsetdate", x = name)) %>% 
  #   ggplot(aes(x = value, fill = name)) + geom_density(alpha = 0.5)
  bind_cols(df, tmp)
}








adjust_case_by_positive_rate <- function(df2, tests){
  #'
  #' returns df2 with an added column case multiplier
  #' https://covid19-projections.com/estimating-true-infections/
  #'    
  #
  tests2<- 
  tests %>% 
    mutate(test = zoo::rollmean(test, k = 7, fill = NA, align  = "right"), 
           postitive_tests = zoo::rollmean(postitive_tests, k = 7, fill = NA, align  = "right")
    ) %>% 
    mutate(percet_positive = postitive_tests/ test) %>% 
    select(date , percet_positive)# %>% pull(percet_positive) %>% plot()
  
  
  df2 %>% 
    filter(! is.na(labspecimencollectiondate1_imputed)) %>% 
    count(labspecimencollectiondate1_imputed) %>%
    rename(date := labspecimencollectiondate1_imputed, 
           n_lab_collect_impute  := n) %>% 
    full_join(tests2, by = "date") %>% 
    mutate(case_estimate = n_lab_collect_impute * (16 * (percet_positive)^(0.5) + 2.5) ) %>%
    mutate(case_multiplier =  case_estimate / n_lab_collect_impute ) %>% 
    rename(labspecimencollectiondate1_imputed := date) %>% 
    select(labspecimencollectiondate1_imputed, case_multiplier) %>% 
    left_join(df2, ., by = "labspecimencollectiondate1_imputed")
}










df <- read_HPOC_cases_data()
df2 <- df %>% do_impute_on_cols()

###################
# adjust for tests
tests <- read_Ottawa_community_data()
df3 <- df2 %>% adjust_case_by_positive_rate(tests = tests)



df_onset <- 
  df3 %>% 
  filter(! is.na(onsetdate_imputed)) %>% 
  group_by(onsetdate_imputed) %>%
  summarise(n = sum(case_multiplier)) %>%
#  count(onsetdate_imputed) %>% 
  rename(date := onsetdate_imputed
         )


virus_stool_weeks_after_onset <- 
  c(5.975077881619939
    , 7.218068535825546
    , 7
    , 9.965732087227416
    , 2.0498442367601246
    , 1.0249221183800623
    , 1.0249221183800623)



df_shedding <- 
  df_onset %>%
  bind_cols(tibble(name = paste0("week_",1:length(virus_stool_weeks_after_onset)), value = virus_stool_weeks_after_onset) %>% pivot_wider()) %>% 
  pivot_longer(matches("^week_")) %>% 
  mutate(week = as.numeric(gsub(pattern = "week_", replacement = "", x = name))) %>% 
  mutate(date_start = date + 7*(week - 1)) %>% 
  mutate(date_end = date_start + (7*week)-1) %>% 
  mutate(shedding = n * value)%>% 
  select(date_start, date_end, shedding)

lapply(1:7, function(i){
  df_shedding[paste0("day_",i)] <<-  df_shedding[["date_start"]] + i - 1
})

df_shedding_onset <- 
  df_shedding %>% 
  pivot_longer(matches("^day_")) %>% 
  group_by(value) %>%
  summarise(shedding = sum(shedding)) %>%
  rename(date := value) %>% 
  full_join(df_onset) 

ropec <- read_ropec_data() %>% mutate(n_avg = (n1_avg+n2_avg)/2 )












full_join(df_onset, tests) 




read_Ottawa_community_data()

df_shedding$date_start %>% is.na() %>% sum()





df_shedding_onset %>% 
  pivot_longer(cols = c("shedding", "onset")) %>%
  ggplot(aes(x = date, y = value, fill = name)) + geom_col() +
  facet_grid(rows = vars(name), scales = "free_y")

  
ropec <- ropec %>% mutate(n_avg = (n1_avg+n2_avg)/2 )

df_shedding_onset %>% 
  full_join(ropec, by = "date") %>% 
  GGally::ggpairs()




df2 %>% 
  count(labspecimencollectiondate1_imputed) %>% 
  rename(date := labspecimencollectiondate1_imputed) %>% 
  full_join(tests, by = "date") %>% 
  select(date, n, postitive_tests) %>% 
  arrange(date) %>%
  replace_na(list(n = 0, postitive_tests = 0)) %>% 
  mutate(n_cum = cumsum(n),
         postitive_tests_cum = cumsum(postitive_tests))  %>% 
  pivot_longer(cols = c("n_cum", "postitive_tests_cum")) %>%   
  ggplot(aes(x = date, y = value, color = name)) + geom_line() #+
  facet_grid(cols = vars(name))

  map_df(function(x){sum(x)})
  pivot_longer(cols = c("n", "postitive_tests")) %>% 
  ggplot(aes(x = date, y = value, fill = name)) + geom_col() +
  facet_grid(cols = vars(name))



  
full_join(tests, )


df = do_impute_on_cases_data()
df_counts <- df %>% 
  rename(date := !!sym("labspecimencollectiondate1")) %>% 
  count(date) 
dt_rng <- df_counts$date %>% range()

dts <- tibble(date = seq(from = as.Date(dt_rng[1]), to = as.Date(dt_rng[2]), by = 1) )

dts %>% 
  left_join(df_counts) %>% 
  replace_na(list(n = 0))



df %>% 
  select("onsetdate", "labspecimencollectiondate1") %>% 
  pivot_longer(cols = c("onsetdate", "labspecimencollectiondate1")) %>% 
  ggplot(aes(x = value, fill = name)) + geom_density(alpha = 0.5)



df %>% 
  select("onsetdate", "labspecimencollectiondate1") %>% 
  mutate(onset_to_testing = labspecimencollectiondate1- onsetdate) %>% 
  ggplot(aes(x = onset_to_testing)) + geom_density(alpha = 0.5)


df %>% 
  select("onsetdate", "phacreporteddate") %>% 
  mutate(onset_to_reporting = phacreporteddate- onsetdate) %>% 
  ggplot(aes(x = onset_to_reporting)) + geom_density(alpha = 0.5)

df %>% 
  select("onsetdate", "labspecimencollectiondate1") %>% 
  mutate(onset_to_testing = labspecimencollectiondate1- onsetdate) %>% 
  ggplot(aes(y = onset_to_testing, x = onsetdate)) + geom_point()


df_test <- read_Ottawa_community_data()
