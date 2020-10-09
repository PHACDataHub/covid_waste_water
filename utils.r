



print(paste0("utils.r getwd() = ", getwd()))



# Because I can never remember which axis is which
APPLY_MRGN_ROW <- 1
APPLY_MRGN_COL <- 2

# Packages need for this file
# TODO: figure out a better way to forst plyr to load before dplyr!
library(plyr)
library(dplyr)
library(tidyverse)





packages_for_utils <- c ("tidyverse", 
                         "xfun", 
                         "docstring", 
                         "rappdirs", 
                         "digest",
                         "fs")

#' Takes a list of packages and installs them and loads them when needed
packages.get <- function(list.of.packages, 
                         library_require = library){
  #' Installs then loads a vector of packages if they are not already
  #'
  #' packages.get
  #' 
  #' @param list.of.packages really a vector of strings with package names
  #' @param library_require either the library or require function
  #' 
  #' uses cran or whatever is already setup on this environment
  
  #Find which packages are not installed
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  # find which packageae need to be loaded
  packages_to_load <- list.of.packages[!list.of.packages %in% .packages()]
  
  print(packages_to_load)
  #Loads the packages
  tmp <- 
    lapply( 
      X = packages_to_load, 
      FUN = library_require,
      character.only = TRUE,
      warn.conflicts = FALSE,
      quietly = TRUE
    )
}
################

##################################
# Get Required Packages
packages.get(packages_for_utils)




###################################
view_sample <- function(df, rows = 1000, replace = F) {
  #' view_sample
  #' 
  #' views only a few rows of the df
  #' @param df a data frame
  #' @param rows number of rows to sample >0
  #' @param replace do we sample with replacement
  sample_size <- min(1000, nrow(df))
  df %>% 
    sample_n(sample_size, replace = replace) %>% view()
}

#############################################
seven_day_week <- function(){
  #' seven_day_week
  #' 
  #' Returns 7 days of the week vector
  #' Basicaly returns "Sunday", "Monday", ...
  weekdays(as.Date(4,"1970-01-07",tz="GMT")+0:6)
}


#############################################
# Global variable needed for the unlog_epsilon and log_epsilon functions below
LOG_UNLOG_EPSILON_LOOK_UP_LIST <- list()


unlog_epsilon <- function(z, 
                          lst_factors = LOG_UNLOG_EPSILON_LOOK_UP_LIST[[digest(z)]],
                           base = lst_factors$base, 
                          epsilon = lst_factors$epsilon
){
  #' log_epsilon
  #' 
  #' takes the log of a vector but first adds a "little bit"to avoid logging a zero and getting a numeric instability
  #' 
  #' @param x vector of doubles
  #' @lst_factors by default comes from a global varaible that is set when log_epsilon is called
  #' @param base default = exp(1), base of the log
  #' @param epsilon added to x before logging, defaults to a huristic that is small relative to most in the vector
  #' 
  #' 
  return((base^z)-epsilon)
}


#y <- log_epsilon(x)
#bind_cols(unlog_epsilon(y), x, log_epsilon(x)) %>% distinct()
log_epsilon <- function(x , 
                        base = exp(1), 
                        epsilon = min(quantile(x[x > 0], probs = 0.05, na.rm = T), 0.0001), 
                        log_fun = log, 
                        ...)
{
  #' log_epsilon
  #' 
  #' takes the log of a vector but first adds a "little bit"to avoid logging a zero and getting a numeric instability
  #' 
  #' @param x vector of doubles
  #' @param base default = exp(1), base of the log
  #' @param epsilon added to x before logging, defaults to a huristic that is small relative to most in the vector
  #' @param log_fun default = log,  function that actually does the logging 
  #' @param  ... passed  to log_fun
  #' 
  #' 
  
  
  
  
  pp("in function log_epsilon, epsilon of ", epsilon," used.")
  
  z <- log_fun(x = (x+epsilon), base = base, ...)
  
  
  LOG_UNLOG_EPSILON_LOOK_UP_LIST[[digest(z)]] <<- list("base" = base, 
                                                       "epsilon" = epsilon)
  
  return(z)
}



str_w <- function(str, begin = "(", ending = ")", ...){
  #' Wrap a string in brackets
  #' 
  #' add somehting to the start and end of a string usually brackets
  #' 
  #' @param str a string for the middle
  #' @param begin default "("
  #' @param ending default ")"
  str_q(str, begin = begin, ending = ending , ...)
}

str_q <- function(str, qt = "'", begin = qt, ending = qt, ...){
  #' wrap a string in some kind of quotes
  #' 
  #' @param str a string for the middle
  #' @param qt a string to put on either side
  #' @param begin default qt
  #' @param ending default qt
  paste0(begin, str, ending, ... )
}



pp <- function(..., 
               sep = ", ", 
               begining = "",
               ending = "",
               paste_fun = paste,
               print_fun = print){
  #' Pastes all arguments togeather then prints them out
  #' 
  #' pp - Pastes all arguments togeather then prints them out
  #' 
  #' @param ... passed to paste
  #' @param sep defaults to ", "
  #' @param begining string at the start
  #' @param ending string at the end
  #' 
  print_fun(paste_fun(begining, ..., ending , sep = sep))
}


get_data_url_from_github_url <- function(web_url){
  #'
  #' returns data url from the web url
  #'
  web_url %>% 
    gsub(pattern = "^https://github.com", replacement = "") %>%
    gsub(pattern = "/blob/", replacement = "/") %>%
    paste0("https://raw.githubusercontent.com", .)
    
}
get_df_from_github <- function(web_url, ...){
  #'
  #' Transforms web url for github into a raw url then gets the dataframe that would come from it.
  #'
  get_df_from_url(get_data_url_from_github_url(web_url), ...)
}





#url= paste0("file://",path)
get_df_from_url <- function(url, 
                            url_file_ext = file_ext(url), #"csv",
                            read_func = rio::import, #read_csv, 
                            cache_sub_dir = "R_tmp_cache",
                            cache_dir = file.path(rappdirs::user_cache_dir(), cache_sub_dir),
                            time_delta_seconds = (60 * 60  * 24 * 0.5) ,
                            ...){
  #' get_df_from_url
  #' 
  #' Return a df from a url with a cache  
  #' 
  #' @param url the url that will get dta
  #' @param url_file_ext the file extension, to that helps figure out the type of file
  #' @param read_func the function that will read once it is in the cache directory
  #' @param cache_sub_dir the subdirectyr to save data in
  #' @param time_delta_seconds the delta time before checking for a new copy
  
  external_URL_pattern = "^http"

  
  if (! dir.exists(cache_dir)){
    dir.create(cache_dir)
  }
  
  
  tmp_fn <- digest(url)
  tmp_fn <- paste0(tmp_fn, ".", url_file_ext)
  tmp_fn <- file.path(cache_dir, tmp_fn)
  
  
  if ( ! file.exists(tmp_fn)){
    paste0("Cache does not exist downloading file ", url) %>% print()
    
    if ( ! grepl(external_URL_pattern, url)){
      fs::file_copy(path = url, new_path = tmp_fn, overwrite = T)
      Sys.setFileTime(tmp_fn, Sys.time())
      #file.copy(from = url, to = tmp_fn, overwrite = T)
    }else
      download.file(url = url, destfile = tmp_fn)
  }
  
  
  dt_sec = as.numeric(Sys.time() - file.info(tmp_fn)$mtime ,  units="secs")
  if (dt_sec > time_delta_seconds){
    paste0("Cache is ", round(dt_sec,1)," seconds old, downloading file ", url) %>% print()
    if ( ! grepl(external_URL_pattern, url)){
      fs::file_copy(path = url, new_path = tmp_fn, overwrite = T)  
      Sys.setFileTime(tmp_fn, Sys.time())
      #file.copy(from = url, to = tmp_fn, overwrite = T)
    }else
      download.file(url = url, destfile = tmp_fn)
  }
  
  
  paste0("reading from cache ", tmp_fn) %>% print()
  df_raw <- read_func(tmp_fn , ...)%>% as_tibble()
  df <- df_raw %>% clean_names()
  return(df)
}





seperate_pivot_longer <- function(df, 
                                  key_col, 
                                  split_col,
                                  sep = "[,;]" , # "[^[:alnum:]]+"
                                  reomve_others = TRUE,
                                  MAX_N_SPLITS = 10){
  #' seperate_pivot_longer
  #' 
  #' Splits the specified column name from the dataframe into mutiple columns, then does a "pivot_longer"
  df_longer <-
    df %>%
    select(c(key_col, split_col )) %>% 
    separate(col = split_col, into =paste0(split_col, "_",1:MAX_N_SPLITS),remove = TRUE, sep = sep) %>% 
    pivot_longer(cols = paste0(split_col, "_",1:MAX_N_SPLITS), names_to = paste0(split_col, "_name"), values_to = "Split_long") %>%
    select(c(key_col,"Split_long" )) %>%
    na.omit() %>%
    mutate(Split_long = toupper(trimws(Split_long))) %>%
    mutate(Split_long =trimws(gsub(pattern = "AND", replacement = "", x = Split_long))) %>%
    mutate(Split_long =trimws(gsub(pattern = "-", replacement = " ", x = Split_long))) %>%
    rename(!!split_col := Split_long) %>%
    distinct()
  
  df_ret <- NULL
  if (reomve_others == TRUE){
    df_ret = df_longer
  }else{
    df_ret = df %>% select(-c(split_col)) %>% left_join(df_longer)
  }
  
  return(df_ret)
}
#######################################




#####################################
#
#
#
summarise_and_relong <- function(df, 
                                 val_cols = c("scr", "financial_cost"), 
                                 grp_cols = c("cat", "cat_typ"), 
                                 funs = mean, 
                                 na.rm = TRUE,
                                 post_fix = "_mean",
                                 ...
){
  #' summarise_and_relong
  #' 
  #' summarizes data, then relongs the data
  
  #  df = votes_df
  df_summary <- 
    df %>%
    group_by_at(grp_cols) %>%
    summarise_at(.vars = val_cols, .funs = funs, na.rm = na.rm , ...)
  
  df_summary <- 
    df_summary %>% 
    rename_at(.vars = val_cols, paste0, post_fix)
  
  ret_val <- 
    df %>% 
    inner_join(df_summary, by = grp_cols)
  
  
  return(ret_val)  
}
###################################




################################
impute_by_group_v <- function(val, grp, fun = mean, ...)
{
  #' impute_by_group_v
  #' 
  #' Imputes data by group vector 
  #' this is called by impute_by_group
  tibble(val, grp) %>%
    group_by(grp) %>%
    mutate(val=ifelse(is.na(val),fun(val,na.rm=TRUE, ...),val)) %>%
    pull(val)
  
}
##############################


################################
impute_by_group<- function(df, grp , cols, fun = mean, ...){
  #' impute_by_group
  #' 
  #' Imputes data by group
  #' 
  #' @param df a dataframe
  #' @param grp string with the column to group by
  #' @param cols vector of strings, names of columns to impute
  #' @param fun function to execute when imputing, default is mean
  df = votes_df
  
  
  for (col in cols){
    df[[col]] = impute_by_group_v(val= df[[col]], 
                                grp = df[[grp]], 
                                fun = fun, 
                                ...)
  }
  return(df)
}
##############################


##############################
"%contained%" <- function(vs,x) {
  #' "%contained%"
  #' 
  #' Checks if all elements of vs are somethere in x
  #' 
  #' @param vs vector to check
  #' @param x vector to check against
  ret = T
  u_vs <- vs %>% unique()
  for (u in u_vs){
    if(! is.na(u)){
      if( nchar(u)> 0 ){
        if (! u %in% x){
          return(F)
        }
      }
    }
    #print(u)
  }
  return(ret)
}



##############################
get_important_dates <- function(){
  # TODO : this is a stupid way to do this, take it out of utils
  read_tsv("dates.tsv")
}




##############################
destroy_environment <- function(){
  #' this will get rid of all environment objects and the attached libraries
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
  gc()
}

