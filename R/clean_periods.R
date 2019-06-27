require(magrittr)
require(lubridate)
require(tidyverse)

#' This function parses the period column returned by AOD
#' and returns two columns, one indicating the type of period
#' and the other the week ending for the period, formatted as a date
#' and cleans up the headers.
#' @examples 
#' data <- clean_periods(data, "Periods")

clean_periods <- function(data, period_col){
  
  data %<>%
    mutate(., week_end = str_sub(.[[period_col]], -8)) %>% 
    mutate(., week_end = mdy(week_end)) %>%
    mutate(., period_type = str_sub(.[[period_col]], 1, -9)) %>%
    mutate(., period_type = str_replace_all(period_type, " - W/E ","")) %>%
    mutate(., period_type = str_replace_all(period_type, " $","")) %>%
    select(., period_type, week_end, everything(), -period_col)
  return(data)
}

