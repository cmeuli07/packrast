require(magrittr)
require(tidyverse)


#' This function accepts a data frame argument
#' and cleans up the headers
#' Only works with data exported as xls


clean_headers <- function(data){

  headers <- colnames(data) %>%
    tolower() %>%
    str_replace_all(., "\\s", "_") %>%
    str_replace_all(., "\\$", "dol") %>%
    str_replace_all(., "&", "and") %>%
    str_replace_all(., "#", "num") %>%
    str_replace_all(., "bc_department", "dept") %>%
    str_replace_all(., "bc_super_category", "super_cat") %>%
    str_replace_all(., "bc_category", "cat") %>%
    str_replace_all(., "bc_sub_category", "sub_cat") %>%
    str_replace_all(., "bc_segment", "segment") %>%
    str_replace_all(., "w/o", "wo") %>%
    str_replace_all(., "%", "pct") %>%
    str_replace_all(., "pctacv", "pct_acv") %>%
    str_replace_all(., "short_product_description", "short_prod_desc") %>%
    str_replace_all(., "long_product_description", "long_prod_desc") %>% 
    str_replace_all(., "disp_wo_feat", "disp") %>%
    str_replace_all(., "feat_wo_disp", "feat") %>% 
    str_replace_all(., "\\[|\\]", "") %>% 
    str_replace_all(., "\\(|\\)", "") %>% 
    str_replace_all(., "\\?", "")

  colnames(data) <- headers

  return(data)

}
