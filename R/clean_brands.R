require(magrittr)
require(tidyverse)


#' This function accepts a data frame argument
#' It searches for any column header having to do with brands
#' and then will replace the strings "PRIVATE LABEL" and "NATIONAL" 
#' with "PRIVATE BRAND" and "BRANDED", respectively.
#' Note: This function is insensitive to case in header names, but 
#' IS CASE SENSITIVE to brand names.


clean_brands <- function(x) {
  
  col_names <- colnames(x)
  i <- str_detect(str_to_lower(col_names), "brand")
  
  x[which(x[,i] == "PRIVATE LABEL"), i] <- "PRIVATE BRAND"
  x[which(x[,i] == "NATIONAL"), i] <- "BRANDED"
  
  return(x)
}
