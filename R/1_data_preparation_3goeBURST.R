library(tidyverse)

x <- readLines("outputs/phyloviz_goeBURST_output.txt")
# annoying R dimension requirement
current_cluster <- NULL
sts <- character()
founders <- character()
out <- list()

extract_founder <- function() {
  if (length(sts) == 0) return()
  
  cc_name <- paste0("CC", min(as.integer(founders)))
  
  out[[length(out)+1]] <<- tibble(
    ST = sts,
    CC = cc_name
  )
}

for(line in x){
  
  if(stringr::str_detect(line, "^CC\\s+\\d+")){
    extract_founder()
    
    current_cluster <- stringr::str_extract(line,"\\d+")
    sts <- character()
    founders <- character()
    next
  }
  
  if(stringr::str_detect(line, "^ST\\s*\\d+")){
    st <- stringr::str_extract(line, "(?<=ST )\\d+")
    # cc <- c(cc,rdimension_req)
    sts <- c(sts,st)
    
    if(str_detect(line,"\\*$")){
      founders <- c(founders,st)
    }
  }
}

extract_founder()

goeresult <- dplyr::bind_rows(out) %>% 
  glimpse()

write.csv(goeresult, "outputs/phyloviz_goeBURST_output_table.csv",
          row.names = FALSE)
