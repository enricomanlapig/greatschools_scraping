# Scrape school list


## URLS

library(tidyverse)
library(rvest)
library(V8)
library(jsonlite)
library(snakecase)
library(polite)

## URLS

#school_list_url_left <- "https://www.greatschools.org/california/santa-barbara/schools/?page="
#school_list_url_right <- "&view=table"
#school_list_js_xpath <- "/html/head/script[1]"





## List scraping function

fn_scrape_school_page <- function(my_left_url, my_page_num, my_right_url){
  
  url <- paste(my_left_url, my_page_num, my_right_url, sep = "")
  
  current_page <- nod(session, url) %>% scrape(verbose = FALSE)

  current_page %>%
#  read_html(url) %>%
    html_element(xpath = school_list_js_xpath) %>%
    html_text(trim = TRUE) %>%
    str_replace(fixed("window.gon"), "gon") -> txt
  
  tmp <- tempfile()
  
  write_file(
    txt,
    file = tmp
  )
  
  txt2 <- read_lines(
    tmp
  )
  
  
  ctx <- v8()
  
  ctx$eval(txt2[2])
  l_school_reviews <- ctx$get("gon")

  total_pages <- l_school_reviews$search$totalPages

  my_df <- data.frame(l_school_reviews$search$schools, 
                      pg_num = my_page_num, 
                      tot_pages = total_pages)
  
  df_ethnicity_info <- data.frame()
  
  for (i in 1:length(my_df$id)){
    
    my_df$ethnicityInfo[[i]] %>% 
      pivot_wider(names_from = label,
                  values_from = setdiff(names(my_df$ethnicityInfo[[i]]), "label"),
                  names_glue = "{.value}_{label}",
                  names_sep = "_") %>%
      mutate(id = my_df$id[i]) %>% 
      bind_rows(df_ethnicity_info) -> df_ethnicity_info
  }
  
  my_df <- left_join(my_df, df_ethnicity_info, by = c("id"))
  
  return(my_df)
}



## Scrape school list




df_schools <- data.frame()

page_to_scrape <- 1
total_pages <- 2

while (page_to_scrape <= total_pages){
  
  print(paste("Scraping page", page_to_scrape))
  
  df_schools <- bind_rows(df_schools, 
                          fn_scrape_school_page(school_list_url_left, 
                                                page_to_scrape, 
                                                school_list_url_right))
  
  page_to_scrape <- page_to_scrape + 1
  total_pages <- max(df_schools$tot_pages)
  
  Sys.sleep(5)
}



## Clean school list

df_schools %>% 
  mutate(pre_k = ifelse(grepl("p", levelCode),1,0),
         elementary = ifelse(grepl("e", levelCode),1,0),
         middle = ifelse(grepl("m", levelCode),1,0),
         high = ifelse(grepl("h", levelCode),1,0)) %>%
         
  cbind(df_schools$collegeEnrollmentData,
        df_schools$subratings,
        df_schools$remediationData,
        df_schools$links,
        df_schools$address) -> df_schools

names(df_schools) <- to_any_case(names(df_schools), case = "snake")
