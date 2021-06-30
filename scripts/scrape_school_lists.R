# Scrape school list


## URLS

library(tidyverse)
library(rvest)
library(V8)
library(jsonlite)
library(snakecase)


## URLS

#school_list_url_left <- "https://www.greatschools.org/california/santa-barbara/schools/?page="
#school_list_url_right <- "&view=table"
#school_list_js_xpath <- "/html/head/script[1]"





## List scraping function

fn_scrape_school_page <- function(my_left_url, my_page_num, my_right_url){
  
  url <- paste(my_left_url, my_page_num, my_right_url, sep = "")
  
  read_html(url) %>%
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
  
  my_df <- data.frame(l_school_reviews$search$schools, pg_num = my_page_num)
  
  
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

df_schools <- 


df_schools <- data.frame()

for (i in 1:num_pages_to_scrape){
  
  print(paste("Scraping page", i))
  
  df_schools <- bind_rows(df_schools, fn_scrape_school_page(school_list_url_left, i, school_list_url_right))
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

#names(df_schools) <- gsub(names(df_schools), "-", "_")
#names(df_schools) <- gsub(names(df_schools), " ", "_")
  # select(
  #   id,
  #   district_name = districtName,
  #   district_city = districtCity,
  #   lat,
  #   lon,
  #   school_name = name,
  #   address,
  #   street1,
  #   street2,
  #   city,
  #   zip,
  #   
  #   links_reviews = reviews,
  #   links_profile = profile,
  #   links_college = collegeSuccess,
  # 
  #   enrollment,
  #   students_per_teacher = studentsPerTeacher,
  #   
  #   rating_low_income = 'rating_Low-income',
  #   rating_all_students = 'rating_All students',
  #   rating_hispanic = 'rating_Hispanic',
  #   rating_white = 'rating_White',
  #   rating_asian = 'rating_Asian',
  #   rating_african_american = 'rating_African American',
  #   rating_amer_indian_alaska_native = 'rating_American Indian/Alaska Native',
  #   rating_filipino = 'rating_Filipino',
  #   
  #   percentage_low_income = 'percentage_Low-income',
  #   percentage_all_students = 'percentage_All students',
  #   percentage_hispanic = 'percentage_Hispanic',
  #   percentage_white = 'percentage_White',
  #   percentage_asian = 'percentage_Asian',
  #   percentage_african_american = 'percentage_African American',
  #   percentage_amer_indian_alaska_native = 'percentage_American Indian/Alaska Native',
  #   percentage_filipino = 'percentage_Filipino',
  #   
  #   rating,
  #   rating_scale = "ratingScale",
  #   
  #   school_type = schoolType,
  #   pre_k,
  #   elementary,
  #   middle,
  #   high,
  #   test_scores_rating = "Test Scores Rating",
  #   academic_progress_rating = "Academic Progress Rating",
  #   equity_overview_rating = "Equity Overview Rating",
  #   college_readiness_rating = "College Readiness Rating",
  #   
  #   num_reviews = numReviews,
  # 
  #   pg_num
    
  #   ) -> df_schools

