

### Libraries


library(tidyverse) # For dplyr and stringr
library(rvest) # For scraping finite scroll pages
library(RSelenium) # For infinite scroll pages
library(V8) # For evaluating JavaScript
library(jsonlite) # For working with JSON
library(stringr) # For str_replace
library(snakecase) # For converting strings
library(polite) # For respecting host rules



### Inputs

state_name <- "california"

#city_name <- "santa barbara" # 5 pages
city_name <- "goleta" # 2 pages
#city_name <- "summerland" # 1 page
#city_name <- "montecito" # 1 page
#city_name <- "carpinteria" # 1 page
#city_name <- "ojai" # 2 pages
#city_name <- "ventura" # 4 page
#city_name <- "santa-ynez" # 1 page
#city_name <- "solvang" # 1 pages
#city_name <- "buellton" # 1 pages




### Create URLs

city_name <- gsub(" ", "-", city_name)

great_schools_url <- "https://www.greatschools.org"

school_list_url_left <- paste0(great_schools_url,
                               "/",
                               to_snake_case(state_name, sep_out = "-"),
                               "/",
                               to_snake_case(city_name, sep_out = "-"),
                               "/schools/?page=",
                               sep = "")

school_list_url_right <- "&view=table"

school_list_js_xpath <- "/html/head/script[1]"





### Bow to website, check robot.txt and scrape list pages

session <- bow(school_list_url_left)

source("scripts/scrape_school_lists.R")





### Create school review URLs

v_school_url_right <- gsub("#Reviews", "reviews/", df_schools$reviews)

v_school_urls <- paste0(great_schools_url, v_school_url_right)

v_school_urls <- setdiff(v_school_urls[df_schools$num_reviews > 0], NA)
  # Do not scrape school pages with no reviews



### RSelenium

rD <- rsDriver(browser = "firefox")

remDr <- rD[["client"]]

remDr$setWindowSize(width = 1920 / 2, height = 1080) 
  # Ensures window is the right size for scrolling





### Scrape review pages

for (s in 1:length(v_school_urls)) {
  
  school_url <- v_school_urls[s]

  print(paste("Scraping: ", school_url))
  
  nod(session, school_url)
    # Nod to host
  
  source("scripts/scrape_school_reviews.R")
  
  if (s == 1) {
    
    df_all_school_reviews <- df_school_review
    
  } else{
    df_all_school_reviews <- bind_rows(df_all_school_reviews,
                                       df_school_review)
  }
    # Combine review dataframes
  
  
  
}

pid <- rD$server$process$get_pid()
system(paste0("Taskkill /F /T" ," /PID ", pid))

remDr$close()
rD$server$stop()
rm(rD)







data_path <- "data/"

school_list_outfile <- paste0(city_name, "_list.RData", sep = "")
save(df_schools, file = paste(data_path, school_list_outfile, sep = ""))

school_reviews_outfile <- paste(city_name, "_reviews.RData", sep = "")
save(df_all_school_reviews, file = paste(data_path, school_reviews_outfile, sep = ""))


