### Scrape school reviews


library(tidyverse)
library(rvest)
library(V8)
library(jsonlite)


### URLs

#school_url_left <- 'https://www.greatschools.org/'

#v_school_url_right <- df_school_list$links$reviews

#v_school_url_right <- gsub("#Reviews", "reviews/", v_school_url_right)

#v_school_urls <- paste0(school_url_left, v_school_url_right)

school_url <- 'https://www.greatschools.org/california/santa-barbara/11279-Crane-Country-Day-School/reviews/'

#school_js_xpath <- "/html/body/div[5]/section/script"


### Scrape reviews

read_html(school_url) %>%
  html_element(xpath = school_js_xpath) %>%
  #html_text(trim = TRUE) %>%
  #str_replace(fixed("window.gon"), "gon")  %>%
  fromJSON(simplifyVector = FALSE, flatten = TRUE) -> l_school_reviews


### Function to extract review elements

##### Overall elements

fn_extract_overall_review_element <- function(my_element_name){
  
  all_elements <- sapply(l_school_reviews$reviews, 
                         function(l) l[[2]])[paste0(my_element_name),]
  my_element <- unlist(all_elements, use.names = FALSE)
  
  return(my_element)
}



### Extract overall review elements

fn_extract_overall_review_element("topic_label")

date_published <- as.Date(fn_extract_overall_review_element("date_published"), "%B %d, %Y")

school_id <- fn_extract_overall_review_element("school_id")
print(school_id)

overall_exp_rating <- as.numeric(
  fn_extract_overall_review_element("answer_value"))

overall_exp_comments <- fn_extract_overall_review_element("comment")



### Function to extract topic review elements

fn_extract_topic_review_element <- 
  function(my_topic_index, my_element_name){
    
    all_elements <- sapply(l_school_reviews$reviews, function(l) l[[3]])
    my_element <- sapply(all_elements[my_topic_index,], 
                         function(l) l[paste0(my_element_name)], USE.NAMES = FALSE)
    
    my_element[sapply(my_element, is.null)] <- NA
    
    my_element <- unlist(my_element, use.names = FALSE)
    
    return(my_element)
  }


### Define topic labels for extraction

v_topics <- c("hw", "teacher", "character", "leaders", "learning_diffs", "bullying")

v_topic_suffices <- c("answer", "answer_value", "comment")

expand.grid(seq_along(v_topics), v_topic_suffices, stringsAsFactors = FALSE) %>%
  mutate(labels = paste0(v_topics[Var1], "_", Var2)) -> df_combinations

names(df_combinations) <- c("topics", "suffices", "labels")

### Extract topic elements

df_school_reviews <- as.data.frame(
  mapply(fn_extract_topic_review_element,
         df_combinations$topics, df_combinations$suffices))

names(df_school_reviews) <- df_combinations$labels


### Clean topic reviews

df_school_reviews %>%
  mutate_at(vars(contains("_value")), as.numeric) %>%
  mutate(school_id = fn_extract_topic_review_element(1, "school_id")[1],
         overall_exp_value = overall_exp_rating,
         overall_exp_comments = overall_exp_comments) -> df_school_reviews
