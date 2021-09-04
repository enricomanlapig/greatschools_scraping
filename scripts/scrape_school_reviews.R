# Scraping review pages


## Libraries

#library(RSelenium)
#library(rvest)
#library(stringr)




## Setting up page to scrape

### URL

#school_url <- "https://www.greatschools.org/california/santa-barbara/11279-Crane-Country-Day-School/reviews/"


### Navigate to page

remDr$navigate(school_url)


### Scroll down to show all reviews 
### expand reviews by clicking on "more" links

while (length(remDr$findElements("link text", "More")) != 0) {
  print(paste("Found", length(remDr$findElements("link text", "More")), "link(s) to click. Clicking and scrolling!"))
  remDr$findElements("link text", "More")[[1]]$clickElement()
  remDr$executeScript(paste("scroll(0,", 1080, ");"))
  Sys.sleep(5)
}







## Read page source

webpage <- read_html(remDr$getPageSource()[[1]])



## Start extracting elements


### Extract global elements

webpage %>%
  html_elements(".community-breadcrumbs a") %>%
  html_text() -> school_name


webpage %>%
  html_elements("div.review-item-container") -> review_container

review_container %>%
  html_element("div.user-type") %>%
  html_text() -> user_type


review_container %>%
  html_element("div.comment") %>%
  html_text() -> overall_comments

review_container %>%
  html_element("div.answer") %>%
  as.character() %>%
  str_count("filled-star") -> overall_rating

review_container %>%
  html_element("span.type-and-date") %>%
  html_text() %>%
  as.Date("%B %d, %Y") -> review_date





### Extract topic reviews

review_container %>%
  html_elements("div.topic") %>%
  length() -> num_topic_reviews

review_container %>%
  html_elements("div.topic") %>%
  html_text() -> topic_labels

review_container %>%
  html_elements("div.topical-review-detail") %>%
  html_element("div.comment") %>%
  html_text() -> topic_comments



#### Function to extract topic comments

review_index <- rep(1:length(num_topic_reviews), times = num_topic_reviews)

fn_extract_topic_comments <- function(my_topic) {
  tmp <- rep(NA, times = length(review_date))
  tmp[review_index[topic_labels == my_topic]] <- topic_comments[which(topic_labels == my_topic)]
    return(tmp)
}

#### Extract topic comments

homework_comments <- fn_extract_topic_comments("Homework")
teacher_comments <- fn_extract_topic_comments("Teachers")
character_comments <- fn_extract_topic_comments("Character")
leadership_comments <- fn_extract_topic_comments("Leadership")
bullying_comments <- fn_extract_topic_comments("Bullying")
learning_diffs_comments <- fn_extract_topic_comments("Learning Differences")







### Topic ratings


review_container %>%
  html_elements("div.topical-item") %>%
  html_element("div.topic-details") %>%
  html_text() -> topic_rating_label

review_container %>%
  html_elements("div.topical-item") %>%
  html_element("div.topical-average") %>%
  html_element("div.numeric") %>%
  html_text() %>%
  as.numeric() -> topic_rating

review_container %>% 
  as.character() %>%
  str_count('<div class="topical-item"') -> num_topic_ratings

webpage %>%
  as.character() %>%
  str_count('<div class="topical-item"') -> num_topic_ratings








#### Function to extract topic ratings

review_index_ratings <- rep(1:length(num_topic_reviews), times = num_topic_ratings)

fn_extract_topic_rating <- function(my_topic) {
  tmp <- rep(NA, times = length(review_date))
  tmp[review_index_ratings[grep(my_topic, topic_rating_label)]] <-
      topic_rating[grep(my_topic, topic_rating_label)]
    return(tmp)
}


#### Extract topic ratings

homework_rating <- fn_extract_topic_rating("Homework")
teacher_rating <- fn_extract_topic_rating("Teachers")
character_rating <- fn_extract_topic_rating("Character")
leadership_rating <- fn_extract_topic_rating("Leadership")
bullying_rating <- fn_extract_topic_rating("Bullying")
learning_diffs_rating <- fn_extract_topic_rating("Learning Differences")





## Combine everything together in data frame

df_school_review <- data.frame(
  rep(school_name, times = length(user_type)),
  user_type,
  review_date,
  overall_rating,
  overall_comments,
  
  homework_rating,
  teacher_rating,
  character_rating,
  leadership_rating,
  bullying_rating,
  learning_diffs_rating,
  
  homework_comments,
  teacher_comments,
  character_comments,
  leadership_comments,
  bullying_comments,
  learning_diffs_comments
)
