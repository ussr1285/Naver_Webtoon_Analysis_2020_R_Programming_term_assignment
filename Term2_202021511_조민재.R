library(httr)
library(rvest)
library(RSelenium)
library(seleniumPipes)
library(tidyverse)
library(wdman)

clean_text <- function(arg_text){
  arg_text <- gsub("\n", "", arg_text)
  arg_text <- gsub("\t", "", arg_text)
  arg_text <- trimws(arg_text)
  return(arg_text)
}

cDrv <- chrome()
eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))

remD <- remoteDriver(port = 4445L, browserName = "chrome", extraCapabilities = eCaps) # 포트번호 입력, 사용할 브라우저
remD$open() # 서버에 연결

# 댓글 담을 df
comment_df <- tibble(
  id <- c(),
  episode <- c(),
  amount_comments <- c(),
  episode_comments <- c(),
  episode_user <- c(),
  comments_good <- c()
)
colnames(comment_df) <- c("id", "episode", "amount_comments", "episode_comments", "episode_user", "comments_good")

# 평점 및 좋아요 참여자 담을 df
evaluate_df <- tibble(
  id <- c(),
  episode <- c(),
  starpoint <- c(),
  participant <- c(),
  heart <- c()
)
colnames(evaluate_df) <- c("id",  "episode",  "starpoint",  "participant",  "heart")

# 웹툰 정보 담을 df
info_df <- tibble(
  id <- c(),
  title <- c(),
  cartoonist <- c(),
  genre <- c(),
  last_episode <- c()
)
colnames(info_df) <- c("id", "title", "cartoonist", "genre", "last_episode")

# 변수
main_url <- "https://comic.naver.com/webtoon/weekday.nhn"
list_url <- "https://comic.naver.com/webtoon/list.nhn"
webtoon_url <- "https://comic.naver.com/webtoon/detail.nhn"
comment_url <- "https://comic.naver.com/comment/comment.nhn"

###### 크롤링 시작 ######
title_source <- GET(main_url) %>%
  read_html()

title_ids <- title_source %>%
  html_nodes(".title") %>%
  html_attrs()

#excluded_id <- c("15", "35", "47", "48", "101", "121") # 
#title_ids

## 웹툰 정보 크롤링
for(i_id in 1:length(title_ids)){ # 
#  if(i_id %in% excluded_id){
#    next
#  }
  title_id <- title_ids[[i_id]]
  title_id <- substr(title_id, 27, gregexpr("&weekday", title_id)[[1]][1] - 1) # /webtoon/list.nhn?titleId= 여기까지의 길이가 26이기에 여기에 1을 더한 값 부터, 그리고 &weekday가 포함된 인덱스까지에서 -1 까지의 문자열을 긁어오면 titleId 와 같은 값이 됨. 
  title_id <- title_id[[1]]
  
  print(paste0("id = ", i_id))
  
  # 웹툰 리스트 화면
  list_source <- GET(list_url,
                     query=list(titleId=title_id)) %>%
    read_html()
  
  # 작가 파악
  cartoonist <- list_source %>%
    html_nodes(".wrt_nm") %>%
    html_text()
  cartoonist <- clean_text(cartoonist) # 전처리 필요
  
  # 제목 파악
  webtoon_title <- list_source %>%
    html_nodes("h2") %>%
    html_text()
  webtoon_title <- clean_text(webtoon_title[[2]])
  webtoon_title <- gsub(cartoonist, "", webtoon_title)
  
  # 장르 파악
  genre <- list_source %>%
    html_nodes(".genre") %>%
    html_text()
  
  # 마지막화 파악
  last_episode <- list_source %>%
    html_nodes(".v2+ tr a") %>% 
    html_attrs()
  
  if(!is_empty(last_episode)){
    last_episode <- last_episode[[1]]["onclick"]
    last_episode <- last_episode %>%
      substr(35, gregexpr(")",  last_episode)[[1]][[1]] -2) %>%
      as.integer()
  }else{
    last_episode <- 0 
  }
  
  # df에 넣기
  info_df <- add_row(info_df, id = title_id, title = webtoon_title, cartoonist = cartoonist, genre = genre, last_episode = last_episode)
}
remD$close()
cDrv$stop()

write.csv(info_df, file="info_df.csv")


