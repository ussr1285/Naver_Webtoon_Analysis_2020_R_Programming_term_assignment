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

# 연재중인 모든 웹툰을 돌음.
for(i_id in 1:length(title_ids)){
  print(paste0("id = ", i_id))
  title_id <- title_ids[[i_id]]["href"]
  title_id <- substr(title_id, 27, gregexpr("&weekday", title_id)[[1]][1] - 1) # /webtoon/list.nhn?titleId= 여기까지의 길이가 26이기에 여기에 1을 더한 값 부터, 그리고 &weekday가 포함된 인덱스까지에서 -1 까지의 문자열을 긁어오면 titleId 와 같은 값이 됨. 
  
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
  last_episode <- last_episode[[1]]["onclick"]
  last_episode <- last_episode %>%
    substr(35, gregexpr(")",  last_episode)[[1]][[1]] -2) %>%
    as.integer()
last_episode

  # df에 넣기
  info_df <- add_row(info_df, id = title_id, title = webtoon_title, cartoonist = cartoonist, genre = genre, last_episode = last_episode)
  
  # 첫화부터 마지막 화까지 for문으로 돌음.
  last_episode
  for(i in (last_episode-2):last_episode){
    ## 평가 크롤링
    print(paste0("episode : ", i))
    # 웹툰 화면
    remD$navigate(paste0(webtoon_url, "?titleId=", title_id,"&no=",i))
    webtoon_source <- remD$getPageSource()[[1]] %>%
      read_html()
    
    # 별점
    webtoon_star <- webtoon_source %>%
      html_nodes("#bottomPointTotalNumber strong") %>%
      html_text()
    
    # 별점 참여자
    webtoon_participate <- webtoon_source %>%
      html_nodes("#bottomTotalStarPoint .pointTotalPerson em") %>%
      html_text()
    
    # 하트 수
    webtoon_heart <- webtoon_source %>%
      html_nodes(".u_cnt") %>%
      html_text()
    
    evaluate_df <- add_row(evaluate_df, id = title_id, episode = i, starpoint = webtoon_star, participant = webtoon_participate, heart = webtoon_heart)
    
    
    ## 댓글 크롤링
    remD$navigate(paste0(comment_url, "?titleId=", title_id, "&no=",i))
    episode_comment_source <- remD$getPageSource()[[1]] %>%
      read_html()
    
    temp_amount_comments <- episode_comment_source %>% 
      html_nodes(".u_cbox_count") %>%
      html_text()
    
    temp_episode_comments <- episode_comment_source %>%
      html_nodes(".u_cbox_contents") %>%
      html_text()
    
    temp_episode_user <- episode_comment_source %>%
      html_nodes(".u_cbox_nick") %>%
      html_text() 
    
    temp_comments_good <- episode_comment_source %>%
      html_nodes(".u_cbox_cnt_recomm") %>%
      html_text() 
    
    comment_df <- add_row(comment_df, id = title_id, episode = i, amount_comments = temp_amount_comments, episode_comments = temp_episode_comments, episode_user = temp_episode_user, comments_good = temp_comments_good)
    
  }
}

remD$close()
cDrv$stop()

write.csv(info_df, file="info_df.csv")
write.csv(evaluate_df, file="evaluate_df.csv")
write.csv(comment_df, file="comment_df.csv")



