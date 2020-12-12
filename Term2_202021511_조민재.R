library(httr)
library(xml2)
library(rvest)

title_id <- "703852" # 웹툰 개별 id, 벡터로 여러가지 담아야 함.

# title_id에 따른 반복문
n_webtoon_url <- paste0("https://comic.naver.com/webtoon/list.nhn?titleId=", title_id)

n_webtoon_source <- read_html(n_webtoon_url) 
last_episode <- n_webtoon_source %>%
  html_nodes(".v2+ tr a") %>%
  html_text()
last_episode <- last_episode[[2]]

n_comment_url <- paste0("https://comic.naver.com/comment/comment.nhn?titleId=", title_id) 

#for(i in 1:last_episode){
  n_episode_comment_url <- paste0(n_comment_url, "&no=1") # ,i
  n_comment_source <- read_html(n_episode_comment_url) 

  amount_comments <- n_comment_source %>%
    html_nodes(xpath = '//*[@id="cbox_module"]/div/div[1]/span') %>%
    html_text()
  
  episode_comments <- n_comment_source %>%
    html_nodes("._user_id_no_3to9K .u_cbox_contents") %>%
    html_text()
#}


youtube_title <- gsub("\n", "", youtube_title)
youtube_title <- trimws(youtube_title)
youtube_title[1:10] 
