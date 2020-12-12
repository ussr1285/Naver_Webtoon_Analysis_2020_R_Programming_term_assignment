library(httr)
library(xml2)
library(rvest)
install.packages("rvest")

title_id <- "703852" # 웹툰 개별 id, 벡터로 여러가지 담아야 함.

# title_id에 따른 반복문
n_webtoon_url <- "https://comic.naver.com/webtoon/list.nhn"
n_webtoon_source <- GET(n_webtoon_url,
                        query=list(titleId=title_id)) %>%
  read_html(content(res, as="text"))

last_episode <- n_webtoon_source %>%
  html_nodes(".v2+ tr a") %>%
  html_text()
last_episode <- last_episode[[2]]

n_comment_url <- paste0("https://comic.naver.com/comment/comment.nhn") # ?titleId=", title_id 

#for(i in 1:last_episode){
  n_episode_comment_source <- GET(n_comment_url, 
                                query=list(title_id=title_id, no=1)) %>% # i
    read_html()
  
  View(n_episode_comment_source)
  
  amount_comments <- n_episode_comment_source %>%
  html_nodes(".u_cbox_title , .u_cbox_count") %>%
  html_text()

  episode_comments <- n_comment_source %>%
    html_nodes("._user_id_no_3to9K .u_cbox_contents") %>%
    html_text()
#}


youtube_title <- gsub("\n", "", youtube_title)
youtube_title <- trimws(youtube_title)
youtube_title[1:10] 
