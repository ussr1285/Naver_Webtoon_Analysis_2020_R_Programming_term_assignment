
## 평가 크롤링
remD <- remoteDriver(port = 4445L, browserName = "chrome", extraCapabilities = eCaps) # 포트번호 입력, 사용할 브라우저
remD$open() # 서버에 연결

title_ids <- info_df$id
for(i_id in 15:length(title_ids)){ # 
  # 마지막화
  print(paste0("id = ", i_id))
  title_id <- title_ids[[i_id]]
  last_episode <- info_df$last_episode[i_id]
  
  # 첫화부터 마지막 화까지 for문으로 돌음.
  for(i in 1:last_episode){
    print(paste0("episode : ", i))
    # 웹툰 화면
    
    remD$navigate(paste0(webtoon_url, "?titleId=", title_id,"&no=",i))
    webtoon_source <- remD$getPageSource()[[1]] %>%
      read_html()
    
    # 별점
    webtoon_star <- webtoon_source %>%
      html_nodes("#topPointTotalNumber strong") %>%
      html_text()
    
    # 별점 참여자
    webtoon_participate <- webtoon_source %>%
      html_nodes("#topTotalStarPoint .pointTotalPerson em") %>%
      html_text()
    
    # 하트 수
    webtoon_heart <- webtoon_source %>%
      html_nodes(".u_cnt") %>%
      html_text()
    
    evaluate_df <- add_row(evaluate_df, id = title_id, episode = i, starpoint = webtoon_star, participant = webtoon_participate, heart = webtoon_heart)
  }
}

remD$close()
cDrv$stop()

write.csv(evaluate_df, file="evaluate_df.csv")



### 댓글 크롤링
cDrv <- chrome()
eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))

remD <- remoteDriver(port = 4445L, browserName = "chrome", extraCapabilities = eCaps) # 포트번호 입력, 사용할 브라우저
remD$open() # 서버에 연결


for(i_id in 1:length(title_ids)){
  print(paste0("id = ", i_id)) 
  title_id <- title_ids[[i_id]]["href"]
  title_id <- substr(title_id, 27, gregexpr("&weekday", title_id)[[1]][1] - 1)
  
  # 웹툰 리스트 화면
  list_source <- GET(list_url,
                     query=list(titleId=title_id)) %>%
    read_html()
  
  # 마지막화 파악
  last_episode <- list_source %>%
    html_nodes(".v2+ tr a") %>%
    html_attrs()
  last_episode <- last_episode[[1]]["onclick"]
  last_episode <- last_episode %>%
    substr(35, gregexpr(")",  last_episode)[[1]][[1]] -2) %>%
    as.integer()
  
  for(i in (last_episode-1):last_episode){
    print(paste0("episode = ", i)) 
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
    
    print(temp_episode_user)
    
    temp_comments_good <- episode_comment_source %>%
      html_nodes(".u_cbox_cnt_recomm") %>%
      html_text() 
    
    comment_df <- add_row(comment_df, id = title_id, episode = i, amount_comments = temp_amount_comments, episode_comments = temp_episode_comments, episode_user = temp_episode_user, comments_good = temp_comments_good)
  }
}

write.csv(comment_df, file="comment_df.csv")


