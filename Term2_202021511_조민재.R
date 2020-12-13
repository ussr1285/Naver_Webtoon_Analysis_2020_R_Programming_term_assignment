# 시작 전 라이브러리 불러오기 및 상수 정의
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

# 변수
main_url <- "https://comic.naver.com/webtoon/weekday.nhn"
list_url <- "https://comic.naver.com/webtoon/list.nhn"
webtoon_url <- "https://comic.naver.com/webtoon/detail.nhn"
comment_url <- "https://comic.naver.com/comment/comment.nhn"
week_url <- "https://comic.naver.com/webtoon/weekdayList.nhn"
week_set <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun") # 요일


# 네이버 웹툰 스크래핑
## 웹툰 기본 정보 가져오기
# 웹툰 정보 담을 df
info_df <- tibble(
  id <- c(),
  title <- c(),
  cartoonist <- c(),
  genre <- c(),
  last_episode <- c(),
  week_rank <- c(),
  week <- c(),
  star_point <- c()
)
colnames(info_df) <- c("id", "title", "cartoonist", "genre", "last_episode", "week_rank", "week", "star_point")

title_source <- GET(main_url) %>%
  read_html()

# 웹툰 메인사이트
title_ids <- title_source %>%
  html_nodes(".title") %>%
  html_attrs()

cnt_week <- 0 # 요일 구분

## 웹툰 정보 크롤링
for(i_id in 1:length(title_ids)){ # 
  # 웹툰별 식별 id 가져오기
  title_id <- title_ids[[i_id]][1]
  title_id <- substr(title_id, 27, gregexpr("&weekday", title_id)[[1]] - 1) # /webtoon/list.nhn?titleId= 여기까지의 길이가 26이기에 여기에 1을 더한 값 부터, 그리고 &weekday가 포함된 인덱스까지에서 -1 까지의 문자열을 긁어오면 titleId 와 같은 값이 됨. 
  title_id <- title_id[[1]]
  
  # 웹툰별 인기 순위 파악
  title_rank <- title_ids[[i_id]][2]
  title_rank <- substr(title_rank, 31, gregexpr("')", title_rank)[[1]] - 1)
  title_rank <- title_rank[[1]]
  
  # 요일
  #print(paste0("id = ", i_id)) # 에러 테스트용
  if(title_rank == "1"){
    cnt_week <- cnt_week + 1
  }
  week <- week_set[[cnt_week]]
  
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
  
  # 총 별점 파악
  week_source <- GET(week_url, query=list(week=week)) %>%
    read_html()
  
  star_points <- week_source %>%
    html_nodes(".rating_type strong") %>%
    html_text
  star_point <- star_points[[as.integer(title_rank)]]
  
  
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
  info_df <- add_row(info_df, id = title_id, title = webtoon_title, cartoonist = cartoonist, genre = genre, last_episode = last_episode, week_rank = title_rank, week = week, star_point = star_point)
}
remD$close()
cDrv$stop()

write.csv(info_df, file="info_df.csv")

## 매 화당 평가 가져오기
#문서로 제작하는데 시간이 너무 오래 걸리는 관계로 주석으로 처리한 점 양해부탁드립니다.
#스크래핑 결과물은 evaluate_df.csv 파일에 저장됩니다.

# 평점 및 좋아요 참여자 담을 df
evaluate_df <- tibble(
  id <- c(),
  episode <- c(),
  starpoint <- c(),
  participant <- c(),
  heart <- c()
)
colnames(evaluate_df) <- c("id",  "episode",  "starpoint",  "participant",  "heart")

remD <- remoteDriver(port = 4445L, browserName = "chrome", extraCapabilities = eCaps) # 포트번호 입력, 사용할 브라우저
remD$open() # 서버에 연결

title_ids <- info_df$id
for(i_id in 1:length(title_ids)){ # 
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


# 분석
## 데이터 수집을 위한 준비.

#install.packages("ggplot2")
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
theme_set(theme_minimal())



info_df <- read.csv("info_df.csv") # 웹툰 정보를 담은 데이터프레임 불러옴.
str(info_df)

info_df$week_rank <- as.integer(info_df$week_rank) # 주 순위 정수형으로 변경
info_df$week <- as.factor(info_df$week) # 연재 요일 팩터형으로 변경


## 장르 비율 분석

### 서사 방식(스토리, 에피소드, 옴니버스) 포함

all_genre <- table(info_df$genre)
label <- paste(names(all_genre), "\n", all_genre)

pie(all_genre, labels=label, radius=1.3)

### 서사 방식(스토리, 에피소드, 옴니버스) 비율

split_genre_narrative <-  info_df$genre %>%
  strsplit(split=",")

narrative_method <- c()
for(i in 1:length(split_genre_narrative)){
  narrative_method <- append(narrative_method, split_genre_narrative[[i]][1])
}
narrative_method <- table(narrative_method)

label <- paste(names(narrative_method), "\n", narrative_method)
pie(narrative_method, labels=label, radius=0.8)


### 서사 방식(스토리, 에피소드, 옴니버스) 미포함 장르 비율

genre <- c()
for(i in 1:length(split_genre_narrative)){
  genre <- append(genre, split_genre_narrative[[i]][2])
}
genre <- table(genre)

label <- paste(names(genre), "\n", genre)
pie(genre, labels=label, radius=1.2)

### 인기 많은 웹툰 서사 방식
#스토리 웹툰이 옴니버스나 에피소드 웹툰보다 인기가 많다. 

hot_toon <- info_df %>%
  subset(week_rank <= 6) # 각 주별 순위 6위 이내만 추출

split_genre_narrative <-  hot_toon$genre %>%
  strsplit(split=",")

narrative_method <- c()
for(i in 1:length(split_genre_narrative)){
  narrative_method <- append(narrative_method, split_genre_narrative[[i]][1])
}
narrative_method <- table(narrative_method)

label <- paste(names(narrative_method), "\n", narrative_method)
pie(narrative_method, labels=label, radius=1.0)


### 인기 많은 웹툰 장르(서사 방식 미포함)
#액션 장르가 전체 비율에 비해 인기가 많다. 감성 장르, 시대극 장르는 인기 6위 항목에 포함되지 못한다.

genre <- c()
for(i in 1:length(split_genre_narrative)){
  genre <- append(genre, split_genre_narrative[[i]][2])
}
genre <- table(genre)

label <- paste(names(genre), "\n", genre)
pie(genre, labels=label, radius=1.0)


## 웹툰 시작에 따른 에피소드 별점 및 별점 참여 수 변화 추이
#모든 웹툰을 스크래핑하는데 너무 많은 시간이 걸리기 때문에 월요일, 화요일 연재 웹툰만 사용하여 분석함.
### 웹툰 연재 시기에 따른 에피소드 별점 변화 추이
#그래프 분석 결과 대다수의 웹툰들이 높은 별점을 유지함. 장기 웹툰 중 잠시 별점이 내려가는 웹툰들도 있으나 다시 높은 점수로 회복함. 이러한 웹툰들은 대체로 초반에 높은 별점을 받으며 시작함. 그래서 평균 높은 별점을 가짐.
#초반에 높은 별점을 받지 못한 웹툰들은 계속 별점이 떨어지는 경우가 많음. 그 결과 전체 별점이 낮아짐.

evaluate_df <- read.csv("evaluate_df.csv")
str(evaluate_df)

evaluate_df$heart <- gsub(",", "", evaluate_df$heart) %>%
  as.integer()

temp_info_df <- info_df %>%
  select(id, week, title, )

evaluate_df <- left_join(evaluate_df, temp_info_df, by="id")

mon_df <- evaluate_df %>%
  subset(week == "mon") %>%
  group_by(id)
mon_df$id <- as.factor(mon_df$id)

tue_df <- evaluate_df %>%
  subset(week == "tue") %>%
  group_by(id)

#length(table(mon_df$id))

# 월요 웹툰
mon_g <- ggplot(mon_df, aes(x=episode, y=starpoint, group=id, colour = title), show.legend = FALSE)
mon_g <- mon_g + geom_line()
#mon_g <- mon_g + facet_wrap(~ id)
mon_g

# 화요 웹툰
tue_g <- ggplot(tue_df, aes(x=episode, y=starpoint, group=id, colour = title), show.legend = FALSE)
tue_g <- tue_g +geom_line()
#tue_g <- tue_g + facet_wrap(~ id)
tue_g

mon_nojam <- info_df %>%
  subset(week == "mon") %>%
  subset(star_point < 9.0)

mon_nojam$title

tue_nojam <-  info_df %>%
  subset(week == "tue") %>%
  subset(star_point < 9.0)
tue_nojam



### 웹툰 연재 시기에 따른 에피소드 별점 리뷰 참여 수 변화 추이
#아래의 그래프를 보면 뒤 에피소드로 갈 수록 별점을 평가하는 사람이 적어진다.
#조회수 데이터를 스크래핑 해오지 못해 정확히 알 수는 없으나, 2가지 경우로 추측된다.
#첫째, 사람들이 최신 화까지 챙겨보지 않는 경우로 볼 수 있다.
#둘째, 웹툰의 특성상 몰입하여 보다가 별점 평가하는 것을 까먹는 사람들의 경향으로 볼 수 있다.

# 월요 웹툰
mon_g <- ggplot(mon_df, aes(x=episode, y=participant, group=id, colour = title), show.legend = FALSE)
mon_g <- mon_g + geom_line()
#mon_g <- mon_g + facet_wrap(~ id)
mon_g

# 화요 웹툰
tue_g <- ggplot(tue_df, aes(x=episode, y=participant, group=id, colour = title), show.legend = FALSE)
tue_g <- tue_g +geom_line()
#tue_g <- tue_g + facet_wrap(~ id)
tue_g




## 별점이 낮은 만화는 첫화부터 댓글 반응이 안 좋은가? 그리고 그 댓글은 무슨 내용인가?
### 월요일 웹툰 중 별점 낮은 웹툰 1화의 댓글 스크래핑

cold_toon <- mon_nojam

### 댓글 스크래핑
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


cDrv <- chrome()
eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))

remD <- remoteDriver(port = 4445L, browserName = "chrome", extraCapabilities = eCaps) # 포트번호 입력, 사용할 브라우저
remD$open() # 서버에 연결


cold_toon_ids <- rownames(table(cold_toon$id))

for(i_id in 1:length(cold_toon_ids)){
  #print(paste0("id = ", i_id)) 
  print(cold_toon_ids[[i_id]])
  title_id <- cold_toon_ids[[i_id]]
  # 웹툰 리스트 화면
  list_source <- GET(list_url,
                     query=list(titleId=title_id)) %>%
    read_html()
  
  remD$navigate(paste0(comment_url, "?titleId=", title_id, "&no=",1))
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
  #
}
### 댓글 스크래핑 끝
write.csv(comment_df, file="comment_df.csv")

### 첫 화 댓글 반응 분석
#사전에 등재되지 않은 인터넷 용어가 많아 정확한 분석이 어려운 것 같다.
#그것 때문에 임의로 사전에 단어들을 추가했다.
#별점이 낮은 작품의 첫화에 대한 반응은 생각보다 비난이 그렇게 많지는 않았다.
#하지만 시간관계상 데이터셋이 작은 것이므로 첫화 댓글 반응을 긍정 혹은 부정으로 단정짓기는 현 상황에서 어려울 것 같다.

#install.packages("rJava")
#install.packages("memoise")
#install.packages("multiinguer")
#install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
#install.packages("KoNLP")
#install.packages("remotes")
#remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"), force = TRUE)
#install.packages("wordcloud2")

library("KoNLP")
library(wordcloud2)

useNIADic()

text_ <- extractNoun(comment_df$episode_comments)

extract_text <- c()

for(i in 1:length(text_)){
  extract_text <- append(extract_text, text_[[i]])
}

modif_text <- sort(table(extract_text), decreasing = TRUE)

wordcloud2(modif_text)
