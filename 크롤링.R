install.packages('rvest')
library(httr)
library(rvest)
library(stringr)
library(dplyr)

#  게시판 URL
list.url = 'https://kin.naver.com/userinfo/expert/answerList.nhn?u=N2f%2FrU51fGsz6CZHSa%2BHYp2zU0uiS9GocqlYhwm638s%3D'

# 제목과 본문을 저장할 벡터
titles = c()
contents = c()
dates = c()

# 게시판 1~2페이지 크롤링
for(i in 56:60){
  url = modify_url(list.url, query=list(page=i))  # 게시판 URL에서 페이지를 변경
  h.list = read_html(url)  # 게시물 목록을 가져옴
  
  # 게시물 링크 추출
  title.links = h.list%>% html_nodes('#au_board_list')%>% html_nodes('.title') %>%  html_nodes('a')
  article.links = html_attr(title.links, 'href')  
  article.links <- paste('https://kin.naver.com', article.links, sep='')
  
  for(link in article.links){
    h = read_html(link)  # 게시물을 가져옴
    
    # 제목
    title = html_text(html_nodes(h, '.end_question._end_wrap_box h3'))
    
    title = str_trim(repair_encoding(title))
    
    titles = c(titles, title)
    
    
    date1 <-  h %>% html_nodes('.end_tit') %>% html_nodes('.date') %>% html_text() 
    
    dates <- c(dates, date1[4])
    
    
    # 내용
    content = html_nodes(h, '.end_question .end_content._endContents')
    
    ## 모바일 질문 내용 
    no.content = html_text(html_nodes(content, '.end_ext2'))
    
    content = repair_encoding(html_text(content))
    
    ## 모바일 질문 내용 삭제
    ## 예) http://kin.naver.com/qna/detail.nhn?d1id=8&dirId=8&docId=235904020&qb=7Jes65Oc66aE&enc=utf8&section=kin&rank=19&search_sort=0&spq=1
    if (length(no.content) > 0)
    {
      content = str_replace(content, repair_encoding(no.content), '')
    }
    
    content <- str_trim(content)
    
    contents = c(contents, content)
    
    print(link)
    
  }
}

dates
length(dates)
length(contents)
length(titles)
contents <- contents[-1]
# 결과 저장

h_result1 <- data.frame(titles, contents, dates)
nrow(h_result1)
h_result2 <- data.frame(titles, contents, dates)
nrow(h_result2)
h_result3 <- data.frame(titles, contents, dates)
nrow(h_result3)
h_result4 <- data.frame(titles, contents, dates)
nrow(h_result4)


h_result <- rbind(h_result1,h_result2,h_result3,h_result4)


head(h_result$titles)
str(h_result)
summary(h_result)
str(unique(h_result))
summary(unique(h_result$contents))

################## 전은복 영양사님 ########


#  게시판 URL
list.url = 'https://kin.naver.com/userinfo/expert/answerList.nhn?u=frY5rG3TYLbj%2B2NiunpLEZC5%2BxPfRZTUre6ihlrNhAA%3D'

# 제목과 본문을 저장할 벡터
titles = c()
contents = c()
dates <- c()
# 게시판 1~2페이지 크롤링
for(i in 61:65){
  url = modify_url(list.url, query=list(page=i))  # 게시판 URL에서 페이지를 변경
  h.list = read_html(url)  # 게시물 목록을 가져옴
  
  # 게시물 링크 추출
  title.links = h.list%>% html_nodes('#au_board_list')%>% html_nodes('.title') %>%  html_nodes('a')
  article.links = html_attr(title.links, 'href')  
  article.links <- paste('https://kin.naver.com', article.links, sep='')
  
  for(link in article.links){
    h = read_html(link)  # 게시물을 가져옴
    
    # 제목
    title = html_text(html_nodes(h, '.end_question._end_wrap_box h3'))
    
    title = str_trim(repair_encoding(title))
    
    titles = c(titles, title)
    
    
    
    date1 <-  h %>% html_nodes('.end_tit') %>% html_nodes('.date') %>% html_text() 
    
    dates <- c(dates, date1[4])
    
    # 내용
    content = html_nodes(h, '.end_question .end_content._endContents')
    
    ## 모바일 질문 내용 
    no.content = html_text(html_nodes(content, '.end_ext2'))
    
    content = repair_encoding(html_text(content))
    
    ## 모바일 질문 내용 삭제
    ## 예) http://kin.naver.com/qna/detail.nhn?d1id=8&dirId=8&docId=235904020&qb=7Jes65Oc66aE&enc=utf8&section=kin&rank=19&search_sort=0&spq=1
    if (length(no.content) > 0)
    {
      content = str_replace(content, repair_encoding(no.content), '')
    }
    
    content <- str_trim(content)
    
    contents = c(contents, content)
    
    print(link)
    
  }
}

# 결과 저장

length(dates)
length(contents)
length(titles)

j_result1 <- data.frame(titles, contents,dates)
nrow(j_result1)
j_result2 <- data.frame(titles, contents, dates)
nrow(j_result2)
j_result3 <- data.frame(titles, contents, dates)
nrow(j_result3)
j_result4 <- data.frame(titles, contents, dates)
nrow(j_result4)
j_result5 <- data.frame(titles, contents, dates)
nrow(j_result5)



j_result <- rbind(j_result1,j_result2,j_result3,j_result4,j_result5)
nrow(j_result)
nrow(h_result)

result <- rbind(j_result,h_result)
write.csv(result, file = 'result.csv',row.names = F)
write.csv(j_result, file = 'j_result.csv',row.names = F)
write.csv(h_result, file = 'h_result.csv',row.names = F)


str(result)
result$titles <- as.character(result$titles)
result$contents <- as.character(result$contents)
result$dates <- as.character(result$dates)
result_nn <- data.frame()
nrow(result)
for(j in 1:100){
  if(nchar(result[j,3])==17 || result[j,3]== NA){
    result_nn <- rbind(result_nn,result[j,])
  }
}

if(nchar(result[10,3])==17) print(1)
