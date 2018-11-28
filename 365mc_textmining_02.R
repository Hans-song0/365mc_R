setwd("C:/Users/365mc/Documents/텍스트분석")
library(topicmodels)
library(NIADic)
library(ggplot2)
useNIADic()
useSejongDic( )
peos <- peo[,6]
str(peo)
str(peos)

peos <- as.data.frame(peos)


for(i in 1:6358){
write.table(peos[i,], file = paste0('peos',i,'.txt'),col.names = F,row.names = F)
}

paste0(1,'wre',2)

write.table(peos[1,], file = paste0('peos',1,'.txt'),col.names = F,row.names = F)

plans1[[1]]$content
plans<- VCorpus(DirSource("peos", pattern="txt"))
plans1 <- plans
plans <- plans1
# 여러줄 한줄로 변호
for(i in seq_along(plans)){
  plans[[i]]$content <- paste(plans[[i]]$content, collapse=" ")
}
plans[[1]]$content

#문장부호 제거 
plans <- tm_map(plans, removePunctuation)
plans[[1]]$content

#숫자 제거
plans = tm_map(plans, removeNumbers)
plans[[1]]$content

#공백문자 제거 
plans = tm_map(plans, stripWhitespace)



#단어 체인지 & 제거
for(i in seq_along(plans)){
  plans[[i]]$content <- gsub("최소 지방량", "최소지방량", plans[[i]]$content);
  plans[[i]]$content <- gsub("내장 지방", "내장지방", plans[[i]]$content);
  plans[[i]]$content <- gsub("스케줄", "스케쥴", plans[[i]]$content);
  plans[[i]]$content <- gsub("엄마", "가족", plans[[i]]$content);
  plans[[i]]$content <- gsub("아빠", "가족", plans[[i]]$content);
  plans[[i]]$content <- gsub("어머니", "가족", plans[[i]]$content);
  plans[[i]]$content <- gsub("아버지", "가족", plans[[i]]$content);
  plans[[i]]$content <- gsub("어머님", "가족", plans[[i]]$content);
  plans[[i]]$content <- gsub("아버님", "가족", plans[[i]]$content);
  plans[[i]]$content <- gsub("아내", "배우자", plans[[i]]$content);
  plans[[i]]$content <- gsub("와이프", "배우자", plans[[i]]$content);
  plans[[i]]$content <- gsub("남편", "배우자", plans[[i]]$content);
  plans[[i]]$content <- gsub("부모", "가족", plans[[i]]$content);
  plans[[i]]$content <- gsub("언니", "가족", plans[[i]]$content);
  plans[[i]]$content <- gsub("동생", "가족", plans[[i]]$content);
  plans[[i]]$content <- gsub("금액", "비용", plans[[i]]$content);
  plans[[i]]$content <- gsub("재수술", "재수", plans[[i]]$content);
  plans[[i]]$content <- gsub("군데", "쇼핑", plans[[i]]$content);
  plans[[i]]$content <- gsub("쇼핑족", "쇼핑", plans[[i]]$content);
  plans[[i]]$content <- gsub("기 대치", "기대치", plans[[i]]$content);
  plans[[i]]$content <- gsub("타 병원", "타병원", plans[[i]]$content);
  plans[[i]]$content <- gsub("가족랑", "가족이랑 ", plans[[i]]$content);
  plans[[i]]$content <- gsub("상의", "상의함", plans[[i]]$content);
  plans[[i]]$content <- gsub("여동생", "가족", plans[[i]]$content);
  plans[[i]]$content <- gsub("딸", "가족", plans[[i]]$content);
  plans[[i]]$content <- gsub("따님", "가족", plans[[i]]$content);
  plans[[i]]$content <- gsub("신랑", "배우자", plans[[i]]$content);
  plans[[i]]$content <- gsub("남자친구", "배우자", plans[[i]]$content);
  plans[[i]]$content <- gsub("남친", "배우자", plans[[i]]$content);
  }

#kst <- c('하시','때문','정도','필요','만원','해서','하심','전체','이번','오늘','현재',
#         '안내','월요일','생각','당일','결정','확인','예약','내원','흡입','가능','고민',
#         '진행','전화','권유','대상','종아리','복부','시술','허벅지','라인','cm','으로','수술')
kst <- c('하시','때문','정도','필요','만원','해서','하심','전체','이번','오늘','현재',
         '월요일','당일','확인','내원','흡입','가능',
         '진행','전화','종아리','복부','시술','허벅지','라인','cm','으로')
rm_words <- paste(kst, collapse = '|')
for(i in seq_along(plans)){
  plans[[i]]$content <- stringr::str_replace_all(plans[[i]]$content, rm_words, "")
}



#사전단어 추가
#new_term <- c("재수술", "내장지방", "가슴이식", "타병원",'배우자','예약병원','기대치','가멘트','회복기간','최소지방량')
#new_dic <- data.frame(new_term , "ncn")
#buildDictionary(ext_dic=c('sejong', 'woorimalsam', 'insighter'), user_dic=new_dic)
#extractNoun(plans[[1]]$content)

#단어 추출
for(i in seq_along(plans)){
  nouns <- extractNoun(plans[[i]]$content)
  nouns <- nouns[nchar(nouns) > 1]
  plans[[i]]$content <- paste(nouns, sep =" ")
}



plan_tdm <- TermDocumentMatrix(plans, control=list(tokenize="scan", wordLengths=c(2, 7)))
#plan_tdm_1 <- TermDocumentMatrix(plans, control=list(wordLengths=c(2, 7)))
inspect(plan_tdm)
inspect(plan_tdm_1)


 nTerms(plan_tdm_1)

Terms(plan_tdm)[nchar(Terms(plan_tdm)) == 2 ]


findFreqTerms(plan_tdm, lowfreq = 5, highfreq = Inf)

########
plan_tdm <- removeSparseTerms(plan_tdm, sparse=0.99)


wordFreq <- slam::row_sums(plan_tdm)
wordFreq <- sort(wordFreq, decreasing=TRUE)
library(wordcloud)
pal <- brewer.pal(8,"Dark2")
w <- names(wordFreq)
wordcloud(words=w, freq=wordFreq,
          min.freq=1, random.order=F,
          random.color=T, colors=pal)


tds1 <- weightTfIdf(plan_tdm)
M <- t(as.matrix(tds1))
g <- cor(M)
diag(g) <- 0
g[is.na(g)] <- 0
g[g < 0.4 ] <- 0
rownames(g) <- colnames(g) <- Terms(tds1)
library(sna)
library(igraph)
sna::gplot(g, label=colnames(g), gmode="graph",
           label.cex=0.6, vertex.cex=1)

g1 <- graph_from_adjacency_matrix(g, weighted=TRUE)
set.seed(12345)
plot(g1, edge.curved=.1, vertex.label.cex=0.7, edge.color="grey",
     
     
     
     vertex.frame.color="grey", vertex.size=1, vertex.shape="none", vertex.color="white",
     main=paste0("진로 계획 연관-키워드 네트워크"))


wc <- cluster_walktrap(g1)
set.seed(12345)
plot(wc, g1, edge.curved=.1, vertex.label.cex=0.7, edge.color="grey",
     vertex.frame.color="grey", vertex.size=0, vertex.shape="none", vertex.color="white"
     ,
     main=paste0("진로 계획 연관-키워드 네트워크 커뮤니티"))





plan_tdm_1 <- plan_tdm[,slam::col_sums(plan_tdm)>0]
dtm <- as.DocumentTermMatrix(plan_tdm_1)
lda <- LDA(dtm, k = 29, control=list(seed=12456)) # find 10 topics
#plot(lda@loglikelihood, type="l")
#(lda@loglikelihood[length(lda@loglikelihood)])
#(term <- terms(lda, 10))


x <- posterior(lda)$terms
y <- data.frame(t(x[, apply(x, 2, max) > 0.03]))
z <- data.frame(type=paste("Topic", 1),
                keyword=rownames(y), posterior=y[,1])
for(i in 2:10){
  z <- rbind(z, data.frame(type=paste("Topic", i),
                           keyword=rownames(y), posterior=y[,i]))
}
ggplot(z, aes(keyword, posterior, fill=as.factor(keyword)))+
  geom_bar(position="dodge",stat="identity")+
  coord_flip() +
facet_wrap(~type,nrow=1) +
  theme(legend.position="none")



tt <- apply(posterior(lda)$topics, 1, which.max)
str(tt)
tt1 <- data.frame(topic=tt)
tt1$topic <- as.factor(tt1$topic)
sort(summary(tt1$topic),decreasing = T)
head(tt1)
tt1row <- rownames(tt1)
ttt <- cbind(tt1row,tt1$topic)
head(ttt)



install.packages('doParallel')
library(doParallel)
library(scales)


burnin = 1000
iter = 1000
keep = 50

full_data  <- dtm
n <- nrow(full_data)
#-----------validation--------
k <- 5

splitter <- sample(1:n, round(n * 0.75))
train_set <- full_data[splitter, ]
valid_set <- full_data[-splitter, ]

fitted <- LDA(train_set, k = k, method = "Gibbs",
              control = list(burnin = burnin, iter = iter, keep = keep) )
perplexity(fitted, newdata = train_set) # about 2700
perplexity(fitted, newdata = valid_set) # about 4300

#----------------5-fold cross-validation, different numbers of topics----------------
# set up a cluster for parallel processing
cluster <- makeCluster(detectCores(logical = TRUE) - 1) # leave one CPU spare...
registerDoParallel(cluster)

# load up the needed R package on all the parallel sessions
clusterEvalQ(cluster, {
  library(topicmodels)
})

folds <- 5
splitfolds <- sample(1:folds, n, replace = TRUE)
candidate_k <- c(2, 20:40, 50) # candidates for how many topics

# export all the needed R objects to the parallel sessions
clusterExport(cluster, c("full_data", "burnin", "iter", "keep", "splitfolds", "folds", "candidate_k"))

# we parallelize by the different number of topics.  A processor is allocated a value
# of k, and does the cross-validation serially.  This is because it is assumed there
# are more candidate values of k than there are cross-validation folds, hence it
# will be more efficient to parallelise
system.time({
  results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
    k <- candidate_k[j]
    results_1k <- matrix(0, nrow = folds, ncol = 2)
    colnames(results_1k) <- c("k", "perplexity")
    for(i in 1:folds){
      train_set <- full_data[splitfolds != i , ]
      valid_set <- full_data[splitfolds == i, ]
      
      fitted <- LDA(train_set, k = k, method = "Gibbs",
                    control = list(burnin = burnin, iter = iter, keep = keep) )
      results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
    }
    return(results_1k)
  }
})
stopCluster(cluster)

results_df <- as.data.frame(results)

ggplot(results_df, aes(x = k, y = perplexity)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("5-fold cross-validation of topic modelling with the 'comment' dataset",
          "(각 토픽수에 대한 5개 모델)") +
  labs(x = "number of topics", y = "Perplexity")
