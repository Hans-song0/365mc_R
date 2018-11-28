install.packages('lda')
install.packages('RTextTools')
install.packages('topicmodels')
install.packages('Rcpp')
install.packages('LDAvis')

setwd("C:/Users/365mc/Documents/�ؽ�Ʈ�м�")
library(KoNLP)
library(wordcloud)
library(stringr)
library(rJava)
library(RColorBrewer)
library(tm)
library(httr)
library(qgraph)
library(xml2)
library(arules)
library(igraph)
library(combinat)
library(KoSpacing)
library(lda)
library(RTextTools)
library(topicmodels)
library(Rcpp)
library(plyr)
library(LDAvis)


peo <- read.csv('�������.csv',header = T)

peo$�ڸ�Ʈ <- as.character(peo$�ڸ�Ʈ)
peo$ó���ڸ�Ʈ <- as.character(peo$ó���ڸ�Ʈ)
mergeUserDic(data.frame(readLines("mergefile.txt"), "ncn"))

str(peo)

nrow(subset(peo,���� == '�λ�'))
#�߰��ܾ�
head(full_sort,100)



gsubpeo <- readLines('gsubpeo.txt')

tran11 <- Map(extractNoun, peo[,6])
tran111 <- unique(tran11)
tran1 <- sapply(tran111,unique)

for(i in 1:length(gsubpeo)){
  tran2 <- rapply(tran1, function(x) gsub(gsubpeo[i],'',x), how = 'replace')
}
tran3 <- sapply(tran2, function(x) {Filter(function(y) {nchar(y) <= 8 && nchar(y) > 1},x)} )

write(unlist(tran3),'tran3.txt')
data4 <- read.table('tran3.txt')

for(i in 1:length(gsubpeo)){
  tran3 <- rapply(tran3, function(x) gsub(gsubpeo[i],'',x), how = 'replace')
}
tran3 <- sapply(tran3, function(x) {Filter(function(y) {nchar(y) <= 8 && nchar(y) > 1},x)} )


tran3 <- rapply(tran3, function(x) gsub("������", "������", x), how = "replace")
tran3 <- rapply(tran3, function(x) gsub("\\d+", "", x), how = "replace")
tran3 <- rapply(tran3, function(x) gsub("����", "�θ��", x), how = "replace")
tran3 <- rapply(tran3, function(x) gsub("�ƺ�", "�θ��", x), how = "replace")
tran3 <- rapply(tran3, function(x) gsub("�ƹ���", "�θ��", x), how = "replace")
tran3 <- rapply(tran3, function(x) gsub("��Ӵ�", "�θ��", x), how = "replace")
tran3 <- rapply(tran3, function(x) gsub("����", "�����", x), how = "replace")
tran3 <- rapply(tran3, function(x) gsub("�Ƴ�", "�����", x), how = "replace")
tran3 <- rapply(tran3, function(x) gsub("������", "�����", x), how = "replace")
tran3

head(wordcount)


write(unlist(tran3),'tran3.txt')
data4 <- read.table('tran3.txt')



wordcount <- table(data4)
wordcount <- Filter(function(x) {nchar(x) <= 10}, wordcount)
full_sort <- sort(wordcount, decreasing=T)
full_sort <- as.data.frame(full_sort)

palete <- brewer.pal(7,"Set2")
wordcloud(names(wordcount),
          freq=wordcount,
          max.words = 100,
          min.freq = 5,
          random.order = FALSE,
          rot.per = 0.25,
          colors = brewer.pal(8, "Dark2"))
wordcloud(names(wordcount),
          freq=wordcount,
          scale=c(5,1),
          rot.per=0.25,
          min.freq=10,
          random.order=F,
          random.color=T,
          colors=palete)

#��Ʈ��ũ �м� 

tran <- Map(extractNoun, peo[,6])
tran <- unique(tran)
tran <- sapply(tran, unique)
tran <- sapply(tran, function(x) {Filter(function(y) {nchar(y) <= 6 && nchar(y) > 1 && is.hangul(y)},x)} )
tran <- Filter(function(x){length(x) >= 2}, tran)
names(tran) <- paste("Tr", 1:length(tran), sep="")
wordtran <- as(tran, "transactions")

#co-occurance table 
wordtab <- crossTable(wordtran)


ares <- apriori(wordtran, parameter=list(supp=0.005, conf=0.4))
inspect(ares)
rules <- labels(ares, ruleSep=" ")
rules <- sapply(rules, strsplit, " ",  USE.NAMES=F)
rulemat <- do.call("rbind", rules)


rules_d <-apriori(wordtran, parameter=list(supp=0.05,conf = 0.6), 
                  appearance = list(default="lhs",rhs="���̾�Ʈ"))
rules_d
rules_d <-sort(rules_e , decreasing=TRUE,by="confidence")
inspect(rules_d)
rules_e <-apriori(wordtran, parameter=list(supp=0.05,conf = 0.6), 
                  appearance = list(default="lhs",rhs="�"))
rules_e
rules_e <-sort(rules_e , decreasing=TRUE,by="confidence")
inspect(rules_e)
rules_f <-apriori(wordtran, parameter=list(supp=0.05,conf = 0.6), 
                  appearance = list(default="lhs",rhs="�Ĵ�"))
rules_f
rules_f <-sort(rules_f , decreasing=TRUE,by="confidence")
inspect(rules_f)
# ares <- apriori(wordtran, parameter=list(supp=0.05, conf=0.05))
# inspect(ares)
# rules <- labels(ares, ruleSep="/", setStart="", setEnd="")
# rules <- sapply(rules, strsplit, "/",  USE.NAMES=F)
# rules <- Filter(function(x){!any(x == "")},rules)
# rulemat <- do.call("rbind", rules)
# rulequality <- quality(ares)
# ruleg <- graph.edgelist(rulemat,directed=F)

#plot for important pairs 
ruleg <- graph.edgelist(rulemat[-c(1:16),],directed=F)
plot.igraph(ruleg, vertex.label=V(ruleg)$name, vertex.label.cex=1, vertex.size=1, layout=layout.fruchterman.reingold.grid,vertex.label.font=10,edge.color="green")


#plot for all pairs
# tranpairs <- sapply(tran, function(x){t(combn(x,2))})
# sapply(tran,function(x){x })
# edgelist  <- do.call("rbind", tranpairs)
# edgelist <- unique(edgelist)
# g <- graph.edgelist(edgelist, directed=F)
# plot(g)


closen <- closeness(ruleg)
head(closen)
dfclosen <- as.data.frame(closen)
head(sort(dfclosen)
     sortclosen <- sort(closen, decreasing = T)
     plot(closen, col="red",xaxt="n", lty="solid", type="b", xlab="�ܾ�", ylab="closeness")
     points(closen, pch=16, col="navy")
     axis(1, seq(1, length(closen)), V(ruleg)$name, cex=5))
     
     

     
#lda


nouns <- sapply(peo[,6], extractNoun, USE.NAMES=F)
nouns <- nouns[nchar(nouns)>=2]  
head(nouns)

##LDA�� ���� ����

##K�� ���ϴ� �з� ���� ����, num�� �ݺ���

K<-10
set.seed(1004)  
num.iterations <- 1000

##LDA�� ���� ����. gsub�� ������ ��ó�� ����

corpus <- lexicalize(nouns, lower=TRUE)
corpus$vocab<-gsub("������","������",corpus$vocab)
corpus$vocab<-gsub("\\d+","",corpus$vocab)
corpus$vocab<-gsub("����","�θ��",corpus$vocab)
corpus$vocab<-gsub("�ƺ�","�θ��",corpus$vocab)
corpus$vocab<-gsub("��Ӵ�","�θ��",corpus$vocab)
corpus$vocab<-gsub("�ƹ���","�θ��",corpus$vocab)
corpus$vocab<-gsub("����","�����",corpus$vocab)
corpus$vocab<-gsub("�Ƴ�","�����",corpus$vocab)
corpus$vocab<-gsub("������","�����",corpus$vocab)
corpus$vocab<-gsub("\\(","",corpus$vocab)
corpus$vocab<-gsub("c","",corpus$vocab)
corpus$vocab<-gsub("\\)","",corpus$vocab)
corpus$vocab<-gsub("\"","",corpus$vocab)
corpus$vocab<-gsub(",","",corpus$vocab)

str(corpus)
##LDA

result <- lda.collapsed.gibbs.sampler(corpus$documents, K, corpus$vocab, 
                                      num.iterations, 0.1, 0.1, compute.log.likelihood = TRUE)

##������ 500�� �ܾ� Ȯ��

top.words <- top.topic.words(result$topics, 1000, by.score = TRUE)
print(top.words)
##������ ����
write.csv(top.words,file="LDA10.csv")

lda10 <- read.csv('LDA10.csv',header = T)


theta <- t(apply(result$document_sums + 0.1, 2, function(x) x/sum(x)))
phi <- t(apply(t(result$topics) + 0.1, 2, function(x) x/sum(x)))

peoreview <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

# create the JSON object to feed the visualization:
json <- createJSON(phi = MovieReviews$phi, 
                   theta = MovieReviews$theta, 
                   doc.length = MovieReviews$doc.length, 
                   vocab = MovieReviews$vocab, 
                   term.frequency = MovieReviews$term.frequency)