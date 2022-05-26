invade <- read.csv("Russia_invade.csv")
ncol(invade)
nrow(invade)
prewar <- invade %>% filter(date < '2022-02-24 23:59:00+00:00')
content <- prewar$content

prewarnews <- prewar %>% filter(grepl('invade|nato|war|troops|invasion|military',content))%>%
  filter(grepl('https',content)) %>%
          summarise(date,as.character(id),content,replyCount,retweetCount,likeCount,quoteCount)%>%
          filter(retweetCount>=1 & replyCount>=1)%>%
        arrange(desc(retweetCount))

library(lubridate)
prewarnews3<- prewarnews %>% filter(grepl('inva[ds]',content))%>% #invade
              group_by(week=week(date))%>%
              mutate(avg.likeCount=(mean(likeCount)))%>%
              mutate(avg.retweetCount=(mean(retweetCount)))%>%
              mutate(avg.replyCount=mean(replyCount))%>%
              mutate(avg.quoteCount=mean(quoteCount))%>%
              add_count(week)%>%
              arrange(week)

ggplot(prewarnews3,aes(x=week,y=avg.likeCount))+
  geom_line()+
  xlim(0,8)
ggplot(prewarnews3,aes(x=week,y=avg.retweetCount))+
  geom_line()+
  xlim(0,8)
ggplot(prewarnews3,aes(x=week,y=avg.replyCount))+
  geom_line()+
  xlim(0,8)
ggplot(prewarnews3,aes(x=week,y=avg.quoteCount))+
  geom_line()+
  xlim(0,8)

affinnur<- afinn.analysis %>%
  bind_rows (afinn.analysis.russia)

ggplot (affinnur, aes(y=sentiment, x=week, color= country))+
  geom_line()+
  xlim(0,11)+ # From week 0 to 11
  ggtitle ("Affinn Sentiment Analysis")



write.csv(prewarnews3,"prewarnews3.avg.week.csv")
prewarnews2 <- prewarnews %>% mutate(https=word(content,-1))
write.csv(prewarnews2,"prewarnews2.csv")
#filtered invasion
library(tidyverse)
filtered <- invade %>% filter(grepl('invasion|invade',content))%>%
  filter(retweetCount>'1'&likeCount!='0')%>%filter(date<='2022-02-24 23:59:50+00:00')%>% arrange(aesc(date))%>%
  summarise(content,retweetCount,likeCount,date)

#children/child re-tweeted more than once
filtered <- invade %>% filter(grepl('children|child',content))%>%
  filter(retweetCount>'1')%>%arrange(desc(retweetCount))%>%
  summarise(content,retweetCount)

str(invade$date)
#top 10 filtered child
filteredchild<- top_n(filtered,10)

write.csv(filteredchild,'filtered_children_top10.csv')


#refugee re-tweeted 
filtered2<- invade %>% filter(grepl('refugee',content))%>% 
  summarise(content,retweetCount)%>%filter(retweetCount!='0') %>% summarise(content,retweetCount)

write.csv(filtered,'filtered_children.csv')

#filtered NATO
filterednato <- invade %>% filter(grepl('NATO',content))%>% summarise(content)

library(dplyr)
install.packages("tidytext")
library(tidytext)
install.packages("stringr")
library (stringr)
library(ggplot2)

text <- paste(content, collapse = " ")
text <- str_replace_all(text, pattern = '\"', replacement="")
text <- str_replace_all(text, pattern = '\n', replacement="")
text <- str_replace_all(text, pattern = '\u0092', replacement="'")
text <- str_replace_all(text, pattern = '\u0091', replacement = "'")

text_df <- data_frame(Text=text)

text_words <- text_df %>%
  unnest_tokens (output = word, input = Text)

data(stop_words)
text_words <- text_words %>%
  anti_join(stop_words)

#Frequency of the words
text_wordcounts <-text_words %>% count(word,sort=TRUE)

text_graph <- text_wordcounts %>%
  filter(n>200)%>%
  mutate(word = reorder(word,n))%>%
  slice(1:22)
#graph
ggplot(text_graph, aes(word,n))+geom_col(fill="black")+
  coord_flip()+
  geom_text(aes(label=n),color="red")+
  labs(title="Most Frequent word before the war")

library(wordcloud)
library(tm)
library(SnowballC)

#wordcloud Nato
set.seed(1234)
wordcloud(words=text_wordcounts$word, 
          freq=text_wordcounts$n,
          min.freq = 200,
          scale=c(5,0.35),
          max.words=200,
          random.order=FALSE,
          rot.per=0.35,
          use.r.layout = FALSE,
          colors=brewer.pal(8,"Dark2"))
