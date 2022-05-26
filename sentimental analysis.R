library (tidytext)
library (tidyverse)
library (dplyr)

#1. Import data
war <- read.csv("Russia_invade.csv")

#2. Filter data into Ukraine and Russia Related Data
Ukraine <- war %>% filter(grepl('Ukraine|Ukraina|Ukrainian',content)) %>%
             summarise(date,id, content)

russia <- war %>% filter(grepl('Russia|Russian',content)) %>%
  summarise(date,id, content)

# 3. unnest token ukraine_filtered twitter 
ukraineword <- Ukraine %>% unnest_tokens(word, content)

write.csv (ukraineword, "ukraineword.csv")

# 3. unnest token russian_filtered twitter
russianword <- russia %>% unnest_tokens(word, content)

#Count the word (cols: date / id / word / count(n))
# 4. add_count() Ukraine
cleaned_ukraine <- ukraineword %>% anti_join(get_stopwords())
cleaned_ukraine <- cleaned_ukraine %>% add_count(word,sort=TRUE)

write.csv(cleaned_ukraine,"cleaned_ukraine_word.csv")

# 4. add_count() Russia
cleaned_russia <- russianword %>% anti_join(get_stopwords())
cleaned_russia <- cleaned_russia %>% add_count(word,sort=TRUE)
                  
#Bing                    
bing <- get_sentiments("bing")

#Affinn
library(tidytext)
install.packages("textdata")
afinn <- get_sentiments("afinn")
nrc<- get_sentiments("nrc")

1#both positive and negative & average sentiment
library(tidyr)

# 5. UKRAINE

#word Count 
positiveukraine5<- cleaned_ukraine %>% inner_join(bing)%>% 
  count(word,sort=TRUE)
positiveukraine5 <- positiveukraine5 %>% filter(word!='trump')

library(wordcloud)
set.seed(1234)
cloudimg = wordcloud(words=positiveukraine5$word, 
                     freq=positiveukraine5$n,
                     min.freq = 200,
                     max.words=150,
                     scale=c(5,0.35),
                     random.order = FALSE,
                     rot.per=0.35,
                     colors=brewer.pal(8,"Dark2"))


#sentiment count
positiveukraine6<- cleaned_ukraine %>% inner_join(bing)%>% 
  count(word,date,id,sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) 

#eliminate trump from the list as it refers to the person
positiveukraine7 <- positiveukraine6 %>%
                    filter(word!='trump')

#grouped by week and got an average of the sentiment
positiveukraine8 <- positiveukraine7 %>%
                    group_by(week=week(date)) %>%
                    mutate(sentiment.average= mean(sentiment))

positiveukraine9 <- positiveukraine7 %>%
  group_by(week=week(date)) %>%
  summarise(sentiment.average= mean(sentiment))

#afinn
afinn.analysis <- cleaned_ukraine %>% 
                  inner_join(afinn) %>%
                  group_by(week=week(date)) %>%
                  filter(word!="trump")%>%
                  summarise(sentiment = sum(value)) %>%
  mutate(country="Ukraine")


afinn.word<- cleaned_ukraine %>% inner_join(afinn)%>%
  count(word,sort=TRUE)

afinn.word.russia <- cleaned_russia %>% inner_join(afinn)%>%
  count(word,sort=TRUE)

affinnmg = wordcloud(words=afinn.word$word, 
                     freq=afinn.word$n,
                     min.freq = 1,
                     max.words=150,
                     scale=c(3,0.35),
                     random.order = FALSE,
                     rot.per=0.35,
                     colors=brewer.pal(8,"Dark2"))


afinn.analysis.russia <- cleaned_russia %>% 
                         inner_join(afinn) %>%
                         group_by(week=week(date)) %>%
  filter(word!="trump")%>%
  summarise(sentiment = sum(value))%>%
                          mutate(country="Russia")

afinn.analysis.russia2 <- afinn.analysis.russia %>%group_by(week,word,n)%>%
                          summarise(word,n)
#neg russia afinn

afinn.neg <- get_sentiments("afinn")%>% filter(value>=0)

afneg.analysis.russia <- cleaned_russia %>% 
  inner_join(afinn.neg)%>%
  group_by(week=week(date)) %>%
  summarise(sentiment = sum(value))%>%
  mutate(country="Russia")

afneg.analysis.ukraine <- cleaned_ukraine%>% 
  inner_join(afinn.neg)%>%
  group_by(week=week(date)) %>%
  summarise(sentiment = sum(value))%>%
  mutate(country="Ukraine")
 
afnegrusukraine<- afneg.analysis.russia %>%
  bind_rows (afneg.analysis.ukraine)

ggplot (afnegrusukraine, aes(y=sentiment, x=week, color= country))+
  geom_line()+
  xlim(0,11)+ # From week 0 to 11
  ggtitle ("Affinn Negative Sentiment Analysis")




affinnur<- afinn.analysis %>%
  bind_rows (afinn.analysis.russia)

ggplot (affinnur, aes(y=sentiment, x=week, color= country))+
  geom_line()+
  xlim(0,11)+ # From week 0 to 11
  ggtitle ("Affinn Sentiment Analysis")

##nrc

nrcukraine<- cleaned_ukraine %>% inner_join(nrc)%>%
  count(word,date,id,sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

nrcukraine<-nrcukraine %>%filter(word!='trump')

nrcukraine <- nrcukraine %>%
  group_by(week=week(date)) %>%
  mutate(sentiment.average= mean(sentiment))
# nrc russia 
nrcrussia<- cleaned_russia %>% inner_join(nrc)%>%
  count(word,date,id,sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

nrcrussia<-nrcrussia %>%filter(word!='trump')

nrcrussia <- nrcrussia %>%
  group_by(week=week(date)) %>%
  mutate(sentiment.average= mean(sentiment))

nrcgraph<- nrcukraine %>% mutate(Country='Ukraine')%>%
  bind_rows (nrcrussia %>% mutate(Country = 'Russia'))

ggplot (nrcgraph, aes(y=sentiment.average, x=week, color= Country))+
  geom_line()+
  xlim(0,11)+ # From week 0 to 11
  ggtitle ("NRC Sentiment Analysis")

# 5. RUSSIA
sentimentrussia <- cleaned_russia %>% inner_join(bing)%>%
  count(word,sentiment,date,id) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

sentimentrussia <- sentimentrussia %>%
  filter(word!='trump')
#grouped by week and got average
sentimentrussia2 <- sentimentrussia %>%
                    group_by(week=week(date)) %>%
  mutate(sentiment.average= mean(sentiment))%>%
  arrange(desc(week))


# Draw Graph
library(ggplot2)

# Geom col 
ggplot(positiveukraine7, aes(date,sentiment))+
  geom_col(show.legend = FALSE)+
  ylim(-3,3)+
  facet_wrap(~week, ncol=2)

# 6. Line graph Ukraine
ggplot(positiveukraine8, aes(week, sentiment.average))+
  geom_line()+
  xlim(0,11)

# 6. line graph Russia
ggplot(sentimentrussia2, aes(week, sentiment.average))+
  geom_line()+
  xlim(0,11)

# 7. Draw two lines in one graph
ukn_rus <- positiveukraine8 %>% mutate(Country='Ukraine') %>%
          bind_rows (sentimentrussia2 %>% mutate(Country = 'Russia'))

ggplot (ukn_rus, aes(y=sentiment.average, x=week, color= Country))+
  geom_line()+
  xlim(0,11)+ # From week 0 to 11
  ggtitle ("Sentiment Analysis")

# Get week from date
library(lubridate)
date <- ukraineword$date
week <- week(date)

# positive bing
pos.bing <- get_sentiments("bing")%>% filter(sentiment=='positive')


#Positive Ukraine
positiveukraine2<- cleaned_ukraine %>% inner_join(pos.bing)%>% 
  count(word,sort=TRUE)
positiveukraine2 <- positiveukraine2 %>% filter(word!='trump')

cloudimg = wordcloud(words=positiveukraine2$word, 
                     freq=positiveukraine5$n,
                     min.freq = 200,
                     max.words=150,
                     scale=c(3,0.4),
                     random.order = FALSE,
                     rot.per=0.35,
                     colors=brewer.pal(8,"Dark2"))

#mutate
pos.ukraine <- cleaned_ukraine %>% inner_join(pos.bing)%>% 
  count(word,date,id,sentiment) %>%
  spread(sentiment, n, fill = 0)

pos.ukraine2 <- pos.ukraine %>% filter(word!='trump')

pos.ukraine3 <- pos.ukraine2 %>%
  group_by(week=week(date)) %>%
  mutate(positiveness.average= mean(positive))

#change 53 to -53 (year of 2021 dec)
pos.ukraine3$week[pos.ukraine3$week==53]<--53

#Positive Russia
#wordcloud for russia
pos.russia.word <- cleaned_russia %>% inner_join(pos.bing)%>% 
  count(word,sort=TRUE)
pos.russia.word <- pos.russia.word %>% filter(word!='trump')

cloudimg.rus = wordcloud(words=pos.russia.word$word, 
                     freq=pos.russia.word$n,
                     min.freq = 200,
                     max.words=150,
                     scale=c(3,0.4),
                     random.order = FALSE,
                     rot.per=0.35,
                     colors=brewer.pal(8,"Dark2"))

#mutate
pos.russia <- cleaned_russia %>% inner_join(pos.bing)%>% 
  count(word,date,id,sentiment) %>%
  spread(sentiment, n, fill = 0)

pos.russia2 <- pos.russia %>% filter(word!='trump')

pos.russia2 <- pos.ukraine2 %>%
  group_by(week=week(date)) %>%
  mutate(positiveness.average= mean(positive))

pos.russia2$week[pos.russia2$week==53]<--53

# Positive Graph
ggplot(pos.russia2, aes(y=positiveness.average, x= week))+
  geom_line() #+xlim(0,11)

ggplot(pos.ukraine3, aes(y=positiveness.average, x= week))+
  geom_line() #+xlim(0,11)

pos.ukn.russ <- pos.ukraine3 %>% mutate(Country='Ukraine') %>%
  bind_rows(pos.russia2 %>% mutate(Country = 'Russia'))

ggplot(pos.ukn.russ, aes(y=positiveness.average, x=week, color= Country))+
  geom_line()+
  xlim(0,11)
  ggtitle("Positive Sentiment Analysis")


# negative bing 
pos.neg <- get_sentiments("bing")%>% filter(sentiment=='negative')

#wordcloud ukraine
neg.ukraine.word <- cleaned_ukraine %>% inner_join(pos.neg)%>% 
  count(word,sort=TRUE)

cloudimg.neg.ukraine = wordcloud(words=neg.ukraine.word$word, 
                         freq=neg.ukraine.word$n,
                         min.freq = 200,
                         max.words=200,
                         scale=c(3,0.2),
                         random.order = FALSE,
                         rot.per=0.35,
                         colors=brewer.pal(8,"Dark2"))
#barplot of ukraine
neg.ukraine.bar <- neg.ukraine.word%>%arrange(desc(n))
neg.ukraine.bar<-top_n(neg.ukraine.bar,20) 

neg.ukraine.bar %>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n))+
  geom_col()+
  coord_flip()+
  labs(y="\n Count",title="Frequent Words in Negative Sentiment of Ukraine")+
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold")

#mutate ukraine
neg.ukraine <- cleaned_ukraine %>% inner_join(pos.neg)%>% 
  count(word,date,id,sentiment) %>%
  spread(sentiment, n, fill = 0)

neg.ukraine2 <- neg.ukraine %>%
  group_by(week=week(date)) %>%
  mutate(negativeness.average= mean(negative))

neg.ukraine2$week[neg.ukraine2$week==53]<--53

#wordcloud russia
neg.russia.word <- cleaned_russia %>% inner_join(pos.neg)%>% 
  count(word,sort=TRUE)

cloudimg.neg.russia = wordcloud(words=neg.russia.word$word, 
                                 freq=neg.russia.word$n,
                                 min.freq = 200,
                                 max.words=200,
                                 scale=c(2.5,0.2),
                                 random.order = FALSE,
                                 rot.per=0.35,
                                 colors=brewer.pal(8,"Dark2"))
#barplot
neg.russia.bar <- neg.russia.word%>%arrange(desc(n))
neg.russia.bar<-top_n(neg.russia.bar,20) 

neg.russia.bar %>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n))+
  geom_col()+
  coord_flip()+
  labs(y="\n Count",title="Frequent Words in Negative Sentiment of Russia")+
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") 

#mutate russia
neg.russia <- cleaned_russia %>% inner_join(pos.neg)%>% 
  count(word,date,id,sentiment) %>%
  spread(sentiment, n, fill = 0)

neg.russia2 <- neg.russia %>%
  group_by(week=week(date)) %>%
  mutate(negativeness.average= mean(negative))

neg.russia2$week[neg.russia2$week==53]<--53

#individual line
dev.off()
ggplot(neg.ukraine2, aes(y=negativeness.average, x= week))+
  geom_line()+xlim(0,11)

ggplot(neg.russia2, aes(y=negativeness.average, x= week))+
  geom_line()+xlim(0,11)

#combine line
neg.russ.ukrn <- neg.ukraine2 %>% mutate(Country='Ukraine') %>%
  bind_rows (neg.russia2 %>% mutate(Country = 'Russia'))

ggplot (neg.russ.ukrn, aes(y=negativeness.average, x=week, color= Country))+
  geom_line()+
  xlim(-53,11)+ # From week 0 to 11
  ggtitle ("Sentiment Analysis")


#extract text with most likes
Ukraine2 <- war %>% filter(grepl('Ukraine|Ukraina|Ukrainian',content)) %>%
  summarise(date,as.character(id), content,likeCount) %>% filter(likeCount>=10) %>%
  arrange(desc(likeCount))

attack <- Ukraine2 %>% filter(grepl('attack',content))%>%
  group_by(week=week(date))

write.csv(attack,"attackextractedtwitter.csv")


