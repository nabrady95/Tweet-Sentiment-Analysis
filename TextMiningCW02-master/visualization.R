# load the libraries
library(twitteR)
library(plyr)
library(stringr)
library(ggplot2)
library(tm)
library(devtools)
library(stringi)
library(wordcloud)
library(scales)
library(sos)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)
library(ggplot2)
library(lubridate)
library(syuzhet)
library(reshape2)
library (rtweet)
library(plotly)

# import your data set to analyse,
# ensure it is in the same directory as your code, otherwise you need to add the path
Dataset2 <- read.csv("phoenix_data.csv")
tweets_organic <- Dataset2[Dataset2$retweet=="False",]
# using subset function
## FIND BETTER WAY
before <- subset(tweets_organic, date == '2020-02-01' | date == '2020-02-02' | date == '2020-02-03' |
                   date == '2020-02-04' | date == '2020-02-05' | date == '2020-02-06' | date == '2020-02-07' |
                 date == '2020-02-08' | date == '2020-02-09' )

after <- subset(tweets_organic, date == '2020-02-10' | date == '2020-02-11' | date == '2020-02-12' |
                   date == '2020-02-13' | date == '2020-02-14' | date == '2020-02-15' | date == '2020-02-16' |
                   date == '2020-02-17' | date == '2020-02-18' | date == '2020-02-19' )




#################################################################################################
#################################################################################################
# DATA CLEANING BEFORE OSCARS
#################################################################################################
#################################################################################################
# get rid of problem characters
before$tweet <- sapply(before$tweet,function(row) iconv(row, "latin1", "ASCII", sub=""))

# Data cleaning
before$tweet = gsub('https\\S+\\s*', '', before$tweet) ## Remove URLs
before$tweet = gsub('http\\S+\\s*', '', before$tweet) ## Remove URLs
before$tweet = gsub('pic\\S+\\s*', '', before$tweet) ## Remove URLs
before$tweet = gsub('www.\\S+\\s*', '', before$tweet) ## Remove URLs
before$tweet = gsub('\\b+RT', '', before$tweet) ## Remove RT
before$tweet = gsub('#\\S+', '', before$tweet) ## Remove Hashtags
before$tweet = gsub('@\\S+', '', before$tweet) ## Remove Mentions
before$tweet = gsub('[[:cntrl:]]', '', before$tweet) ## Remove Controls and special characters
before$tweet = gsub("\\d", '', before$tweet) ## Remove Controls and special characters
before$tweet = gsub('[[:punct:]]', '', before$tweet) ## Remove Punctuations
before$tweet = gsub("^[[:space:]]*","",before$tweet) ## Remove leading whitespaces
before$tweet = gsub("[[:space:]]*$","",before$tweet) ## Remove trailing whitespaces
before$tweet = gsub(' +',' ',before$tweet) ## Remove extra whitespaces
before$tweet = gsub('<.*>', '', enc2native(before$tweet)) #remove emojis
before$tweet <- str_replace_all(before$tweet," "," ") # get rid of unnecessary spaces
before$tweet <- str_replace(before$tweet,"RT @[a-z,A-Z]*: ","") # take out the retweet header (there is only one)
#before$tweet = stri_remove_empty(before$tweet, na_empty = FALSE)


#################################################################################################
#################################################################################################
# DATA CLEANING AFTER OSCARS
#################################################################################################
#################################################################################################
# get rid of problem characters
after$tweet <- sapply(after$tweet,function(row) iconv(row, "latin1", "ASCII", sub=""))
# Data cleaning
after$tweet = gsub('https\\S+\\s*', '', after$tweet) ## Remove URLs
after$tweet = gsub('http\\S+\\s*', '', after$tweet) ## Remove URLs
after$tweet = gsub('pic\\S+\\s*', '', after$tweet) ## Remove URLs
after$tweet = gsub('www.\\S+\\s*', '', after$tweet) ## Remove URLs
after$tweet = gsub('\\b+RT', '', after$tweet) ## Remove RT
after$tweet = gsub('#\\S+', '', after$tweet) ## Remove Hashtags
after$tweet = gsub('@\\S+', '', after$tweet) ## Remove Mentions
after$tweet = gsub('[[:cntrl:]]', '', after$tweet) ## Remove Controls and special characters
after$tweet = gsub("\\d", '', after$tweet) ## Remove Controls and special characters
after$tweet = gsub('[[:punct:]]', '', after$tweet) ## Remove Punctuations
after$tweet = gsub("^[[:space:]]*","",after$tweet) ## Remove leading whitespaces
after$tweet = gsub("[[:space:]]*$","",after$tweet) ## Remove trailing whitespaces
after$tweet = gsub(' +',' ',after$tweet) ## Remove extra whitespaces
after$tweet = gsub('<.*>', '', enc2native(after$tweet)) #remove emojis
after$tweet <- str_replace_all(after$tweet," "," ") # get rid of unnecessary spaces
after$tweet <- str_replace(after$tweet,"RT @[a-z,A-Z]*: ","") # take out the retweet header (there is only one)
#after$tweet = stri_remove_empty(after$tweet, na_empty = FALSE)
data("stop_words")

#################################################################################################
#################################################################################################
# MOST FREQUENT WORDS (USING STOPWORDS)
#################################################################################################
#################################################################################################
# tweets <- before %>%
#   dplyr::select(tweet) %>%
#   unnest_tokens(word, tweet)
# tweets <- tweets %>%
#   anti_join(stop_words)
# 
# 
# tweets %>% # gives you a bar chart of the most frequent words found in the tweets
#   count(word, sort = TRUE) %>%
#   top_n(30) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +
#   labs(y = "Count",x = "Unique words",title = "Most frequent words found", subtitle = "Stop words removedt")
# 
# tweets <- after %>%
#   dplyr::select(tweet) %>%
#   unnest_tokens(word, tweet)
# tweets <- tweets %>%
#   anti_join(stop_words)
# 
# 
# tweets %>% # gives you a bar chart of the most frequent words found in the tweets
#   count(word, sort = TRUE) %>%
#   top_n(30) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(x = word, y = n)) + geom_col() + xlab(NULL) + coord_flip() +
#   labs(y = "Count",x = "Unique words",title = "Most frequent words found", subtitle = "Stop words removedt")




#################################################################################################
#################################################################################################
# SMALR 10.Detecting Sentiment Polarity FOR BEFORE AND AFTER
#################################################################################################
#################################################################################################
# pos <- readLines("positive_words.txt")
# neg <- readLines("negative_words.txt")
# 
# #function to calculate sentiment score
# score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
# {
#   # Parameters
#   # sentences: vector of text to score
#   # pos.words: vector of words of postive sentiment
#   # neg.words: vector of words of negative sentiment
#   # .progress: passed to laply() to control of progress bar
# 
#   # create simple array of scores with laply
#   scores <- laply(sentences,
#                   function(sentence, pos.words, neg.words)
#                   {
#                     # remove punctuation
#                     sentence <- gsub("[[:punct:]]", "", sentence)
#                     # remove control characters
#                     sentence <- gsub("[[:cntrl:]]", "", sentence)
#                     # remove digits
#                     sentence <- gsub('\\d+', '', sentence)
# 
#                     #convert to lower
#                     sentence <- tolower(sentence)
# 
# 
#                     # split sentence into words with str_split (stringr package)
#                     word.list <- str_split(sentence, "\\s+")
#                     words <- unlist(word.list)
# 
#                     # compare words to the dictionaries of positive & negative terms
#                     pos.matches <- match(words, pos)
#                     neg.matches <- match(words, neg)
# 
#                     # get the position of the matched term or NA
#                     # we just want a TRUE/FALSE
#                     pos.matches <- !is.na(pos.matches)
#                     neg.matches <- !is.na(neg.matches)
# 
#                     # final score
#                     score <- sum(pos.matches) - sum(neg.matches)
#                     return(score)
#                   }, pos.words, neg.words, .progress=.progress )
#   # data frame with scores for each sentence
#   scores.df <- data.frame(text=sentences, score=scores)
#   return(scores.df)
# }
# #sentiment score
# scores_twitter <- score.sentiment(before$tweet, pos.txt, neg.txt, .progress='text')
# 
# 
# View(scores_twitter)
# 
# #Summary of the sentiment scores
# summary(scores_twitter)
# 
# scores_twitter$score_chr <- ifelse(scores_twitter$score < 0,'Negtive', ifelse(scores_twitter$score > 0, 'Positive', 'Neutral'))
# 
# 
# View(scores_twitter)
# 
# 
# #Convert score_chr to factor for visualizations
# scores_twitter$score_chr <- as.factor(scores_twitter$score_chr)
# names(scores_twitter)[3]<-paste("Sentiment")
# 
# #plot to show number of negative, positive and neutral comments
# Viz1 <- ggplot(scores_twitter, aes(x=Sentiment, fill=Sentiment))+ geom_bar(aes(y = (..count..)/sum(..count..))) +
#   scale_y_continuous(labels = percent)+labs(y="Score")+
#   theme(text =element_text(size=15))+theme(axis.text = element_text(size=15))+ theme(legend.position="none")+ coord_cartesian(ylim=c(0,0.6)) + scale_fill_manual(values=c("firebrick1", "grey50", "limeGREEN"))
# Viz1

# pos <- readLines("positive_words.txt")
# neg <- readLines("negative_words.txt")
# 
# #function to calculate sentiment score
# score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
# {
#   # Parameters
#   # sentences: vector of text to score
#   # pos.words: vector of words of postive sentiment
#   # neg.words: vector of words of negative sentiment
#   # .progress: passed to laply() to control of progress bar
# 
#   # create simple array of scores with laply
#   scores <- laply(sentences,
#                   function(sentence, pos.words, neg.words)
#                   {
#                     # remove punctuation
#                     sentence <- gsub("[[:punct:]]", "", sentence)
#                     # remove control characters
#                     sentence <- gsub("[[:cntrl:]]", "", sentence)
#                     # remove digits
#                     sentence <- gsub('\\d+', '', sentence)
# 
#                     #convert to lower
#                     sentence <- tolower(sentence)
# 
# 
#                     # split sentence into words with str_split (stringr package)
#                     word.list <- str_split(sentence, "\\s+")
#                     words <- unlist(word.list)
# 
#                     # compare words to the dictionaries of positive & negative terms
#                     pos.matches <- match(words, pos)
#                     neg.matches <- match(words, neg)
# 
#                     # get the position of the matched term or NA
#                     # we just want a TRUE/FALSE
#                     pos.matches <- !is.na(pos.matches)
#                     neg.matches <- !is.na(neg.matches)
# 
#                     # final score
#                     score <- sum(pos.matches) - sum(neg.matches)
#                     return(score)
#                   }, pos.words, neg.words, .progress=.progress )
#   # data frame with scores for each sentence
#   scores.df <- data.frame(text=sentences, score=scores)
#   return(scores.df)
# }
# #sentiment score
# scores_twitter <- score.sentiment(after$tweet, pos.txt, neg.txt, .progress='text')
# 
# 
# View(scores_twitter)
# 
# #Summary of the sentiment scores
# summary(scores_twitter)
# 
# scores_twitter$score_chr <- ifelse(scores_twitter$score < 0,'Negtive', ifelse(scores_twitter$score > 0, 'Positive', 'Neutral'))
# 
# 
# View(scores_twitter)
# 
# 
# #Convert score_chr to factor for visualizations
# scores_twitter$score_chr <- as.factor(scores_twitter$score_chr)
# names(scores_twitter)[3]<-paste("Sentiment")
# 
# #plot to show number of negative, positive and neutral comments
# Viz1 <- ggplot(scores_twitter, aes(x=Sentiment, fill=Sentiment))+ geom_bar(aes(y = (..count..)/sum(..count..))) +
#   scale_y_continuous(labels = percent)+labs(y="Score")+
#   theme(text =element_text(size=15))+theme(axis.text = element_text(size=15))+ theme(legend.position="none")+ coord_cartesian(ylim=c(0,0.6)) + scale_fill_manual(values=c("firebrick1", "grey50", "limeGREEN"))
# Viz1





#################################################################################################
#################################################################################################
# 9. Emotion detection
#################################################################################################
#################################################################################################
# remove emojis or special characters
# tweet_clean = gsub('<.*>', '', enc2native(before$tweet))
# 
# emotions <- get_nrc_sentiment(tweet_clean)
# emo_bar = colSums(emotions)
# emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
# emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
# 
# emo_sum <- emo_sum[1:8,]
# emo_sum$percent<-(emo_sum$count/sum(emo_sum$count))*100
# 
# #Visualize the emotions from NRC sentiments
# plot_ly(emo_sum, x=~emotion, y=~percent, type="bar", color=~emotion) %>%
#   layout(xaxis=list(title=""),  yaxis = list(title = "Emotion count"),
#          showlegend=FALSE,title="Distribution of emotion categories") %>%
#   layout(yaxis = list(ticksuffix = "%"))
# 
# tweet_clean = gsub('<.*>', '', enc2native(after$tweet))
# 
# emotions <- get_nrc_sentiment(tweet_clean)
# emo_bar = colSums(emotions)
# emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
# emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
# 
# emo_sum <- emo_sum[1:8,]
# emo_sum$percent<-(emo_sum$count/sum(emo_sum$count))*100
# 
# #Visualize the emotions from NRC sentiments
# plot_ly(emo_sum, x=~emotion, y=~percent, type="bar", color=~emotion) %>%
#   layout(xaxis=list(title=""),  yaxis = list(title = "Emotion count"),
#          showlegend=FALSE,title="Distribution of emotion categories") %>%
#   layout(yaxis = list(ticksuffix = "%"))





#################################################################################################
#################################################################################################
# Good Sentiment
#################################################################################################
#################################################################################################
# ew_sentiment<-get_nrc_sentiment((after$tweet))
# sentimentscores<-data.frame(colSums(ew_sentiment[,]))
# names(sentimentscores) <- "Score"
# sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
# rownames(sentimentscores) <- NULL
# ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
#   geom_bar(aes(fill=sentiment),stat = "identity")+
#   theme(legend.position="none")+
#   xlab("Sentiments")+ylab("Scores")+
#   ggtitle("Total sentiment based on scores")+
#   theme_minimal()






# PART 2 TWITTER WAY OF RETRIEVAL

#################################################################################################
#################################################################################################
# DATA RETRIEVAL
#################################################################################################
#################################################################################################
# twitter_token <- create_token(
#   app = 'TexMiningET',
#   consumer_key = 'PlF4pX0IxCPSWsbF0HwJbdKxH',
#   consumer_secret = 'zEKsGMcxstZUzynR1bKmerifhiXTqGljytD0Bu5Z4mmVPF4LW8',
#   set_renv = TRUE)
# Phoenix <- get_timeline("@JoaqPhoenix", n= 5000)


#Convert the timestamps of the tweets to the same time zone
# Phoenix$created_at <- ymd_hms(Phoenix$created_at)
# Phoenix$created_at <- with_tz(Phoenix$created_at, "America/New_York")
#Phoenix$created_at <-as.Date(Phoenix$created_at)
#Phoenix$year <- as.numeric(format(Phoenix$created_at, "%Y"))
#JP <- subset(Phoenix, year == '2020')


#################################################################################################
#################################################################################################
# Sentiment During the Year
#################################################################################################
#################################################################################################
# JP$clean_text <- str_replace_all(JP$text, "@\\w+", "")
# Sentiment <- get_nrc_sentiment(JP$clean_text)
# alltweets_senti <- cbind(JP, Sentiment)
# 
# alltweets_senti$month <- month(alltweets_senti$created_at, label = TRUE)
# monthlysentiment <- alltweets_senti %>% group_by(month) %>%
#   summarise(anger = mean(anger),
#             anticipation = mean(anticipation),
#             disgust = mean(disgust),
#             fear = mean(fear),
#             joy = mean(joy),
#             sadness = mean(sadness),
#             surprise = mean(surprise),
#             trust = mean(trust)) %>% melt
# 
# names(monthlysentiment) <- c("month", "sentiment", "meanvalue")
# 
# ggplot(data = monthlysentiment, aes(x = month, y = meanvalue, group = sentiment)) +
#   geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
#   geom_point(size = 0.5) +
#   ylim(0, NA) +
#   theme(legend.title=element_blank(), axis.title.x = element_blank()) +
#   ylab("Average sentiment score") +
#   ggtitle("Sentiment During the Year")


#################################################################################################
#################################################################################################
# Show twitter volume by month
#################################################################################################
#################################################################################################
# ggplot(data = Phoenix, aes(x = month(created_at, label = TRUE))) +
#   geom_bar(aes(fill = ..count..)) +
#   theme(legend.position = "none") +
#   xlab("Month") + ylab("Number of tweets") +
#   scale_fill_gradient(low = "midnightblue", high = "aquamarine4")


#################################################################################################
#################################################################################################
#Show twitter volume by hour
#################################################################################################
#################################################################################################
#extract days
# Phoenix$timeonly <- as.numeric(Phoenix$created_at - trunc(Phoenix$created_at, "days"))
# class(Phoenix$timeonly) <- "POSIXct"
# 
# ggplot(data = Phoenix, aes(x = timeonly)) +
#   geom_histogram(aes(fill = ..count..)) +
#   theme(legend.position = "none") +
#   xlab("Time") + ylab("Number of tweets") + 
#   scale_x_datetime(breaks = date_breaks("2 hours"), 
#                    labels = date_format("%H:00")) +
#   scale_fill_gradient(low = "midnightblue", high = "aquamarine4")



#################################################################################################
#################################################################################################
#SHOW WHEN THE TWEETS ARE PUBLISHED
#################################################################################################
#################################################################################################
# colnames(Phoenix)[colnames(Phoenix)=="screen_name"] <- "Twitter_Account"
# ts_plot(dplyr::group_by(Phoenix, Twitter_Account), "month") +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
#   ggplot2::labs(
#     x = NULL, y = NULL,
#     title = "Frequency of Tweets from Joaquin Phoenix",
#     subtitle = "Tweet counts aggregated by month",
#     caption = "\nSource: Data collected from Twitter's REST API via rtweet"
#   )



#################################################################################################
#################################################################################################
#SHOW THE ACCOUNTS FROM WHICH MOST RETWEETS ORIGINATE
#################################################################################################
#################################################################################################
# set.seed(1234)
# wordcloud(Phoenix$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25,
#           colors=brewer.pal(8, "Dark2"))