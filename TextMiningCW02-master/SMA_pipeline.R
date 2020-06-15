# Load the libraries
library(rtweet)
library(twitteR)
library(plyr)
library(stringr)
library(ggplot2)
library(stringi)
library(wordcloud)
library(tm)
library(devtools)
library(reshape)
library(qdapRegex)
library(qdap)
library(RColorBrewer)
library(syuzhet)
library(hunspell)
library(scales)
source("D:/Msc_AI_UoM/Semester 2/Text mining/cw02/my_functions.R")

# Load the datasets
# tweets <- read.csv('D:/Msc_AI_UoM/Semester 2/Text mining/cw02/joker_data.csv')
# tweets <- read.csv('D:/Msc_AI_UoM/Semester 2/Text mining/cw02/phoenix_data.csv')
tweets <- read.csv('D:/Msc_AI_UoM/Semester 2/Text mining/cw02/parasite_data.csv')
pos <- readLines("D:/Msc_AI_UoM/Semester 2/Text mining/cw02/positive_words.txt")
neg <- readLines("D:/Msc_AI_UoM/Semester 2/Text mining/cw02/negative_words.txt")

# Order the tweets based on the favorite_count and get the 1000 most popular
tweets_ordered<-tweets[order(tweets$likes_count, decreasing = TRUE),]
tweets_selection<-head(tweets_ordered,10000)

# Extract only the text of the tweets
tweets.df <- tweets_selection$tweet

# Transform "created_at" column
created_at_tweets<- as.POSIXct(paste(tweets$date, tweets$time), format="%Y-%m-%d %H:%M:%S")
tweets$created_at<-created_at_tweets

# Create a time series object
tweets_ts<-ts_data(tweets, by='hours')
names(tweets_ts)<- c("time","tweets_n")

# # Merge the two time series objects
# merged_df<-merge(tweets_ts, by ='time', all=TRUE)
# melt_df<-melt(merged_df, na.rm=TRUE, id.vars='time')
# 
# # Plot frequency of tweets on Joker and Parasite
# ggplot(data = melt_df,
#        aes(x = time, y = value, col = variable))+
#   geom_line(lwd = 0.8)

######################################################################

# Processing / Cleaning
tweets_clean <- cleanse_them_tweets(tweets.df)

# Convert to corpus
tweets_corpus<-tweets_clean %>%
  VectorSource() %>%
  Corpus()


###########################################################################

# Extract top 50 terms from corpus
term_counts_tweets <- freq_terms(tweets_corpus,50)

# Create a subset dataframe
tweets_50<-subset(term_counts_tweets, FREQ > 50)

# Create a bar plot of frequent items
ggplot(tweets_50, aes(x = reorder(WORD, -FREQ), y = FREQ)) +
  geom_bar(stat='identity', fill = 'red') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Create a WordCloud
wordcloud(tweets_corpus, min.freq = 20, colors = 'red',
          scale = c(3,0.5), random.order = FALSE)

# More colorful WordCloud
wordcloud(tweets_corpus, max.words = 100,
          colors = brewer.pal(6,"Dark2"), scale = c(2.5,.5),
          random.order = FALSE)
############################################################################
# Sentiment Analysis
sa_tweets.value<- get_nrc_sentiment(tweets_clean)

# Calculating the sum of sentiment scores
score_tweets <- colSums(sa_tweets.value[,])

# Convert into a dataframe
score_tweets_df <- data.frame(score_tweets)
sa_tweets.score <- cbind(sentiment = row.names(score_tweets_df),
                        score_tweets_df, row.names = NULL)


ggplot(data = sa_tweets.score, aes(x = sentiment, y = score_tweets,
                                  fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############################################################################
# Sentiment score aggregated


#sentiment score
tweets_scores <- score.sentiment(tweets_clean, pos.txt, neg.txt, .progress='text')
tweets_scores$score_chr <- ifelse(tweets_scores$score < 0,'Negative', ifelse(tweets_scores$score > 0, 'Positive', 'Neutral'))

#Convert score_chr to factor for visualizations
tweets_scores$score_chr <- as.factor(tweets_scores$score_chr)
names(tweets_scores)[3]<-paste("Sentiment")  

#plot to show number of negative, positive and neutral comments
score_visual <- ggplot(tweets_scores, aes(x=Sentiment, fill=Sentiment))+ geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+labs(y="Score")+
  theme(text =element_text(size=15))+theme(axis.text = element_text(size=15))+ theme(legend.position="none")+ coord_cartesian(ylim=c(0,0.6)) + scale_fill_manual(values=c("firebrick1", "grey50", "limeGREEN"))
score_visual


