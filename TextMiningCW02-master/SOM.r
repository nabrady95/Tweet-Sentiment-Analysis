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

# Load the datasets
tweets_joker <- read.csv('C:/Users/alexa/Desktop/TextMiningCW2/Datasets/joker_data.csv')
tweets_phoenix <- read.csv('C:/Users/alexa/Desktop/TextMiningCW2/Datasets/phoenix_data.csv')
tweets_parasite <- read.csv('C:/Users/alexa/Desktop/TextMiningCW2/Datasets/parasite_data.csv')
pos <- readLines("C:/Users/alexa/Desktop/TextMiningCW2/Datasets/positive_words.txt")
neg <- readLines("C:/Users/alexa/Desktop/TextMiningCW2/Datasets/negative_words.txt")

# Order the tweets based on the favorite_count and get the 5000 most popular
tweets_joker_ordered<-tweets_joker[order(tweets_joker$likes_count, decreasing = TRUE),]
tweets_joker_selection<-head(tweets_joker_ordered,5000)
tweets_phoenix_ordered<-tweets_phoenix[order(tweets_phoenix$likes_count, decreasing = TRUE),]
tweets_phoenix_selection<-head(tweets_phoenix_ordered,5000)
tweets_parasite_ordered<-tweets_parasite[order(tweets_parasite$likes_count, decreasing = TRUE),]
tweets_parasite_selection<-head(tweets_parasite_ordered,5000)

# Extract only the text of the tweets
tweets_joker.df <- tweets_joker_selection$tweet
tweets_phoenix.df <- tweets_phoenix_selection$tweet
tweets_parasite.df <- tweets_parasite_selection$tweet

# Transform "created_at" column
created_at_joker<- as.POSIXct(paste(tweets_joker$date, tweets_joker$time), format="%Y-%m-%d %H:%M:%S")
tweets_joker$created_at<-created_at_joker
created_at_phoenix<- as.POSIXct(paste(tweets_phoenix$date, tweets_phoenix$time), format="%Y-%m-%d %H:%M:%S")
tweets_phoenix$created_at<-created_at_phoenix
created_at_parasite<- as.POSIXct(paste(tweets_parasite$date, tweets_parasite$time), format="%Y-%m-%d %H:%M:%S")
tweets_parasite$created_at<-created_at_parasite


# # Create a time series object 
joker_ts<-ts_data(tweets_joker, by='hours')
names(joker_ts)<- c("time","joker_n")
phoenix_ts<-ts_data(tweets_phoenix, by='hours')
names(phoenix_ts)<- c("time","phoenix_n")
parasite_ts<-ts_data(tweets_phoenix, by='hours')
names(parasite_ts)<- c("time","parasite_n")

# Merge the two time series objects
merged_df<-merge(parasite_ts, phoenix_ts, by ='time', all=TRUE)
melt_df<-melt(merged_df, na.rm=TRUE, id.vars='time')

# Plot frequency of tweets on Joker and Parasite
ggplot(data = melt_df,
       aes(x = time, y = value, col = variable))+
  geom_line(lwd = 0.8)

######################################################################
# Processing / Cleaning

# Remove the tweet text URLs
tweets_parasite_clean<- clear_tweets_df(tweets_parasite.df)
tweets_joker_clean <- clear_tweets_df(tweets_joker.df)
tweets_phoenix_clean <- clear_tweets_df(tweets_phoenix.df)

# Remove special char,punctuations & number
tweets_parasite_clean<-gsub("[^A-Za-z]"," ",tweets_parasite_clean)
tweets_joker_clean<-gsub("[^A-Za-z]"," ",tweets_joker_clean)
tweets_phoenix_clean<-gsub("[^A-Za-z]"," ",tweets_phoenix_clean)

# LowerCase
tweets_parasite_clean <- tolower(tweets_parasite_clean)
tweets_joker_clean <- tolower(tweets_joker_clean)
tweets_phoenix_clean <- tolower(tweets_phoenix_clean)

# Remove punctuations
tweets_parasite_clean <- removePunctuation(tweets_parasite_clean)
tweets_joker_clean <- removePunctuation(tweets_joker_clean)
tweets_phoenix_clean <- removePunctuation(tweets_phoenix_clean)

# remove stopwords
tweets_parasite_clean <- removeWords(tweets_parasite_clean, stopwords("english"))
tweets_joker_clean <- removeWords(tweets_joker_clean, stopwords("english"))
tweets_phoenix_clean <- removeWords(tweets_phoenix_clean, stopwords("english"))

# Removing custom stop words
custom_stop <- c("movie","parasite","s","t","m","y","re","get","re",
                 "also","can","amp","one","like","will","go",
                 "know","us","just","now","ve","got","haven",
                 "won","don","ho","get","say","even","didn",
                 "oscars","oscar","nominees","academyawards",
                 "theoscars","awards","movie", "night", "film",
                 "carpet", "red","actor","actress")
tweets_parasite_clean <- removeWords(tweets_parasite_clean, custom_stop)
tweets_joker_clean <- removeWords(tweets_joker_clean, custom_stop)
tweets_phoenix_clean <- removeWords(tweets_phoenix_clean, custom_stop)


# Remove unecessary spaces
tweets_parasite_clean <- stripWhitespace(tweets_parasite_clean)
tweets_joker_clean <- stripWhitespace(tweets_joker_clean)
tweets_phoenix_clean <- stripWhitespace(tweets_phoenix_clean)


# Convert to corpus
tweets_parasite_corpus<-tweets_parasite_clean %>%
  VectorSource() %>%
  Corpus()
tweets_joker_corpus<-tweets_joker_clean %>%
  VectorSource() %>%
  Corpus()
tweets_phoenix_corpus<-tweets_phoenix_clean %>%
  VectorSource() %>%
  Corpus()

###########################################################################
# Extract top 50 terms from corpus
term_counts_parasite <- freq_terms(tweets_parasite_corpus,50)
term_counts_joker <- freq_terms(tweets_joker_corpus,50)
term_counts_phoenix <- freq_terms(tweets_phoenix_corpus,50)

# Create a subset dataframe
parasite_50<- subset(term_counts_parasite, FREQ > 50)
joker_50<-subset(term_counts_joker, FREQ > 50)
phoenix_50<-subset(term_counts_phoenix, FREQ > 50)

# Create a bar plot of frequent items
ggplot(parasite_50, aes(x = reorder(WORD, -FREQ), y = FREQ)) +
  geom_bar(stat='identity', fill = 'blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(joker_50, aes(x = reorder(WORD, -FREQ), y = FREQ)) +
  geom_bar(stat='identity', fill = 'red') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(phoenix_50, aes(x = reorder(WORD, -FREQ), y = FREQ)) +
  geom_bar(stat='identity', fill = 'red') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a WordCloud
wordcloud(tweets_parasite_corpus, min.freq = 20, colors = 'blue',
          scale = c(3,0.5), random.order = FALSE)

wordcloud(tweets_joker_corpus, min.freq = 20, colors = 'red',
          scale = c(3,0.5), random.order = FALSE)

wordcloud(tweets_phoenix_corpus, min.freq = 20, colors = 'green',
          scale = c(3,0.5), random.order = FALSE)

# More colorful WordCloud
wordcloud(tweets_parasite_corpus, max.words = 100,
          colors = brewer.pal(6,"Dark2"), scale = c(2.5,.5),
          random.order = FALSE)

wordcloud(tweets_joker_corpus, max.words = 100,
          colors = brewer.pal(6,"Dark2"), scale = c(2.5,.5),
          random.order = FALSE)

wordcloud(tweets_phoenix_corpus, max.words = 100,
          colors = brewer.pal(6,"Dark2"), scale = c(2.5,.5),
          random.order = FALSE)
############################################################################
# Sentiment Analysis

sa_parasite.value<- get_nrc_sentiment(tweets_parasite_clean)
sa_joker.value<- get_nrc_sentiment(tweets_joker_clean)
sa_phoenix.value<- get_nrc_sentiment(tweets_phoenix_clean)

# Calculating the sum of sentiment scores
score_parasite <- colSums(sa_parasite.value[,])
score_joker <- colSums(sa_joker.value[,])
score_phoenix <- colSums(sa_phoenix.value[,])

# Convert into a dataframe
score_parasite_df <- data.frame(score_parasite)
score_joker_df <- data.frame(score_joker)
score_phoenix_df <- data.frame(score_phoenix)

sa_parasite.score <- cbind(sentiment = row.names(score_parasite_df),
                           score_parasite_df, row.names = NULL)

sa_joker.score <- cbind(sentiment = row.names(score_joker_df),
                        score_joker_df, row.names = NULL)

sa_phoenix.score <- cbind(sentiment = row.names(score_joker_df),
                          score_phoenix_df, row.names = NULL)

ggplot(data = sa_parasite.score, aes(x = sentiment, y = score_parasite,
                                     fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = sa_joker.score, aes(x = sentiment, y = score_joker,
                                  fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = sa_phoenix.score, aes(x = sentiment, y = score_phoenix,
                                    fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############################################################################
# Sentiment score aggregated


#sentiment score
scores_parasite <- score.sentiment(tweets_parasite_clean, pos.txt, neg.txt, .progress='text')
scores_joker <- score.sentiment(tweets_joker_clean, pos.txt, neg.txt, .progress='text')
scores_phoenix <- score.sentiment(tweets_phoenix_clean, pos.txt, neg.txt, .progress='text')

scores_parasite$score_chr <- ifelse(scores_parasite$score < 0,'Negative', ifelse(scores_parasite$score > 0, 'Positive', 'Neutral'))
scores_joker$score_chr <- ifelse(scores_joker$score < 0,'Negative', ifelse(scores_joker$score > 0, 'Positive', 'Neutral'))
scores_phoenix$score_chr <- ifelse(scores_phoenix$score < 0,'Negative', ifelse(scores_phoenix$score > 0, 'Positive', 'Neutral'))

#Convert score_chr to factor for visualizations
scores_parasite$score_chr <- as.factor(scores_parasite$score_chr)
names(scores_parasite)[3]<-paste("Sentiment")
scores_joker$score_chr <- as.factor(scores_joker$score_chr)
names(scores_joker)[3]<-paste("Sentiment")  
scores_phoenix$score_chr <- as.factor(scores_phoenix$score_chr)
names(scores_phoenix)[3]<-paste("Sentiment")  

#plot to show number of negative, positive and neutral comments
score_visual <- ggplot(scores_parasite, aes(x=Sentiment, fill=Sentiment))+ geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+labs(y="Score")+
  theme(text =element_text(size=15))+theme(axis.text = element_text(size=15))+ theme(legend.position="none")+ coord_cartesian(ylim=c(0,0.6)) + scale_fill_manual(values=c("firebrick1", "grey50", "limeGREEN"))
score_visual

score_visual <- ggplot(scores_joker, aes(x=Sentiment, fill=Sentiment))+ geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+labs(y="Score")+
  theme(text =element_text(size=15))+theme(axis.text = element_text(size=15))+ theme(legend.position="none")+ coord_cartesian(ylim=c(0,0.6)) + scale_fill_manual(values=c("firebrick1", "grey50", "limeGREEN"))
score_visual

score_visual <- ggplot(scores_phoenix, aes(x=Sentiment, fill=Sentiment))+ geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = percent)+labs(y="Score")+
  theme(text =element_text(size=15))+theme(axis.text = element_text(size=15))+ theme(legend.position="none")+ coord_cartesian(ylim=c(0,0.6)) + scale_fill_manual(values=c("firebrick1", "grey50", "limeGREEN"))
score_visual
