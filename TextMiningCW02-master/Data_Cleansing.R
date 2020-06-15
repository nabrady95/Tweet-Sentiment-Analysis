library(hunspell)
############################################################################################################################
############################################################################################################################
############################################################################################################################

#function to calculate sentiment score
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores <- laply(sentences,
                  function(sentence, pos.words, neg.words)
                  {
                    # remove punctuation
                    sentence <- gsub("[[:punct:]]", "", sentence)
                    # remove control characters
                    sentence <- gsub("[[:cntrl:]]", "", sentence)
                    # remove digits
                    sentence <- gsub('\\d+', '', sentence)
                    
                    #convert to lower
                    sentence <- tolower(sentence)
                    
                    
                    # split sentence into words with str_split (stringr package)
                    word.list <- str_split(sentence, "\\s+")
                    words <- unlist(word.list)
                    
                    # compare words to the dictionaries of positive & negative terms
                    pos.matches <- match(words, pos)
                    neg.matches <- match(words, neg)
                    
                    # get the position of the matched term or NA
                    # we just want a TRUE/FALSE
                    pos.matches <- !is.na(pos.matches)
                    neg.matches <- !is.na(neg.matches)
                    
                    # final score
                    score <- sum(pos.matches) - sum(neg.matches)
                    return(score)
                  }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df <- data.frame(text=sentences, score=scores)
  return(scores.df)
}

############################################################################################################################
############################################################################################################################
############################################################################################################################

#function to perform basic tweet cleaning
clear_tweets_df <- function(tweets.df)
{
  tweets.df = gsub('https\\S+\\s*', '', tweets.df) ## Remove URLs
  tweets.df = gsub('http\\S+\\s*', '', tweets.df) ## Remove URLs
  tweets.df = gsub('pic\\S+\\s*', '', tweets.df) ## Remove URLs
  tweets.df = gsub('www.\\S+\\s*', '', tweets.df) ## Remove URLs
  tweets.df = gsub('\\b+RT', '', tweets.df) ## Remove RT
  tweets.df = gsub('#\\S+', '', tweets.df) ## Remove Hashtags
  tweets.df = gsub('@\\S+', '', tweets.df) ## Remove Mentions
  tweets.df = gsub('[[:cntrl:]]', '', tweets.df) ## Remove Controls and special characters
  tweets.df = gsub("\\d", '', tweets.df) ## Remove Controls and special characters
  return(tweets.df)
}

############################################################################################################################
############################################################################################################################
############################################################################################################################

#function to perform basic tweet cleaning
remove_slang_misspells <- function(tweets_movie_clean)
{
  # Collect all slang/misspelled words
  #get all tweets as one string
  oneTweet<-toString(tweets_movie_clean)
  # get list of all words
  words <- strsplit(oneTweet, " ")[[1]]
  
  badWords<-hunspell(words)
  badWords<-badWords[lapply(badWords,length)>0]
  badWords<-tolower(badWords)
  
  #split into groups as too large to remove all at once
  group <- 100
  n <- length(badWords)
  r <- rep(1:ceiling(n/group),each=group)[1:n]
  d <- split(badWords,r)
  
  # Remove "bad words" and slang
  for (i in 1:length(d)) {
    
    tweets_movie_clean <- removeWords(tweets_movie_clean, c(paste(d[[i]])))
    
  }
  return(tweets_movie_clean)
}

############################################################################################################################
############################################################################################################################
############################################################################################################################

# Cleaning procedure
cleanse_them_tweets <- function(tweets.df)
{
  # Remove the tweet text URLs
  tweets_movie_clean<- clear_tweets_df(tweets.df)

  # Remove special char,punctuations & number
  tweets_movie_clean<-gsub("[^A-Za-z]"," ",tweets_movie_clean)

  # LowerCase
  tweets_movie_clean <- tolower(tweets_movie_clean)

  # Remove punctuations
  tweets_movie_clean <- removePunctuation(tweets_movie_clean)

  # Remove stopwords
  tweets_movie_clean <- removeWords(tweets_movie_clean, stopwords("english"))
  
  # Removing custom stop words
  custom_stop <- c("movie","parasite","s","t","m","y","re","get","re",
                   "also","can","amp","one","like","will","go",
                   "know","us","just","now","ve","got","haven",
                   "won","don","ho","get","say","even","didn",
                   "oscars","oscar","nominees","academyawards",
                   "theoscars","awards","movie", "night", "film",
                   "carpet", "red","actor","actress")
  tweets_movie_clean <- removeWords(tweets_movie_clean, custom_stop)
  
  # Remove unecessary spaces
  tweets_movie_clean <- stripWhitespace(tweets_movie_clean)

  
  # Remove duplicates
  tweets_movie_clean <- unique(tweets_movie_clean)

  # Remove slang/misspells
  tweets_movie_clean <- remove_slang_misspells(tweets_movie_clean)
  
  return(tweets_movie_clean)

}
