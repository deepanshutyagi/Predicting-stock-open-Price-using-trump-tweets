library(gdata);library(plyr);library(dplyr);library(lubridate);library(PerformanceAnalytics);library(tidyr);library(stringr);library(xts);library(readr);library(tidytext);library(PerformanceAnalytics);library(pbapply)
yahoo_read <- function(url){
     require(RCurl)
     if(url.exists(url)){
          dat <- read.table(url,header=TRUE,sep=",")
          df <- dat[,c(1,5)]
          df$Date <- as.Date(as.character(df$Date))
          return(df) 
     }
}

positive_words <- toupper(unlist(read.delim(file = 'C:/Users/neste/Google Drive/RWD/NLP/positive-words.txt', stringsAsFactors = F)));names(positive_words) <-NULL
positive_words <- positive_words[-c(1:32)]
negative_words <- toupper(unlist(read.delim(file = 'C:/Users/neste/Google Drive/RWD/NLP/negative-words.txt', stringsAsFactors = F)));names(negative_words) <-NULL
negative_words <- negative_words[-c(1:32)]

trump_tweets_df <- read_csv("C:/Users/neste/Google Drive/RWD/Twitter/trump/trump_tweets.csv") %>% 
     separate(created, c("Date", "Time"), sep = " ", remove = T) %>%
     mutate(Date = ymd(Date))

get_sentiment_score <- function(a){
     a_split <- unlist(strsplit(toupper(iconv(a)), " "))
     
     positive_count <- length(intersect(a_split, positive_words))
     negative_count <- length(intersect(a_split, negative_words))
     
     return(positive_count / (positive_count + negative_count))
}

get_capital_letters <- function(a){
     a_split <- unlist(strsplit(iconv(a), NULL))
     return(sum(str_detect(a_split, "[A-Z]"), na.rm = T))
}


#Get Trump Daily Sentiment
trump_sentiment_df <- trump_tweets_df %>% 
     rowwise() %>% 
     mutate(sentiment_score = get_sentiment_score(text)) %>% 
     mutate(capital_score = get_capital_letters(text)) %>% 
     ungroup() %>% 
     group_by(Date) %>% 
     summarise(daily_sentiment_score = mean(sentiment_score, na.rm = T), total_capital_letters = sum(capital_score, na.rm=T), retweets_favorites = sum(c(retweetCount, favoriteCount),na.rm=T))

trump_sentiment_df$daily_sentiment_score[is.na(trump_sentiment_df$daily_sentiment_score)] <- mean(trump_sentiment_df$daily_sentiment_score,na.rm=T)


ticker_list <- read_csv("C:/Users/neste/Google Drive/Stevens/FE-582(DataScience)/group_project/nyse.csv") %>% 
     select(Symbol) %>% 
     unlist
names(ticker_list) <- NULL

the_money_team <- function(ticker){
     cat(ticker); cat(' ')
     ticker_url <- paste0(paste0('http://real-chart.finance.yahoo.com/table.csv?s=',ticker,'&a=07&b=24&c=2010&d=12&e=22&f=2016&g=d&ignore=.csv'))
     
     ticker_df <- try(yahoo_read(ticker_url), silent = T)
     if(any(class(ticker_df) == "try-error", nrow(ticker_df) < 20, is.null(ticker_df))){
          return(NA)
     }
     ticker_df$returns <- CalculateReturns(ts(ticker_df$Close))
     
     
     trump_vs_ticker_df <- trump_sentiment_df %>% 
          inner_join(select(ticker_df, Date, returns), by = "Date")
     cor(trump_vs_ticker_df[,-1])
     
     fit <- try(lm(returns ~., data = trump_vs_ticker_df[,-1]), silent = T)
     if(class(fit) == "try-error"){
          return(NA)
     }
     rsq <- summary(fit)$r.squared
     return(rsq)
}

stocks_to_invest <- pbsapply(ticker_list, the_money_team)

stocks_to_invest_df <- data.frame(ticker = names(stocks_to_invest), rsq = stocks_to_invest, stringsAsFactors = F)

write.csv(stocks_to_invest_df, file = "C:/Users/neste/Google Drive/RWD/Twitter/trump/trump_etf.csv")

#### Trump word frequencies


summary(sapply(trump_tweets_df$text, get_sentiment_score))
which(sapply(trump_tweets_df$text, get_capital_letters) == 85)


data("stop_words")
tmp_word_freq <- beer_style_ratings %>% 
     unnest_tokens(output = word, beer_review) %>% 
     anti_join(stop_words)

trump_tweets_tidy <- trump_tweets_df %>% 
     mutate(text = toupper(iconv(text))) %>% 
     unnest_tokens(output = word, text) %>% 
     anti_join(stop_words) %>% 
     count(word, sort=T) %>% 
     ungroup()

write_csv(x = trump_tweets_tidy, path = "C:/Users/neste/Google Drive/RWD/Twitter/trump/trump_freq_words.csv")


length(intersect(trump_tweets_tidy$word, tolower(negative_words)))
