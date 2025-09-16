# JAMES TAYLOR


setwd('/Users/jean_taylor_1/Downloads')

df <- read.csv('Donald-Trump_7375-Tweets-Excel.csv')

df$Date = NULL
df$Time = NULL
df$Type = NULL
df$Media_Type = NULL
df$Hashtags = NULL
df$Tweet_Id = NULL
df$Tweet_Url = NULL
df$twt_favourites_IS_THIS_LIKE_QUESTION_MARK = NULL
df$Retweets = NULL
df$x = NULL
df$X.1 = NULL 
df$X = NULL


df$Tweet_Text <- as.character(df$Tweet_Text)

tweets <- as.character(df$Tweet_Text) 

dtTxtDf <- tibble (line = 1:7375, text = tweets)

dtWords <- dtTxtDf %>% unnest_tokens(word, text, token = "tweets")

dtWords <- dtWords %>% count(word, sort = TRUE)

dt.at.symbol <- subset(dtWords, grepl('@', word))

dt.hashtag <- subset(dtWords, grepl('#', word))

write.csv(dt.at.symbol,"PATH WHERE YOU WANT CSV TO BE PLACED", row.names = FALSE)
write.csv(dt.hashtag,"PATH WHERE YOU WANT CSV TO BE PLACED", row.names = FALSE)
