#Original Author Selam 3/17
#Last Updated by Jeffrey Watson on 4/17/2020 @ 0005hrs EST
#Last Updates: Added time analysis, comments, cleaned up some repetative code, added angry clouds
#Code is tested full run
#Note run the code slowly and allow processing to complete or there is a chance of crashing RStudio


###########Set Up and Initial Analysis#####################

#1. Load you libraries and stop words
# Load libraries
library(dplyr)
library(tidytext)
library(tm)
library(tidyr)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

# Load stop words
data("stop_words")

#2. Set your working space
#set your working directory and read desired file
setwd("~/UMUC/DATA630/Team5")
dt <- read.csv("Donald-Tweets!.csv")
# Read the source file
#dt <- read.csv("C:/Users/Seli/Desktop/SelfDev/umuc630/Project/Donald-Tweets.csv")


#3. Data preprocessing - clean up
# Remove other attributes from the dataset
dt$Date = NULL
dt$Time = NULL
dt$Type = NULL
dt$Media_Type = NULL
dt$Hashtags = NULL
dt$Tweet_Id = NULL
dt$Tweet_Url = NULL
dt$twt_favourites_IS_THIS_LIKE_QUESTION_MARK = NULL
dt$Retweets = NULL
dt$x = NULL
dt$X.1 = NULL 
dt$X = NULL

# View the final dataset after clean up
View(dt)
glimpse(dt)
# 7,375 rows


#4. Create your data frame to analyze
# Create a variable with text as char by reading the tweet column
dtTxt <- as.character(dt$Tweet_Text)
# Check the dataset
View(dtTxt)

# Create dataframe using tibble which cleans any unneccessary formating problems and makes df 'modern'
#lines included below came from 'View(dtTxt)' above
dtTxtDf <- tibble (line = 1:7375, text = dtTxt)
# View the dataframe 
View(dtTxtDf)
str(dtTxtDf)
# View changes in console
dtTxtDf
###Developer note.  This is the end of the 'baseline code'  IF you have not run this code, the follow on modules/steps will
###not work.




#5. Convert and split the words in df as tokens
# Jeff modification for tweet specific aspect.  Alternate token options available, check out tidy libraries for details.
dtWords <- dtTxtDf %>% unnest_tokens(word, text, token = "tweets")

#6. Remove stop words from data frame to be analyzed
#make a custom stoplist for 'undesirable words'. Bind it to the pre-defined 'stop_words'.  Here we've removed the twitter cusomization for a URL link, https, amp
#note: 'rt' was originally in this list.  Jeff reincluded because this is short for 'retweet', which is relevant to the core question of how trump uses twitter.
dtCustomStopWords <- bind_rows(tibble(word=c("t.co", "https", "amp", "http"), 
                                      lexicon = c("custom")), stop_words)

#who your custom stop list and validate list.
dtCustomStopWords

# Remove common stop words from the dataframe 
dtWordStopWords <- dtWords %>% anti_join(stop_words, by = c("word" = "word"))
# View result in console
dtWordStopWords

# Remove additional common words using custom stop words
dtWord <- dtWords %>% anti_join(dtCustomStopWords, by = c("word" = "word"))
# View result in console
dtWord

#7. Count and sort the residual words
# Sort words with count
dtWordSorted <- dtWord %>% count(word, sort = TRUE)
# Sorted words view
dtWordSorted


class(dtWordSorted)

#8. Visualize top 8 word frequency
# Graph words with high frequency 
dtWord %>% 
  count(word, sort = TRUE) %>% 
  filter (n> 400) %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n))+ geom_col() +
  xlab(NULL) + coord_flip()
###Finding: In the top 8 findings, '@realdonaldtrump' and 'trump'are a dominating frequency.
###Finding: In the top 8 findings, '#trump2016 and #makeamericagreatagain, are used.  Which may not be surprising but shows the outsized impact of #
###Finding: In the top 8 findings, trump retweets 
###Finding: In the top 8 findings, trump references his oponent 'hillary' is only second to self-referencing
###Finding: In the top 8 findings, trump reference the buzz term 'america' and 'people' the most only after self-referening, 
### 


#9. Now that there is a strong and normalized data set. Apply sentiment by identifying positive and negative constructs from the nrc lexicon.
#then innerjoin with bing 

# Identify positive words from nrc library
nrc_pos <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")
# Identify negative words from nrc library 
nrc_neg <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")

# Count words considered positive with nrc library on the tweet. 1292 positive words
dtWord %>% inner_join(nrc_pos) %>%
  count(word, sort = TRUE)

# Count words considered negative with nrc library on the tweet. 1,256 negative words 
dtWord %>% inner_join(nrc_neg) %>%
  count(word, sort = TRUE)

###Finding this library is questionable and needs further research...'john' is listed as negative 61 times.  Why?
###NRC documentation is here: https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm

# Determine if word is positive or negative words using bing sentiment framework 
dtSentiment <- dtWord %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE)%>%
  spread (sentiment, n, fill = 0) %>% 
  mutate (sentiment = positive - negative )
# View result 
dtSentiment

# Sentiment count using bing
dtSentimentCounts <- dtWord %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE)%>%
  ungroup()
# View result which is a list of words classified as positive or negative with a frequency count
dtSentimentCounts

# Graph sentiment of top 15 counts as a comparison graph.  One for negative other for positive
dtSentimentCounts %>% group_by(sentiment) %>%
  top_n(15) %>% ungroup() %>% 
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to Sentiment", 
       x = NULL) + coord_flip()
###Finding: 'trump' is an overwieght positive term
###Finding: 'crooked' is the top negative sentiment used.  Expect further analysis will link 'crooked' with "Hillary" and/or "clinton"

dtSentimentCounts %>% group_by(sentiment) %>%
  top_n(15) %>% ungroup() %>% 
  mutate(sentiment = reorder(sentiment, n)) %>%
  ggplot(aes(sentiment, n, fill=sentiment )) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to Sentiment", 
       x = NULL) + coord_flip()
###Finding.  Clearly there are more positive sentiments than negative.


#Tally the sentiment counts.  
dtSentimentCounts %>% group_by(sentiment) %>% tally()
###Finding: Negative = 722 (~62.3%) / Positive = 437(~37.7%)

###Finding: Overall Trump speaks more positively than negatively.###
###Finding: When negative he uses the word "crooked" the most###
###Finding: When postitive he uses his own name the most###

#10.
################################ Create word cloud#######################################
# Ensure to have enough space on plot to see all the words 

#set seed for reproducability
set.seed(4321)
dtWord %>% count(word) %>% with(wordcloud(word, n, max.words = 100,
                                          colors = brewer.pal(8, "Dark2")))
#visual
#wordcloud

#11.
###############What @is used the most?##################################
#In this section we will make a list and then a word cloud
#Reload the sorted word variable used earlier.  You don't have to do this, but its a good error check
dtWordSorted

#make a list of talkers by finding all the words from dtWordSorted that starts with '@' which tweetish for a twitter entitiy
theTalkers <-dtWordSorted %>% 
  filter(grepl("^@", tolower(word)))
#validate the talkers
theTalkers

#Let's make a word cloud for people referenced, aka "the talkers"
#set for reproducability
set.seed(4321)
wordcloud(words = theTalkers$word, freq = theTalkers$n, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
###Finding: while a LOT of people are referenced, Trump is by FAR the most. 5.5x more than the next entity###

#12.
##########What hashtag '#' is used the most?###############'
#Create variable 'hashtag' using the dtsorted word variable and all words that start with '#'
# this is 'rinse and repeat' of above but for hashtags
hashtags <-dtWordSorted %>% 
  filter(grepl("^#", tolower(word)))
hashtags

set.seed(4321)
wordcloud(words = hashtags$word, freq = hashtags$n, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
###Finding: There is a clear use of two hastags above all others, '#trump2016' and '#makeamericagreatagain'

#13.
####################Bigrams section###############
##
# Create a bygram using the dtword list as a base.  Tokenize the data using 'ngrams'
dtBigrams <- dtWord %>% 
  unnest_tokens(bigram, word, token = "ngrams", n = 2)

#Remove undesirable words from bigram column A and B
dtBigrams <- dtBigrams %>%
  separate(bigram, into = c("WordA", "WordB"), sep = " ", remove = FALSE) %>%
  anti_join(dtCustomStopWords, by =c("WordA" = "word"))%>%
  anti_join(dtCustomStopWords, by =c("WordB" = "word"))

# view the bigram results
dtBigrams  
# sort the bigrams with count
dtBigrams %>% count(bigram, sort = TRUE)

# sort merged words graphically
dtBigrams %>%
  count(bigram, sort = TRUE) %>% 
  filter (n> 100) %>% 
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n))+ geom_col() +
  xlab(NULL) + coord_flip()
  
# Seperate bigrams words
dtBigramsSeparate <- dtBigrams %>%
  separate(bigram, c("WordA", "WordB"), sep = " ")

# View result
dtBigramsSeparate

# Count separated words 
dtBigramsSeparateCount <- dtBigramsSeparate %>%
  count(WordA, WordB, sort = TRUE)

# View results
dtBigramsSeparateCount
#Finding: 5 of the top 10 bigrams are people, specifically presidential candidates
#Finding: 2 of the top 10 bigrams are the top 2 hashtags '#trump2016' and '#makeamericagreatagain'
#Finding: retweeting danscavino was number 10.  https://en.wikipedia.org/wiki/Dan_Scavino.

#14.
##Now let's explore the bigram a bit.  In the english language the analysis must consider the arrangement of adjectives/adverbs vs nouns
## Ex. green tree, green=adjective and tree=noun
# As hillary was 2 of the top 3 bigrams...explore how many times hillary is referenced as the second word, aka 'prefix 'hillary'"
dtBigramsSeparate %>% 
  filter (WordB=="hillary")%>%
  count (WordA, WordB, sort = TRUE)
###Finding: 'crooked hillary' counted 193.  6.65x more than any other bigram

# Explore how many time hillary is referenced
dtBigramsSeparate %>% 
  filter (WordA=="hillary")%>%
  count (WordA, WordB, sort = TRUE)
###Finding: Nothing significant

# Explore how many time huge is referenced as an adjective
dtBigramsSeparate %>% 
  filter (WordA=="huge")%>%
  count (WordA, WordB, sort = TRUE)
###Finding: Trump uses the term often, but the term 'crowd' is the most frequent

# Explore how many time cnn is referenced
dtBigramsSeparate %>% 
  filter (WordB=="cnn")%>%
  count (WordA, WordB, sort = TRUE)
###Finding: Trump refers to himself via '@realdonaldtrump' when mentioning cnn

# bigrams tf idf values
dtBigramstf <- dtBigrams %>%
  count(bigram, sort = TRUE) %>% bind_tf_idf(bigram, bigram, n)%>%
  arrange(desc(tf_idf))
# view results
dtBigramstf

#15.
#########trigram variabion: by Jeff#############
##now we will do the same thing, but with a trigram in hopes the information will be more complete for a sense of context.
dttrigrams <- dtWord %>% 
  unnest_tokens(trigram, word, token = "ngrams", n = 3)

dttrigrams <- dttrigrams %>%
  separate(trigram, into = c("WordA", "WordB", "WordC"), sep = " ", remove = FALSE) %>%
  anti_join(dtCustomStopWords, by = c("WordA" = "word"))%>%
  anti_join(dtCustomStopWords, by = c("WordB" = "word"))%>%
  anti_join(dtCustomStopWords, by = c("WordC" = "word"))

# view the tigram results
dttrigrams 

# count and sort the trigrams 
dttrigrams %>% count(trigram, sort = TRUE)

# sorted merged words grapically
dttrigrams %>%
  count(trigram, sort = TRUE) %>% 
  filter (n> 100) %>% 
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n))+ geom_col() +
  xlab(NULL) + coord_flip()
###Finding: there are 42,085 rows.  Of those they are counted as 11 or less events below the top 10

# Seperate trigram words
dttrigramsSeparate <- dttrigrams %>%
  separate(trigram, c("WordA", "WordB", "WordC"), sep = " ")
# View result
dttrigramsSeparate
# Count separated words 
dttrigramsSeparateCount <- dttrigramsSeparate %>%
  count(WordA, WordB, WordC, sort = TRUE)
# View results
dttrigramsSeparateCount
###Finding: These results are more resonating with topics discussed in the news
###Finding: 'crooked hillary clinton' is disproportionately used compared to any other trigram by 3.6 times.
###Finding: 'saturday night live' is the 6th most frequent trigram
###Finding: 3 of the top 4 terms reference a 'nick name' with one of his opponents.  Ex. 'goofy elizabeth warren'

# Explore how many time hillar is referenced
dttrigramsSeparate %>% 
  filter (WordB=="hillary")%>%
  count (WordA, WordB, WordC, sort = TRUE)

# Explore how many time hillar is referenced
dttrigramsSeparate %>% 
  filter (WordA=="hillary")%>%
  count (WordA, WordB,WordC, sort = TRUE)

# Explore how many time huge is referenced
dttrigramsSeparate %>% 
  filter (WordA=="huge")%>%
  count (WordA, WordB,WordC, sort = TRUE)

# Explore how many time cnn is referenced
dttrigramsSeparate %>% 
  filter (WordB=="cnn")%>%
  count (WordA, WordB,WordC, sort = TRUE)

# bigrams tf idf values
dttrigramstf <- dttrigrams %>%
  count(trigram, sort = TRUE) %>% bind_tf_idf(trigram, trigram, n)%>%
  arrange(desc(tf_idf))
# view results
dttrigramstf
###Finding: Top 10 trump trigram tweets always contain a website link, with 1 exception "crooked hillary clinton"###
###Finding: Of the top trigram tweets,6 contain hashtags, 1, contains an entity (@realdonaldtrump), one references buying tickets..

#16.
#################syuzhet sentiment analysis section #####################
###Using the Syuzhet package lets understand what is being said
#Install package (if required) and load library
#install.packages("syuzhet")
library("syuzhet")

#reuse variable for cleaned data done earlier 
dtTxtDf

#make a new name to avoid confusion
sy<-dtTxtDf
#Syuzhet complains about "character vectors", so we're creating a variable with a single column 
syu<-sy$text
#verify the class
class(syu)
#alternatively, could write syu<-dtTxtDf$text

#Following the directions from "Introduction to the Syuzhet Package" by Matthew Jockers
#https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
#Word of caution when using sentiment libraries, such as sexism can be found here: 
#https://hoyeolkim.wordpress.com/2018/02/25/the-limits-of-the-bing-afinn-and-nrc-lexicons-with-the-tidytext-package-in-r/

#16.Create a new variable for sentiment analysis 
#use get tokens and create a variable
poa_word_v <- get_tokens(sy$text, pattern = "\\W")

#using the variable just created get sentiments using the syuzhet method
syuzhet_vector <- get_sentiment(poa_word_v, method="syuzhet")
head(syuzhet_vector)

#Using the same methodology create vectors using methods: bing, afinn and nrc
bing_vector <- get_sentiment(poa_word_v, method="bing")
head(bing_vector)
afinn_vector <- get_sentiment(poa_word_v, method="afinn")
head(afinn_vector)
nrc_vector <- get_sentiment(poa_word_v, method="nrc", lang = "english")
head(nrc_vector)

#17. Compare the vectors as apples to apples
#When doing a comparison analysis  a the different methods use different scales, 
#to compare them using R's built in sign function. The sign function converts 
#all positive number to 1, all negative numbers to -1 and all zeros remain 0.

rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector)),
  sign(head(nrc_vector))
)
#validate these new scales
sum(syuzhet_vector)
mean(syuzhet_vector)
summary(syuzhet_vector)

#18.
#Now lets get emotional, aka get some sentiment analysis and visualize it
sy_sentiment <- get_sentiment(sy$text)

##And plot that sentiment##
#plot emotional score frequency over time with lines
plot(
  sy_sentiment, 
  type="l", 
  main="Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

#plot emotional score over time with histogram like up and down
plot(
  syuzhet_vector, 
  type="h", 
  main="Plot Trajectory Histogram", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)
##turns out this is hard to see/read...too blocky

#plot emotionanal score over time with points only.  This will show 'density'
plot(
  syuzhet_vector, 
  type="p", 
  main="Plot Trajectory of Emotion over Time", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)
###Finding: While Trump is overall positive, its moderate positivity. When he is negative he is extremely negative as shown by the density of -1

#19 
#Getting fancy with Math and using cos
#plot the syuzhet vector and cos
plot(syuzhet_vector, cos(syuzhet_vector),
     main="The Cos Function Using Syuzhet",
     ylab="cos(syuzhet)",
     type="l",
     col="blue")

#Using nrc vecotor with cos of syuzhet
plot(nrc_vector, cos(syuzhet_vector),
     main="The Sine Function",
     ylab="cos(nrc)",
     type="l",
     col="blue")

#syuzhet over nrc
plot(nrc_vector, syuzhet_vector,
     main="Emotional Valence Useing Syuzjet Lexicon over NRC Lexicon",
     ylab="syuzhet_vector",
     type="l",
     col="blue")
###Finding: Visual representation of the sentiment balance of trump tweets shows they are relatively balanced, but weighted to positive dispersion.

#20.
##Apply 'smoothing to these data sets###

ft_values <- get_transformed_values(
  syuzhet_vector, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  padding_factor = 2,
  scale_vals = TRUE,
  scale_range = FALSE
)

#Plot out the smothed sentiments using a line
plot(
  ft_values, 
  type ="l", 
  main ="Trump Tweet Sentiment Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)
###Finding: With the data set provided Trump started 
###negative and gradually spoke positively, with a sharp positive spike of short duration and then negative again.
###this suggests that while the corpus of tweets is moderately positive on average, it actually represents a short 
###duration of positivity, with negative tweets as the constant expectation.  It's possible this hightens the strength 
### of positive comments, as it is not the norm/expectation over time

#21.
######Question: What is the overall sentiment using syuzhet#################
######Question: Who and what is Trump talking about when expressing emotions angry, joyful and trust?#####

#Create dataframes for emotions: angry, joy, trust
#anger is example of negative
#joy is example of positive
#trust is the most expressed emotion

#Convert sentiment to a numerical value by cataegory (for a plot) and create a new list for corresponding 
#emotion to be used in a word cloud later on
nrc_data <- get_nrc_sentiment(syu)
angry_items <- which(nrc_data$anger > 0)
angry_items


#angry_items
#View(angry_tweets)
syu[angry_items]

#angry_tweetsdf<-as.data.frame(angry_tweets)
#View(angry_tweetsdf)


trust_items <- which(nrc_data$trust > 0)
trust_tweets<-syu[trust_items]
head(trust_tweets)
trust_items


joy_items <- which(nrc_data$joy > 0)
syu[joy_items]
head(syu[joy_items])
joy_tweets<-syu[joy_items]
head(joy_tweets)

#Scale the data for emotions per syuhzet formatting
pander::pandoc.table(nrc_data[, 1:8], split.table = Inf)

#Plot those emotions in order of highest frequency at top
barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Trump Tweets", xlab="Percentage",
  col = "green"
)
###Finding: 
###Trust is >20% of emotions
###suprise is ~16% of emotions
###anticipation is ~15% of emotions
###joy is is ~12% of emotions
###anger is ~11% of emotions
###sadness is ~11% of emotions
###fear is ~=10% of emotions
###disgust is ~7% of emotions
###Overall, Trump's style useses ~41% anxiety related emotions 
###(surprise, anticipation, fear), with 20% on trust.  The pairing of this dichotemy
###represents 61% of all tweets for the given data set, giving insight to the 'majority' messaging
###Alternatively, 31% of the tweets (surprise & anticipation) suggest an 'unknown' theme/thought processes
###ethier by or to his audience. 



#########################Let's visualize sentiment using Syuzhet sentiment#########################
#create a new variable of the syu dataframeto avoid conflicts
mydataCopy <- syu
#carryout sentiment mining using the get_nrc_sentiment()function #log the findings under a variable result
result <- get_nrc_sentiment(as.character(mydataCopy))
#change result from a list to a data frame and transpose it 
result1<-data.frame(t(result))
#rowSums computes column sums across rows for each level of a #grouping variable.
new_result <- data.frame(rowSums(result1))
#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL
#plot the first 8 rows,the distinct emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("Trump Tweets Sentiments")

#plot the last 2 rows ,positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Trump Tweets Sentiments")




## Lets make emotional word clouds

#make a list of emotions at entities by finding all the words from emotion list (joy, angry, trust) that 
#starts with '@' which tweetish for a twitter entitiy (aka person or company, aka noun)
########################################angry tweent filter
#View and validate the angry data set

#Making df for word cloud
#Using the angry terms create a new variable
angry_tweets_char<-syu[angry_items]

#convert this into a properly formated data frame and validate
angryTxtDf <- tibble (line = 1:1810, text = angry_tweets_char)
View(angryTxtDf)
str(angryTxtDf)

#Split and tokenize using same methodology earlier and validate
angry.split <- angryTxtDf %>% unnest_tokens(word, text, token = "tweets")
View(angry.split)

#remove stopwords, same methodology as earlier
angry.split <- angry.split %>% anti_join(dtCustomStopWords, by = c("word" = "word"))

#Find all the '@' which designate 'talking to' or referencing an entity
angryTalkers <-angry.split %>% 
  filter(grepl("^@", tolower(word)))

#set the seed for reproducability
set.seed(4321)
angryTalkers %>% count(word) %>% with(wordcloud(word, n, max.words = 100,
                                                colors = brewer.pal(8, "Dark2")))
###Finding: Even while angry, trump mentions his name the most by far
###Finding: Aside from himself, his nytimes, foxnews,cnn, and megynkelly are his angriest targets
###Finding: Notably "hillary" is absent from this emotion.  At the begining we noted that 'hillary' was among the highest'talkers' and '#' themes


#same process as above but with the '#' method
angryHash <-angry.split %>% 
  filter(grepl("^#", tolower(word)))

set.seed(4321)

angryHash %>% count(word) %>% with(wordcloud(word, n, max.words = 100,
                                             colors = brewer.pal(8, "Dark2")))


########Time Analysis################################
library(ggplot2)
#set working directory
setwd("~/UMUC/DATA630/Team5")
#Read file and format time
df <- read.csv("Donald-Tweets!.csv")
df$HMS <- hms(as.character(df$Time))
df$YMD <- ymd(as.character(df$Date))
df$time <- hour(df$HMS)
df$day <- day(df$YMD)
df$month <- month(df$YMD)

str(df)
view(df)

#graph the hour, month, day of the tweets against the count
ggplot(df,aes(x=time))+geom_histogram(aes(y = (..count..),col=""),binwidth=1)
ggplot(df,aes(x=month))+geom_histogram(aes(y = (..count..),col=""),binwidth=1)
ggplot(df,aes(x=day))+geom_histogram(aes(y = (..count..),col=""),binwidth=1)

###Finding: Trump tweeted relatively consistenly except during the hours of 4-11. The lowest point being hour 7.  
###This implies he is likely sleeping during those hours and while his sleep cycle is consistent, his success with sleep is variable


#End of Line