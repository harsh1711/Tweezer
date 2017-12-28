## Tweezer ##

## Install the following libraries, if you already have then skip this step.

# libraries
library(plyr);
library(dplyr);
library(syuzhet);
library(SnowballC);
library(RCurl);
library(twitteR);
library(syuzhet);
library(tm);
library(wordcloud);
library(plotrix);
library(rlist);
library(ggplot2);
library(plotrix);
library(stringr);
library(reshape);
library(plotrix);
library(rlist);

#Give hashtag on which you want to perform sentimental analysis 
#Replace #hashtah with an actual hashtag ex. #india
# n = number of tweets you want to fetch

#downloading tweets
tweets <- searchTwitter("#modi",n = 500, lang = "en",resultType = "recent")
# removing re tweets
no_retweets <- strip_retweets(tweets, strip_manual = TRUE)

#converts to data frame
df <- do.call("rbind", lapply(no_retweets, as.data.frame))

# storing into diffrent data frame 
result<-df

# checking length of the result data frame
nrow(result)

#remove odd characters
result$text <- sapply(result$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #remove emoticon
result$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", result$text) #remove URL
sample <- result$text

# if you want to save the tweets as CSV file in your local machine 
# replace with the actual path and give file .csv extension 
write.csv(sample,"Replace with actual path")
write.csv(sample,"C:\\Harsh\\tweets_text.csv")

# Cleaning Tweets 
sum_txt1 <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",sample)
sum_txt2 <- gsub("http[^[:blank:]]+","",sum_txt1)
sum_tx3 <- gsub("@\\w+","",sum_txt2)
sum_tx4 <- gsub("[[:punct:]]"," ", sum_tx3)
sum_tex5 <- gsub("[^[:alnum:]]", " ", sum_tx4)
sum_tx6 <- gsub("RT  ","", sum_tex5)

# data frame is not good for text convert it corpus
corpus <- Corpus(VectorSource(sum_tx6))
clean <- tm_map(corpus, content_transformer(tolower)) #converting everything to lower cases
clean <- tm_map(clean,removeWords, stopwords("english")) #stopword are words like of, the, a, as..
clean <- tm_map(clean, removeNumbers)
clean <- tm_map(clean, stripWhitespace)
# Removing #hashtag as it is obviously will be there data 
clean <- tm_map(clean, removeWords, "#hashtag")

pal <- brewer.pal(8,"Dark2")

# generate word cloud
wordcloud(guj_clean,min.freq = 20,max.words = Inf, rot.per=.1,random.order  = FALSE,colors = pal)


#################### START: Sentiment Graph ######################

mysentiment <- get_nrc_sentiment(sum_tx6)
sentimentscore <- data.frame(colSums(mysentiment[,]))
names(sentimentscore) <- "Score"

sentimentscore <- cbind("sentiment" = rownames(sentimentscore), sentimentscore)
rownames(sentimentscore)<- NULL
#install.packages("ggplot2")
library(ggplot2)
ggplot(data = sentimentscore,aes(x = sentiment,y = Score)) +
  geom_bar(aes(fill=sentiment),stat = "identity") +
  theme(legend.position = "none") +
  xlab("sentiments") +
  ylab("Score") +
  ggtitle("sentimets")

#################### END: Sentiment Graph ######################


#After downloading the positive-words.txt.pos and negative-words.txt, 
# mention below the location of the file

pos.words = scan('"replace with your acual path"/positive-words.txt', what='character', comment.char=';')
neg.words = scan('"replace with your acual path"/negative-words.txt', what='character', comment.char=';')

#Adding words to positive and negative databases

pos.words = c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader')
neg.words = c(neg.words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')

#install.packages("plyr")
#install.packages("stringr")

library(plyr)
library(stringr)

#Function for the generating sentiment score

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]',' ',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)  #removes decimal number
    sentence = gsub('\n','',sentence)    #removes new lines
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)  #changes a list to character vector
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp = sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1 = c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new = lapply(list, `[[`, 1)
  pp1 = lapply(list, `[[`, 2)
  nn1 = lapply(list, `[[`, 3)
  
  scores.df = data.frame(score = score_new, text=sentences)
  positive.df = data.frame(Positive = pp1, text=sentences)
  negative.df = data.frame(Negative = nn1, text=sentences)
  
  list_df = list(scores.df, positive.df, negative.df)
  return(list_df)
}

# Clean the tweets and returns merged data frame
result = score.sentiment(sample, pos.words, neg.words)

library(reshape)

test1=result[[1]]
test2=result[[2]]
test3=result[[3]]

#Creating three different data frames for Score, Positive and Negative
#Removing text column from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL
#Storing the first row(Containing the sentiment scores) in variable q
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]
qq1=melt(q1, ,var='Score')
qq2=melt(q2, ,var='Positive')
qq3=melt(q3, ,var='Negative') 
qq1['Score'] = NULL
qq2['Positive'] = NULL
qq3['Negative'] = NULL


#Creating data frame
table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[2]]$text, Score=qq2)
table3 = data.frame(Text=result[[3]]$text, Score=qq3)

#Merging three data frames into one
table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)

#Positive Percentage

#Renaming
posSc=table_final$Positive
negSc=table_final$Negative

#Adding column
table_final$PosPercent = posSc/ (posSc+negSc)

#Replacing Nan with zero
pp = table_final$PosPercent
pp[is.nan(pp)] <- 0
table_final$PosPercent = pp

#Negative Percentage

#Adding column
table_final$NegPercent = negSc/ (posSc+negSc)

#Replacing Nan with zero
nn = table_final$NegPercent
nn[is.nan(nn)] <- 0
table_final$NegPercent = nn

#Histogram
hist(table_final$Positive, col=rainbow(10))
hist(table_final$Negative, col=rainbow(10))
hist(table_final$Score, col=rainbow(10))


########## Pie Chart 
pos_sum <- sum(table_final$Positive)
neg_sum <- sum(table_final$Negative)
chart_pos_header <- paste("Positive(",pos_sum, ")",SEP="")

chart_neg_header <- paste("Negative(",neg_sum,")" ,SEP="")

lbls <- c(chart_pos_header, chart_neg_header)


# Pie Chart with Percentages

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 

#Pie
slices <- c(sum(table_final$Positive), sum(table_final$Negative))
#install.packages("plotrix")
library(plotrix)

pie3D(slices, labels = lbls, col=rainbow(length(labels)),explode=0.00, main="Tweezer Sentiment Analysis")


