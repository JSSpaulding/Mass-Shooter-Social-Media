### Mass Shooter Social Media Analytics   ###
### Case Study - Connor Betts Twitter     ###
###                                       ###

#### REQUIRED PACKAGES/DIRECTORIES =============================================
pkgs <- c("here","pdftools","stringr","tm","ggplot2","dplyr","plotly","hrbrthemes","wordcloud")
install.packages(pkgs) #installs packages listed in pkgs
lapply(pkgs, library, character.only = TRUE) #loads required packages in pkgs

#### PART I: DATA PRE-PROCESSING ===============================================
## STEP 1 - Read in Tweets from PDF ----
raw_data <- pdf_text(here("data", "Twitter-iamthespookster-2019-08-04.pdf"))
# if OCR needed: pdf_ocr_text Requires tesseract package


## STEP 2 - Extract Tweets from Raw Data ----
dat <- strsplit(raw_data, "\n") #each element of list is a pdf page
dat <- unlist(dat)

dat <- dat[-c(1:4)] #remove first four lines (heading/whitespace)
head(dat,n=40) #view semi-structured data

# Find Line Breaks
LBs <- c()
jj <- 1
for (i in 1:length(dat)) {
  line <- dat[i]
  #record line breaks for removal
  if(line == ""){
    LBs[jj] <- i
    jj <- jj + 1
  } 
}

# Remove Line Breaks
dat <- dat[-c(LBs)]

# Find the Line Containing the Source of Each Tweet
# Note: in this case, source is always 1 linebreak after tweet/date 
source_line <- c()
jj <- 1
for (i in 1:length(dat)) {
  line <- dat[i]
  if(grepl("(Source:", line, fixed = TRUE)){
    source_line[jj] <- i
    jj <- jj + 1
  }
}

# Extract Tweet, Date, Source from Content
tweet <- c()
date <- c()
source <- c()
jj <- 1
start <- 1
for (i in source_line) {
  tweet_in <- dat[start:i]
  content <- tweet_in[1:(length(tweet_in) - 2)]
  tweet[jj] <- paste(content, collapse = " ")
  date[jj] <- tweet_in[length(tweet_in) - 1]
  source[jj] <- tweet_in[length(tweet_in)]
  start <- i + 1
  jj <- jj + 1
}

data <- data.frame(tweet,date,source)


## STEP 3 - Extract Data from Tweet ----
# RT, Tagged Users, External Links
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

RT <- c()
user_tag <- c()
account <- c()
ext_link <- c()
link <- c()
for (i in 1:nrow(data)) {
  line <- data$tweet[i]
  # RT
  if(substring(line, 1, 3) == "RT "){
    RT[i] <- TRUE
    data$tweet[i] <- substring(line, 4) #removes RT from tweet contents
  } else{
    RT[i] <- FALSE
  }
  # Tagged Users
  if(grepl("@", line, fixed = TRUE)){
    user_tag[i] <- TRUE
    account[i] <- paste(unlist(str_extract_all(line, "(?<=^|\\s)@[^\\s]+")), sep = ",")
    data$tweet[i] <- str_remove_all(data$tweet[i], unlist(str_extract_all(line, "(?<=^|\\s)@[^\\s]+"))) #removes twitter handle from tweet content
  } else{
    user_tag[i] <- FALSE
    account[i] <- ""
  }
  # Ext. Link
  if(grepl("http", line, fixed = TRUE)){
    ext_link[i] <- TRUE
    link[i] <- paste(unlist(str_extract(line, url_pattern)), sep = ",")
    #if whitespace in twitter link, adjust and remove link from tweet contents
    if(is.na(link[i]) | link[i] == "NA"){
      link[i] <- paste0(str_extract(line, "https://"),str_replace_all(string=sub(".*https://", "", data$tweet[i]), pattern=" ", repl=""))
      data$tweet[i] <- str_extract(data$tweet[i], "^.*(?=(https://))") # including the @
    }
    data$tweet[i] <- str_remove_all(data$tweet[i], link[i]) #remove link from tweet contents
  } else{
    ext_link[i] <- FALSE
    link[i] <- ""
  }
}

data <- cbind(data,RT,user_tag,account,ext_link,link)


## STEP 4 - CONVERT TIMESTAMP TO FORMAL DATE/TIME CLASS ----
data$date <- as.POSIXct(data$date, tz = "EST", format = "%b %e, %Y, %I:%M %p")


#### PART II: DATA ANALYSIS ====================================================
## STEP 5 - Time Series of Social Media Activity ----
day <- as.Date(data$date)
ts <- as.data.frame(table(day))
ts$day <- as.Date(ts$day)

p <- ggplot(ts, aes(x=day, y=Freq)) +
  geom_area(alpha=0.5) +
  geom_line() +
  geom_smooth(alpha=0.5) +
  ggtitle("Daily Tweet Frequency") +
  ylab("Daily Tweets") + xlab("Time") 
p

## Figure 1.1
#png("F01.1-daily-time-series.png",width=12,height=6,units="in",res=600)
p + theme(axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold"),
          plot.title=element_text(size=24,face="bold"))
dev.off()

# Construct a corpus, and specify the source to be characters by row 
# (aka character vectors)
corpus <- Corpus(VectorSource(data$tweet))
inspect(corpus[1:10])

# Clean Text
cleanset <- tm_map(corpus, tolower) # Convert all text to lower case
cleanset <- tm_map(cleanset, removePunctuation) # Remove all puncutation
cleanset <- tm_map(cleanset, removeNumbers) # Remove all numbers
cleanset <- tm_map(cleanset, removeWords, stopwords('english'))

removeURL <- function(x) gsub("http[^[:space:]]*", "", x) # Remove URL function
cleanset <- tm_map(cleanset, content_transformer(removeURL)) # Remove URLs
inspect(cleanset[1:10])

# Remove Extra Text
cleanset <- tm_map(cleanset, removeWords, 
                   c("include", "make", "also", "from", "can", "cant","etc", 
                     "use", "like","one","must", "will", "doesnt","thats", 
                     "didnt", "dont", "youve", "ive", "youre","theyre", 
                     "theyve", "amp", "theres"))
inspect(cleanset[1:10])

#Replacing words->plural to singular and others
cleanset <- tm_map(cleanset, gsub, pattern = "'m", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = "’s", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = "'s", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = " 's", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = "'re", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = "“", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = "'m", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = "fucking", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "fucks", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "nights", replacement = "night")
cleanset <- tm_map(cleanset, gsub, pattern = "fucktains", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "fick", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "superjail", replacement = "jail")
cleanset <- tm_map(cleanset, gsub, pattern = "fideliscare ", replacement = "healthcare")
cleanset <- tm_map(cleanset, gsub, pattern = "judg", replacement = "judge")
cleanset <- tm_map(cleanset, gsub, pattern = "fucknsbm", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "metalagainstfascism", replacement = "metal against fascism")
cleanset <- tm_map(cleanset, gsub, pattern = "shutdownnazimetal", replacement = "shut down nazi metal")
cleanset <- tm_map(cleanset, gsub, pattern = " f ", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "fuckace", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "speaking", replacement = "speak")
cleanset <- tm_map(cleanset, gsub, pattern = "fuckuck", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "pls", replacement = "please")
cleanset <- tm_map(cleanset, gsub, pattern = "things", replacement = "thing")
cleanset <- tm_map(cleanset, gsub, pattern = "'re", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = "…", replacement = "")

# Remove Extra White Space
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:10])

# Term Document Matrix
# This line of code converts all text in the corpus to PlainTextDocument,
# on which the DocumentTermMatrix function does not work properly: 
# cleanset <- tm_map(cleanset, PlainTextDocument)
tdm <- TermDocumentMatrix(cleanset, control = list(WordLength=c(1,Inf)))

# Review the summary of tdm
tdm

# View tdm via a matix
tdm <- as.matrix(tdm)
tdm[1:10, 1:20] 

# Number Of Times A Term Appears
termFreqency <- rowSums(tdm)
termFreqency
head(sort(termFreqency,decreasing = TRUE),n=20)

# Subset fequent words
termFreqency <- subset(termFreqency, termFreqency>40)
termFreqency

termFreqency <- sort(termFreqency, decreasing = TRUE)

# Figure 2.1 - Bar Plot
#png("F02.1-term-freq-barplot.png",width=12,height=6,units="in",res=600)
barplot(termFreqency, las=2)

# If the plot labels need t be shifted
sas <- barplot(termFreqency, axes = TRUE, axisnames = FALSE, las=2, 
               main = "Word Usage Frequency", cex.main=1.5, cex.axis=1.5)
text(sas[,1], -1.0, srt = 60, adj = c(1.1,1.1), xpd = TRUE, 
     labels = names(termFreqency),cex=1.5)
dev.off()

# Wordcloud 
# - max.words will plot the specified number of words and discard least frequent terms
# - min.freq will discard all terms whose frequency is below the specified value
wordFreq <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(123)

#png("F03-wordcloud.png",width=12,height=6,units="in",res=600)
wordcloud(words = names(wordFreq), freq = wordFreq, random.order = F, max.words = 30, 
          colors = brewer.pal(6, 'Dark2'), scale = c(3.5,0.5), rot.per = 0.0,)
#dev.off()

# STEP 6 - Sentiment Analysis and Structural Networks ----
# Load Sentiment Packages
sent <- c("syuzhet","lubridate","ggplot2","scales","reshape2","dplyr")
lapply(sent, library, character.only = TRUE) #loads required packages in pkgs

# Get sentiment scores
sent <- get_nrc_sentiment(data$tweet)
head(sent)
data$tweet[4] #example used in-text

# Exploratory Bar Plot
barplot(colSums(sent), las = 2,  ylab = "Count", 
        main = "NRC Sentiment Scores Tweets")
tw <- barplot(colSums(sent), axes = TRUE, axisnames = FALSE, las=2,  
              ylab = "Count", main = "NRC Sentiment Scores for Tweets")
text(tw[,1], -1.0, srt = 60, adj = c(1.1,1.1), xpd = TRUE, labels = names(sent), cex=1)
dev.off()

# Figure 5 - Radar Plot (Part 1, combined later)
library(radarchart)
#png("sent-scores-radar.png",width=12,height=6,units="in",res=600)
radarsent <- data.frame(Sentiment = str_to_title(colnames(sent)),
                        Frequency = colSums(sent))
chartJSRadar(radarsent, labelSize = 24)
#dev.off()

# Terms Association
#install.packages('BiocManager')
#BiocManager::install("graph")
#BiocManager::install("Rgraphviz")
library(graph)
library(Rgraphviz)

# Find frequent terms
tdm <- TermDocumentMatrix(cleanset, control = list(WordLength=c(1,Inf)))
(freq.terms <- findFreqTerms(tdm, lowfreq = 60))

# Find network of terms
#png("F04.1-tdm-linkage-network.png",width=12,height=6,units="in",res=600)
plot(tdm, term = freq.terms, corThreshold = 0.03, weighting = T)
dev.off()

## STEP 7: SENTIMENT VALUES - NUMERICAL ========================================
sent_new <- get_sentiment(data$tweet)
head(sent_new)

data2 <- data
data2$date <- as.Date(data2$date)

date_sorted <- list()
daily_sent <- c()
daily_min <- c()
daily_max <- c()
jj <- 1
for (i in unique(data2$date)) {
  date_sorted[[jj]] <- subset(data2,data2$date==i)
  sentbase <- get_sentiment(date_sorted[[jj]]$tweet)
  daily_sent[jj] <- mean(sentbase)
  daily_min[jj] <- min(sentbase)
  daily_max[jj] <- max(sentbase)
  jj <- jj + 1
}

daily_sent_score <- data.frame(date=unique(data2$date),
                               score=daily_sent)

#png("F06.1-daily-sentiment-ts.png",width=12,height=6,units="in",res=600)
plot(daily_sent_score,type='l', ylim=c(-4,4), cex.axis=1.5, cex.lab=1.5,
     xlab="Date", ylab="Sentiment Score")
title("Daily Tweet Sentiment Score", adj = 0, line = 2, cex.main=2)
polygon(c(daily_sent_score$date,rev(daily_sent_score$date)),
        c(daily_min,rev(daily_max)),col = "grey75", border = FALSE)
lines(daily_sent_score$date, daily_sent_score$score, lwd = 3)
#add red lines on borders of polygon
lines(daily_sent_score$date, daily_max, col="red",lty=2)
lines(daily_sent_score$date, daily_min, col="red",lty=2)
abline(h=0)

min_smooth <- predict(loess(daily_min ~ as.numeric(daily_sent_score$date), span = 0.25))
lines(daily_sent_score$date,min_smooth,col="blue")

max_smooth <- predict(loess(daily_max ~ as.numeric(daily_sent_score$date), span = 0.25))
lines(daily_sent_score$date,max_smooth,col="blue")

legend("top", fill = c("black","gray","red","blue"), 
       legend = c("Average", "Score Range", "Daily Min/Max","Smoothed Daily"), 
       horiz = TRUE, inset = c(0,-0.06), xpd = TRUE)
dev.off()

## Frequency of Links, RT, and Tags
sum(data$RT)/nrow(data)
sum(data$user_tag)/nrow(data)
sum(data$ext_link)/nrow(data)
unique(data$account)


#### PART III: DATA ANALYSIS EXCLUDING RETWEETS ================================
data2 <- filter(data, RT==FALSE)
data2$date <- as.POSIXct(data2$date, tz = "EST", format = "%b %e, %Y, %I:%M %p")

day <- as.Date(data2$date)
ts <- as.data.frame(table(day))
ts$day <- as.Date(ts$day)

p <- ggplot(ts, aes(x=day, y=Freq)) +
  geom_area(alpha=0.5) +
  geom_line() +
  geom_smooth(alpha=0.5) +
  ggtitle("Daily Tweet Frequency (Excluding Retweets)") +
  ylab("Daily Tweets") + xlab("Time") 
p

#png("F01.3-no-RT-daily-time-series.png",width=12,height=6,units="in",res=600)
p + theme(axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold"),
          plot.title=element_text(size=24,face="bold"))
dev.off()


# Reconstruct corpus
corpus <- Corpus(VectorSource(data2$tweet))

# Clean Text
cleanset <- tm_map(corpus, tolower) # Convert all text to lower case
cleanset <- tm_map(cleanset, removePunctuation) # Remove all puncutation
cleanset <- tm_map(cleanset, removeNumbers) # Remove all numbers
cleanset <- tm_map(cleanset, removeWords, stopwords('english'))

removeURL <- function(x) gsub("http[^[:space:]]*", "", x) # Remove URL function
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:10])

# Remove Extra Text
cleanset <- tm_map(cleanset, removeWords, 
                   c("include", "make", "also", "from", "can", "cant","etc", 
                     "use", "like","one","must", "will", "doesnt","thats", 
                     "didnt", "dont", "youve", "ive", "youre","theyre", 
                     "theyve", "amp", "theres"))
#inspect(cleanset[1:10])

#Replacing words->plural to singular and others
cleanset <- tm_map(cleanset, gsub, pattern = "'m", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = "’s", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = "'s", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = " 's", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = "'re", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = "“", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = "'m", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = "fucking", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "fucks", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "nights", replacement = "night")
cleanset <- tm_map(cleanset, gsub, pattern = "fucktains", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "fick", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "superjail", replacement = "jail")
cleanset <- tm_map(cleanset, gsub, pattern = "fideliscare ", replacement = "healthcare")
cleanset <- tm_map(cleanset, gsub, pattern = "judg", replacement = "judge")
cleanset <- tm_map(cleanset, gsub, pattern = "fucknsbm", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "metalagainstfascism", replacement = "metal against fascism")
cleanset <- tm_map(cleanset, gsub, pattern = "shutdownnazimetal", replacement = "shut down nazi metal")
cleanset <- tm_map(cleanset, gsub, pattern = " f ", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "fuckace", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "speaking", replacement = "speak")
cleanset <- tm_map(cleanset, gsub, pattern = "fuckuck", replacement = "fuck")
cleanset <- tm_map(cleanset, gsub, pattern = "pls", replacement = "please")
cleanset <- tm_map(cleanset, gsub, pattern = "things", replacement = "thing")
cleanset <- tm_map(cleanset, gsub, pattern = "'re", replacement = "")
cleanset <- tm_map(cleanset, gsub, pattern = "…", replacement = "")

# Remove Extra White Space
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:10])

# Term Document Matrix
tdm <- TermDocumentMatrix(cleanset, control = list(WordLength=c(1,Inf)))
tdm

# View tdm via a matix
tdm <- as.matrix(tdm)
tdm[1:10, 1:20] 

# Number Of Times A Term Appears
termFreqency <- rowSums(tdm)
termFreqency
head(sort(termFreqency,decreasing = TRUE),n=20)

# Subset fequent words
termFreqency <- subset(termFreqency, termFreqency>10)
termFreqency
termFreqency <- sort(termFreqency, decreasing = TRUE)

# Bar Plot
#png("F02.2-no-RT-term-freq-barplot.png",width=12,height=6,units="in",res=600)
barplot(termFreqency, las=2)
sas <- barplot(termFreqency, axes = TRUE, axisnames = FALSE, las=2, 
               main = "Word Usage Frequency (Excluding Retweets)", cex.main=1.5, cex.axis=1.5)
text(sas[,1], -1.0, srt = 60, adj = c(1.1,1.1), xpd = TRUE, 
     labels = names(termFreqency),cex=1.5)
dev.off()

# Wordcloud 
wordFreq <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(123)

#png("F03.2-no-RT-term-freq-wordcloud.png",width=12,height=6,units="in",res=600)
wordcloud(words = names(wordFreq), freq = wordFreq, random.order = F, max.words = 30, 
          colors = brewer.pal(6, 'Dark2'), scale = c(3.5,0.5), rot.per = 0.0)
dev.off()


## Sentiment Analysis and Structural Networks
# Get sentiment scores
sent <- get_nrc_sentiment(data2$tweet)
head(sent)

# Bar plot
tw <- barplot(colSums(sent), axes = TRUE, axisnames = FALSE, las=2,  
              ylab = "Count", main = "NRC Sentiment Scores for Tweets (Excluding Retweets)")
text(tw[,1], -1.0, srt = 60, adj = c(1.1,1.1), xpd = TRUE, labels = names(sent), cex=1)
dev.off()

# Figure 5: Radar Plot - Combined
library(webshot)
library(htmlwidgets)
radarsent_combined <- cbind(radarsent,colSums(sent))
names(radarsent_combined) <- c(names(radarsent_combined)[1:2],"Frequency (Excluding Retweets)")
plt <- chartJSRadar(radarsent_combined, labelSize = 24)
saveWidget(plt, "plt.html")
webshot("plt.html")

## How are terms connected
# Find frequent terms
tdm <- TermDocumentMatrix(cleanset, control = list(WordLength=c(1,Inf)))
(freq.terms <- findFreqTerms(tdm, lowfreq = 15))
dev.off()
# Find network of terms
#png("F04.1-noRT-tdm-linkage-network.png",width=12,height=6,units="in",res=600)
plot(tdm, term = freq.terms, corThreshold = 0.03, weighting = T)
dev.off()

#### PART IV: CONNECTIVITY TO NETWORK ==========================================
rtdata <- filter(data, RT==TRUE)
rtdata$date <- as.POSIXct(rtdata$date, tz = "EST", format = "%b %e, %Y, %I:%M %p")

day <- as.Date(rtdata$date)
ts <- as.data.frame(table(day))
ts$day <- as.Date(ts$day)

p <- ggplot(ts, aes(x=day, y=Freq)) +
  geom_area(alpha=0.5) +
  geom_line() +
  geom_smooth(alpha=0.5) +
  ggtitle("Daily Retweet Frequency") +
  ylab("Daily Retweets") + xlab("Time") 
p

#png("F01.2-RT-daily-time-series.png",width=12,height=6,units="in",res=600)
p + theme(axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold"),
          plot.title=element_text(size=24,face="bold"))
dev.off()

## No-RT NEW SENTIMENT
sent_new <- get_sentiment(data2$tweet)
head(sent_new)

data3 <- data
data2$date <- as.Date(data2$date)

date_sorted <- list()
daily_sent <- c()
daily_min <- c()
daily_max <- c()
jj <- 1
for (i in unique(data2$date)) {
  date_sorted[[jj]] <- subset(data2,data2$date==i)
  sentbase <- get_sentiment(date_sorted[[jj]]$tweet)
  daily_sent[jj] <- mean(sentbase)
  daily_min[jj] <- min(sentbase)
  daily_max[jj] <- max(sentbase)
  jj <- jj + 1
}

daily_sent_score <- data.frame(date=unique(data2$date),
                               score=daily_sent)

#png("F06.2-no-RT-daily-sentiment-ts.png",width=12,height=6,units="in",res=600)
plot(daily_sent_score,type='l', ylim=c(-4,4), cex.axis=1.5, cex.lab=1.5,
     xlab="Date", ylab="Sentiment Score")
title("Daily Tweet Sentiment Score (Excluding Retweets)", adj = 0, line = 2, cex.main=2)
polygon(c(daily_sent_score$date,rev(daily_sent_score$date)),
        c(daily_min,rev(daily_max)),col = "grey75", border = FALSE)
lines(daily_sent_score$date, daily_sent_score$score, lwd = 3)
#add red lines on borders of polygon
lines(daily_sent_score$date, daily_max, col="red",lty=2)
lines(daily_sent_score$date, daily_min, col="red",lty=2)
abline(h=0)

min_smooth <- predict(loess(daily_min ~ as.numeric(daily_sent_score$date), span = 0.25))
lines(daily_sent_score$date,min_smooth,col="blue")

max_smooth <- predict(loess(daily_max ~ as.numeric(daily_sent_score$date), span = 0.25))
lines(daily_sent_score$date,max_smooth,col="blue")

legend("top", fill = c("black","gray","red","blue"), 
       legend = c("Average", "Score Range", "Daily Min/Max","Smoothed Daily"), 
       horiz = TRUE, inset = c(0,-0.06), xpd = TRUE)
dev.off()


## Valence Shifters - Polarized Words
library(sentimentr)
sentiment_attributes(data$tweet)
sentiment_attributes(data2$tweet)
sentiment_attributes(subset(data,data$RT==TRUE)$tweet)

sessionInfo() # see GitHub Repo

### END ###
###     ###