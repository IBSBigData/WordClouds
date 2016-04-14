###############################################
#
# basic framework for analyzing adzuna data
#
###############################################

# setwd(to your working directory)

# invoke all packages
library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)
library(RColorBrewer)

set.seed(2016)  # any number you choose 

# The raw file is csv, and the text we want is in Column 3 and 4
options(header=TRUE, stringsAsFactors = FALSE,FileEncoding="latin1")
adztr <- read.csv("adzunatrain10.csv")
titles <- as.character(adztr$Title) # one object with titles
desc <-as.character(adztr$FullDescription) # one object with descriptions

# repeat: once for titles, once for full description
text <- titles
corpus <- Corpus(VectorSource(text))

#Clean-up 
corpus <- tm_map(corpus, tolower)  #make all text lower case
corpus <- tm_map(corpus, removePunctuation) #remove all punctuation
corpus <- tm_map(corpus, removeNumbers) # remove numbers
cleanset <- corpus

# apply standard English stopwords, and add other words of your choice
# remove "big" or other relevant from English stop words
custom <- c("xxx", "yyy"<etc.) # choose your own terms here
myStopwords <- c(stopwords('english'), custom)
#Keepwords should be a list of words you want to retain
keepwords <- c("one","two", etc) # words of your choice
myStopwords <- setdiff(myStopwords, keepwords)

# now prepare "cleanset" by removing stopwords, extra white space and then stem the corpus

cleanset <- tm_map(cleanset, removeWords, myStopwords)
cleanset <- tm_map(cleanset, stripWhitespace) # purge extra white space
cleanset <-tm_map(cleanset,stemDocument) # now stem -

##############################################
## CAUTION: for the descriptions, the corpus is too large for later work; random 
# split into a more manageable subset
# demonstrate another way to split
#
#  this section is NOT needed for job titles data
#  just convert the next few commands to #comments for Titles data

nn <- length(cleanset)
ff <- as.factor(c(rep('Train',ceiling(nn/3)),  
                  rep('Test',nn-ceiling(nn/3))))  
ll <- split(cleanset,ff)

# need to stem both sets of text
#stemming to treat related terms alike

cleanset <-tm_map(ll$Train,stemDocument)
####################################

## Both job titles and full descriptions continue here
#Build term document matrix

cleanset <- tm_map(cleanset, PlainTextDocument)
#think about min word length
dtm <- TermDocumentMatrix(cleanset, control=list(minWordLength=3))

# inspect frequent words

lf <- nnn  # choose a number to cut off low frequency
findFreqTerms(dtm, lowfreq=lf)

# NOTE: At this point, might choose to alter stop words list

#Bar plot -- define parameter hf for high frequency
hf <- nnn   # you must choose this.

termFrequency <- rowSums(as.matrix(dtm))
termFrequency <- subset(termFrequency, termFrequency>=hf)

# ggplot bar chart
barplot(termFrequency, las=2)

#WORD CLOUD

m<- as.matrix(dtm)
wordFreq <- sort(rowSums(m), decreasing=TRUE)

grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )

# experiment with settings, as in class example
wordcloud(words=names(wordFreq), freq=wordFreq, max.words=75,scale = c(2, 0.2), random.order=F, colors=grayLevels)
######################
# Now for some common associations
#  just do this for the job titles, NOT for the descriptions

# lim is a value you choose for the correlation limit
# myterms is a list of a few words to explore to find pairs

lim <- 0.5  # feel free to change this!
myterms <- c("term1", "term2","term3")
findAssocs(dtm, myterms, corlimit=lim)

