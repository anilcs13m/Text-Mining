# get working directory
getwd()
# load data set
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
str(wiki)
# convert Vandal to a factor with the command:
wiki$Vandal = as.factor(wiki$Vandal)
# to see how many vandal are there use table 
table(wiki$Vandal)
#  BAGS OF WORDS
# we will use bag of words aproach to build model
# 1) Create the corpus for the Added column, and call it "corpusAdded".
# 2) Remove the English-language stopwords.
# 3) Stem the words.
# 4) Build the DocumentTermMatrix, and call it dtmAdded.
library(tm)
library(SnowballC)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caTools)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded
# Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
# add the char "A" 
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
# use the same number of steps for the remove the bag-of-words dataframe

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
# combined both of the column remove and added
wikiWords = cbind(wordsAdded, wordsRemoved)
# add the vandal variable from the original data frame
wikiWords$Vandal = wiki$Vandal
# split the data frame use 70% of the data

# library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain = subset(wikiWords, spl==TRUE)
wikiTest = subset(wikiWords, spl==FALSE)
# what is the accuracy of baseline model
table(wikiWords$Vandal)
618/(618+545) 
# Build a CART model to predict Vandal, using all of the other variables as independent variables.
wikiCART = rpart(Vandal ~ ., data=wikiTrain, method="class")
# make predictions on the test set
testPredictCART = predict(wikiCART, newdata=wikiTest, type="class")
# accuracy
table(wikiTest$Vandal, testPredictCART)
(618+12)/(618+533+12)
prp(wikiCART)
# PROBLEM-SPECIFIC KNOWLEDGE
# Create a copy of your dataframe
wikiWords2 = wikiWords
# add a new colunm http 
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
# base on the colunm, how many revisions added a link
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
# create a new CART model using this new variable as one of the independent variables, threshold 0.5
wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
testPredictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, testPredictCART2)
(609+57)/(609+9+488+57)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
testPredictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, testPredictCART3)
(514+248)/(514+104+297+248)
#######
# USING NON-TEXTUAL DATA
######
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)
wikiCART4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")
predictTestCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, predictTestCART4)
(595+241)/(595+23+304+241)
prp(wikiCART4)











