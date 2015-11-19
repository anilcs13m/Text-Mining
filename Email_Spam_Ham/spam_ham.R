# get working directory
getwd()
# load data
emails = read.csv("emails.csv", stringsAsFactors=FALSE)
# How many of the emails are spam
table(emails$spam)
# Which word appears at the beginning of every email in the dataset
emails$text[1] #subject
# How many characters are in the longest email in the dataset 
# (where longest is measured in terms of the maximum number of characters)?
max(nchar(emails$text))

min(nchar(emails$text)) # 13
# Which row contains the shortest email in the dataset?
which(nchar(emails$text) == 13)
# OR
which.min(nchar(emails$text))
#####
#  PREPARING THE CORPUS 
###
# 1) Build a new corpus variable called corpus.
# 2) Using tm_map, convert the text to lowercase.
# 3) Using tm_map, remove all punctuation from the corpus.
# 4) Using tm_map, remove all English stopwords from the corpus.
# 5) Using tm_map, stem the words in the corpus.
# 6) Build a document term matrix from the corpus, called dtm.
library(tm)
library(SnowballC)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caTools)

corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

spdtm = removeSparseTerms(dtm, 0.95)
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))
#use the make.names function to make the variable names of emailsSparse valid.
colnames(emailsSparse) = make.names(colnames(emailsSparse))
# What is the word stem that shows up most frequently across all the emails in the dataset
sort(colSums(emailsSparse))
which.max(colSums(emailsSparse))

emailsSparse$spam = emails$spam
# We can read the most frequent terms in the ham dataset
sort(colSums(subset(emailsSparse, spam == 0)))
# dataset to the spam emails
subset(emailsSparse, spam == 1)
# we can read the most frequent terms 
sort(colSums(subset(emailsSparse, spam == 1)))
#####
# BUILDING MACHINE LEARNING MODELS
#####
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
# split the data set
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)
#1) A logistic regression model called spamLog. You may see a warning message here - we'll discuss this more later.
#2) A CART model called spamCART, using the default parameters to train the model
spamLog = glm(spam~., data=train, family="binomial")
spamCART = rpart(spam~., data=train, method="class")
set.seed(123)
spamRF = randomForest(spam~., data=train)

predTrainLog = predict(spamLog, type="response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type="prob")[,2]

table(predTrainLog < 0.00001)
table(predTrainLog > 0.99999)
table(predTrainLog >= 0.00001 & predTrainLog <= 0.99999)
summary(spamLog)
prp(spamCART)
# What is the training set accuracy of spamLog threshold 0.5
table(train$spam, predTrainLog > 0.5)
(3052+954)/nrow(train)
# What is the training set AUC of spamLog?
predictionTrainLog = predict(predTrainLog, train$spam)
as.numeric(performance(predictionTrainLog, "auc")@y.values)

table(train$spam, predTrainCART > 0.5)
# accuracy
(2885+894)/nrow(train)
# What is the training set AUC of spamCART?
predictionTrainCART = prediction(predTrainCART, train$spam)
as.numeric(performance(predictionTrainCART, "auc")@y.values)
# What is the training set accuracy of spamRF with threshold 0.5
table(train$spam, predTrainRF > 0.5)
# accuracy 
(3013+914)/nrow(train)
# What is the training set AUC of spamRF?
predictionTrainRF = prediction(predTrainRF, train$spam)
as.numeric(performance(predictionTrainRF, "auc")@y.values)
###
# EVALUATING ON THE TEST SET
###
predTestLog = predict(spamLog, newdata=test, type="response")
predTestCART = predict(spamCART, newdata=test)[,2]
predTestRF = predict(spamRF, newdata=test, type="prob")[,2]
# testing set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(test$spam, predTestLog > 0.5)
#accuracy
(1257+376)/nrow(test)
# What is the testing set AUC of spamLog
predictionTestLog = prediction(predTestLog, test$spam)
as.numeric(performance(predictionTestLog, "auc")@y.values)
# What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?
table(test$spam, predTestCART > 0.5)
#accuracy 
(1228+386)/nrow(test)
# What is the testing set AUC of spamCART?
predictionTestCART = prediction(predTestCART, test$spam)
as.numeric(performance(predictionTestCART, "auc")@y.values)
# What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
table(test$spam, predTestRF > 0.5)
#accuracy 
(1290+385)/nrow(test)
# What is the testing set AUC of spamRF?
predictionTestRF = prediction(predTestRF, test$spam)
as.numeric(performance(predictionTestRF, "auc")@y.values)














