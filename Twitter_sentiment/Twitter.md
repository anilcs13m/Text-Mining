
#   Twitter Sentiment Analysis
###  Reproducible notes for Twitter Sentiment Analysis

#            Anil Kumar  IIT Madras



## [[source files available on GitHub](https://github.com/anilcs13m)]]
## [[connect on linkedin]](https://in.linkedin.com/in/anilcs13m)]]


#  PRELIMINARIES

Load the library that are required in the assignment:

```r
library("tm")
library("SnowballC")

library("caTools")
library("rpart")
library("rpart.plot")
library("ROCR")
library("randomForest")
```


## INTRODUCTION
 
We will be trying to understand sentiment of tweets about the company Apple, By using the twitter for better understand public perception, Apple wants to monitor how people feel over time and how
people receive new announcements. 

Our challenge in this lecture is to see if we can correctly classify tweets as being negative,
positive, or neither about Apple.


### Using __Text__ as data  

Using data as a data is a difficult task, as text data is not structured as accoring to the requirement and not well written, use of the symbol and other symbolic representation make text analytics more difficult. so handling text data is a challenging problem.
So for this field is called Natural Language Processing comes, goal of NLP is to understand and derive meaning from
human language in a meaning full way so that machine can understand.



### Sentiment Mining - Apple
  
  * Apple is a computer company known for its laptops,phones, tablets, and personal media players
  * Large numbers of fans, large number of “haters”
  * Apple wants to monitor how people feel about them over time, and how people receive new announcements.

# Challenge:

  * Can we correctly classify tweets as being negative, positive, or neither about Apple?

### use of technique to understand the text __bag__of__words__

fully understanding the course is difficult so count the number of time a word appears in the document




### Preprocessing of data

Text data often has many inconsistencies that will cause algorithms trouble
like: Apple, apple and aPple in the text data should we consider as a single word, not multiple words as text data is releted with the apple company only. So for this we diffirent preprocessing techniq to over come such problems, related to the text data.

Here are some of the following steps that we will cover presentation:

 * change all the words in words in lower or upper
 * remove punctuation
 * remove stop words
 * stemming


# Data

To collect the data needed for this task, we had to perform two steps.

#### Collect Twitter data

The first was to collect data about tweets from the internet.   
Twitter data is publicly available, and it can be collected it through scraping the website or via the Twitter API.

The __sender__ of the tweet _might be useful to predict sentiment_, but we will ignore it to keep our data anonymized.   
So we will just be using the text of the tweet.

#### Construct the outcome variable

Then we need to construct the outcome variable for these tweets, which means that we have to label
them as __positive__, __negative__, or __neutral__ sentiment.

We would like to label thousands of tweets, and we know that two people might disagree over the
correct classification of a tweet.
To do this efficiently, one option is to use the _Amazon Mechanical Turk_.

The task that we put on the _Amazon Mechanical Turk_ was to judge the sentiment expressed by the
following item toward the software company Apple.   
The items we gave them were tweets that we had collected.
The workers could pick from the following options as their response: 

* strongly negative, 
* negative,
* neutral,
* positive, and 
* strongly positive.

These outcomes were represented as a number on the scale from __-2__ to __2__.

Each tweet was labeled by five workers.
For each tweet, we take the average of the five scores given by the five workers, hence the 
final scores can range from -2 to 2 in increments of 0.2.

The following graph shows the distribution of the number of tweets classified into each of the
categories.  We can see here that the majority of tweets were classified as neutral, with a small
number classified as strongly negative or strongly positive.


## LOADING AND PROCESSING DATA



```r
tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)
```
__Note__: when working on a text data we add `stringsAsFactors = FALSE`, as an argument.

Explore the structure of our data: 


```r
str(tweets)
```

```
## 'data.frame':	1181 obs. of  2 variables:
##  $ Tweet: chr  "I have to say, Apple has by far the best customer care service I have ever received! @Apple @AppStore" "iOS 7 is so fricking smooth & beautiful!! #ThanxApple @Apple" "LOVE U @APPLE" "Thank you @apple, loving my new iPhone 5S!!!!!  #apple #iphone5S pic.twitter.com/XmHJCU4pcb" ...
##  $ Avg  : num  2 2 1.8 1.8 1.8 1.8 1.8 1.6 1.6 1.6 ...
```

We have __1181__ observations of __2__ variables:

* __Tweet__: the text of the tweet.
* __Avg__: the average sentiment score.

The tweet texts are real tweets that gathered on the internet directed to Apple with a few cleaned up words.

We can view tweets sentiment what is the avg of tweets

```r
hist(tweets$Avg,breaks = 5)
```

![plot of chunk hist](figure/hist-1.png) 

We are more interested in being able to detect the tweets with __clear negative__ sentiment, so
let's define a __new variable__ in our data set called `Negative`.

* equal to TRUE if the average sentiment score is __less than or equal to -1__ 
* equal to FALSE if the average sentiment score is greater than -1.



```r
tweets$Negative <- as.factor(tweets$Avg <= -1)
```
We can see how many tweets are there in the catogory of negative, this can be done with the help of __table__


```r
table(tweets$Negative)
```

```
## 
## FALSE  TRUE 
##   999   182
```
Add one more variable for the positive tweets, tweet for that average sentiment score is __grater than of equal to 1


```r
tweets$Positive <- as.factor(tweets$Avg>=1)
```
Now we can see the structure of our dataframe


```r
str(tweets)
```

```
## 'data.frame':	1181 obs. of  4 variables:
##  $ Tweet   : chr  "I have to say, Apple has by far the best customer care service I have ever received! @Apple @AppStore" "iOS 7 is so fricking smooth & beautiful!! #ThanxApple @Apple" "LOVE U @APPLE" "Thank you @apple, loving my new iPhone 5S!!!!!  #apple #iphone5S pic.twitter.com/XmHJCU4pcb" ...
##  $ Avg     : num  2 2 1.8 1.8 1.8 1.8 1.8 1.6 1.6 1.6 ...
##  $ Negative: Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Positive: Factor w/ 2 levels "FALSE","TRUE": 2 2 2 2 2 2 2 2 2 2 ...
```
# frequency of positive tweets

```r
table(tweets$Positive)
```

```
## 
## FALSE  TRUE 
##  1120    61
```


## CREATING A CORPUS

One of fundamental concepts in text analysis, implemented in the package `tm` as well, 
is that of a __corpus__.    
A __corpus is a collection of documents__.

We will need to convert our tweets to a corpus for pre-processing. 
Various function in the `tm` package can be used to create a corpus in many different ways.    
We will create it from the `tweet` column of our data frame using two functions, `Corpus()` and `VectorSource()`.
We feed to this latter the `Tweets` _variable_ of the `tweets` _data frame_.


```r
corpus <- Corpus(VectorSource(tweets$Tweet))
```

Let's check out our corpus:

```r
corpus
```

```
## <<VCorpus>>
## Metadata:  corpus specific: 0, document level (indexed): 0
## Content:  documents: 1181
```

We can check that the documents match our tweets by using double brackets `[[`.   
To inspect the first tweet in our corpus, we select the first element as: 


```r
corpus[[1]]
```

```
## <<PlainTextDocument>>
## Metadata:  7
## Content:  chars: 101
```

### Pre-processing steps 
   
To deal with text data following pre-processing is required. 

Follow the standard steps to build and pre-process the corpus:

	1) Build a new corpus variable called corpus.

	2) Using tm_map, convert the text to lowercase.

	3) Using tm_map, remove all punctuation from the corpus.

	4) Using tm_map, remove all English stopwords from the corpus.

	5) Using tm_map, stem the words in the corpus.

	6) Build a document term matrix from the corpus, called dtm.

Each operation, like stemming or removing stop words, can be done with one line in R,  
where we use the `tm_map()` function which takes as

* its first argument the name of a __corpus__ and 
* as second argument a __function performing the transformation__ that we want to apply to the text.

First step is to transform all text _to lower case_:


```r
corpus <- tm_map(corpus, tolower)
```

After performing the first step we can check the same "documents" as before:
and we can see that there is no word present in the tweet having upper case character:


```r
corpus[[1]]
```

```
## [1] "i have to say, apple has by far the best customer care service i have ever received! @apple @appstore"
```
### Plain Text Document

converts corpus to a Plain Text Document


```r
corpus <- tm_map(corpus, PlainTextDocument)
```

### Removing punctuation


```r
corpus <- tm_map(corpus, removePunctuation)
```
### Stop Words

Look at stop words 

```r
stopwords("english")[1:10]
```

```
##  [1] "i"         "me"        "my"        "myself"    "we"       
##  [6] "our"       "ours"      "ourselves" "you"       "your"
```
Stop words are the words that having no meaning or very less meaning in the corpus, 
so we remove those words which caring less meaning for our corpus
### Removing _stop words_ (and _apple_)


Removing words can be done with the `removeWords` argument to the `tm_map()` function, with an
extra argument, _i.e._ what the stop words are that we want to remove.  

We will remove all of these English stop words, but we will also remove the word "_apple_"
since all of these tweets have the word "_apple_" and it probably won't be very useful in our
prediction problem.


```r
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
```
Now check out our corpus

```r
corpus[[1]]
```

```
## <<PlainTextDocument>>
## Metadata:  7
## Content:  chars: 67
```

### Stemming

Lastly, we want to stem our document with the `stemDocument` argument.


```r
corpus <- tm_map(corpus, stemDocument)
```


```r
corpus[[1]]
```

```
## <<PlainTextDocument>>
## Metadata:  7
## Content:  chars: 61
```

We can see that this took off the ending of "customer," "service," "received," and "appstore."


## BAG OF WORDS IN R

### Create a _Document Term Matrix_

We are now ready to extract the __word frequencies__ to be used in our prediction problem.
The `tm` package provides a function called `DocumentTermMatrix()` that generates a __matrix__ where:

* the __rows__ correspond to __documents__, in our case tweets, and 
* the __columns__ correspond to __words__ in those tweets.

The values in the matrix are the number of times that word appears in each document.


```r
DTM <- DocumentTermMatrix(corpus)
```


```r
DTM
```

```
## <<DocumentTermMatrix (documents: 1181, terms: 3289)>>
## Non-/sparse entries: 8980/3875329
## Sparsity           : 100%
## Maximal term length: 115
## Weighting          : term frequency (tf)
```

We see that in the corpus there are __3289__ __unique words__.

Let's see what this matrix looks like using the `inspect()` function, in particular
slicing a block of rows/columns from the _Document Term Matrix_ by calling by their indices:

```r
inspect(DTM[1000:1005, 505:515])
```

```
## <<DocumentTermMatrix (documents: 6, terms: 11)>>
## Non-/sparse entries: 1/65
## Sparsity           : 98%
## Maximal term length: 9
## Weighting          : term frequency (tf)
## 
##               Terms
## Docs           cheapen cheaper check cheep cheer cheerio cherylcol chief
##   character(0)       0       0     0     0     0       0         0     0
##   character(0)       0       0     0     0     0       0         0     0
##   character(0)       0       0     0     0     0       0         0     0
##   character(0)       0       0     0     0     0       0         0     0
##   character(0)       0       0     0     0     0       0         0     0
##   character(0)       0       0     0     0     1       0         0     0
##               Terms
## Docs           chiiiiqu child children
##   character(0)        0     0        0
##   character(0)        0     0        0
##   character(0)        0     0        0
##   character(0)        0     0        0
##   character(0)        0     0        0
##   character(0)        0     0        0
```

In this range we see that the word "cheer" appears in the tweet 1005, but "cheap" does not appear
in any of these tweets.
This __data__ is what we call __sparse__.  This means that there are many zeros in our matrix.

We can look at what the most popular terms are, or words, with the function `findFreqTerms()`, 
selecting a minimum number of 20 occurrences over the whole corpus:

```r
freq <- findFreqTerms(DTM, lowfreq = 20)

freq 
```

```
##  [1] "android"              "anyon"                "app"                 
##  [4] "appl"                 "back"                 "batteri"             
##  [7] "better"               "buy"                  "can"                 
## [10] "cant"                 "come"                 "dont"                
## [13] "fingerprint"          "freak"                "get"                 
## [16] "googl"                "ios7"                 "ipad"                
## [19] "iphon"                "iphone5"              "iphone5c"            
## [22] "ipod"                 "ipodplayerpromo"      "itun"                
## [25] "just"                 "like"                 "lol"                 
## [28] "look"                 "love"                 "make"                
## [31] "market"               "microsoft"            "need"                
## [34] "new"                  "now"                  "one"                 
## [37] "phone"                "pleas"                "promo"               
## [40] "promoipodplayerpromo" "realli"               "releas"              
## [43] "samsung"              "say"                  "store"               
## [46] "thank"                "think"                "time"                
## [49] "twitter"              "updat"                "use"                 
## [52] "via"                  "want"                 "well"                
## [55] "will"                 "work"
```
Out of the __3289__ words in our matrix, only __56__ words
appear at least 20 times in our tweets.

This means that we probably have a lot of terms that will be pretty useless for our prediction model.
The number of terms is an issue for two main reasons:

* One is __computational__: more terms means more independent variables, which usually means it takes
longer to build our models.
* The other is that in building models the ratio of independent variables to observations will
affect how well the __model will generalize__. so remove those words which are present less.

### Remove sparse terms

Therefore let's remove some terms that don't appear very often. 

```r
sparse_DTM <- removeSparseTerms(DTM, 0.995)
```
This function takes a second parameters, the __sparsity threshold__.
The sparsity threshold works as follows.

* If we say 0.98, this means to only keep terms that appear in 2% or more of the tweets.
* If we say 0.99, that means to only keep terms that appear in 1% or more of the tweets.
* If we say 0.995, that means to only keep terms that appear in 0.5% or more of the tweets, 
  about six or more tweets.

Let's see what the new _Document Term Matrix_ properties look like:

```r
sparse_DTM
```

```
## <<DocumentTermMatrix (documents: 1181, terms: 309)>>
## Non-/sparse entries: 4669/360260
## Sparsity           : 99%
## Maximal term length: 20
## Weighting          : term frequency (tf)
```
It only contains __309__ unique terms, _i.e._ only about 
__9.4%__ of the full set.


### Convert the DTM to a data frame

Now let's convert the sparse matrix into a data frame that we will be able to use for our
predictive models.

```r
tweetsSparse <- as.data.frame(as.matrix(sparse_DTM))
```

#### Fix variables names in the data frame

Since R struggles with variable names that start with a number, and we probably have some words
here that start with a number, we should run the `make.names()` function to make sure all of our
words are appropriate variable names.
It will convert the variable names to make sure they are all appropriate names for R before we
build our predictive models.
You should do this each time you build a data frame using text analytics.

To make all variable names _R-friendly_ use:

```r
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
```

### Add the _dependent_ variable

We should add back to this data frame our dependent variable to this data set.
We'll call it `tweetsSparse$Negative` for the __Negative__ variable and `tweetsSparse$Positive` for the positive variable, these variables are set from the original `Negative` variable and `Positive` variable from
the tweets data frame.
Add Negative variable

```r
tweetsSparse$Negative <- tweets$Negative
```
Add Positive Variable

```r
tweetsSparse$Positive <- tweets$Positive
```
Now our data is ready for analysis, now build __Machine Learning__ system.

# BUILDING MACHINE LEARNING MODEL
Before Building the machine learning model, we need to split our data in training and training dataset

### Split data in training/testing sets

Lastly, let's split our data into a training set and a testing set, putting __70%__ of the data in
the __training__ set.
Before doing so, we need to set seed, some value so that, we all will get the same result
Split data based on the __Negative__ variable 


```r
set.seed(123)

splitNegative <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparseNegative <- subset(tweetsSparse, splitNegative == TRUE)
testSparseNegative <- subset(tweetsSparse, splitNegative == FALSE)
```
Split data based on the __positive__ variable


```r
splitPositive <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparsePositive <- subset(tweetsSparse, splitPositive == TRUE)
testSparsePositive <- subset(tweetsSparse, splitPositive == FALSE)
```

## PREDICTING SENTIMENT

In this prediction, we are using both the data for positive and negative, training and testing and we are we are going to 
different machine learning model to train on these data set.

So, Let's first use __CART__ to build a predictive model, using the `rpart()` function to predict
`Negative` using all of the other variables as our independent variables and the data set `trainSparseNegative`.

We'll add one more argument here, which is `method = "class"` so that the `rpart()` function knows
to build a classification model.
We keep default settings for all other parameters, in particular we are not adding anything for
`minbucket` or `cp`.


```r
tweetCARTNegative <- rpart(Negative ~ . , data = trainSparseNegative, method = "class")
```
# Plot CART for tweetCARTNegative model

```r
prp(tweetCARTNegative)
```

![plot of chunk model_CART-plot](figure/model_CART-plot-1.png) 

The tree says that

* if the word _"freak"_ is in the tweet, then predict `TRUE`, or __negative__ sentiment.
* If the word _"freak"_ is not in the tweet, but the word _"hate"_ is again predict `TRUE`.
* If neither of these two words are in the tweet, but the word _"wtf"_ is, also predict `TRUE`, 
or __negative__ sentiment.
* If __none of these three words__ are in the tweet, then predict `FALSE`, or __non-negative__ sentiment.

This tree makes sense intuitively since these three words are generally seen as negative words.


Now, Let's build one more model use __CART__ to for prediction, using the `rpart()` function to predict
`Positive` using all of the other variables as our independent variables and the data set `trainSparsePositive`.

We'll add one more argument here, which is `method = "class"` so that the `rpart()` function knows
to build a classification model.
We keep default settings for all other parameters, in particular we are not adding anything for
`minbucket` or `cp`.

```r
tweetCARTPositive <- rpart(Positive ~ . , data = trainSparsePositive, method = "class")
```

Now we have two model ready, one for positive sentiment and one for negative sentiment

### Out-of-Sample performance of the model

Using the `predict()` function we compute the predictions of our model `tweetCARTNegative` and `tweetCARTPositive` on the new data
set `testSparsePositive` and `testSparseNegative`.
Be careful to add the argument `type = "class"` to make sure we get class predictions.

Prediction for the __negative__ sentiment:


```r
predictCARTNegative <- predict(tweetCARTNegative, newdata = testSparseNegative, type = "class")
```
Prediction for the __positive__ sentiment:


```r
predictCARTPositive <- predict(tweetCARTPositive, newdata = testSparsePositive, type = "class")
```

Now, Evalute our model accuracy using different approaches like __confusion matrix__ and __AUC__
So for our prediction let's compute the confusion matrix:


```r
cmat_CARTNegative <- table(testSparseNegative$Negative, predictCARTNegative)
cmat_CARTNegative 
```

```
##        predictCARTNegative
##         FALSE TRUE
##   FALSE   294    6
##   TRUE     37   18
```

```r
accu_CART <- (cmat_CARTNegative[1,1] + cmat_CARTNegative[2,2])/sum(cmat_CARTNegative)
```
# Overall Accuracy for negative sentiment

* Overall Accuracy = __0.8789__    
  Sensitivity = 18 / 55 = __0.3273__ ( = TP rate)    
  Specificity = 294 / 300 = __0.98__    
  FP rate = 6 / 300 = __0.02__


Compute the confusion matrix for the positve:


```r
cmat_CARTPositive <- table(testSparsePositive$Positive, predictCARTPositive)
cmat_CARTPositive
```

```
##        predictCARTPositive
##         FALSE TRUE
##   FALSE   335    0
##   TRUE     20    0
```

```r
accu_CARTP <- (cmat_CARTPositive[1,1] + cmat_CARTPositive[2,2])/sum(cmat_CARTPositive)
```

# Overall Accuracy for the posistive sentiment

* Overall Accuracy For Positive = __0.9437__    
  Sensitivity = 0 / 20 = __0__ ( = TP rate)    
  Specificity = 335 / 335 = __1__    
  FP rate = 0 / 335 = __0__



# BASELINE MODEL

#### Comparison with the _baseline model_

Let's compare this to a simple baseline model that __always predicts non-negative__ (_i.e._ the
most common value of the dependent variable).

To compute the accuracy of the baseline model, let's make a table of just the outcome variable Negative.

```r
cmat_baseline <- table(testSparseNegative$Negative)
cmat_baseline
```

```
## 
## FALSE  TRUE 
##   300    55
```

```r
accu_baseline <- max(cmat_baseline)/sum(cmat_baseline)
```
To compute the accuracy of the baseline model, let's make a table of just the outcome variable Positive.

```r
cmat_baselineP <- table(testSparsePositive$Positive)
cmat_baselineP
```

```
## 
## FALSE  TRUE 
##   335    20
```

```r
accu_baselineP <- max(cmat_baselineP)/sum(cmat_baselineP)
```


The accuracy of the baseline model is then __0.8451__, for __negative__ and __0.9437__,  for positive   
So the __CART model doing better__ than the simple baseline model for both the cases.


# RANDOM FOREST

#### Comparison with a _Random Forest_ model

So, now we are going to build a new model called __Random Forest__ model for both negative and positive dataset.

We use the `randomForest()` function to predict `Negative` and `Positive` again __using all of our other variables__ 
as independent variables and the data set `trainSparsePositive` and `trainSparseNegative`.

For this model also we are using the default parameter settings:
random forest for the negative sentiment


```r
set.seed(123)
tweetRFN <- randomForest(Negative ~ . , data = trainSparseNegative)

tweetRFN
```

```
## 
## Call:
##  randomForest(formula = Negative ~ ., data = trainSparseNegative) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 17
## 
##         OOB estimate of  error rate: 10.46%
## Confusion matrix:
##       FALSE TRUE class.error
## FALSE   686   17  0.02418208
## TRUE     70   59  0.54263566
```
random forest for positive sentiment


```r
set.seed(123)
tweetRFP <- randomForest(Positive ~ . , data = trainSparsePositive)

tweetRFP
```

```
## 
## Call:
##  randomForest(formula = Positive ~ ., data = trainSparsePositive) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 17
## 
##         OOB estimate of  error rate: 6.37%
## Confusion matrix:
##       FALSE TRUE class.error
## FALSE   779    1 0.001282051
## TRUE     52    0 1.000000000
```

# ACCURACY

And then compute the Out-of-Sample predictions for negative:

```r
predictRFN <- predict(tweetRFN, newdata = testSparseNegative)
```
And then compute the Out-of-Sample predictions for positive:

```r
predictRFP <- predict(tweetRFP, newdata = testSparsePositive)
```
for calculating the accuracy of model, there are so many ways here, we are using the confusion matrix 
and AUC 

compute the _confusion matrix_ for negative:

```r
cmat_RFN <- table(testSparseNegative$Negative, predictRFN)
cmat_RFN 
```

```
##        predictRFN
##         FALSE TRUE
##   FALSE   296    4
##   TRUE     26   29
```

```r
accu_RFN <- (cmat_RFN[1,1] + cmat_RFN[2,2])/sum(cmat_RFN)
```

compute the _confusion matrix_ for positive:

```r
cmat_RFP <- table(testSparsePositive$Positive, predictRFP)
cmat_RFP 
```

```
##        predictRFP
##         FALSE TRUE
##   FALSE   335    0
##   TRUE     18    2
```

```r
accu_RFP <- (cmat_RFP[1,1] + cmat_RFP[2,2])/sum(cmat_RFP)
```

The overall __accuracy__ of this _Random Forest_ model is __0.9155__ for the __Negative__ and __0.9493__ for the __Positive__ sentiment.     
This model is a __little better than the CART model__, but due to the __interpretability of the CART model__,
this latter would probably be preferred over the random forest model.

If you were to use __cross-validation to pick__ the `cp` parameter for the _CART model_, the accuracy
would increase to about the same as the random forest model.

So by using a bag-of-words approach and these models, we can reasonably predict sentiment even
with a relatively small data set of tweets.

# Logistic Regression Model

#### Comparison with _logistic regression_ model

we are creating a logistic regression model called as __tweetLogN__ for __Negative__ and __tweetLogP__ for the __Positive__, for logistic regression we use function __glm__ with family binomial.

Build the model for the __Negative__ sentiment, using all independent variables as predictors:

```r
tweetLogN <- glm(Negative ~ . , data = trainSparseNegative, family = "binomial")
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
# summary(tweetLogN)
```

Build the model for the __Positive__ sentiment, using all independent variables as predictors:

```r
tweetLogP <- glm(Positive ~ . , data = trainSparsePositive, family = "binomial")
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
# summary(tweetLogP)
```
# ACCURACY

Prediction the accuracy of this model on the testing set for negative:

```r
tweetLog_predict_testN <- predict(tweetLogN, type = "response", newdata = testSparseNegative)
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

Prediction the accuracy of this model on the testing set for positive:

```r
tweetLog_predict_testP <- predict(tweetLogP, type = "response", newdata = testSparsePositive)
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

Confusion matrix for negative with threshold more then 0.5:

```r
cmat_logRegrN <- table(testSparseNegative$Negative, tweetLog_predict_testN > 0.5)
cmat_logRegrN
```

```
##        
##         FALSE TRUE
##   FALSE   280   20
##   TRUE     21   34
```

```r
accu_logRegrN <- (cmat_logRegrN[1,1] + cmat_logRegrN[2,2])/sum(cmat_logRegrN)
```

Confusion matrix for positive with threshold more then 0.5:

```r
cmat_logRegrP <- table(testSparsePositive$Positive, tweetLog_predict_testP > 0.5)
cmat_logRegrP
```

```
##        
##         FALSE TRUE
##   FALSE   313   22
##   TRUE      4   16
```

```r
accu_logRegrP <- (cmat_logRegrP[1,1] + cmat_logRegrP[2,2])/sum(cmat_logRegrP)
```

### The Perils of Over-fitting

The overall __accuracy__ of this _logistic regression_ model is __0.8845__ for __Negative__  and __ 0.9268__ for __Positive__ sentiment 

which is __worse than the baseline (?!)__.    

If you were to compute the accuracy on the training set instead, you would see that the model does
really well on the training set.     
This is an example of __over-fitting__. The model fits the training set really well, but does not
perform well on the test set. 
A __logistic regression model with a large number of variables is particularly at risk for overfitting__.

---

## THE ANALYTICS EDGE

* Analytical sentiment analysis can replace more labor-intensive methods like polling.
* Text analytics can deal with the massive amounts of unstructured data being generated on the internet.
* Computers are becoming more and more capable of interacting with humans and performing human tasks.

---
