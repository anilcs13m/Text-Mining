
#   Vandalism Wiki
###  Reproducible notes for Wiki

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
 
Wikipedia is a free online encyclopedia that anyone can edit and contribute to. It is available in many languages and is
growing all the time. On the English language version of Wikipedia:

One of the consequences of being editable by anyone is that some people vandalize pages. This can take the form of
removing content, adding promotional or inappropriate content, or more subtle shifts that change the meaning of the
article. With this many articles and edits per day it is difficult for humans to detect all instances of vandalism
and revert (undo) them. As a result, Wikipedia uses bots - computer programs that automatically revert edits that look like
vandalism. In this assignment we will attempt to develop a vandalism detector that uses machine learning to distinguish
between a valid edit and vandalism.

The data for we are using this problem is based on the revision history of the page Language. Wikipedia provides a history for each page
that consists of the state of the page at each revision. Rather than manually considering each revision, a script was run that
checked whether edits stayed or were reverted. If a change was eventually reverted then that revision is marked as
vandalism. This may result in some misclassifications, but the script performs well enough for our needs.

As a result of this preprocessing, some common processing tasks have already been done, including lower-casing and
punctuation removal. The columns in the dataset are:

	* Vandal = 1 if this edit was vandalism, 0 if not.
	* Minor = 1 if the user marked this edit as a "minor edit", 0 if not.
	* Loggedin = 1 if the user made this edit while using a Wikipedia account, 0 if they did not.
	* Added = The unique words added.
	* Removed = The unique words removed.




### Using __Text__ as data  

Using data as a data is a difficult task, as text data is not structured as accoring to the requirement and not well written, use of the symbol and other symbolic representation make text analytics more difficult. so handling text data is a challenging problem.
So for this field is called Natural Language Processing comes, goal of NLP is to understand and derive meaning from
human language in a meaning full way so that machine can understand.


### use of technique to understand the text __bag__of__words__

fully understanding the course is difficult so count the number of time a word appears in the document




### Preprocessing of data

Text data often has many inconsistencies that will cause algorithms trouble

Here are some of the following steps that we will cover presentation:

 * change all the words in words in lower or upper
 * remove punctuation
 * remove stop words
 * stemming


# Data

To collect the data needed for this task, we had to perform two steps.



## LOADING AND PROCESSING DATA



```r
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
```
__Note__: when working on a text data we add `stringsAsFactors = FALSE`, as an argument.

Explore the structure of our data: 


```r
str(wiki)
```

```
## 'data.frame':	3876 obs. of  7 variables:
##  $ X.1     : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ X       : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ Vandal  : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ Minor   : int  1 1 0 1 1 0 0 0 1 0 ...
##  $ Loggedin: int  1 1 1 0 1 1 1 1 1 0 ...
##  $ Added   : chr  "  represent psycholinguisticspsycholinguistics orthographyorthography help text all actions through human ethnologue relationsh"| __truncated__ " website external links" " " " afghanistan used iran mostly that farsiis is countries some xmlspacepreservepersian parts tajikestan region" ...
##  $ Removed : chr  " " " talklanguagetalk" " regarded as technologytechnologies human first" "  represent psycholinguisticspsycholinguistics orthographyorthography help all actions through ethnologue relationships linguis"| __truncated__ ...
```
Suppose we want to check how many cases of vandalism are there in the page, this can be done analysis variable vandal which represent 0 and 1 for a particuler page is vandalism or not, this can be done by view table of all the values in vandal variable.
for view table vandal variable we first convert vandal variable to __factor__ variable by using the following command:

```r
wiki$Vandal = as.factor(wiki$Vandal)
```
Now we can see how many cases of vandalism are there.


```r
table(wiki$Vandal)
```

```
## 
##    0    1 
## 2061 1815
```
we can view histogram also

```r
hist(wiki$Vandal)
```

```
## Error in hist.default(wiki$Vandal): 'x' must be numeric
```


## CREATING A CORPUS

One of fundamental concepts in text analysis, implemented in the package `tm` as well, 
is that of a __corpus__.    
A __corpus is a collection of documents__.

We will need to convert our wiki to a corpus for pre-processing. 
Various function in the `tm` package can be used to create a corpus in many different ways.    

We will now use the bag of words approach to build a model. We have two columns of textual data, with different
meanings. For example, adding rude words has a different meaning to removing rude words.

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

Here are the few steps:
And we can view how many terms are there in the __dtmAdded__

```r
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded
```

```
## <<DocumentTermMatrix (documents: 3876, terms: 6675)>>
## Non-/sparse entries: 15368/25856932
## Sparsity           : 100%
## Maximal term length: 784
## Weighting          : term frequency (tf)
```
We see that in the corpus there are __6675__ __unique words__.


### Remove sparse terms

We can Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions, and call the new matrix
sparseAdded. 


```r
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
```
This function takes a second parameters, the __sparsity threshold__.
The sparsity threshold works as follows.

* If we say 0.98, this means to only keep terms that appear in 2% or more.
* If we say 0.99, that means to only keep terms that appear in 1% or more.
* If we say 0.995, that means to only keep terms that appear in 0.5% or more. 
  

Let's see what the new _Document Term Matrix_ properties look like:

```r
sparseAdded
```

```
## <<DocumentTermMatrix (documents: 3876, terms: 166)>>
## Non-/sparse entries: 2681/640735
## Sparsity           : 100%
## Maximal term length: 28
## Weighting          : term frequency (tf)
```

It only contains __166__ unique terms, _i.e._ only about 
__2.5%__ of the full set.


### Convert the DTM to a data frame

Now let's convert the sparse matrix into a data frame that we will be able to use for our
predictive models.

```r
wordsAdded = as.data.frame(as.matrix(sparseAdded))
```
Prepend all the words with the letter A, by using the command:

```r
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
```
Now repeat all of the steps we've done so far (create a corpus, remove stop words, stem the document, create a sparse
document term matrix, and convert it to a data frame) to create a Removed bag-of-words dataframe, called
wordsRemoved, except this time, prepend all of the words with the letter R:

All the steps that we did for adding for remoing:


```r
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
```
we can see how many words are there in the removable dataframe

```r
ncol(wordsRemoved) 
```

```
## [1] 162
```
# combine two data frame

we can combine both the data frame by using the command __cbind__ in R.

```r
wikiWords = cbind(wordsAdded, wordsRemoved)
```
Let's add original variable __vandal__ from original dataframe __wiki__ to our new dataframe:

```r
wikiWords$Vandal = wiki$Vandal
```
# Building model 
now it's time to build model, before building the model we need to split data into training and testing:
In this split we are usign the 70% of data in training and rest of data in testing:

```r
set.seed(123)
spl = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain = subset(wikiWords, spl==TRUE)
wikiTest = subset(wikiWords, spl==FALSE)
```
# Accuracy
Now we can check what is the accuracy of baseline model:

```r
table(wikiTest$Vandal)
```

```
## 
##   0   1 
## 618 545
```

```r
618/(618+545) 
```

```
## [1] 0.5313844
```

## BUILDING CART MODEL

Building the __CART__ model to predict __Vandal__, using all other variables as independent variables, and are using default paramenters,
then we will check __accuracy__ of our model on the test dataset, using threshold of 0.5:

```r
wikiCART = rpart(Vandal ~ ., data=wikiTrain, method="class")
```

### make predictions on the test set

```r
testPredictCART = predict(wikiCART, newdata=wikiTest, type="class")
```
# accuracy

```r
table(wikiTest$Vandal, testPredictCART)
```

```
##    testPredictCART
##       0   1
##   0 618   0
##   1 533  12
```

```r
(618+12)/(618+533+12)
```

```
## [1] 0.5417025
```
# plot__CART__

```r
prp(wikiCART)
```

![plot of chunk plot_CART](figure/plot_CART-1.png) 
In this plot we can this that __CART__ model use two words "R arbitr" and "R thousa".

# PROBLEM-SPECIFIC KNOWLEDGE

We weren't able to improve on the baseline using the raw textual information. More specifically, the words themselves
were not useful. There are other options though, and in this section we will try two techniques - identifying a key class of
words, and counting words.
The key class of words we will use are website addresses. "Website addresses" (also known as URLs - Uniform Resource
Locators) are comprised of two main parts. An example would be "http://www.google.com". The first part is the protocol,
which is usually "http" (HyperText Transfer Protocol). The second part is the address of the site, e.g. "www.google.com". We
have stripped all punctuation so links to websites appear in the data as one word, e.g. "httpwwwgooglecom". We
hypothesize that given that a lot of vandalism seems to be adding links to promotional or irrelevant websites, the presence
of a web address is a sign of vandalism.

Create a copy of dataframe 

```r
wikiWords2 = wikiWords
```
In this dataframe add a new variable call __HTTP__

```r
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
```
Here this function __grepl__ is searching for the __http__ in __Added__ variable and if it is present in the variable and add 1 to the new variable __HTTP__ of new dataframe __wikiWords2__. We can see how many are added by using table command


```r
table(wikiWords2$HTTP)
```

```
## 
##    0    1 
## 3659  217
```
# Building Model
Now building new model by using the new dataframe __wikiWords2__, before building this model we need to split this data frame into training and testing, 

```r
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
```
# NEW CART MODEL

```r
wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
testPredictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, testPredictCART2)
```

```
##    testPredictCART2
##       0   1
##   0 609   9
##   1 488  57
```

```r
(609+57)/(609+9+488+57)
```

```
## [1] 0.5726569
```

Another possibility is that the number of words added and removed is predictive, perhaps more so than the actual words
themselves. We already have a word count available in the form of the document-term matrices (DTMs).
Sum the rows of dtmAdded and dtmRemoved and add them as new variables in your data frame wikiWords2 (called
NumWordsAdded and NumWordsRemoved).

```r
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)
```

```
## [1] 4.050052
```
# CART MODEL
Let's build one more cart model, after adding the variables:


```r
wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
testPredictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, testPredictCART3)
```

```
##    testPredictCART3
##       0   1
##   0 514 104
##   1 297 248
```

```r
(514+248)/(514+104+297+248)
```

```
## [1] 0.6552021
```
# Use Non-texual Data
we have two pieces of metadata lets use those data variable 
before using those data variable let's make a new data frame __wikiWords3__:

```r
wikiWords3 = wikiWords2
```
Now add those two __metadata__ to our new __wikiWords3__ dataframe:

```r
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
```
# New split 
now split dataframe after adding these two new variable:

```r
wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)
```
#CART MODEL

```r
wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)
wikiCART4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")
predictTestCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, predictTestCART4)
```

```
##    predictTestCART4
##       0   1
##   0 595  23
##   1 304 241
```

```r
(595+241)/(595+23+304+241)
```

```
## [1] 0.7188306
```

```r
prp(wikiCART4)
```

![plot of chunk cart_model4](figure/cart_model4-1.png) 



