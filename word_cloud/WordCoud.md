
#   Word Cloud From Document
#### Here in this report word cloud creation
#            Anil Kumar  IIT Madras



## [[source files available on GitHub](https://github.com/anilcs13m)]]


#  PRELIMINARIES

Load the library that are required in the assignment:

```r
library("tm")
library("SnowballC")
library("wordcloud")
```


## INTRODUCTION
Here in this report we are going to create a word cloud for a given document, basically in word cloud we show the word with higher frequency with bigger size and the words with lower frequency with smaller size

In this report we are going to generate a word cloud from the __PDF__ document





# loading the corpus 

our corpus contain only PDF file so before loading the corpus into memory it require to conver that into the txt file. 
create a directory name __corpus__ inside this directory create another directory name __pdf__ so full path is like this 
our __.R__ file and our __corpus__ directory are in the same directory and __pdf__ document is in __corpus/pdf/*.pdf__


## path of the all pdf file present in the corpus

```r
file_path = file.path(".","corpus","pdf") 
```
you can list all the files present in the directory by using the __dir__ function

```r
dir(file_path)
```

```
##  [1] "ausdm07.pdf"                        
##  [2] "eJHI06.pdf"                         
##  [3] "hdm05.pdf"                          
##  [4] "jeff.pdf"                           
##  [5] "miningmodels.pdf"                   
##  [6] "performance-modeling-message.pdf"   
##  [7] "probability_cheatsheet.pdf"         
##  [8] "RJournal_2009-1_Guazzelli+et+al.pdf"
##  [9] "RJournal_2009-2_Williams.pdf"       
## [10] "story.pdf"
```
Load the files from the directory and make the corpus, as all of the file present here are __PDF__ file so for that we use reader as readPDF

```r
myCorpus <-Corpus(DirSource(file_path), readerControl = list(reader = readPDF ))
```
let's view our corpus

```r
myCorpus[1]
```

```
## <<VCorpus>>
## Metadata:  corpus specific: 0, document level (indexed): 0
## Content:  documents: 1
```
Now apply some preprocessing to this corpus to create word cloud


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



```r
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
myCorpus <- tm_map(myCorpus, toSpace, "/|@|\\|")
```

# conver to lower use tm_map function from the tm package

```r
myCorpus <- tm_map(myCorpus,content_transformer(tolower))
```
# remove stop words

```r
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
```
## remove some other words
some of the words which are not present in the stop words list but we are required to remove them because these words no carry any meaning in our document

```r
myCorpus <- tm_map(myCorpus, removeWords, c("one","let","set","prove","path","use","case","follow","number"))
```

## remove punctuation

```r
myCorpus <- tm_map(myCorpus, removePunctuation)
```
## remove white space

```r
myCorpus <- tm_map(myCorpus, stripWhitespace)
```

## remove number 

```r
myCorpus <- tm_map(myCorpus, removeNumbers)
```

## stemming
stem the document for stemming the document we are using the __SnowballC__ package from __cran__

```r
myCorpus <-tm_map(myCorpus,stemDocument)
```
Now our corpus is ready to apply our algorithm for creating the corpus, we first conver this to document term matrix
to create document term matrix __R__ provied a function __DocumentTermMatrix__

## DocumentTermMatrix

```r
myCorpusDTM <- DocumentTermMatrix(myCorpus)
```
Let's inspect __DTM__ 

```r
inspect(myCorpusDTM[1:4,100:106])
```

```
## <<DocumentTermMatrix (documents: 4, terms: 7)>>
## Non-/sparse entries: 6/22
## Sparsity           : 79%
## Maximal term length: 13
## Weighting          : term frequency (tf)
## 
##              Terms
## Docs          advoc aﬀect aerophobesrus aex afa afﬂatus affect
##   ausdm07.pdf     0     0             0   0   0       0      0
##   eJHI06.pdf      0     0             0   0   0       0      1
##   hdm05.pdf       0     1             0   0   1       0      0
##   jeff.pdf        0     0             1   0   0       1      4
```
## exploring the document term matrix

```r
findFreqTerms(myCorpusDTM, lowfreq=100)
```

```
##   [1] "actual"     "algorithm"  "also"       "alway"      "amort"     
##   [6] "analysi"    "analyz"     "anoth"      "answer"     "approxim"  
##  [11] "arbitrari"  "array"      "assign"     "assum"      "base"      
##  [16] "binari"     "bit"        "bolt"       "bound"      "call"      
##  [21] "can"        "capac"      "case"       "chang"      "choos"     
##  [26] "class"      "cluster"    "color"      "common"     "compon"    
##  [31] "comput"     "connect"    "consid"     "constant"   "contain"   
##  [36] "correct"    "cost"       "cover"      "cut"        "cycl"      
##  [41] "data"       "dataset"    "decis"      "deﬁn"       "deﬁnit"    
##  [46] "denot"      "depth"      "describ"    "determin"   "develop"   
##  [51] "differ"     "direct"     "distanc"    "distribut"  "dont"      
##  [56] "dynam"      "edg"        "edit"       "efﬁcient"   "either"    
##  [61] "element"    "els"        "end"        "equal"      "even"      
##  [66] "event"      "everi"      "exact"      "exampl"     "expect"    
##  [71] "fact"       "feasibl"    "follow"     "form"       "formula"   
##  [76] "function"   "game"       "general"    "get"        "give"      
##  [81] "given"      "graph"      "greedi"     "hash"       "hint"      
##  [86] "http"       "impli"      "includ"     "increas"    "independ"  
##  [91] "indic"      "induct"     "input"      "insert"     "integ"     
##  [96] "interest"   "item"       "jeff"       "just"       "key"       
## [101] "know"       "larg"       "largest"    "least"      "lectur"    
## [106] "length"     "level"      "licens"     "like"       "line"      
## [111] "linear"     "list"       "log"        "look"       "make"      
## [116] "mani"       "map"        "match"      "maximum"    "may"       
## [121] "mean"       "method"     "might"      "mine"       "minimum"   
## [126] "model"      "move"       "multipl"    "must"       "ﬁnd"       
## [131] "need"       "network"    "new"        "node"       "note"      
## [136] "now"        "nphard"     "number"     "oper"       "optim"     
## [141] "order"      "origin"     "ﬂow"        "pair"       "particular"
## [146] "path"       "pattern"    "perform"    "pmml"       "point"     
## [151] "polynomi"   "popul"      "posit"      "possibl"    "prioriti"  
## [156] "probabl"    "problem"    "program"    "proof"      "random"    
## [161] "rank"       "rattl"      "recurr"     "recurs"     "reduct"    
## [166] "repres"     "requir"     "result"     "return"     "right"     
## [171] "root"       "ﬁrst"       "run"        "search"     "see"       
## [176] "sequenc"    "set"        "shortest"   "show"       "simpl"     
## [181] "sinc"       "singl"      "size"       "small"      "smallest"  
## [186] "solut"      "solv"       "sort"       "span"       "start"     
## [191] "step"       "strong"     "structur"   "subset"     "sum"       
## [196] "suppos"     "system"     "tabl"       "take"       "target"    
## [201] "term"       "theorem"    "three"      "thus"       "time"      
## [206] "total"      "transform"  "tree"       "true"       "two"       
## [211] "use"        "valu"       "variabl"    "vector"     "vertex"    
## [216] "vertic"     "want"       "way"        "weight"     "whether"   
## [221] "will"       "word"       "work"       "worst"
```
find association in data

```r
# findAssocs(myCorpusDTM, "data", corlimit=0.6)
```
# Sort the corpus

```r
freq <- sort(colSums(as.matrix(myCorpusDTM)), decreasing=TRUE)
```
Now conver __myCorpusDTM__ to the matrix

```r
fmatrixtdm <- as.matrix(myCorpusDTM)
```
Write this to __CSV__ file

```r
# write.csv(fmatrixtdm,file = "myCorpusDTM.csv")
```

# WORD CLOUD

```r
wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))
```

![plot of chunk cloud](figure/cloud-1.png) 
# one more

```r
wordcloud(names(freq), freq, scale=c(6,0.7), max.words=150, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))
```

![plot of chunk onemore](figure/onemore-1.png) 






