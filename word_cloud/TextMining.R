library(tm)

## loading the corpus 
## our corpus contain only PDF file so before loading the corpus into memory it require to conver that into the txt file

file_path = file.path(".","corpus","pdf") ## path of the all pdf file present in the corpus

##file_path  ## path of all files present in the corpus
## dir(file_path)  ## list all the file present 
## loading file from the directory to make a corpus 
## as all the file are pdf so we use pdf ot text using the readPDF()

myCorpus <-Corpus(DirSource(file_path), readerControl = list(reader = readPDF ))

# myCorpus[1]

# getTransformations()
# transformation

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
myCorpus <- tm_map(myCorpus, toSpace, "/|@|\\|")

## conver to lower use tm_map function from the tm package
myCorpus <- tm_map(myCorpus,content_transformer(tolower))
## remove stop words
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))

myCorpus <- tm_map(myCorpus, removeWords, c("one","let","set","prove","path","use","case","follow","number"))

## remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
## remove white space
myCorpus <- tm_map(myCorpus, stripWhitespace)
## remove number 
myCorpus <- tm_map(myCorpus, removeNumbers)

# stem document for that we will use SnowballC package from cran
library(SnowballC)

myCorpus <-tm_map(myCorpus,stemDocument)

## now our corpus is ready
## create a document term matrix using the function DocumentTermMatrix()
myCorpusDTM <- DocumentTermMatrix(myCorpus)
# inspect(myCorpusDTM[1:4,100:106])
# exploring the document term matrix
findFreqTerms(myCorpusDTM, lowfreq=100)
# findAssocs(myCorpusDTM, "data", corlimit=0.6)
freq <- sort(colSums(as.matrix(myCorpusDTM)), decreasing=TRUE)

fmatrixtdm <- as.matrix(myCorpusDTM)
# write.csv(matrixtdm,file = "myCorpusDTM.csv")
# correlations between words
# plot(matrixtdm,terms = findFreqTerms(matrixtdm,lowfreq = 100)[1:50],corThreshold = 0.5)


library(wordcloud)
wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))



# one more

wordcloud(names(freq), freq, scale=c(6,0.7), max.words=150, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

