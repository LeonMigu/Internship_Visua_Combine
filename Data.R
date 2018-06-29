#Data (it will be the preprocessing of Colette)

# Read the text file from internet
filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)

# Load the data as a corpus
docs <- Corpus(VectorSource(text))

inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

#Creating the data frame that will be used by the code

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
r <- abs(rnorm(length(v)))
w <- names(v)
#Changing the data structure in order that the id is a number and not a word, if not it doesn't work
names(v) <- seq(1, length(v))
#Implementing rownames in order to access them later. It will be useful to make the key
wr <- seq(1, length(v))
#Creating the dataframe 
d <- data.frame(rowname = wr, word = w, freq=v, random = r)
head(d, 10)

#Find the maximum frequency and the number of words to implement the sliderinputs
m <- max(d$freq)
n <- NROW(d)