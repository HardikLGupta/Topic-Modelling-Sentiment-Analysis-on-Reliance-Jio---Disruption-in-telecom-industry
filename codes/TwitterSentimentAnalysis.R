# rm(list=ls())                   # Clear workspace

#--------------------------------------------------------#
# Step 0 - Assign Library & define functions             #
#--------------------------------------------------------#

library(text2vec)
library(data.table)
library(stringr)
library(tm)
library(RWeka)
library(tokenizers)
library(slam)
library(wordcloud)
library(ggplot2)

text.clean = function(x)                    # text data
{ require("tm")
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}


#--------------------------------------------------------#
# Step 1 - Reading text data                             #
#--------------------------------------------------------#

# temp.text = readLines('C:\\Users\\30773\\Desktop\\Data Science\\cba batch 7\\text classification\\testdata.txt')
temp.text = readLines(file.choose())  #  iron man reviews
# head(temp.text,1)
data = data.frame(id = 1:length(temp.text), text = temp.text, stringsAsFactors = F)
# dim(data)

# Read Stopwords list
stpw1 = readLines('F:/BIG DATA/ISB/Assignments/Term 1/Practicum-2/stopwords.txt')      #
stpw2 = tm::stopwords('english')                   # tm package stop word list; tokenizer package has the same name function
comn  = unique(c(stpw1, stpw2))                 # Union of two list
stopwords = unique(gsub("'"," ",comn))  # final stop word lsit after removing punctuation

x  = text.clean(data$text)             # pre-process text corpus
x  =  removeWords(x,stopwords)            # removing stopwords created above
x  =  stripWhitespace(x)                  # removing white space
# x  =  stemDocument(x)

#--------------------------------------------------------#
######  Create DTM using text2vec package                #
#--------------------------------------------------------#

t1 = Sys.time()

tok_fun = word_tokenizer

it_m = itoken(x,
              # preprocessor = text.clean,
              tokenizer = tok_fun,
              ids = data$id,
              progressbar = T)

vocab = create_vocabulary(it_m
                          # ngram = c(2L, 2L),
                          #stopwords = stopwords
                          )

pruned_vocab = prune_vocabulary(vocab,
                                term_count_min = 1)
# doc_proportion_max = 0.5,
# doc_proportion_min = 0.001)

vectorizer = vocab_vectorizer(pruned_vocab)

dtm_m  = create_dtm(it_m, vectorizer)
dim(dtm_m)

dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
  a0 = (apply(dtm, 1, sum) > 0)   # build vector to identify non-empty docs
  dtm = dtm[a0,]                  # drop empty docs


print(difftime(Sys.time(), t1, units = 'sec'))

#--------------------------------------------------------#
#             Sentiment Analysis                         #
#--------------------------------------------------------#
require(qdap) || install.packages("qdap") # ensure java is up to date!
library(qdap)

x1 = x[a0]    # remove empty docs from corpus

t1 = Sys.time()   # set timer

pol = polarity(x1)         # Calculate the polarity from qdap dictionary
wc = pol$all[,2]                  # Word Count in each doc
val = pol$all[,3]                 # average polarity score
p  = pol$all[,4]                  # Positive words info
n  = pol$all[,5]                  # Negative Words info  

Sys.time() - t1  # how much time did the above take?

head(pol$all)
head(pol$group)

positive_words = unique(setdiff(unlist(p),"-"))  # Positive words list
negative_words = unique(setdiff(unlist(n),"-"))  # Negative words list

print(positive_words)       # Print all the positive words found in the corpus
print(negative_words)       # Print all neg words

#--------------------------------------------------------#
#   Create Postive Words wordcloud                       #
#--------------------------------------------------------#

pos.tdm = dtm[,which(colnames(dtm) %in% positive_words)]
m = as.matrix(pos.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows() # opens new image window
wordcloud(names(v), v, scale=c(4,1),1, max.words=100,colors=brewer.pal(8, "Dark2"))
title(sub = "Positive Words - Wordcloud")

# plot barchart for top tokens
test = as.data.frame(v[1:15])
windows() # opens new image window
ggplot(test, aes(x = rownames(test), y = test)) + 
  geom_bar(stat = "identity", fill = "blue", width = 0.3) +
  geom_text(aes(label = test), vjust= -0.20) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1, face = "bold", size = 12)) +
  labs(title = "", x = "", y = "")

#--------------------------------------------------------#
#  Create Negative Words wordcloud                       #
#--------------------------------------------------------#

neg.tdm = dtm[,which(colnames(dtm) %in% negative_words) ]
m = as.matrix(neg.tdm)
v = sort(colSums(m), decreasing = TRUE)
windows()
wordcloud(names(v), v, scale=c(4,1),1, max.words=100,colors=brewer.pal(8, "Dark2"))         
title(sub = "Negative Words - Wordcloud")

# plot barchart for top tokens
test = as.data.frame(v[1:15])
windows() # opens new image window
ggplot(test, aes(x = rownames(test), y = test)) + 
  geom_bar(stat = "identity", fill = "red", width = 0.3) +
  geom_text(aes(label = test), vjust= -0.20) + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1, face = "bold", size = 12)) +
  labs(title = "", x = "", y = "")

#--------------------------------------------------------#
#  Positive words vs Negative Words plot                 #
#--------------------------------------------------------#

len = function(x){
  if ( x == "-" && length(x) == 1)  {return (0)} 
  else {return(length(unlist(x)))}
}

pcount = unlist(lapply(p, len))
ncount = unlist(lapply(n, len))
doc_id = seq(1:length(wc))

windows()
plot(doc_id,pcount,type="l",col="green",xlab = "Document ID", ylab= "Word Count")
lines(doc_id,ncount,type= "l", col="red")
title(main = "Positive words vs Negative Words" )
legend("topright", inset=.05, c("Positive Words","Negative Words"), fill=c("green","red"), horiz=TRUE)


# Documet Sentiment Running plot
windows()
plot(pol$all$polarity, type = "l", ylab = "Polarity Score",xlab = "Document Number")
abline(h=0)
title(main = "Polarity Plot" )