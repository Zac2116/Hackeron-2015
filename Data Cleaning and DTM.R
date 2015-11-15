
library(tm)
library(SnowballC)
library(NLP)
library("ngram")

ds = DirSource("article")
fp = Corpus(ds)

#clean
fp = tm_map( fp , content_transformer(tolower));

fp = tm_map(fp, removeNumbers)

fp = tm_map( fp , removePunctuation);
# remove stopwords like the, a, and so on.	
fp = tm_map( fp, removeWords, stopwords("english"));
# remove stems like suffixes
fp = tm_map( fp, stemDocument)
# remove extra whitespace
fp = tm_map( fp, stripWhitespace)

fp = tm_map(fp,PlainTextDocument)

fpCopy <- fp
#stem words ????
#fp <- tm_map(fpCopy,stemDocument)
##**#
#for (i in 1:5){
  #      cat(paste("[[",i,"]]", sep = ""))
 #       writeLines(fp[[i]])
#}
#
#??
#fp <- tm_map(fp,stemCompletion, dictionary = fpCopy)

dtm = DocumentTermMatrix(fp)
#dimnames(dtm)[1]$Docs = paste('fp',1:20,sep ='')
#inspect(dtm[1:5,1:20]) 

dtm_matrix = as.matrix(dtm)
#View(dtm_matrix[1:10,1:10])

term.freq<-rowSums(dtm_matrix)
#View(term.freq[1:10])

dtm_matrix <- term.freq<-rowSums(dtm_matrix)
#term.freq_largerthan15 <- subset(term.freq,term.freq>=15)


#dtm
#inspect(removeSparseTerms(dtm, 0.6)) #better inspect first

# Normally this reduces the matrix dramatically without losing significant relations inherent to the matrix:
#dtmc = removeSparseTerms(dtm,sparse=0.9) 
#tdmc = removeSparseTerms(tdm,sparse=0.6)


#
#tdm <- TermDocumentMatrix(fp,control = list(wordLengths = c(1,Inf)))
#tdm_matrix <- as.matrix(tdm)

#term.freq<-colSums(dtm_matrix)
#term.freq_largerthan15 <- subset(term.freq,term.freq>=15)

#dataSet <- data.frame(term = names(term.freq_largerthan15),freq = term.freq_largerthan15)

#library(ggp)