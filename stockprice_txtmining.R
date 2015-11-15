
setwd("/Users/majunting/Desktop/CU_hackson")
#directory=c("/Users/majunting/Desktop/study/ADA/data/movie_2013_reduced/")


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


partial=c("article") #the document contains partial movie

install.packages("ngram")

neg=readLines("negative_words.txt")
pos=readLines("positive_words.txt")
all=c(pos,neg)
document=c("emotional_word")

partialmatrix=function(document,partial,gram_num,directory){
library(tm)
library(SnowballC)
library(NLP)
library("ngram")

preprocess.directory = function(dirname){
  
  # the directory must have all the relevant text files
  ds = DirSource(dirname)
  # Corpus will make a tm document corpus from this directory
  fp = Corpus( ds )
  # inspect to verify
  # inspect(fp[1])
  # another useful command
  # identical(fp[[1]], fp[["Federalist01.txt"]])
  # now let us iterate through and clean this up using tm functionality
  #for (i in 1:length(fp)){
  # make all words lower case
  fp = tm_map( fp , content_transformer(tolower));
  # remove all punctuation
  fp = tm_map( fp , removePunctuation);
  # remove stopwords like the, a, and so on.	
  fp = tm_map( fp, removeWords, stopwords("english"));
  # remove stems like suffixes
  fp = tm_map( fp, stemDocument)
  # remove extra whitespace
  fp = tm_map( fp, stripWhitespace)	
  # }
  # now write the corpus out to the files for our future use.
  # MAKE SURE THE _CLEAN DIRECTORY EXISTS
  writeCorpus( fp , sprintf('%s_clean',dirname) )
}
##########################################
#dir_list = c( 'fp_hamilton_train' , 'fp_hamilton_test' , 'fp_madison_train', 'fp_madison_test')
#ham.train=preprocess.directory("fp_hamilton_train")

title=preprocess.directory(document)
title.part=preprocess.directory(partial)



##########################################
# This code uses tm to preprocess the papers into a format useful for NB

#################
# Problem 1b
#################

##########################################
# To read in data from the directories:
# Partially based on code from C. Shalizi

read.directory <- function(dirname) {
  # Store the infiles in a list
  infiles = list();
  # Get a list of filenames in the directory
  filenames = dir(dirname,full.names=TRUE);
  for (i in 1:length(filenames)){
    infiles[[i]] = scan(filenames[i],what="",quiet=TRUE);
  }
  return(infiles)
}

#hamilton.train=read.directory("fp_hamilton_train_clean")
#hamilton.test=read.directory("fp_hamilton_test_clean")
#madison.train=read.directory("fp_madison_train_clean")
#madison.test=read.directory("fp_madison_test_clean")
clean=paste(document,"_clean",sep="")
partial.clean=paste(partial,"_clean",sep="")
total_title=read.directory(clean)
partial_directory=read.directory(partial.clean)

##########################################

#################
# Problem 1c
#################

##########################################
# Make dictionary sorted by number of times a word appears in corpus 
# (useful for using commonly appearing words as factors)
# NOTE: Use the *entire* corpus: training, testing, spam and ham
make.sorted.dictionary.df <- function(infiles){
    # This returns a dataframe that is sorted by the number of times 
    # a word appears
    
    # List of vectors to one big vetor
    dictionary.full <- unlist(infiles) 
    # Tabulates the full dictionary
    tabulate.dic <- tabulate(factor(dictionary.full)) 
    # Find unique values
    dictionary <- unique(dictionary.full) 
    # Sort them alphabetically
    dictionary <- sort(dictionary)
    dictionary.df <- data.frame(word = dictionary, count = tabulate.dic)
    sort.dictionary.df <- dictionary.df[order(dictionary.df$count,decreasing=TRUE),];
    return(sort.dictionary.df)
}
##########################################
#full=list(hamilton.train,hamilton.test,madison.train,madison.test)



#################
# Problem 1d
#################

##########################################
# Make a document-term matrix, which counts the number of times each 
# dictionary element is used in a document
make.document.term.matrix <- function(infiles,dictionary){
    # This takes the text and dictionary objects from above and outputs a 
    # document term matrix
    num.infiles <- length(infiles);
    num.words <- length(dictionary);
    # Instantiate a matrix where rows are documents and columns are words
    dtm <- mat.or.vec(num.infiles,num.words); # A matrix filled with zeros
    for (i in 1:num.infiles){
        num.words.infile <- length(infiles[[i]]);
        if (length(infiles[[i]])>0){
          infile.temp <- infiles[[i]];
          for (j in 1:num.words.infile){
            if (infile.temp[j] %in% dictionary){
              ind <- which(dictionary == infile.temp[j])[[1]];
              dtm[i,ind] <- dtm[i,ind] + 1;
            }
        }
        
          
            #print(sprintf('%s,%s', i , ind))
            
            
          }
        }        
return(dtm);
}
##########################################




#dtm.hamilton.train=make.document.term.matrix(hamilton.train, full.dictionary)
#dtm.hamilton.test=make.document.term.matrix(hamilton.test, full.dictionary)
#dtm.madison.train=make.document.term.matrix(madison.train, full.dictionary)
#dtm.madison.test=make.document.term.matrix(madison.test, full.dictionary)
dtm.title=make.document.term.matrix(partial_directory,unique(unlist(total_title)))
colnames(dtm.title)=unique(unlist(total_title))
filenames=dir(partial)
rownames(dtm.title)=filenames
dtm_sentiment=dtm.title
return (dtm.title);
}
#filenames=dir(partial)
dtm_sentiment=partialmatrix(document,partial,1,directory)
#rownames(part_matrix)=filenames
infiles=partial_directory
count=0
location=vector()
for (i in 1:length(infiles)){
  if (length(infiles[[i]])==0){
    count=count+1
    location=cbind(location,i)
  }
}

install.packages('wordcloud')
library('wordcloud')
install.packages('biclust')
library('biclust')
install.packages('cluster')
library('cluster')
install.packages('igraph') 
library('igraph')
install.packages('dplyr')
library('dplyr')
install.packages('scales')
library('scales')
#install.packages('SnowballC')
#install.packages('RColorBrewer')
install.packages('ggplot2')
library('ggplot2')
install.packages('tm')
install.packages('Rgraphviz')
library('Rgraphviz')
install.packages('fpc')
library('fpc')
library('tm')
require(topicmodels)

getSources()
getReaders()


data('crude')
docs = crude

toSpace = content_transformer(function(yourdata, target) gsub(target, ' ', yourdata))
docs = tm_map(docs, toSpace, "/")
docs = tm_map(docs, toSpace, "@")
inspect(docs[1])

docs <- tm_map(docs, content_transformer(tolower))

docs = tm_map(docs, removeNumbers)

docs = tm_map(docs, removePunctuation)

length(stopwords('english'))
stopwords('english')
docs = tm_map(docs, removeWords, stopwords("english"))
docs = tm_map(docs, removeWords,c('school','department'))
toString = content_transformer(function(x,from,to) gsub(from,to,x))
docs = tm_map(docs, toString, 'Columbia Statistics Club','CSC')
docs = tm_map(docs, toString,'Earth and Environmental Engineering', 'EAEE')
docs = tm_map(docs,stemDocument)
docs = tm_map(docs,stripWhitespace)
docs = tm_map(docs,PlainTextDocument)

dtm = DocumentTermMatrix(docs)
tdm = TermDocumentMatrix(docs)
tdmc = removeSparseTerms(tdm,sparse=0.6)



dtm2 = as.DocumentTermMatrix(tdmc)
mlda = LDA(dtm2,k=3) #find k topics
mterms = terms(mlda,4) # find the first 4 terms of each topic
mterms
mterms = apply(mterms,MARGIN=2,paste,collapse=', ')
# First topic identified for every document
mtopic = topics(mlda,1)
mtopics = data.frame(doc=1:20,topic1=mtopic)
qplot(doc,..count..,data=mtopics,geom='density',
      fill=mterms[mtopic],position='stack')

mfreq = colSums(as.matrix(dtm_matrix))
wordcloud(colnames(dtm_matrix)[1:700],mfreq[1:700],
          min.freq=5, # plot words apprear 10+ times
          scale=c(4,0.5), # make it bigger with argument "scale"
          colors=brewer.pal(8, "Dark2"), # use color palettes
          random.color=FALSE, 
          # colors chosen randomly or based on the frequency
          random.order=FALSE)
p1 = ggplot(data.frame(word=names(mfreq),freq=mfreq),aes(word,freq))+ geom_bar(stat='identity') + theme(axis.text.x=element_text(angle=45,hjust=0.5))
p1
sort_freq=sort(mfreq,decreasing=TRUE)
p1 = ggplot(data.frame(word=names(sort_freq[1:10]),freq=sort_freq[1:10]),aes(word,freq))+ geom_bar(stat='identity') + theme(axis.text.x=element_text(angle=45,hjust=0.5))
p1
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
plot(dtm_sentiment,terms=findFreqTerms(dtm_sentiment,lowfreq = 20),corThreshold = 0.5)
findFreqTerms(dtm_sentiment, lowfreq = 30, highfreq = 90)
dtm_sentiment[1:2,1:2]

dim(dtm_matrix_time)
length(change_rate_time)
COLSUM=colSums(dtm_matrix_time)
length(which(COLSUM>10))
reduced_matrix=dtm_matrix_time[,which(COLSUM>10)]
dim(reduced_matrix)
reduced_PCA=prcomp(reduced_matrix)
names(summary(reduced_PCA))
par(mar=c(3,5,3,1))
screeplot(reduced_PCA)
plot(reduced_PCA$sdev,type='l')
scale_matrix=scale(reduced_matrix,center = TRUE,scale = FALSE)
score=scale_matrix%*%reduced_PCA$rotation[,1:220]
stock_price=change_rate_time
dataframe=as.data.frame(cbind(score,stock_price))
reg=lm(stock_price~.,data=dataframe)
summary(reg)
scoretop7=scale_matrix%*%reduced_PCA$rotation[,1:7]
dataframetop7=as.data.frame(cbind(scoretop7,stock_price))
regtop7=lm(stock_price~.,data=dataframetop7)
summary(regtop7)

dim_num=seq(2,50,1)
Error=vector()
for(i in dim_num){
  scoretop100=scale_matrix%*%reduced_PCA$rotation[,1:i]
  dataframetop100=as.data.frame(cbind(stock_price,scoretop100))
  #regtop100=lm(stock_price~.,data=dataframetop100)
  #summary(regtop100)
  
  train100=scoretop100[1:250,]
  test100=scoretop100[251:nrow(scoretop100),]
  dataframetop100.train=as.data.frame(cbind(stock_price[1:250],train100))
  regtraintop100=lm(V1~.,data=dataframetop100.train)
  summary(regtraintop100)
  fit=predict(regtraintop100, newdata =  as.data.frame(test100))
  error=0
  test_stock=stock_price[251:nrow(scoretop100)]
  for (j in 1:nrow(test100)){
    square=(fit[j]-test_stock[j])^2
    error=error+square
  }
  Error=cbind(Error,error)
}
plot(dim_num,Error,type="l")
which(Error==min(Error))
i=4
scoretop100=scale_matrix%*%reduced_PCA$rotation[,1:i]
dataframetop100=as.data.frame(cbind(stock_price,scoretop100))
#regtop100=lm(stock_price~.,data=dataframetop100)
#summary(regtop100)

train100=scoretop100[1:250,]
test100=scoretop100[251:nrow(scoretop100),]
dataframetop100.train=as.data.frame(cbind(stock_price[1:250],train100))
regtraintop100=lm(V1~.,data=dataframetop100.train)
summary(regtraintop100)
fit=predict(regtraintop100, newdata =  as.data.frame(test100))
test_stock=stock_price[251:nrow(scoretop100)]
plot(fit,type="l",ylab=c("stock price"),main=c("Predicted vs Real Stock Price using 4 PCs"))
lines(test_stock,type="l",col=c("red"))
legend(x=40, y = -4, c("predicted","real stock price"), col = c("black","red"),lty=c(1,1))   
#name=colnames(dtm_matrix)
plot(regtraintop100)
y_hat=predict(smooth.spline(train100,stock_price[1:250]),test100)$y


colnames(dtm_matrix)[sort(dtm_matrix[8,],decreasing=T)[1:10]]

require('wordcloud')
require('biclust')
require('cluster')
require('igraph') 
require('dplyr')
require('scales')
require('SnowballC')
require('RColorBrewer')
require('ggplot2')
require('tm')
require('Rgraphviz')
require('fpc')

source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

require(topicmodels)

Rawdata_name = names(table(Rawdata$date))
Rawdata$change_rate[which(is.na(Rawdata$change_rate))] = 0

change_rate_time = rep(NA,length(Rawdata_name))
dtm_matrix_time = matrix(length(Rawdata_name)*dim(dtm_matrix_reduction)[2],length(Rawdata_name),dim(dtm_matrix_reduction)[2])



for (i in 1:length(Rawdata_name)){
  if (length(which(Rawdata$date == Rawdata_name[i])) > 1) {
    temp = colSums (dtm_matrix_reduction[which(Rawdata$date == Rawdata_name[i]),])
  }
  else {
    temp = dtm_matrix_reduction[which(Rawdata$date == Rawdata_name[i]),]
  }
  
  dtm_matrix_time[i,] = temp 
} 


for (i in 1:length(Rawdata_name)){
  temp = Rawdata$change_rate[which(Rawdata$date == Rawdata_name[i])][1]
  change_rate_time[i] = temp 
} 




dtm_matrix_sentiment = matrix(length(Rawdata_name)*dim(dtm_sentiment)[2],length(Rawdata_name),dim(dtm_sentiment)[2])

for (i in 1:length(Rawdata_name)){
  if (length(which(Rawdata$date == Rawdata_name[i])) > 1) {
    temp = colSums (dtm_sentiment[which(Rawdata$date == Rawdata_name[i]),])
  }
  else {
    temp = dtm_sentiment[which(Rawdata$date == Rawdata_name[i]),]
  }
  
  dtm_matrix_sentiment[i,] = temp 
} 

p_vector = vector(length = dim(dtm_matrix_time)[2])
for (i in 1:dim(dtm_matrix_time)[2]){
  if (sum(temp_number_vector[i]) >  1 ) {
    temp_test = t.test(change_rate_time[which ( dtm_matrix_time[,i] == 0)], change_rate_time[which ( dtm_matrix_time[,i] != 0)])
    p_vector[i] = temp_test$p.value
  }
}

temp_number_vector = vector(length = 42597)
for (i in 1:length(Rawdata_name)){
  temp_number = length(change_rate_time[which ( dtm_matrix_time[,i] != 0)]) 
  temp_number_vector[i] = temp_number
} 

colnames(dtm_matrix_time) = colnames(dtm_matrix_reduction)
dtm_matrix_final = dtm_matrix_time[, -which(colSums(dtm_matrix_time) == 2)]
dtm_matrix_final = dtm_matrix_time[, -which(colSums(dtm_matrix_time) == 2)]
dtm_matrix_final = dtm_matrix_final[, -which(colSums(dtm_matrix_final) == 3)]
dim(dtm_matrix_final)
dtm_matrix_final = dtm_matrix_final[, -which(colSums(dtm_matrix_final) == 4)]
dtm_matrix_final = dtm_matrix_final[, -which(colSums(dtm_matrix_final) == 5)]
dtm_matrix_final = dtm_matrix_final[, -which(colSums(dtm_matrix_final) == 6)]
dtm_matrix_final = dtm_matrix_final[, -which(colSums(dtm_matrix_final) == 7)]
dtm_matrix_final = dtm_matrix_final[, -which(colSums(dtm_matrix_final) == 8)]
dtm_matrix_final = dtm_matrix_final[, -which(colSums(dtm_matrix_final) == 9)]
dtm_matrix_final = dtm_matrix_final[, -which(colSums(dtm_matrix_final) == 10)]

fit.lasso <- cv.glmnet(dtm_matrix_final,change_rate_time, alpha = 1)
plot(fit.lasso)

coef.opt.lasso <- coef(fit.lasso, s="lambda.min")
coef.opt.lasso[order(abs(coef.opt.lasso[,1]), decreasing=T),][1:10]  	# top 10



colnames(dtm_matrix_sentiment) = colnames(dtm_sentiment)
fit.lasso1 <- cv.glmnet(dtm_matrix_sentiment,change_rate_time, alpha = 1)
plot(fit.lasso1)

coef.opt.lasso1 <- coef(fit.lasso1, s="lambda.min")
coef.opt.lasso1[order(abs(coef.opt.lasso1[,1]), decreasing=T),][1:10]		# top 10

setwd("D:/Columbia-task/wsj_project")

library(NLP)
library(tm)
ds = DirSource("title")
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

#dtm=as.matrix(dtm)

---------------worldcloud------------------------------
  library("RColorBrewer")
library("wordcloud")

mfreq = colSums(as.matrix(dtm))
library("ggplot2")
p1 = ggplot(data.frame(word=names(mfreq),freq=mfreq),aes(word,freq))+ geom_bar(stat='identity') + theme(axis.text.x=element_text(angle=45,hjust=0.5))
p1

p1 = ggplot(subset(data.frame(word=names(mfreq),freq=mfreq),freq>600),aes(word,freq))+ geom_bar(stat='identity') + theme(axis.text.x=element_text(size=12,color='red',fac='bold.italic',angle=45,hjust=0.5))
p1

mord = order(mfreq, decreasing=TRUE) #increasing order as default
mfreq[head(mord,20)]
mfreq[tail(mord,10)]

set.seed(100) 
wordcloud(names(mfreq),mfreq,
          # plot words apprear 10+ times
          scale=c(4,0.5), # make it bigger with argument "scale"
          colors=brewer.pal(8, "Dark2"), # use color palettes
          random.color=FALSE, 
          random.order=FALSE)

head(table(mfreq),200)
#The top number is the frequency with which words appear and the bottom number reflects how many words appear that frequently.
tail(table(mfreq), 200) 

#############WE NEED A LARGE SPARSE####################
dtmc = removeSparseTerms(dtm,sparse=0.99) 

mfreq2 = colSums(as.matrix(dtmc))
set.seed(100)
wordcloud(names(mfreq2),mfreq2,
          min.freq2=500, # plot words apprear 10+ times
          scale=c(4,0.5), # make it bigger with argument "scale"
          colors=brewer.pal(8, "Dark2"), # use color palettes
          random.color=FALSE, 
          # colors chosen randomly or based on the frequency
          random.order=FALSE)
#################################################3

bagwords = data.frame(word=names(mfreq),freq=as.vector(mfreq))
head(bagwords,20)
head(sort(bagwords[,2],decreasing = TRUE),10)

####words correlation graph###########
library("graph")
library("grid")
library("Rgraphviz")
plot(dtm,terms=findFreqTerms(dtm,lowfreq = 800),corThreshold = 0.01)

cor1 = findAssocs(dtm,terms="appl",corlimit=c(0.7,0.8))
cor1

#######clustering
tdm=t(dtm)
tdmc = removeSparseTerms(tdm,sparse=0.999)
mdist = dist(tdmc,method='euclidian')
mfit = hclust(d=mdist,method='ward.D2')
mfit   

plot(mfit,hang=-1)
# draw dendrogram with red borders that identify groups
rect.hclust(mfit,k=5,border='red
            
            grps = cutree(mfit,k=c(3,5,7)) # specify No. of clusters you want
            # compare different groupings
            table(grp2=grps[,'3'],grp4=grps[,'5'])
            
            
            #cluster words into groups to minimize the total sum of the squared distance of every point to its corresponding cluster centroid. Particitions are independent of each other.
            install.packages("cluster")
            library("cluster")
            mkm = kmeans(mdist,2)
            clusplot(as.matrix(mdist),mkm$cluster,color=T,shade=T,labels=2,lines=0)



            setwd("/Users/majunting/Desktop/CU_hackson")
            #directory=c("/Users/majunting/Desktop/study/ADA/data/movie_2013_reduced/")
            gram_num=1
            document=c("whole1") #the document contains all movies
            partial=c("part1") #the document contains partial movie
            
            install.packages("ngram")
            partialmatrix=function(document,partial,gram_num,directory){
            library(tm)
            library(SnowballC)
            library(NLP)
            library("ngram")
            #################
            # This function is used to show the content of each file
            show.content=function(row_number,dirname){
            ds = DirSource(dirname)
            txt=read.table(ds$filelist[row_number],header=F)
            #print(ds$filelist[row_number])
            #print(txt)
            id=ds$filelist[row_number]
            return(list(ID=id,content=txt))
            }
            #################
            
            ##########################################
            # This code uses tm to preprocess the papers into a format useful for NB
            preprocess.directory = function(dirname){
            
            # the directory must have all the relevant text files
            ds = DirSource(dirname)
            # Corpus will make a tm document corpus from this directory
            fp = Corpus( ds )
            # inspect to verify
            # inspect(fp[1])
            # another useful command
            # identical(fp[[1]], fp[["Federalist01.txt"]])
            # now let us iterate through and clean this up using tm functionality
            #for (i in 1:length(fp)){
            # make all words lower case
            fp = tm_map( fp , content_transformer(tolower));
            # remove all punctuation
            fp = tm_map( fp , removePunctuation);
            # remove stopwords like the, a, and so on.	
            fp = tm_map( fp, removeWords, stopwords("english"));
            # remove stems like suffixes
            fp = tm_map( fp, stemDocument)
            # remove extra whitespace
            fp = tm_map( fp, stripWhitespace)	
            # }
            # now write the corpus out to the files for our future use.
            # MAKE SURE THE _CLEAN DIRECTORY EXISTS
            writeCorpus( fp , sprintf('%s_clean',dirname) )
            }
            ##########################################
            #dir_list = c( 'fp_hamilton_train' , 'fp_hamilton_test' , 'fp_madison_train', 'fp_madison_test')
            #ham.train=preprocess.directory("fp_hamilton_train")
            
            title=preprocess.directory(document)
            title.part=preprocess.directory(partial)
            
            #################
            # Problem 1b
            #################
            
            ##########################################
            # To read in data from the directories:
            # Partially based on code from C. Shalizi
            read.directory.ngram <- function(dirname) {
            # Store the infiles in a list
            infiles = list();
            # Get a list of filenames in the directory
            filenames = dir(dirname,full.names=TRUE);
            filenames.true=list();
            for (i in 1:length(filenames)){
            infile = scan(filenames[i],what="",quiet=TRUE);
            if(length(infile)>gram_num){
            original=readChar(filenames[i],file.info(filenames[i])$size)
            ng=ngram(original,n=gram_num)
            infiles[[i]] = get.ngrams(ng)
            filenames.true[[i]]=filenames[[i]]
            }
            
            }
            return(infiles)
            }
            
            read.directory <- function(dirname) {
            # Store the infiles in a list
            infiles = list();
            # Get a list of filenames in the directory
            filenames = dir(dirname,full.names=TRUE);
            for (i in 1:length(filenames)){
            infiles[[i]] = scan(filenames[i],what="",quiet=TRUE);
            }
            return(infiles)
            }
            
            #hamilton.train=read.directory("fp_hamilton_train_clean")
            #hamilton.test=read.directory("fp_hamilton_test_clean")
            #madison.train=read.directory("fp_madison_train_clean")
            #madison.test=read.directory("fp_madison_test_clean")
            clean=paste(document,"_clean",sep="")
            partial.clean=paste(partial,"_clean",sep="")
            total_title=read.directory(clean)
            partial_directory=read.directory(partial.clean)
            
            ##########################################
            
            #################
            # Problem 1c
            #################
            
            ##########################################
            # Make dictionary sorted by number of times a word appears in corpus 
            # (useful for using commonly appearing words as factors)
            # NOTE: Use the *entire* corpus: training, testing, spam and ham
            make.sorted.dictionary.df <- function(infiles){
            # This returns a dataframe that is sorted by the number of times 
            # a word appears
            
            # List of vectors to one big vetor
            dictionary.full <- unlist(infiles) 
            # Tabulates the full dictionary
            tabulate.dic <- tabulate(factor(dictionary.full)) 
            # Find unique values
            dictionary <- unique(dictionary.full) 
            # Sort them alphabetically
            dictionary <- sort(dictionary)
            dictionary.df <- data.frame(word = dictionary, count = tabulate.dic)
            sort.dictionary.df <- dictionary.df[order(dictionary.df$count,decreasing=TRUE),];
            return(sort.dictionary.df)
            }
            ##########################################
            #full=list(hamilton.train,hamilton.test,madison.train,madison.test)
            #full.dictionary=make.sorted.dictionary.df(full)
            full.title=list(total_title)
            title.dictionary=make.sorted.dictionary.df(full.title)
            
            #################
            # Problem 1d
            #################
            
            ##########################################
            # Make a document-term matrix, which counts the number of times each 
            # dictionary element is used in a document
            make.document.term.matrix <- function(infiles,dictionary){
            # This takes the text and dictionary objects from above and outputs a 
            # document term matrix
            num.infiles <- length(infiles);
            num.words <- nrow(dictionary);
            # Instantiate a matrix where rows are documents and columns are words
            dtm <- mat.or.vec(num.infiles,num.words); # A matrix filled with zeros
            for (i in 1:num.infiles){
            num.words.infile <- length(infiles[[i]]);
            infile.temp <- infiles[[i]];
            for (j in 1:num.words.infile){
            ind <- which(dictionary == infile.temp[j])[[1]];
            #print(sprintf('%s,%s', i , ind))
            dtm[i,ind] <- dtm[i,ind] + 1;
            
            }
            }
            return(dtm);
            }
            ##########################################
            
            
            
            
            #dtm.hamilton.train=make.document.term.matrix(hamilton.train, full.dictionary)
            #dtm.hamilton.test=make.document.term.matrix(hamilton.test, full.dictionary)
            #dtm.madison.train=make.document.term.matrix(madison.train, full.dictionary)
            #dtm.madison.test=make.document.term.matrix(madison.test, full.dictionary)
            dtm.title=make.document.term.matrix(partial_directory,title.dictionary)
            colnames(dtm.title)=title.dictionary$word
            filenames=dir(partial)
            rownames(dtm.title)=filenames
            return (dtm.title);
            }
            #filenames=dir(partial)
            part_matrix1=partialmatrix(document,partial,1,directory)
            #rownames(part_matrix)=filenames
            
            
            
            
            
            

