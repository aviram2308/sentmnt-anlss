
#importing the text mining package tm
library(tm)


url='https://cdn.rawgit.com/aviram2308/sentmnt-anlss/6e63e869/gl%20on%20trn2.txt'

#reading the document 1 using url
d1=readLines(url)


#outputting d1 will give the entire document therefore output only structure and class of d1
#d1
str(d1)
class(d1)


#making it a 1 element vector
d1.2=paste(d1, collapse=" ")



str(d1.2)
class(d1.2)

#removing punctuation using regex \\W
d1.3=gsub(pattern="\\W", replace=" " , d1.2)
#d1.3
str(d1.3)
class(d1.3)


#removing numbers using regex \\d
d1.4=gsub(pattern="\\d", replace=" " , d1.3)
#d1.4
str(d1.4)
class(d1.4)


#making all letters in small case so that we don't have to remove stopwords case wise
d1.5=tolower(d1.4)
#d1.5
str(d1.5)
class(d1.5)


stopwords()

#remove stopwords
d1.6=removeWords(d1.5, stopwords())
#d1.6
str(d1.6)
class(d1.6)


#making a vector of document specific stopwords sw
sw=c('helpful', 'not', 'comment', 'report', 'abuse', 'verified', 'purchase', 'people', 'sort', 'filter', 'review', 'stars', 'formats', 'search', 'keyword', 'top', 'by', 'previous', 'next', 'customer', 'reviews', 'formats', 'reviewer')


d1.7=removeWords(d1.6, sw)
#d1.7
str(d1.7)
class(d1.7)


#removing the residual 1 character words
d1.8=gsub(pattern="\\b[A-z]\\b{1}", replace=" " , d1.7)
#d1.8
str(d1.8)
class(d1.8)


#remove all residual white space
d1.9=stripWhitespace(d1.8)
#d1.9
str(d1.9)
class(d1.9)


#import stringr 
library(stringr)


#split the 1 element vector in list of component words
d1.a=str_split(d1.9, pattern="\\s+")
#d1.a
str(d1.a)
class(d1.a)


#unlist to make it a vector of bag of words
#dbw is document bag of words
dbw=unlist(d1.a)


#dbw
str(dbw)
class(dbw)


#first 50 words of dbw
head(dbw, 50)


#creating a lexicon (a bag of words) of positive words and negative words
posbw=scan('https://cdn.rawgit.com/aviram2308/sentmnt-anlss/f491a815/posd.txt', what='character')
#posbw
head(posbw, 50)
str(posbw)
class(posbw)


negbw=scan('https://cdn.rawgit.com/aviram2308/sentmnt-anlss/f491a815/negd.txt', what='character')
#negbw
head(negbw, 50)
str(negbw)
class(negbw)


#creating a positive matrix pm by matching dbw with posbw
pm=match(dbw, posbw)
#pm
head(pm, 50)


#similarly create a negative matrix nm
nm=match(dbw, negbw)
#nm
head(nm, 50)


# creating a binary matrix 
p=is.na(pm)
#p
head(p, 50)


# making true where there is a match
p=!is.na(pm)
#p
head(p, 50)


n=!is.na(nm)
#n
head(n,50)


#calculating positive score ps and negative score ns
#True=1 and False=0
ps=sum(p)
ps


ns=sum(n)
ns


#calculating sentiment score
sc=ps-ns
sc


#normalised sentiment score
l=length(dbw)
nsc=sc/l
nsc


# creating a corpus of 3 documents using the following urls
url1='https://cdn.rawgit.com/aviram2308/sentmnt-anlss/f6a35562/gone%20gl2.txt'
url2='https://cdn.rawgit.com/aviram2308/sentmnt-anlss/f6a35562/gl%20on%20trn2.txt'
url3='https://cdn.rawgit.com/aviram2308/sentmnt-anlss/f6a35562/into%20d%20wtr2.txt'
urls=c(url1, url2, url3)


#using list apply to apply functions one by one on each element of the list
c1=lapply(urls, FUN=readLines)
str(c1)
class(c1)


c1.2=lapply(c1, FUN= paste, collapse=" ")
str(c1.2)
class(c1.2)


c1.3=gsub(pattern="\\W", replace=" " , c1.2)
str(c1.3)


c1.4=gsub(pattern="\\d", replace=" " , c1.3)
str(c1.4)


c1.5=tolower(c1.4)
str(c1.5)


c1.6=removeWords(c1.5, stopwords())
str(c1.6)


c1.7=removeWords(c1.6, sw)
str(c1.7)


c1.8=gsub(pattern="\\b[A-z]\\b{1}", replace=" " , c1.7)
str(c1.8)


c1.9=stripWhitespace(c1.8)
str(c1.9)


cbw=str_split(c1.9, pattern="\\s+")
str(cbw)


#creating a positive score function and negative score function
psF=function(x){
sum(!is.na(match(x,posbw)))}
nsF=function(x){
sum(!is.na(match(x,negbw)))}


ps=lapply(cbw, psF)
str(ps)
head(ps)


ns=lapply(cbw, nsF)
str(ns)


#creating a score function
scF=function(x){
sum(!is.na(match(x,posbw)))-sum(!is.na(match(x,negbw)))}


sc=lapply(cbw, scF)
sc


nscF=function(x){
ps=sum(!is.na(match(x,posbw)))
ns=sum(!is.na(match(x,negbw)))
l=length(x)
nsc=(ps-ns)/l}


nsc=lapply(cbw, nscF)
nsc

