## Word Clouds
# Term-Frequency Matrix
# based on TDM built from tm, chinese.misc or lsa
# original documents, transformed text files, and segment text files separatly stored in different folders.
# read text files transformed from PDF
segtext<- "E:/201803D/data/ir_las_ac_cn/12502_2017st"
library(tm)
# Read Plain Texts
itemst<- DirSource(directory = segtext,
                encoding = "UTF-8",
                pattern = ".segment",
                recursive = TRUE,
                ignore.case = FALSE,
                mode = "text")
# stopword list
stwcn <- readLines("E:/201803D/data/stopwordscn.txt", encoding = "UTF-8", ok = TRUE)
# transfer Plain Text into Corpus
itmcop <- VCorpus(itemst, readerControl = list(reader = readPlain, stopwords = stwcn, language = "zh-CN"))
#itemcp<- PCorpus(itemst,
#                 readerControl = list(reader = readPlain, language = "zh-CN"),
#                 dbControl = list(dbName = "ftjc.db", dbType = "RDS"))
ircate<- tm_map(itmcop, removeWords, stwcn)
# build TDM from corpus
iri_tdm <- TermDocumentMatrix(itmcop,
                              control = list(removePunctuation = TRUE, removeNumbers = TRUE,
                                             stopwords = stopwords("en"),
                                             weighting = function(m) weightTf(m)))
# import TDM
freq<-rowSums(as.matrix(iri_tdm))
tfm<-as.data.frame(cbind(word=row.names(iri_tdm), freq))
tfm[,"word"] <- as.character(tfm[,"word"])
tfm[,"freq"] <- as.numeric(tfm[,"freq"])
#tfm<-cbind(as.matrix(iri_tdm), freq)
#itfm<-as.matrix(tfm[,"freq"])

## prepare terms for wordcloud
itext<- "E:/201803D/data/ir_las_ac_cn/12502_2017t"
# extract keywords from original text files
txtnames<-list.files(itext, pattern = ".txt", all.files = FALSE,
                     recursive = TRUE, include.dirs = FALSE, full.names=TRUE)
# create storage of terms
pkw<-vector()
#extract the keywords of the papers in the text files
for(i in 1:length(txtnames)) {
  txtls<-readLines(txtnames[i], n=8, encoding = "UTF-8", skipNul = TRUE)
  rawvec<-grep("关键词", txtls, value = TRUE)
  first<-regexpr("关键词", rawvec, ignore.case = FALSE)
  last<-regexpr("分类号", rawvec, ignore.case = FALSE)
  pkw<-append(pkw, substring(rawvec, first+4, last-2))
}
#clean the keyword vectors
kwdict<-gsub("[[:punct:]]", " ", pkw)
kwdict<-gsub("^[[:blank:]+]|[[:blank:]+]$", "", kwdict)
# delete the space between alphabet recover them to a complete word(does not work correctly)
#kwdict<-gsub("[\\w[:space:]\\w]{1}", "", kwdict)
kwdict<-gsub("[[:space:]]{2,}", " ", kwdict)
kwdict<-gsub("中", "", kwdict)
#break vectors into single words
ckwdict<-unlist(strsplit(kwdict, "[[:space:]]"))
#Eliminate duplicated words
#ckwdict <- kwdict[!duplicated(kwdict, incomparables = FALSE)]
ckwdict <- unique(ckwdict)
#cat(kwdict, file = "E:/2015Ddtrial/kwdict.txt", sep = ",")

require(jiebaR)
# return the result by the lines of input files
mixseg <- worker(type = "mix", dict = DICTPATH, hmm = HMMPATH, user = "E:/201703D/data/custom.dict",  stop_word ="E:/201703D/data/stopwords.txt",
                 write = TRUE, qmax = 20, topn = 5, encoding = "UTF-8", detect = TRUE, symbol = FALSE, lines = 1e+05,
                 output = NULL, bylines = TRUE, user_weight = "max")
# segment each file in current directory
kwseg <- segment(ckwdict, mixseg, mod = "mix")
kwseg <- unique(unlist(kwseg))
## extract frequencies of terms from vector
stfm <- tfm[row.names(tfm) %in% kwseg, ]


# wordcloud 1
library(wordcloud)
##### with colors #####
if(require(RColorBrewer)){
  pal <- brewer.pal(9,"Dark2")
  pal <- pal[-(1:4)]
  wordcloud(words,freq,scale=c(4,.5),min.freq=3,max.words=Inf,
          random.order=TRUE, random.color=TRUE, rot.per=.1,
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE, pal)
}

# wordcloud 2
library(wordcloud2)
wordcloud2(stfm, size = 1, minSize = 0, gridSize = 0,
           fontFamily = 'Segoe UI', fontWeight = 'bold',
           color = 'random-dark', backgroundColor = "white",
           minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
           rotateRatio = 0.4, shape = 'diamond', ellipticity = 0.65,
           widgetsize = NULL, figPath = NULL, hoverFunction = NULL)