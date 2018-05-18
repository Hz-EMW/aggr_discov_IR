## full text classification
# index csv file
# locate text files transformed from PDF
segtext<- "E:/201803D/data/ir_las_ac_cn/12502_2017st"
itexts<-list.files(segtext, pattern = ".txt", all.files = FALSE,
                   recursive = TRUE, include.dirs = FALSE, full.names=FALSE)

library(RTextTools)
# Read Plain Texts
itemst <- read_data(segtext, type="folder", index=itexts)
matrix <- create_matrix(cbind(data["Title"],data["Subject"]), language="english",
                        removeNumbers=TRUE, stemWords=FALSE, weighting=tm::weightTfIdf)
# stopword list
stwcn <- readLines("E:/201803D/data/stopwordscn.txt", encoding = "UTF-8", ok = TRUE)
# transfer Plain Text into Corpus
itmcop <- VCorpus(itemst, readerControl = list(reader = readPlain, stopwords = stwcn, language = "zh-CN"))
#itemcp<- PCorpus(itemst,
#                 readerControl = list(reader = readPlain, language = "zh-CN"),
#                 dbControl = list(dbName = "ftjc.db", dbType = "RDS"))