## Co-word Analysis
library(chinese.misc)
# prepare stopwords in Simplified-Chinese
filt<- "E:/201703D/data/stopwords.txt"
stopwords_cn <- make_stoplist(filt, print = TRUE)
# defind cutter
library(jiebaR)
cutter <- jiebaR::worker(type = "mix", dict = DICTPATH, hmm = HMMPATH, user = "E:/201703D/data/custom.dict",  stop_word ="E:/201703D/data/stopwords.txt",
                         write = TRUE, qmax = 20, topn = 5, encoding = "UTF-8", detect = TRUE, symbol = FALSE, lines = 1e+05,
                         output = NULL, bylines = TRUE, user_weight = "max")
# SimpleCorpus construction
iterable<-"E:/201703D/data/origin/"
# TDM
tdm <- corp_or_dtm(iterable, from = "dir", type = "TDM", enc = "auto",
                   mycutter = cutter, stop_word = stopwords_cn, stop_pattern = NULL,
                   control = "auto", myfun1 = function(x) gsub("http[[:graph:]]*","",x), myfun2 = NULL, special = "")
# TTM
ttm <- create_ttm(tdm, type = "TDM", tomatrix = TRUE, checks = TRUE)

library(igraph)
# build graph from TTM
cwg<-graph_from_adjacency_matrix(ttm, weighted = TRUE, mode = "undirected",  add.rownames = TRUE)
# remove loops
cwg<-simplify(cwg, remove.loops = TRUE)

plot(cwg, layout=layout.fruchterman.reingold())
tkplot(cwg, layout=layout.kamada.kawai())