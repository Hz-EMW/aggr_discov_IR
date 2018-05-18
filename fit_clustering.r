## text clustering
#
# locate text files transformed from PDF
segtext<- "E:/201803D/data/ir_las_ac_cn/12502_2017st"
# DTM
require(tm)
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
#record metadata for the corpus
#reuters <- tm_map(itmcop, removeWords, stopwords_cn)
dtm <- DocumentTermMatrix(itmcop, control = list(removePunctuation = TRUE, removeNumbers = TRUE,
                                                 stopwords = stopwords("en"),
                                                 weighting = function(m) weightTf(m)))
dtm_rst <- removeSparseTerms(dtm, 0.4)

#clustering with dendrogram visualizations
library(dendextend)
dtm_df <- as.data.frame(inspect(dtm_rst))
dist_dtm <- dist(dtm_df, method = 'euclidean')
ihc <- hclust(dist_dtm, method = 'ward.D2')
dend0 <- as.dendrogram(ihc, labels = "")
labels_colors(dend) <- df.col[order.dendrogram(dend)]
dend.colorBranch <- color_branches(dend, k = length(df.names), col = df.col[order.dendrogram(dend)])
dend.colorBranch %>% set("branches_lwd", 3) %>% plot(horiz = TRUE)

# Fan
plot(as.phylo(ihc), type = "fan", cex = 0.6,
     edge.color = "steelblue", edge.width = 2, edge.lty = 2,
     tip.color = "blue")
d3dendrogram(dend0, height = 500, width = 700, rightmargin = 200,
             open = TRUE)
member<-cutree(hc, k = 3, h=4)
write.table(member, file = "E:/exp/member.txt", row.names = TRUE, col.names = TRUE)
dev.off()