# information science lexicon gatherer
# get the full name of all the original text files for further processing
txtdir<-"E:/2015Ddtrial/text"
# extract keywords from original text files
txtnames<-list.files(txtdir, pattern = ".txt", all.files = FALSE,
                     recursive = TRUE, include.dirs = FALSE, full.names=TRUE)
# prepared for data input
pkw<-vector()
#extract the keywords of the papers in the text files
for(i in 1:length(txtnames)) {
  fc<-textConnection(txtnames[i], open = "r", encoding = "UTF-8")
  #alternative method of reading files:  txtls<-readLines(fc, n=6, encoding = "UTF-8", skipNul = TRUE)
  txtls<-scan(file = fc, what=character(), nmax = 6, sep = "", blank.lines.skip = TRUE, skipNul = TRUE, comment.char = "", encoding = "UTF-8", strip.white = TRUE)
  rawvec<-grep("关键词", txtls, value = TRUE)
  first<-regexpr("关键词", rawvec, ignore.case = FALSE)
  last<-regexpr("分类号", rawvec, ignore.case = FALSE)
  pkw<-append(pkw, substring(rawvec, first+4, last-2))
  close(fc)
}
#clean the keyword vectors
kwdict<-gsub("[[:punct:]]", " ", pkw)
kwdict<-gsub("^[[:blank:]+]|[[:blank:]+]$", "", kwdict)
# delete the space between alphabet recover them to a complete word(does not work correctly)
#kwdict<-gsub("[\\w[:space:]\\w]{1}", "", kwdict)
kwdict<-gsub("[[:space:]]{2,}", " ", kwdict)
#break vectors into single words
ckwdict<-unlist(strsplit(kwdict, "[[:space:]]"))
#Eliminate duplicated words
#ckwdict <- kwdict[!duplicated(kwdict, incomparables = FALSE)]
ckwdict <- unique(ckwdict)
#cat(kwdict, file = "E:/2015Ddtrial/kwdict.txt", sep = ",")

# user dictionary construction
library(cidian)
# SCEL dictionary transform
decode_scel(scel = "E:/lexicon/pinyin_sogou_com/图书数据录入.scel", output = "E:/2015Doctorial/data/dict/catalog.dict", tag = '1')
decode_scel(scel = "E:/lexicon/pinyin_sogou_com/档案专业词库.scel", output = "E:/2015Doctorial/data/dict/archval.dict", tag = '1')
# specialized vocabulary uploading
udict <- load_user_dict(jiebaR::USERPATH)
sgdict <- load_user_dict("E:/2015Doctorial/data/dict/catalog.dict",default_tag = "n")
udict <- add_user_words(udict, sgdict, rep("n", length(sgdict)))
sgdict <- load_user_dict("E:/2015Doctorial/data/dict/archval.dict",default_tag = "n")
udict <- add_user_words(udict, sgdict, rep("n", length(sgdict)))
# custom vocabulary uploading
udict <- add_user_words(udict, ckwdict, rep("n", length(ckwdict)))
write_dict(udict, "E:/2015Ddtrial/comdict.dict", cpp = TRUE, cpp_progress = TRUE)

# set the workspace
source<-"E:/2015Ddtrial/source"
inames<-list.files(source, pattern = ".txt", all.files = FALSE,
                   recursive = TRUE, include.dirs = FALSE, full.names=TRUE)
# whole text word segment
library(jiebaR)
# initialize MixSegment(MP & HMM) engines simultaneously
# return the result by the lines of input files
mixseg <- worker(type = "mix", dict = DICTPATH, hmm = HMMPATH, user = "E:/2015Ddtrial/comdict.dict",
                 write = TRUE, qmax = 20, topn = 5, encoding = "UTF-8", detect = TRUE, symbol = FALSE, lines = 1e+05,
                 output = NULL, bylines = TRUE, user_weight = "max")
# segment each file in current directory
for (i in 1:length(inames)){
  segment(inames[i], mixseg, mod = "mix")
}

#filter stopwords
target<-"E:/2015Ddtrial/seg/294201"
sinames<-list.files(target, pattern = "segment", all.files = FALSE,
                    recursive = TRUE, include.dirs = FALSE, full.names=TRUE)
for (i in 1:length(sinames)){
  # segmentation function in jiebaR limited in vectors, which could only loading by readLines
  #segtmp<-scan(file = sinames[i], nlines = 0, sep = " ", fileEncoding = "UTF-8")
  segtmp<-readLines(sinames[i], n = -1, encoding = "UTF-8")
  segtmp<-gsub("[[:digit:]+]", "", segtmp)
  #enc2utf8(segtmp)
  #write(segtmp, file = gsub("segment", "segcl", sinames[i]))
  #filter_segment(segtmp, filter_words, unit = 50)
  write.csv(segtmp, file = gsub("segment", "segcl", sinames[i]), quote = FALSE, eol = "\n", row.names = FALSE, col.names = FALSE, fileEncoding = "UTF-8")
}