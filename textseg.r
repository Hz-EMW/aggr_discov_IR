# Dictionary building and Word Segment
# Keywords list gatherer

# custom vocabulary preparation
library(cidian)
udict <- load_user_dict(jiebaR::USERPATH)
udict <- add_user_words(udict, ckwdict, rep("n", length(ckwdict)))
write_dict(udict, "E:/201803D/data/comdict.dict", cpp = TRUE, cpp_progress = TRUE)

# integrating custom dictionary and SCEL dictionary
library(cidian)
# SCEL dictionary(from Sogou Pinyin) transform
decode_scel(scel = "E:/lexicon/pinyin_sogou_com/数学词汇大全.scel", output = "E:/2015Doctorial/data/dict/bigmath.dict", tag = '1')
decode_scel(scel = "E:/lexicon/pinyin_sogou_com/计算机词汇大全.scel", output = "E:/2015Doctorial/data/dict/bigcomp.dict", tag = '1')
# specialized vocabulary uploading
udict <- load_user_dict("E:/201803D/data/custom.dict")
sgdict <- load_user_dict("E:/201803D/data/dict/catalog.dict",default_tag = "n")
udict <- add_user_words(udict, sgdict, rep("n", length(sgdict)))
sgdict <- load_user_dict("E:/201803D/data/dict/mixnoun.dict",default_tag = "n")
udict <- add_user_words(udict, sgdict, rep("n", length(sgdict)))

# word segment for text files
# set the workspace
itemtxt<-"E:/201803D/data/ir_las_ac_cn/12502_2017t"
inames<-list.files(itemtxt, pattern = ".txt", all.files = FALSE,
                   recursive = TRUE, include.dirs = FALSE, full.names=TRUE)
# whole text word segment
library(jiebaR)
# initialize MixSegment(MP & HMM) engines simultaneously
# return the result by the lines of input files
mixseg <- worker(type = "mix", dict = DICTPATH, hmm = HMMPATH, user = "E:/201703D/data/custom.dict",  stop_word ="E:/201703D/data/stopwords.txt",
                 write = TRUE, qmax = 20, topn = 5, encoding = "UTF-8", detect = TRUE, symbol = FALSE, lines = 1e+05,
                 output = NULL, bylines = TRUE, user_weight = "max")
# segment each file in current directory
for (i in 1:length(inames)){
  segment(inames[i], mixseg, mod = "mix")
}

#filter stopwords
target<-"E:/201803D/data/ir_las_ac_cn/12502_2017t"
sinames<-list.files(target, pattern = "segment", all.files = FALSE,
                    recursive = TRUE, include.dirs = FALSE, full.names=TRUE)
for (i in 1:length(sinames)){
  # if scan function does not work, ue readLines instead
  segfile<-scan(file = sinames[i], nlines = 0, sep = "[[:space:]+]", fileEncoding = "UTF-8")
  #segtmp<-readLines(sinames[i], n = -1, encoding = "UTF-8")
  segfile<-gsub("[[:digit:]+]", "", segfile)
  # if write failed, use write.csv instead
  write(segfile, file = gsub("segment", "segcl", sinames[i]))
  #filter_segment(segfile, c("的","了","和"), unit = 50)
  #write.csv(segtmp, file = gsub("segment", "segcl", sinames[i]), quote = FALSE, eol = "\n", row.names = FALSE, col.names = FALSE, fileEncoding = "UTF-8")
}