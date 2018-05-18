## Two-Mode Network
## academic writing of one author retrival
#Step1: Items Retrival
library(RSelenium)
## Start Selenium server in java manually
## C:\windows\system32>java -jar D:\docs_seleniumhq_org\selenium-server-standalone-3.9.1.jar
##
## Start a browser
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browserName="internet explorer")
## send request to server to initialise session
remDr$open()
# navigate to front page of IR
ir_las <- "http://localhost:90/ir.las.ac.cn.htm"
ir_las <- "http://ir.las.ac.cn/"
remDr$navigate(ir_las)
# gether source code of the front page
irlasource <- remDr$getPageSource(header = TRUE, .mapUnicode = FALSE)
# locate community & collection section
margin <- remDr$findElement(using = 'partial link text', value = "href='/browse-author'")
# click 
margin$clickElement()
# gether source code of the edit-communities page
browse-author <- remDr$getPageSource(header = TRUE, .mapUnicode = FALSE)
# locate the link of "next page"
auth <- remDr$findElement(using = 'link text', value = "Next page")
# click the link
middle_arrow$clickElement()

#gain personal home page
personalhp <- remDr$getPageSource(header = TRUE, .mapUnicode = FALSE)
tableSource <- xml2::xml_find_all(xml2::read_html(personalhp[[1]]),
                                  "//body/div")
rvest::html_table(tableSource)
# locate Facet Brawsing section
facetDisplayItem <- remDr$findElement(using = 'css selector', value = "[title='2017']")
middle_arrow <- remDr$findElement(using = 'link text', value = "Next page")
# click 
middle_arrow$clickElement()
# gether source code of community list page
commusource <- remDr$getPageSource(header = TRUE, .mapUnicode = FALSE)
# click 
facetDisplay$clickElement()
# read source code of search result page
itemlasource <- remDr$getPageSource(header = TRUE, .mapUnicode = FALSE)
# collect all the URLs of items in the search result page
irfs <- "http://localhost:90/zzq-000710.htm"
remDr$navigate(irfs)
irfss <- remDr$getPageSource(header = TRUE, .mapUnicode = FALSE)
itemlasource <- remDr$findElement(using = 'link text', value = "href")

# local web page test
pgdir <- "E:/201703D/data/mdweb/browse-author"
#get the full name of all the PDFs in current directory and its sub-directory
pgnames<-list.files(pgdir, pattern = ".htm", all.files = FALSE,
                    recursive = FALSE, include.dirs = FALSE, full.names=FALSE)
inames<-gsub(".htm", "", pgnames)
nspace<-"http://localhost:95/browse-author/"
bahref<-paste0(nspace, pgnames)
batitle<-inames #html_text(ihrefnode)
balst<-cbind.data.frame(bahref, batitle)

# prepare data frame used for storage
dtcon <- data.frame(row.names = c("name", "title", "ORG", "UID", "contact", "topic_interest", "topic"), 
                     check.rows = FALSE, check.names = TRUE, fix.empty.names = TRUE,
                     stringsAsFactors = default.stringsAsFactors())

library(rvest)
library(qdapRegex)
library(tidyr)
for(n in 1:length(balst$bahref)) {
  bapg <- read_html(as.character(balst[n,1]))
  # the 1st block profile
  dfnode <- html_node(bapg, xpath='.//div[@class="detailtcon flo"]')
  dn <- html_node(dfnode, xpath='.//h1[@class="detailname"]')
  detailname <- xml_text(dn, trim = TRUE)
  dtcon["name", as.character(balst[n,2])] <- detailname
  dedu <- html_node(dfnode, xpath='.//span[@class="detailedu"]')
  detailedu <- xml_text(dedu, trim = TRUE)
  dtcon["title", as.character(balst[n,2])] <- detailedu
  hl <- html_node(dfnode, xpath='.//div[@class="header-labels flo"]')
  oghl <- xml_text(hl, trim = TRUE)
  oghl <- rm_repeated_words(oghl)
  dtcon["ORG", as.character(balst[n,2])] <- oghl
  
  detailbid <- html_node(dfnode, xpath='.//div[@class="detailbid"]')
  detailid <- xml_text(detailbid, trim = TRUE)
  detailid <- rm_white(detailid)
  dtcon["UID", as.character(balst[n,2])] <- detailid
  
  # the 2nd block of profile
  accnode <- html_node(bapg, xpath='.//dl[@class="dl-horizontal dl-left author-contact-card"]')
  dacc <- xml_text(accnode, trim = TRUE)
  dacc <- rm_repeated_words(dacc)
  dtcon["contact", as.character(balst[n,2])] <- dacc
  
  # interest of study, the 3rd block of profile
  intnode <- html_node(bapg, xpath='.//div[@class="detailauthor"]/table')
  intab <- html_table(intnode, trim = TRUE, fill = TRUE)
  intrs <- data.frame(t(intab[-1,2]))
  itrts <- unite(intrs, "interest", sep = " ", remove = TRUE)
  row.names(itrts) <- as.character(balst[n,2])
  dtcon["topic_interest", as.character(balst[n,2])] <- itrts[1,1]
  
  # topic, the 4th block of profile
  topnode <- html_node(bapg, xpath='.//div[@class="detailcon"]')
  stopic <- xml_text(topnode, trim = TRUE)
  stopic <- gsub("\r\n", "", stopic)
  stopic <- gsub("\t", "", stopic)
  stopic <- rm_white(stopic)
  dtcon["topic", as.character(balst[n,2])]<-stopic

}

#release organization, contact, interests and topic info 
dtcont <- data.frame(t(dtcon))
require(tidyr)
dprofile <- separate(dtcont, "ORG", c("org", "orgunit", "role"), sep = "[[:space:]]+", remove = TRUE,
                  convert = FALSE, extra = "warn", fill = "warn")
dprofile <- separate(dprofile, "contact", c("email", "tel", "postal","adr"), sep = "[[:space:]]+", remove = TRUE,
                  convert = FALSE, extra = "warn", fill = "warn")
dprofile <- separate(dprofile, "topic_interest", c("int1", "int2", "int3"), sep = "[[:space:]]+", remove = TRUE,
                  convert = FALSE, extra = "warn", fill = "warn")

pnt<-dprofile[,c("name", "topic")]

# simple clustering
library(jiebaR)
engine <- worker(type = "mix", dict = DICTPATH, hmm = HMMPATH, user = USERPATH,
                 idf = IDFPATH, stop_word = STOPPATH, write = T, qmax = 20, topn = 5,
                 encoding = "UTF-8", detect = T, symbol = F, lines = 1e+05,
                 output = NULL, bylines = F, user_weight = "max")
for(n in 1:length(pnt$"topic")) {
  segment(as.vector(pnt[1,"topic"]), engine)
  
}
library(LSAfun)  


#strach out the topic-author matrix, build DTM
library(tm)
#prepare corpora
coscate <- VCorpus(tat, readerControl = list(reader = readDataframe, language = "zh-cn"))
#writeCorpus(coscate, path = ".", filenames = NULL)
atm <- DocumentTermMatrix(kwsc,
                          control = list(removePunctuation = FALSE,
                                         stopwords = TRUE))
#clustering
require(proxy)
dtm_df <- as.data.frame(inspect(dtm_removed))
dist_dtm <- dist(dtm_df, method = 'Euclidean')
hc <- hclust(dist_dtm, method = 'ward.D2')
png(filename = "ddtm_hc.png", width=2000, height=1500)
plot(hc, cex=1.5,cex.axis=1.5,cex.lab=1.5,cex.main=2.0,cex.sub=1.5,lwd=1.2,
     main = "文档向量层次聚类", xlab="离差平方和法", sub="（基于欧几里得距离）", ylab = "高度")
member<-cutree(hc, k = 3, h=4)
write.table(member, file = "E:/exp/member.txt", row.names = TRUE, col.names = TRUE)
dev.off()

library(igraph)
mmg <- graph_from_incidence_matrix(dtm, mode = "all")
# build index for term vertices and item vertices
V(mmg)$color <- c("steel blue", "orange")[V(mmg)$type+1]
V(mmg)$shape <- c("square", "circle")[V(mmg)$type+1]
# labels and sizes
V(mmg)$label <- V(mmg)$name
V(mmg)$label[V(mmg)$type==F] <- inode$V1[V(mmg)$type==F]
V(mmg)$label.cex=.6
V(mmg)$label.font=4
plot(mmg, vertex.label.color="white", vertex.size=10, layout=layout.fruchterman.reingold)

