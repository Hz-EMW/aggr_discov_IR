## gathering metadata and full text for each item
## adopts the new website of NSL OpenIR updated in 2017
# based on rvest package
# Step 1: read the title href from itemlist form
metapg<-"http://ir.las.ac.cn/handle/12502/7163?mode=full&submit_simple=Show+full+item+record"

library(rvest)
# detect encoding of html
meta_source <- read_html(metapg)
#detect<-html_nodes(meta_source, xpath ='//table[@class="itemDisplayTable"]')
#guess_encoding(detect)
# gether the context of html by valid encoding
iDT <- html_node(meta_source, xpath ='//table[@class="itemDisplayTable"]')
#repair_encoding(iDT, from = NULL)
# gether labels and values of item metadata in itemDisplayTable
FieldLabel <- html_nodes(iDT, xpath ='.//td[@class="metadataFieldLabel"]')
meta_label <- html_text(FieldLabel, trim = TRUE)
meta_label <- gsub("[[:punct:]]", "", meta_label)
FieldValue <- html_nodes(iDT, xpath ='.//td[@class="metadataFieldValue"]')
meta_value <- html_text(FieldValue, trim = TRUE)
itemb <- html_nodes(FieldValue, xpath ='.//a[@class="itemb"]')
openacc <- html_attr(itemb,  "href", default = NA_character_)
meta_value <- gsub("^[[:punct:]]|[[:punct:]]$", "", meta_value)
imetadata <- cbind.data.frame(meta_label, meta_value)

# combine labels and values into list
#imeta_dc<- as.data.frame(matrix(meta_value, dimnames = list(meta_label, 'itemDisplayTable')))

# mapping from matadata to linked data
ldmd <- data.frame(ldv=c("dcterms:title","dcterms:creator","dcterms:issued","dcterms:description","dcterms:abstract","dcterms:abstract_en","activitystreams:Event","activitystreams:place","dc:language","dcterms:type","dc:identifier","activitystreams:Mention"),
                   mdv=c("Title","Author","Issued Date","Keyword","Abstract","English Abstract","Conference Name","Conference Place","Language","Content Type","URI","Appears in Collections"))
itemld <- merge.data.frame(ldmd, imetadata, by.x = "mdv", by.y = "meta_label")
# form exchange
library(jsonlite)
itemldm <- t(data.frame(itemld[, "meta_value"], row.names = itemld[, "ldv"], check.names = TRUE))
#itemj <- toJSON(itemld[,c("ldv", "meta_value")], dataframe = c("values"), pretty = TRUE)
itemj <- toJSON(itemldm, dataframe = "values", pretty = TRUE)

# profile web page analysis
# gether the URIs of profile page 
aunode <- html_node(meta_source, xpath ='//td[@class="metadataFieldValue"]/sup/a')
aUri <- html_attr(aunode, "href", default = NA_character_)
aUl <- as.data.frame(aUri)

# prepare data frame used for storage
dtcon <- data.frame(attri = c("name", "title", "ORG", "UID", "contact", "topic_interest", "topic"))
dtcon["detail"] <- NA

require(rvest)
library(qdapRegex)
require(tidyr)
for(n in 1:length(aUl$aUri)) {
  bapg <- read_html(as.character(aUl[n,1]))
  # the 1st block profile
  dfnode <- html_node(bapg, xpath='.//div[@class="detailtcon flo"]')
  dn <- html_node(dfnode, xpath='.//h1[@class="detailname"]')
  detailname <- xml_text(dn, trim = TRUE)
  dtcon[1, "detail"] <- detailname
  dedu <- html_node(dfnode, xpath='.//span[@class="detailedu"]')
  detailedu <- xml_text(dedu, trim = TRUE)
  dtcon[2, "detail"] <- detailedu
  hl <- html_node(dfnode, xpath='.//div[@class="header-labels flo"]')
  oghl <- xml_text(hl, trim = TRUE)
  oghl <- rm_repeated_words(oghl)
  dtcon[3, "detail"] <- oghl
  
  detailbid <- html_node(dfnode, xpath='.//div[@class="detailbid"]')
  detailid <- xml_text(detailbid, trim = TRUE)
  detailid <- rm_white(detailid)
  dtcon[4, "detail"] <- detailid
  
  # the 2nd block of profile
  accnode <- html_node(bapg, xpath='.//dl[@class="dl-horizontal dl-left author-contact-card"]')
  dacc <- xml_text(accnode, trim = TRUE)
  dacc <- rm_repeated_words(dacc)
  dtcon[5, "detail"] <- dacc
  
  # interest of study, the 3rd block of profile
  intnode <- html_node(bapg, xpath='.//div[@class="detailauthor"]/table')
  intab <- html_table(intnode, trim = TRUE, fill = TRUE)
  intrs <- data.frame(t(intab[-1,2]))
  itrts <- unite(intrs, "interest", sep = " ", remove = TRUE)
  row.names(itrts) <- "dprofile"
  dtcon[6, "detail"] <- itrts[1,1]
  
  # topic, the 4th block of profile
  topnode <- html_node(bapg, xpath='.//div[@class="detailcon"]')
  stopic <- xml_text(topnode, trim = TRUE)
  stopic <- gsub("\r\n", "", stopic)
  stopic <- gsub("\t", "", stopic)
  stopic <- rm_white(stopic)
  dtcon[7, "detail"]<-stopic
}

#release organization, contact, interests and topic info 
dtcont <- data.frame(t(dtcon))
require(tidyr)
dprofile <- separate(dtcont, "ORG", c("org", "orgunit", "role"), sep = "[[:space:]]+", remove = TRUE,
                     convert = FALSE, extra = "warn", fill = "warn")
dprofile <- separate(dprofile, "contact", c("email", "tel", "postal","adr"), sep = "[[:space:]]+", remove = TRUE,
                     convert = FALSE, extra = "warn", fill = "warn")
# mapping table from matadata to linked data
ldpf <- data.frame(ldv=c("foaf:name","foaf:title","dcterms:issued","foaf:openid","foaf:mbox","foaf:phone","activitystreams:Event","activitystreams:place","dc:language","dcterms:type","dc:identifier","activitystreams:Mention"),
                   pfv=c("Title","Author","Issued Date","Keyword","Abstract","English Abstract","Conference Name","Conference Place","Language","Content Type","URI","Appears in Collections"))

# metadata cleaning
ldmd <- drop_na(ldmd)
dprofile <- drop_na(dprofile)