## Tree of Metadata
# gathering metadata and full text for each item
# adopts the new website of NSL OpenIR updated in 2017
# Step 1: read different type of metadata from full item record
jounalmd<-"http://ir.las.ac.cn/handle/12502/7163?mode=full&submit_simple=Show+full+item+record"
confmd<-"http://www.irgrid.ac.cn/handle/1471x/1465085?mode=full&submit_simple=Show+full+item+record"
dissertmd<-"http://www.irgrid.ac.cn/handle/1471x/262421?mode=full&submit_simple=Show+full+item+record"
patentmd<-"http://www.irgrid.ac.cn/handle/1471x/262421?mode=full&submit_simple=Show+full+item+record"
presentmd<-"http://ir.las.ac.cn/handle/12502/9472?mode=full&submit_simple=Show+full+item+record"
bookmd<-"http://www.irgrid.ac.cn/handle/1471x/1585314?mode=full&submit_simple=Show+full+item+record"

imd<-function(mdp) {
require(rvest)
# detect encoding of html
meta_source <- read_html(mdp)
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
return(imetadata)
}

# generate all matadata
jmd<-imd(jounalmd)
cmd<-imd(confmd)
dmd<-imd(dissertmd)
ptmd<-imd(patentmd)
psmd<-imd(patentmd)
bmd<-imd(bookmd)

# generate tree of metadata
library(data.tree)
imetadata$pathString <- paste("journal", imetadata$meta_label, sep = "/")
tree <- as.Node(imetadata[,])
par(mfrow=c(2,3))
print(tree, pruneMethod = "dist", limit = 20)
plot()