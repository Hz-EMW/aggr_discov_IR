## Knowledge Map
# based on the project Facet browsing pages of Lanzhou University IR
# Step 1: read the projects title href from itemlist form
results<-"http://ir.lzu.edu.cn/search-filter?field=dc.description.project_filter&gobackurl="

library(RSelenium)
## Start Selenium server in java manually
## C:\windows\system32>java -jar D:\docs_seleniumhq_org\selenium-server-standalone-3.12.0.jar
## Launch a browser(localization)
rD <- remoteDriver(remoteServerAddr = "localhost", browserName="internet explorer")
## send request to server to initialise session
remDr <- rD$open()
# navigate to front page of IR
ir_las <- "http://ir.lzu.edu.cn/"
remDr$navigate(ir_las)
# gether source code of the front page
irlasource <- remDr$getPageSource(header = TRUE, .mapUnicode = FALSE)
# locate element from Facet Display Sector
Facetbrowsing <- remDr$findElement(using = 'xpath', value = "//table[class='miscTable']")
# get the source of retrival result page
rppChanged <- remDr$findElement(using = 'xpath', value = "//option[text() = '100']")
Fb<- remDr$getPageSource(header = TRUE, .mapUnicode = FALSE)
# clean up
rm(remDr)
gc()

# data extract from web testing result
library(rvest)
# detect encoding of html
fDIr <- read_html(Fb[[1]])
# gether the context of html by 3 attribution
Title <- html_nodes(fDIr, xpath=".//tr[@class='itemLine']/td[2]/span[1]/a") %>% 
  html_text( trim = TRUE)

# local web page test
pgdir <- "E:/201803D/data/ir_lzu_edu_cn/project_filter"
#get the full name of all the PDFs in current directory and its sub-directory
pgnames<-list.files(pgdir, pattern = ".htm", all.files = FALSE,
                    recursive = FALSE, include.dirs = FALSE, full.names=FALSE)
inames<-gsub(".htm", "", pgnames)
nspace<-"http://localhost:96/project_filter/"
pfhref<-paste0(nspace, pgnames)
pjtitle<-inames #html_text(ihrefnode)
pjlst<-cbind.data.frame(pfhref, pjtitle)

library(rvest)
# prepare data frame for further processing
pjfb <- data.frame(project = character(0), check.rows = FALSE,
                   check.names = TRUE, fix.empty.names = TRUE,
                   stringsAsFactors = default.stringsAsFactors())
# read name of project into data frame
for(n in 1:length(pjlst$pfhref)) {
  pjpg <- read_html(as.character(pjlst[n,1]))
  # the 1st block profile
  Facetbrowsing <- html_nodes(pjpg, xpath=".//table[@class='miscTable']")
  pjt <- html_table(Facetbrowsing, trim = TRUE)
  pjfb <- rbind(pjfb, pjt[[1]])
}
colnames(pjfb) <- "project"
# tidy data for storage
pjfb <- unique(pjfb)
pjfb_o <- pjfb[order(pjfb$project),]
library(qdapRegex)
pjfb_c <- rm_nchar_words(pjfb_o, 1, clean = TRUE, pattern = "\r\n", replacement = "")
pjfb_c <- rm_white(pjfb_c)
pj_fb <- as.data.frame(pjfb_c)
colnames(pj_fb) <- "project"

library(tidyr)
pjd_fb <- separate(pj_fb, c("project"), c("projname", "num_pro"), sep = "[['('|')']$]{1}", remove = TRUE, 
                   convert = FALSE, extra = "warn", fill = "warn")

pjc <- subset(pjd_fb, grepl("^[美|英|法|德|加|澳|韩|日|台|新]", projname))
pjc <- pjc[!grepl("新世纪", pjc$projname),]
# read the location(cities and countries) of each institute
locate <- read.csv("E:/201803D/dataprocess/location.csv", header = FALSE)
pjl <- cbind(pjc, locate[,4:6])
colnames(pjl) <- c("projname", "num_pro", "city", "country", "continent")

# read the data of airports into dataframe
airports <- read.table("E:/201803D/data/openflights_org/airports-extended.dat", sep = ",", header = FALSE)
worldport <- airports[airports$V5 != "", c("V3", "V4", "V5", 
                                           "V7", "V8", "V9")]
names(worldport) <- c("City", "Country", "Code", "Lat", "Lon", "att")
worldport$Lat <- as.numeric(as.character(worldport$Lat))
worldport$Lon <- as.numeric(as.character(worldport$Lon))
# extract all the related location of projects
region <- c("United States", "United Kingdom", "France", "Germany", "Canada", "Australia", "South Korea", "Japan", "Taiwan", "Singapore")
airport_pr <- worldport[worldport$Country %in% region,]

# extract the location of IR
port_c <- subset(worldport, grepl("Lanzhou", City))
pjal <- read.csv("E:/201803D/dataprocess/location2.csv", header = FALSE)
colnames(pjal) <- c("deforder", "projname", "num_pro", "airport", "location", "Country", "continent")
library(dplyr)
pj_fc <- data.frame()
for(i in 1:nrow(pjal)){
  pj_tmp <- filter(airport_pr, City == as.character(pjal[i,4]) & Country == as.character(pjal[i,6]))
  pj_tmp <- pj_tmp[which.max(pj_tmp$att),]
  pj_fc <- rbind(pj_fc, pj_tmp, deparse.level = 1)
}
pj_fc <- unique(pj_fc)
pjcon <- 

library(maps)
library(mapproj)
map(database = "world", regions = ".", exact = FALSE, boundary = TRUE,
                 interior = TRUE, projection = "rectangular", parameters = 0, orientation = NULL,
                 fill = TRUE, col = "white", plot = TRUE, add = FALSE, namesonly = FALSE,
                 xlim = NULL, ylim = NULL, wrap = TRUE, resolution = 0,
                 type = "s", bg = "lightblue", mar=c(0,0,0,0),
                 myborder = 0.01, namefield="name", lforce="s")
map("world", fill=TRUE, col="white", bg="lightblue", mar=c(0,0,0,0)) 

#c(90,-18,180)
data(world.cities)
cn.cities <- world.cities[world.cities$name == "Lanzhou",]
# extract the location of IR
pjal <- read.csv("E:/201803D/dataprocess/location.csv", header = FALSE)
colnames(pjal) <- c("deforder", "projname", "num_pro", "city", "Country", "continent")
foreign.cities <- data.frame()
library(dplyr)
for(i in 1:nrow(pjal)){
  fc_tmp <- filter(world.cities, name == as.character(pjal[i,4]) & country.etc == as.character(pjal[i,5]))
  foreign.cities <- rbind(foreign.cities, fc_tmp, deparse.level = 1)
}
# small cities coordinates
gmaps<-"https://www.google.com/maps"
require(RSelenium)
rD <- rsDriver(browser = "phantomjs")
remDr <- rD$client
remDr$navigate(gmaps)
psource <- remDr$getPageSource(header = TRUE, .mapUnicode = FALSE)
q <- remDr$findElement("id", "searchboxinput")
q$sendKeysToElement(list('Bethesda'))
button <- remDr$findElement("id", "searchbox-searchbutton")
button$clickElement()
npsource <- remDr$getPageSource(header = TRUE, .mapUnicode = FALSE)
rm(rD)
gc()
# marks of cities
points(cn.cities$lon, cn.cities$lat, pch=10, cex=1.2, col="red")
points(foreign.cities$lon, foreign.cities$lat, pch=10, cex=1.2, col="orange")

# simple connection
library(geosphere)
for (j in 1:nrow(foreign.cities)) { 
  inter <- gcIntermediate(c(cn.cities[1,"long"], cn.cities[1,"lat"]), c(foreign.cities[j,"long"], foreign.cities[j,"lat"]), n=100, addStartEnd=TRUE, breakAtDateLine=FALSE)
  
  lines(inter, col="green", lwd=0.8) 
} 
plot(lines)

# Treemap
ipvol <- pjal[,c("Country","num_pro")]
pvol <- aggregate(. ~ Country, data=ipvol, FUN=sum)
contin <- unique(pjal[,c("Country","continent")])
tp <- merge(pvol, contin, by.x = "Country", by.y = "Country")
library(treemap)
treemap(tp,
        index=c("continent", "Country"),
        vSize="num_pro", vColor= NULL,
        type="index", title = "合作项目总量")

# Scatter Pie plot
library(scatterpie)
world <- map_data('world')
p <- ggplot(world, aes(long, lat)) +
  geom_map(map=world, aes(map_id=region), fill=NA, color="black") +
  coord_quickmap()
p + geom_scatterpie(aes(x=long, y=lat, group=region, r=radius),
                    data=d, cols=LETTERS[1:4], color=NA, alpha=.8) +
  geom_scatterpie_legend(d$radius, x=-160, y=-55)