# download Xpdf(the latest version is 3.04) and language packages from 
# ftp://ftp.foolabs.com/pub/xpdf/
# follow the INSTALL file in the archive to setup Xpdf

# The list of pdf docs for format changing
pdfdir <- "E:/2015Doctoral/data"
#get the full name of all the files in current directory and its sub-directory
pdfnames<-list.files(pdfdir, pattern = ".pdf", all.files = FALSE,
                     recursive = TRUE, include.dirs = FALSE, full.names=TRUE)
#generate the text file name
txtnames<-sub(".pdf", ".txt", pdfnames)

# call pdftotxt.exe and set parameters for convert pdf to text
p2t <- "D:/Program Files/Xpdf/bin64/pdftotext.exe"
xpdfpara <- "-f 1"
#xpdflog <- "E:/2015Doct/data/log.txt"
for(i in 1:length(pdfnames)) {
  if(grepl(" ", pdfnames[i])==TRUE) {
    # quote the file names including space character
    system(paste(shQuote(p2t), xpdfpara, shQuote(pdfnames[i]), shQuote(txtnames[i]), sep = " "), wait = FALSE)
  } else {
    system(paste(shQuote(p2t), xpdfpara, pdfnames[i], txtnames[i], sep = " "), wait = FALSE)
  }
}
# get log-file name and open it  
#shell.exec("E:/2015Doct/data/log.txt")
