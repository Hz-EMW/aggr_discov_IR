# download Xpdf tools(the latest version is 4.00) and language packages from 
# http://www.xpdfreader.com/
# follow the INSTALL file in the archive to setup Xpdf

# The list of PDF docs for format changing
pdfdir <- "E:/201803D/data/ir_las_ac_cn/12502_2017"
#get the full name of all the PDFs in current directory and its sub-directory
pdfnames<-list.files(pdfdir, pattern = ".pdf", all.files = FALSE,
                     recursive = TRUE, include.dirs = FALSE, full.names=TRUE)
#generate the text file name
txtnames<-sub(".pdf", ".txt", pdfnames)

# predefine home directory of pdftotxt.exe and its parameters
p2t <- "D:/Program Files/xpdf-tools-win-4.00/bin64/pdftotext.exe"
xpdfpara <- "-f 1"
#xpdflog <- "E:/2015Doct/data/log.txt"
# call pdftotxt.exe and set parameters for convert pdf to text
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