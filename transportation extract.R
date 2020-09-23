library(quanteda)
library(readtext)
library(pdftools)
library(stringr)
library(plyr)
# First, extract raw TXTs from PDFs. 
# To extract a whole directory, you need to build a function: 

extracttext <- function (filename) { 
  print(filename) 
  txt <- paste("Extraction failed. File size : ", file.size(filename), " bytes") # catches extraction problem due to very large pdfs. 
  try({ txt <- pdf_text(filename) }) 
  f <- gsub("(.*)/([^/]*).pdf", "\\2", filename) 
  print(f) 
  write(txt,file.path(exportpath,paste0(f,".txt"))) 
} 
dir <- "~/R" 
exportpath <- "~/R/Extract" 
files <- list.files(pattern = "pdf$")
if (!dir.exists(file.path(exportpath))) dir.create(file.path(exportpath)) 
# Make a list of the PDFs you want to convert: 
fileswithdir <- list.files(pattern = "pdf$") 
# And run the function on all of them: 
for (file in fileswithdir) {extracttext(file)} 
# Now read your whole directory with TXTs as a readtext object. 
# UTF-8 is default encoding but sometimes, especially on Windows machines, you are dealing with Latin1 or worse. 
# Verify the encoding of your text and adapt the following line accordingly:
txtdf <- readtext(exportpath,encoding='UTF-8')
# remove hard-coded hyphenation:  
txtdf$text <- str_replace_all(txtdf$text, "¬[\\s]+", "") 
txtdf$text <- str_replace_all(txtdf$text, "-[\\s]+", "") 
# remove rubbish characters:
txtdf$text <- str_replace_all(txtdf$text, "[^[:alnum:].:,?!;]", " ") 
# remove multiple whitespaces: 
txtdf$text <- str_replace(gsub("\\s+", " ", str_trim(txtdf$text)), "B", "b") 
# Join words that have been split in single characters, like
# "sans i l l u s i o n s quoique" by "sans illusions quoique":
txtdf$text <- str_replace_all(txtdf$text,"(?<=\\w\\s)(\\w)(\\s)(?!\\w\\w)","\\1") 
# Then you can convert the readtext data.frame to a corpus and you are ready to analyze: 
txtc <- corpus(txtdf$text)
#keywords <- c("vehicle", "car", "transport", "transportation", "train", "bus", 
#              "uber", "lyft", "highway", "route", "intrastate", "interstate", 
#              "greyhound", "busses", "amtrak", "station", "metro", "subway", 
#              "truck", "suv", "hotel", "commute", "commuter rail", 
#              "public transportation", "transit", "luxury car")
# get all  files with extension "txt" in the current directory
file.list <- list.files(path = exportpath, pattern="*.txt", full.names=TRUE)

# this creates a vector where each element contains one file
all.files <- sapply(file.list, FUN = function(x)readChar(x, file.info(x)$size))

# create a dataframe
df <- data.frame( files= all.files, stringsAsFactors=FALSE)
df <- data.frame( files= sapply(file.list, 
                                FUN = function(x)readChar(x, file.info(x)$size)),
                  stringsAsFactors=FALSE)

textdata <- df[df$data, ]
textdata = gsub("[[:punct:]]", "", textdata)
textdata = gsub("[[:punct:]]", "", textdata)
textdata = gsub("[[:digit:]]", "", textdata)
textdata = gsub("http\\w+", "", textdata)
textdata = gsub("[ \t]{2,}", "", textdata)
textdata = gsub("^\\s+|\\s+$", "", textdata)
try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}
textdata = sapply(textdata, try.error)
textdata = textdata[!is.na(textdata)]
names(textdata) = NULL

tip <- readtext(paste0(exportpath, "*"))
df_corpus <- corpus(tip)
summary(df_corpus, 5)
toks_df <- tokens(df_corpus)
head(toks_df[[1]], 50)
#kw_transport <- kwic(toks_df, pattern =  'transport*')
#head(kw_transport, 10)
#kw_transport2 <- kwic(toks_df, pattern = c('transport*', 'vehicle*', 'car*', 'truck*', 'bus*', '*plane*', 'train*', 'highway*', 'station'))
#head(kw_transport2, 10)
#kw_transport3 <- kwic(toks_df, pattern = phrase('transporting victim*'))
#head(kw_transport3, 10)
dict_transportation <- dictionary(list(vehicle = c("vehicle", "car", "truck", "suv", "automobile", "motor vehicle", "bicycle", "van", "jeep", "motorcycle", "taxi", "cab", "uber", "lyft", "rideshare", "rental", "moving van", "caravan", "ridesharing"),
                                       public_transportation = c("transportation", "public transportation", "subway", "metro", "train", "amtrak", "greyhound", "light rail", "rapid transit", "busline", "railway", "mass transit", "commuter rail", "commuter bus", "bus", "plane", "airplane"),
                                       road = c("road", "street", "avenue", "boulevard", "freeway", "roadway", "interstate", "intrastate", "turnpike", "exit route", "toll road", "parkway", "long distance"),
                                       station = c("station", "terminal", "security checkpoint", "security check point", "train station", "bus station", "bus stop"),
                                       action = c("transport", "drove", "drive", "ride", "rode", "moved", "cross", "crossed", "ship", "shipped", "transfer", "transferred", "movement", "driving", "driver", "travel","walk", "flight", "escape", "escaped", "traveled")))
toks_transport <- tokens_lookup(toks_df, dictionary = dict_transportation, levels = 1)
head(toks_transport)
dfm(toks_transport)
results <- kwic(toks_df, dict_transportation)
options(max.print=999999)
sink("KWIC Transport Output.txt")
print(results)
sink()

art_dict <- dictionary(list(article_count = "https",
                       article_count2 = "http"
))

toks_articles <- dfm(toks_df, remove_punct = TRUE)
dfmat_articles <- dfm(toks_articles)
dfmat_articles_lg <- dfm_lookup(dfmat_articles, dictionary = art_dict)
head(dfmat_articles_lg)
dfmat_articles_lg <- convert(dfmat_articles_lg, to =  "data.frame")
dfmat_articles_lg

library(writexl)

write_xlsx(x = dfmat_articles_lg, path = "List of Article Count.xlsx", col_names = TRUE)

toks_transport2 <- dfm(toks_df, remove_punct = TRUE)
dfmat_transport2 <- dfm(toks_transport2)
dfmat_transport2_lg <- dfm_lookup(dfmat_transport2, dictionary = dict_transportation)
head(dfmat_transport2_lg)
dfmat_transport2_lg <- convert(dfmat_transport2_lg, to =  "data.frame")
dfmat_transport2_lg

library(writexl)

write_xlsx(x = dfmat_transport2_lg, path = "List of Articles Transportation2.xlsx", col_names = TRUE)