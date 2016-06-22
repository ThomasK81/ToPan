library(RCurl)
library(XML)

xml.url <- "http://cts.perseids.org/api/cts/?request=GetCapabilities"
xmlfile <- xmlTreeParse(xml.url)
xmltop <- xmlRoot(xmlfile)
xmltop <- xmltop[[2]]
xmltop <- xmltop[[1]]

output <- list()
counter <- 0
for (i in 1:length(xmltop)) {
  for (j in 1:length(xmltop[[i]])) {
    for (x in 1:length(xmltop[[i]][[j]])) {
      counter <- counter + 1
      output[[counter]] <- xmlAttrs(xmltop[[i]][[j]][[x]])["urn"]
    }
  }
}
urns <- unique(unlist(output))
urns <- urns[!is.na(urns)]
metadata <- data.frame(urns)

#### fetch reffs & texts

#### fetch reffs

baseURL <- "http://cts.perseids.org/api/cts/?request=GetPassage&urn="
reffURL <- "http://cts.perseids.org/api/cts/?request=GetValidReff&urn="
requestURN <- urns[340]

fetch_reffs <- function(x){
  message("Retrieve Reffs for ", x)
  URL <- paste(reffURL, x, sep = "")
  URLcontent <- tryCatch({getURLContent(URL)},
                         error = function(err)
                         {result <- "NoReturn"
                         return(result)})
  reffs <- unlist(strsplit(URLcontent, split="<urn>|</urn>"))
  reffs <- reffs[2:length(reffs)]
  reffs <- reffs[seq(1, length(reffs), 2)]
  return(reffs)
}

parse_reffs <- function(x){
  reffs <- unlist(strsplit(x, split="<urn>|</urn>"))
  reffs <- reffs[2:length(reffs)]
  reffs <- reffs[seq(1, length(reffs), 2)]
  return(reffs)
}

first_reffs <- fetch_reffs(requestURN)

urls <- paste(reffURL, first_reffs, sep = "")

batch_urls <- split(urls, ceiling(seq_along(urls)/100))
output_list <- list()
for (i in 1:length(batch_urls)) {
  counter <- 0
  temp_vector <- getURIAsynchronous(batch_urls[[i]])
  temp_vector <- unname(unlist(sapply(temp_vector, parse_reffs)))
  temp_vector <- temp_vector[!is.na(temp_vector)]
  if(length(temp_vector) == 0) break
  output_list[[i]] <- temp_vector
  }
second_reffs <- unlist(output_list)

urls <- paste(reffURL, second_reffs, sep = "")

batch_urls <- split(urls, ceiling(seq_along(urls)/100))
output_list <- list()
for (i in 1:length(batch_urls)) {
  counter <- 0
  temp_vector <- getURIAsynchronous(batch_urls[[i]])
  temp_vector <- unname(unlist(sapply(temp_vector, parse_reffs)))
  temp_vector <- temp_vector[!is.na(temp_vector)]
  if(length(temp_vector) == 0) break
  output_list[[i]] <- temp_vector
}
third_reffs <- unlist(output_list)

urls <- paste(reffURL, third_reffs, sep = "")
batch_urls <- split(urls, ceiling(seq_along(urls)/100))
output_list <- list()
for (i in 1:length(batch_urls)) {
  counter <- 0
  temp_vector <- getURIAsynchronous(batch_urls[[i]])
  temp_vector <- unname(unlist(sapply(temp_vector, parse_reffs)))
  temp_vector <- temp_vector[!is.na(temp_vector)]
  if(length(temp_vector) == 0) break
  output_list[[i]] <- temp_vector
}
fourth_reffs <- unlist(output_list)


if(length(fourth_reffs) != 0) {
  reffs <- fourth_reffs
} else if(length(third_reffs) != 0) {
  reffs <- third_reffs
} else if(length(second_reffs) != 0) {
  reffs <- second_reffs
} else {
  reffs <- first_reffs
} 

#### fetch texts
urls <- paste(baseURL, reffs, sep = "")
t1 <- Sys.time()
batch_urls <- split(urls, ceiling(seq_along(urls)/100))
output_list <- list()
for (i in 1:length(batch_urls)) {
  counter <- 0
  temp_vector <- getURIAsynchronous(batch_urls[[i]])
  while(length(which(temp_vector == "")) > 0) {
    print(paste("Fetch rest of batch-request ", as.character(i), "/", as.character(length(batch_urls)), sep="")); 
    temp_vector[which(temp_vector == "")] <- getURIAsynchronous(batch_urls[[i]][which(temp_vector == "")]);
    counter <- counter+1; if (counter == 3) break}
  output_list[[i]] <- temp_vector
  print(paste("Fetched ", as.character(i), "/", as.character(length(batch_urls)), sep=""))
}
XMLcorpus <- unlist(output_list)
t2 <- Sys.time()
Time_fetchingBatches <- t2 - t1
Time_fetchingBatches

XMLminer <- function(x){
  xname <- xmlName(x)
  xattrs <- xmlAttrs(x)
  c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)}

XMLpassage1 <-function(xdata){
  result <- xmlParse(xdata)
  result <- as.data.frame(t(xpathSApply(result, "//*/tei:body", XMLminer)), stringsAsFactors = FALSE)[[1]]
  result <- gsub("\n", "", result, fixed = FALSE)
  result <- gsub("\t", "", result, fixed = FALSE)
  result}

corpus <- unlist(lapply(XMLcorpus, XMLpassage1))
corpus.df <- data.frame(reffs, corpus)
colnames(corpus.df) <- c("identifier", "text")
write.csv(corpus.df, "corpus.csv", row.names = FALSE)
