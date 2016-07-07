library(XML)
library(httr)

baseURL <- "http://cts.perseids.org/api/cts/?request=GetPassage&urn="
reffURL <- "http://cts.perseids.org/api/cts/?request=GetValidReff&urn="
requestURN <- "urn:cts:latinLit:phi1002.phi001.perseus-eng2"

fetch_reffs <- function(x){
  message("Retrieve Reffs for ", x)
  URL <- paste(reffURL, x, sep = "")
  URLcontent <- content(GET(URL), type = "text/xml")
  xmlfile <- xmlTreeParse(URLcontent)
  xmltop <- xmlRoot(xmlfile)
  reffs <- vector()
  for (i in 1:length(xmltop[[2]][[1]])) {
    reffs[i] <- xmlValue(xmltop[[2]][[1]][[i]])
  }
  return(reffs)
}

test_reffs <- function(x){
  message("Retrieve Reffs for ", x)
  URL <- paste(reffURL, x, sep = "")
  URLcontent <- content(GET(URL), type = "text/xml")
  xmlfile <- xmlTreeParse(URLcontent)
  xmltop <- xmlRoot(xmlfile)
  if(xmlValue(xmltop[[2]][[1]][[1]]) == "Internal Server Error"){
    return(FALSE)
  }
  return(TRUE)
}

test_passage <- function(x){
  message("Retrieve Reffs for ", x)
  URL <- paste(baseURL, x, sep = "")
  URLcontent <- content(GET(URL), type = "text/xml")
  xmlfile <- xmlTreeParse(URLcontent)
  xmltop <- xmlRoot(xmlfile)
  if(xmlValue(xmltop[[2]][[1]][[1]]) == "Internal Server Error"){
    return(FALSE)
  }
  return(TRUE)
}

fetch_passage <- function(x){
  message("Retrieve Reffs for ", x)
  URL <- paste(baseURL, x, sep = "")
  URLcontent <- content(GET(URL), type = "text/xml")
  xmlfile <- xmlTreeParse(URLcontent)
  xmltop <- xmlRoot(xmlfile)
  response <- xmltop[[2]]
  reffs <- vector()
  for (i in 1:length(xmltop[[2]][[1]])) {
    reffs[i] <- xmlValue(xmltop[[2]][[1]][[i]])
  }
  return(reffs)
}
  

parse_reffs <- function(x){
  reffs <- unlist(strsplit(x, split="<urn>|</urn>"))
  reffs <- reffs[2:length(reffs)]
  reffs <- reffs[seq(1, length(reffs), 2)]
  return(reffs)
}

first_reffs <- fetch_reffs(requestURN)
if (test_reffs(first_reffs[1]) == TRUE) {
  second_reffs <- unlist(lapply(first_reffs, fetch_reffs))
} else {second_reffs <- vector()}
if (test_reffs(second_reffs[1]) == TRUE) {
  third_reffs <- unlist(lapply(second_reffs, fetch_reffs))
} else {third_reffs <- vector()}
if (test_reffs(third_reffs[1]) == TRUE) {
  fourth_reffs <- unlist(lapply(third_reffs[1:10], fetch_reffs))
} else {fourth_reffs <- vector()}



raw.result <- GET(url = reffURL, path = first_reffs)

urls <- paste(reffURL, first_reffs, sep = "")
  
  batch_urls <- split(urls, ceiling(seq_along(urls)/100))
  output_list <- list()
  for (i in 1:length(batch_urls)) {
    print("ha")
    temp_vector <- GET(batch_urls[[i]])
    temp_vector <- unname(unlist(sapply(temp_vector, parse_reffs)))
    temp_vector <- temp_vector[!is.na(temp_vector)]
    if(length(temp_vector) == 0) break
    output_list[[i]] <- temp_vector}
  
  second_reffs <- unlist(output_list)



  urls <- paste(reffURL, second_reffs, sep = "")
  
  batch_urls <- split(urls, ceiling(seq_along(urls)/100))
  output_list <- list()
  for (i in 1:length(batch_urls)) {
    temp_vector <- getURIAsynchronous(batch_urls[[i]])
    temp_vector <- unname(unlist(sapply(temp_vector, parse_reffs)))
    temp_vector <- temp_vector[!is.na(temp_vector)]
    if(length(temp_vector) == 0) break
    output_list[[i]] <- temp_vector}

  third_reffs <- unlist(output_list)


  urls <- paste(reffURL, third_reffs, sep = "")
  batch_urls <- split(urls, ceiling(seq_along(urls)/100))
  output_list <- list()
  for (i in 1:length(batch_urls)) {
    temp_vector <- getURIAsynchronous(batch_urls[[i]])
    temp_vector <- unname(unlist(sapply(temp_vector, parse_reffs)))
    temp_vector <- temp_vector[!is.na(temp_vector)]
    if(length(temp_vector) == 0) break
    output_list[[i]] <- temp_vector}
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
XMLminer <- function(x){
  xname <- xmlName(x)
  xattrs <- xmlAttrs(x)
  c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)}

XMLpassage1 <-function(xdata){
  if (xdata == "NotRetrieved") {return(xdata)
  } else {result <- xmlParse(xdata)
  result <- as.data.frame(t(xpathSApply(result, "//*/tei:body", XMLminer)), stringsAsFactors = FALSE)[[1]]
  result <- gsub("\n", "", result, fixed = FALSE)
  result <- gsub("\t", "", result, fixed = FALSE)
  return(result)}}

withProgress(message = 'Fetch Texts', value = 0, {
  urls <- paste(baseURL, reffs, sep = "")
  t1 <- Sys.time()
  batch_urls <- split(urls, ceiling(seq_along(urls)/100))
  output_list <- vector("list", length(batch_urls))
  
  d <- debugGatherer()
  
  for (i in 1:length(batch_urls)) {
    temp_vector <- getURI(batch_urls[[i]],
                          debugfunction = d$update, 
                          verbose = TRUE)
    d$value()
    temp_vector <- unlist(lapply(temp_vector, XMLpassage1))
    output_list[[i]] <- temp_vector
    rm(temp_vector)
    incProgress(1/length(batch_urls), detail = paste("Fetched Batch", i))
    message(d$value())
  }
  corpus <- unlist(output_list)})
