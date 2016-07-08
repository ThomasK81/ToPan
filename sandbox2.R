library(XML) #also install XML2
library(httr)

# baseURL <- "http://192.168.99.100:32779/api/cts/?request=GetPassage&urn="
# reffURL <- "http://192.168.99.100:32779/api/cts/?request=GetValidReff&urn="

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
  if(xmlValue(xmltop[[2]]) == ""){
    return(FALSE)
  }
  if(xmlValue(xmltop[[2]][[1]][[1]]) == "Internal Server Error"){
    return(FALSE)
  }
  return(TRUE)
}

fetch_passage <- function(x){
  message("Retrieve Passage for ", x)
  URL <- paste(baseURL, x, sep = "")
  URLcontent <- content(GET(URL), type = "text/xml")
  xmlfile <- xmlTreeParse(URLcontent)
  xmltop <- xmlRoot(xmlfile)
  response <- xmltop[[2]]
  passage <- vector()
  for (i in 1:length(xmltop[[2]][[1]])) {
    passage[i] <- xmlValue(response[[2]][[1]])
  }
  passage <- gsub("\n", "", passage, fixed = FALSE)
  passage <- gsub("\t", "", passage, fixed = FALSE)
  return(passage)
}
  
first_reffs <- fetch_reffs(requestURN)
if (test_reffs(first_reffs[1]) == TRUE) {
  second_reffs <- unlist(lapply(first_reffs, fetch_reffs))
  second_reffs <- unique(second_reffs)
  if (length(grep("Internal Server Error", second_reffs)) != 0) {
    second_reffs <- second_reffs[-grep("Internal Server Error", second_reffs)]
  }
} else {second_reffs <- vector()}
if (test_reffs(second_reffs[1]) == TRUE) {
  third_reffs <- unlist(lapply(second_reffs, fetch_reffs))
  third_reffs <- unique(third_reffs)
  if (length(grep("Internal Server Error", third_reffs)) != 0) {
    third_reffs <- third_reffs[-grep("Internal Server Error", third_reffs)]
  }
} else {third_reffs <- vector()}
if (test_reffs(third_reffs[1]) == TRUE) {
  fourth_reffs <- unlist(lapply(third_reffs, fetch_reffs))
  fourth_reffs <- unique(fourth_reffs)
  if (length(grep("Internal Server Error", fourth_reffs)) != 0) {
    fourth_reffs <- fourth_reffs[-grep("Internal Server Error", fourth_reffs)]
  }
  } else {fourth_reffs <- vector()}

if (length(fourth_reffs) != 0) {
  reffs <- fourth_reffs
  } else if (length(third_reffs) != 0) {
    reffs <- third_reffs
    } else if (length(second_reffs) != 0) {
      reffs <- second_reffs
      } else {
        reffs <- first_reffs
        }

corpus <- unlist(lapply(reffs, fetch_passage))
