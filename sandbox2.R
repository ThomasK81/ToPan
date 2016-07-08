library(XML) #also install XML2
library(httr)

# baseURL <- "http://192.168.99.100:32779/api/cts/?request=GetPassage&urn="
# reffURL <- "http://192.168.99.100:32779/api/cts/?request=GetValidReff&urn="

baseURL <- "http://cts.perseids.org/api/cts/?request=GetPassage&urn="
reffURL <- "http://cts.perseids.org/api/cts/?request=GetValidReff&urn="

requestURN <- "urn:cts:latinLit:phi1002.phi001.perseus-eng2"


  
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

morpheusURL <- "https://services.perseids.org/bsp/morphologyservice/analysis/word?word="


