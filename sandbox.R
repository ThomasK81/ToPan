setwd("~/OneDrive/GithubProjects/ToPan")
library(RCurl)
library(XML)

data <- xmlParse("treebank.xml")
xml_data <- xmlToList(data)
xml_data <- xml_data[5:(length(xml_data)-1)]
df <- list()
counter <- 0
for (i in 1:length(xml_data)) {
  for (j in 1:(length(xml_data[[i]])-1)) {
    counter <- counter + 1
    row_number <- counter
    df[[counter]] <- c(xml_data[[i]][[length(xml_data[[i]])]], xml_data[[i]][[j]])
  }
}
df <- df[-which(unlist(lapply(df, length)) == 9)]
df <- data.frame(df, stringsAsFactors = FALSE)
df <- t(df)

identifier <- vector()
corpus <- vector()
for (i in 1:max(as.integer(df[,1]))) {
  location <- which(as.integer(df[,1]) == i)
  corpus[i] <- paste(unname(df[location, 5]), sep = "", collapse = " ")
  identifier[i] <- paste(unname(df[location, 9])[1], "@", unname(df[location, 5])[1], "-", tail(unlist(strsplit(unname(df[location, 9])[length(unname(df[location, 9]))-1], ":", fixed = TRUE)), n = 1), "@", unname(df[location, 5])[length(unname(df[location, 5]))-1], sep = "")
}
corpus <- data.frame(as.character(identifier), as.character(corpus))
names(corpus) <- c("identifier", "text")

saveRDS(corpus, "corpus.rds")

withProgress(message = 'Reading Texts', value = 0, {
  research_corpus <- readRDS("corpus.rds")
})
stopword_corpus <- as.character(research_corpus[,2])
output_names <- as.character(research_corpus[,1])
research_corpus <- as.character(research_corpus[,2])

withProgress(message = 'Start normalisation', value = 0, {
  research_corpus <- gsub("^[[:space:]]+", "", research_corpus) # remove whitespace at beginning of documents
  research_corpus <- gsub("[[:space:]]+$", "", research_corpus) # remove whitespace at end of documents
  research_corpus <- gsub("[[:space:]]+", " ", research_corpus) # remove multiple whitespace
  research_corpus <- trimws(research_corpus)
  
  output_names <- gsub("^[[:space:]]+", "", output_names) # remove whitespace at beginning of documents
  output_names <- gsub("[[:space:]]+$", "", output_names) # remove whitespace at end of documents
  output_names <- gsub("[[:space:]]+", " ", output_names) # remove multiple whitespace
  output_names <- trimws(output_names)
  
  research_corpus <- gsub("[[:punct:]]", " ", research_corpus)  # replace punctuation with space
  research_corpus <- gsub("[[:cntrl:]]", " ", research_corpus)  # replace control characters with space
  research_corpus <- gsub("^[[:space:]]+", "", research_corpus) # remove whitespace at beginning of documents
  research_corpus <- gsub("[[:space:]]+$", "", research_corpus) # remove whitespace at end of documents
  research_corpus <- gsub("[0-9]", "", research_corpus) #remove numbers
  
  incProgress(0.1, detail = "tokenize on space")
  # tokenize on space and output as a list:
  doc.list <- strsplit(research_corpus, "[[:space:]]+")
  all_words <- unlist(doc.list)
  incProgress(0.1, detail = "compute terms")
  # compute the table of terms:
  term.table <- table(all_words)
  term.table <- sort(term.table, decreasing = TRUE)
  
  incProgress(0.1, detail = "determing stopwords")
  # determing stopwords
  
  stopword_corpus <- gsub("[[:punct:]]", " ", stopword_corpus)  # replace punctuation with space
  stopword_corpus <- gsub("[[:cntrl:]]", " ", stopword_corpus)  # replace control characters with space
  stopword_corpus <- gsub("^[[:space:]]+", "", stopword_corpus) # remove whitespace at beginning of documents
  stopword_corpus <- gsub("[[:space:]]+$", "", stopword_corpus) # remove whitespace at end of documents
  stopword_corpus <- gsub("[0-9]", "", stopword_corpus) #remove numbers
  
  # tokenize stopword_corpus on space and output as a list:
  doc.list2 <- strsplit(stopword_corpus, "[[:space:]]+")
  
  # compute the table of stop_words:
  all_for_stop_words <- unlist(doc.list2)
  term.table2 <- table(all_for_stop_words)
  term.table2 <- sort(term.table2, decreasing = TRUE)
  
  stop_words <- as.data.frame(term.table2)
  rm(term.table2)
  stop_words <- row.names(as.data.frame(stop_words[1:200,]))
  occurences <- 5
  del <- names(term.table) %in% stop_words | term.table < occurences
  term.table <- term.table[!del]
  vocab <- names(term.table)
})

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

# Fit the model:
set.seed(73)
K <- 12
iterations <- 500
alpha <- 0.2
eta <- 0.2
number_terms <- 30

fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = iterations, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

research_corpusAbstracts <- list(phi = phi,
                                 theta = theta,
                                 doc.length = doc.length,
                                 vocab = vocab,
                                 term.frequency = term.frequency)

# create the JSON object to feed the visualization:
json <- createJSON(phi = research_corpusAbstracts$phi, 
                   theta = research_corpusAbstracts$theta, 
                   doc.length = research_corpusAbstracts$doc.length, 
                   vocab = research_corpusAbstracts$vocab, 
                   term.frequency = research_corpusAbstracts$term.frequency,
                   R=number_terms)

#Visulise and start browser
serVis(json, out.dir = 'www/temp_vis', open.browser = FALSE)