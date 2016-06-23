library(shiny)
library(RCurl)
library(XML)
library(lda)
library(LDAvis)

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

ui <- navbarPage(theme = "bootstrap.min.css", div(img(src = "melete.png", height = "25"), "ToPān v.0.1"), windowTitle = "ToPān v.0.1",
                 tabPanel("Home", mainPanel(includeMarkdown("home.md"))),
                 
                 navbarMenu("Data Input",
                            tabPanel("CTS API", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("cts_urn", label = "CTS URN", choices = urns),
                                         submitButton("Submit")
                                         ),
                                       mainPanel(
                                         dataTableOutput("catalogue")
                                       )
                                       )),
                            tabPanel("Local CAPITainS", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         textInput("api_url", label = "API URL", value = "http://192.168.99.100:32778/api/cts/?request="),
                                         textInput("api_cts_urn", label = "CTS URN", value = ""),
                                         submitButton("Submit")
                                       ),
                                       mainPanel(
                                         dataTableOutput("catalogue2")
                                       ))),
                            tabPanel("DNZ API",
                                     sidebarLayout(
                                       sidebarPanel(
                                         textInput("search_text", label = "Search Text", value = "christchurch+earthquake"),
                                         textInput("api_key", label = "DNZ API Key", value = ""),
                                         radioButtons("collection", label = "Collection", 
                                                      choices = list("ATL Cartoon Descriptions" = 1, "Index NZ Abstracts" = 2, "no search" = 3
                                                      ), selected = 3),
                                         textInput("date", label = "Year", value = "2011"),
                                         submitButton("Submit")
                                       ),
                                       mainPanel(
                                         dataTableOutput("catalogue3")
                                       ))),
                            tabPanel("CSV",
                                     sidebarLayout(
                                       sidebarPanel(
                                         fileInput('file1', 'Choose CSV File',
                                                   accept=c('text/csv', 
                                                            'text/comma-separated-values,text/plain', 
                                                            '.csv')),
                                         checkboxInput('header', 'Header', TRUE),
                                         radioButtons('sep', 'Separator',
                                                      c(Comma=',',
                                                        Semicolon=';',
                                                        Tab='\t'),
                                                      ','),
                                         radioButtons('quote', 'Quote',
                                                      c(None='',
                                                        'Double Quote'='"',
                                                        'Single Quote'="'"),
                                                      '"'),
                                         actionButton("button4", "Submit")
                                       ),
                                       mainPanel(
                                         dataTableOutput("catalogue4")
                                       )))),
                 
                 tabPanel("Morphology Service", 
                          # Sidebar with a slider input for the number of bins
                          mainPanel()
                 ),
                 
                 tabPanel("LDA TM",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("tm_corpus", label = "TM Corpus", choices = dir("./www/")[grep(".rds", dir("./www/"))]),
                              textInput("add_stopwords", label = "Additional Stopwords", value = ""),
                              sliderInput("number_topics", label = "Number of Topics", min = 2, max = 25, value = 15),
                              sliderInput("alpha", label = "Alpha", min = 0.00, max = 0.10, value = 0.02),
                              sliderInput("eta", label = "Eta", min = 0.00, max = 0.10, value = 0.02),
                              sliderInput("number_terms", label = "Number of Terms Shown", min = 15, max = 50, value = 25),
                              sliderInput("iterations", label = "Iterations", min = 500, max = 5000, value = 500),
                              submitButton("Submit")
                            ),
                            mainPanel(
                              dataTableOutput("topicmodelling")
                            ))),
                 
                 tabPanel("LDAvis", 
                          # Sidebar with a slider input for the number of bins
                          mainPanel(htmlOutput("topicmodels"))
                          ),
                 
                 navbarMenu("LDA Tables", 
                            tabPanel("DocumentTopic (θ)", mainPanel(dataTableOutput("theta"))),
                            tabPanel("TermTopic (φ)", mainPanel(dataTableOutput("phi")))
                 ),
                 
                 navbarMenu("About", 
                            tabPanel("About ToPān", mainPanel(includeMarkdown("home.md"))),
                            tabPanel("About Me", mainPanel(includeMarkdown("home.md")))
                            )
                 )

server <- function(input, output, session) {
  
  output$topicmodels <- renderUI({
    getPage<-function() {
      return(tags$iframe(src = "./temp_vis/index.html"
                         , style="width:150%;",  frameborder="0"
                         ,id="iframe"
                         , height = "800px"))
    }
    getPage()})
  
  output$catalogue <- renderDataTable({
    baseURL <- "http://cts.perseids.org/api/cts/?request=GetPassage&urn="
    reffURL <- "http://cts.perseids.org/api/cts/?request=GetValidReff&urn="
    requestURN <- input$cts_urn
    
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
    
    withProgress(message = 'First Set of References', value = 0, {
      first_reffs <- fetch_reffs(requestURN)
    })
    
    withProgress(message = 'Second Set of References', value = 0, {
      urls <- paste(reffURL, first_reffs, sep = "")
    
    batch_urls <- split(urls, ceiling(seq_along(urls)/100))
    output_list <- list()
    for (i in 1:length(batch_urls)) {
      temp_vector <- getURIAsynchronous(batch_urls[[i]], async = FALSE)
      temp_vector <- unname(unlist(sapply(temp_vector, parse_reffs)))
      temp_vector <- temp_vector[!is.na(temp_vector)]
      if(length(temp_vector) == 0) break
      output_list[[i]] <- temp_vector
      incProgress(1/length(batch_urls), detail = paste("Fetched Batch", i))
    }
    second_reffs <- unlist(output_list)
    })
    
    withProgress(message = 'Third Set of References', value = 0, {
      urls <- paste(reffURL, second_reffs, sep = "")
    
    batch_urls <- split(urls, ceiling(seq_along(urls)/100))
    output_list <- list()
    for (i in 1:length(batch_urls)) {
      temp_vector <- getURIAsynchronous(batch_urls[[i]], async = FALSE)
      temp_vector <- unname(unlist(sapply(temp_vector, parse_reffs)))
      temp_vector <- temp_vector[!is.na(temp_vector)]
      if(length(temp_vector) == 0) break
      output_list[[i]] <- temp_vector
      incProgress(1/length(batch_urls), detail = paste("Fetched Batch", i))
    }
    third_reffs <- unlist(output_list)})
    
    withProgress(message = 'Fourth Set of References', value = 0, {
      urls <- paste(reffURL, third_reffs, sep = "")
    batch_urls <- split(urls, ceiling(seq_along(urls)/100))
    output_list <- list()
    for (i in 1:length(batch_urls)) {
      temp_vector <- getURIAsynchronous(batch_urls[[i]], async = FALSE)
      temp_vector <- unname(unlist(sapply(temp_vector, parse_reffs)))
      temp_vector <- temp_vector[!is.na(temp_vector)]
      if(length(temp_vector) == 0) break
      output_list[[i]] <- temp_vector
      incProgress(1/length(batch_urls), detail = paste("Fetched Batch", i))
    }
    fourth_reffs <- unlist(output_list)})
    
    
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
    for (i in 1:length(batch_urls)) {
      temp_vector <- getURI(batch_urls[[i]], async = FALSE)
      temp_vector <- unlist(lapply(temp_vector, XMLpassage1))
      output_list[[i]] <- temp_vector
      rm(temp_vector)
      incProgress(1/length(batch_urls), detail = paste("Fetched Batch", i))
    }
    corpus <- unlist(output_list)})
    
    
    
    # withProgress(message = 'Parse Texts', value = 0, {
    #  corpus <- unlist(lapply(XMLcorpus, XMLpassage1))
    corpus.df <- data.frame(reffs, corpus)
    colnames(corpus.df) <- c("identifier", "text")
    write.csv(corpus.df, "./www/corpus.csv", row.names = FALSE)
    # })
    withProgress(message = 'Reading Texts', value = 0, {
      read.csv("./www/corpus.csv", header = TRUE, sep = ",", quote = "\"")
    })
  })
  
  output$catalogue2 <- renderDataTable({
    baseURL <- paste(input$api_url, "GetPassge&urn=", sep = "")
    reffURL <- paste(input$api_url, "GetValidReff&urn=", sep = "")
    requestURN <- input$api_cts_urn
    
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
    
    withProgress(message = 'First Set of References', value = 0, {
      first_reffs <- fetch_reffs(requestURN)
    })
    
    withProgress(message = 'Second Set of References', value = 0, {
      urls <- paste(reffURL, first_reffs, sep = "")
      
      batch_urls <- split(urls, ceiling(seq_along(urls)/100))
      output_list <- list()
      for (i in 1:length(batch_urls)) {
        temp_vector <- getURIAsynchronous(batch_urls[[i]])
        temp_vector <- unname(unlist(sapply(temp_vector, parse_reffs)))
        temp_vector <- temp_vector[!is.na(temp_vector)]
        if(length(temp_vector) == 0) break
        output_list[[i]] <- temp_vector
        incProgress(1/length(batch_urls), detail = paste("Fetched Batch", i))
      }
      second_reffs <- unlist(output_list)
    })
    
    withProgress(message = 'Third Set of References', value = 0, {
      urls <- paste(reffURL, second_reffs, sep = "")
      
      batch_urls <- split(urls, ceiling(seq_along(urls)/100))
      output_list <- list()
      for (i in 1:length(batch_urls)) {
        temp_vector <- getURIAsynchronous(batch_urls[[i]])
        temp_vector <- unname(unlist(sapply(temp_vector, parse_reffs)))
        temp_vector <- temp_vector[!is.na(temp_vector)]
        if(length(temp_vector) == 0) break
        output_list[[i]] <- temp_vector
        incProgress(1/length(batch_urls), detail = paste("Fetched Batch", i))
      }
      third_reffs <- unlist(output_list)})
    
    withProgress(message = 'Fourth Set of References', value = 0, {
      urls <- paste(reffURL, third_reffs, sep = "")
      batch_urls <- split(urls, ceiling(seq_along(urls)/100))
      output_list <- list()
      for (i in 1:length(batch_urls)) {
        temp_vector <- getURIAsynchronous(batch_urls[[i]])
        temp_vector <- unname(unlist(sapply(temp_vector, parse_reffs)))
        temp_vector <- temp_vector[!is.na(temp_vector)]
        if(length(temp_vector) == 0) break
        output_list[[i]] <- temp_vector
        incProgress(1/length(batch_urls), detail = paste("Fetched Batch", i))
      }
      fourth_reffs <- unlist(output_list)})
    
    
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
      for (i in 1:length(batch_urls)) {
        temp_vector <- getURI(batch_urls[[i]], async = FALSE)
        temp_vector <- unlist(lapply(temp_vector, XMLpassage1))
        output_list[[i]] <- temp_vector
        rm(temp_vector)
        incProgress(1/length(batch_urls), detail = paste("Fetched Batch", i))
      }
      corpus <- unlist(output_list)})
    
    
    
    # withProgress(message = 'Parse Texts', value = 0, {
    #  corpus <- unlist(lapply(XMLcorpus, XMLpassage1))
    corpus.df <- data.frame(reffs, corpus)
    colnames(corpus.df) <- c("identifier", "text")
    write.csv(corpus.df, "./www/corpus.csv", row.names = FALSE)
    # })
    withProgress(message = 'Reading Texts', value = 0, {
      read.csv("./www/corpus.csv", header = TRUE, sep = ",", quote = "\"")
    })
  })
  
  output$catalogue4 <- renderDataTable({
    input$button4
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    withProgress(message = 'Reading Texts', value = 0, {
      CSVcatalogue <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    })
    withProgress(message = 'Save Binary...', value = 0, {
      file_name <- unlist(strsplit(as.character(CSVcatalogue[1,1]), ":", fixed = TRUE))[4]
      file_name <- paste("./www/", file_name, ".rds", sep = "")
      saveRDS(CSVcatalogue, file_name)
      CSVcatalogue
    })
  })
  
  output$topicmodelling <- renderDataTable({
    file_name <- input$tm_corpus
    file_name <- paste("./www/", file_name, sep = "")
    withProgress(message = 'Reading Texts', value = 0, {
      research_corpus <- readRDS(file_name)
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
    K <- input$number_topics
    iterations <- input$iterations
    alpha <- input$alpha
    eta <- input$eta
    number_terms <- input$number_terms
    
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
    
    withProgress(message = 'Reading Texts', value = 0, {
      research_corpus <- readRDS(file_name)
    })
    research_corpus

  })
  
  output$theta <- renderDataTable({
    withProgress(message = 'Reading Texts', value = 0, {
      read.csv("./www/temp_tab/theta.csv", header = TRUE, sep = ",", quote = "\"")
    })
  })
  
  output$phi <- renderDataTable({
    withProgress(message = 'Reading Texts', value = 0, {
      read.csv("./www/temp_tab/phi.csv", header = TRUE, sep = ",", quote = "\"")
    })
  })
  
  }

shinyApp(ui = ui, server = server)