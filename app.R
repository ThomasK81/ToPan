##### 0. Libraries needed #######

library(shiny)
library(RCurl)
library(XML)
library(lda)
library(LDAvis)
  ##### 0.1. Preprocessing of CTS API inventory #######

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

##### 1. User Interface #######

ui <- navbarPage(theme = "bootstrap.min.css", div(img(src = "melete.png", height = "25"), "ToPān v.0.1"), windowTitle = "ToPān v.0.1",
                 tabPanel("Home", mainPanel(includeMarkdown("home.md"))),
##### 1.1. DATA INPUT #######                
                 navbarMenu("Data Input",
##### 1.1.1. CTS API INPUT #######
                            tabPanel("CTS API", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("cts_urn", label = "CTS URN", choices = urns),
                                         actionButton("apigo", "Submit")
                                         ),
                                       mainPanel(
                                         dataTableOutput("catalogue")
                                       )
                                       )),
##### 1.1.2. LOCAL CAPITAINS INPUT #######
                            tabPanel("Local CAPITainS", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         textInput("api_url", label = "API URL", value = "http://192.168.99.100:32778/api/cts/?request="),
                                         textInput("api_cts_urn", label = "CTS URN", value = ""),
                                         actionButton("CAPITAINSgo", "Submit")
                                       ),
                                       mainPanel(
                                         dataTableOutput("catalogue2")
                                       ))),
##### 1.1.3. DNZ API INPUT #######
                            tabPanel("DNZ API",
                                     sidebarLayout(
                                       sidebarPanel(
                                         textInput("search_text", label = "Search Text", value = "christchurch+earthquake"),
                                         textInput("api_key", label = "DNZ API Key", value = ""),
                                         radioButtons("collection", label = "Collection", 
                                                      choices = list("ATL Cartoon Descriptions" = 1, "Index NZ Abstracts" = 2, "no search" = 3
                                                      ), selected = 3),
                                         textInput("date", label = "Year", value = "2011"),
                                         actionButton("DNZgo", "Submit")
                                       ),
                                       mainPanel(
                                         dataTableOutput("catalogue3")
                                       ))),
##### 1.1.4. CSV INPUT #######
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
                                         actionButton("CSVgo", "Submit")
                                       ),
                                       mainPanel(
                                         dataTableOutput("catalogue4")
                                       ))),
##### 1.1.5. TREEBANK XML INPUT #######
                            tabPanel("TreeBank XML",
                                     sidebarLayout(
                                       sidebarPanel(
                                         fileInput('file2', 'Choose TreeBank XML File',
                                                   accept=c('.xml')),
                                         actionButton("Treebankgo", "Submit")
                                       ),
                                       mainPanel(
                                         dataTableOutput("catalogue5")
                                       )))
                            ),
##### 1.2. Morphology Service Input #######
                 
tabPanel("Morphology Service",
         sidebarLayout(
           sidebarPanel(
             selectInput("morph_corpus", label = "Corpus selection", choices = dir("./www/")[grep(".rds", dir("./www/"))]),
             radioButtons("morph_method", label = "Method", choices = c("Morpheus API", "Local StemDictionary")),
             uiOutput("MorphUI"),
             actionButton("Morphgo", "Submit")
           ),
           mainPanel(
             dataTableOutput("MorpCorpus")
           ))),
##### 1.3. Topic Modelling Input #######
                 
                 tabPanel("LDA TM",
                          sidebarLayout(
                            sidebarPanel(
                              "INPUT SELECTION",
                              selectInput("tm_corpus", label = "TM Corpus", choices = dir("./www/")[grep(".rds", dir("./www/"))]),
                              "STOPWORD SETTINGS",
                              sliderInput("occurrence", label = "Occurrence threshold", min = 1, max = 5, value = 3),
                              sliderInput("stopnumber", label = "Number of Stopwords", min = 0, max = 400, value = 200),
                              textInput("add_stopwords", label = "Additional Stopwords", value = ""),
                              "TM SETTINGS",
                              sliderInput("number_topics", label = "Number of Topics", min = 2, max = 25, value = 15),
                              sliderInput("alpha", label = "Alpha", min = 0.00, max = 0.10, value = 0.02),
                              sliderInput("eta", label = "Eta", min = 0.00, max = 0.10, value = 0.02),
                              sliderInput("number_terms", label = "Number of Terms Shown", min = 15, max = 50, value = 25),
                              sliderInput("iterations", label = "Iterations", min = 500, max = 5000, value = 500),
                              actionButton("TMgo", "Submit")
                            ),
                            mainPanel(
                              dataTableOutput("topicmodelling")
                            ))),
##### 1.4. Topic Modelling Visualisation Input #######
                 
                 tabPanel("LDAvis", 
                          # Sidebar with a slider input for the number of bins
                          mainPanel(htmlOutput("topicmodels"))
                          ),
##### 1.5. Topic Modelling Tables #######

                 navbarMenu("LDA Tables", 
                            tabPanel("DocumentTopic (θ)", mainPanel(dataTableOutput("theta"))),
                            tabPanel("TermTopic (φ)", mainPanel(dataTableOutput("phi")))
                 ),
##### 1.6. About #######

                 navbarMenu("About", 
                            tabPanel("About ToPān", mainPanel(includeMarkdown("home.md"))),
                            tabPanel("About Me", mainPanel(includeMarkdown("home.md")))
                            )
                 )

##### 2. Server #######

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2)
  
##### 2.1. Catalogues #######
##### 2.1.1. Output CTS API Corpus #######  
  
  output$catalogue <- renderDataTable({
    if (input$apigo == 0)
      return()
    
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
      temp_vector <- getURI(batch_urls[[i]])
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
##### 2.1.2. Output Local Capitains API Corpus ####### 
  
  output$catalogue2 <- renderDataTable({
    if (input$CAPITAINSgo == 0)
      return()
  
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
        temp_vector <- getURI(batch_urls[[i]])
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
##### 2.1.3. Output DNZ API Corpus #######
##### 2.1.4. Output CSV Corpus #######
  
  output$catalogue4 <- renderDataTable({
    
    if (input$CSVgo == 0)
      return()
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
    })
    CSVcatalogue
  })
##### 2.1.5. Output Treebank Corpus #######
  
  output$catalogue5 <- renderDataTable({
    
    if (input$Treebankgo == 0)
      return()

    inFile <- input$file2
    
    if (is.null(inFile))
      return(NULL)
    
    withProgress(message = 'Reading Texts', value = 0, {
      data <- xmlParse(inFile$datapath)
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
      parsedCorpus <- vector()
      for (i in 1:max(as.integer(df[,1]))) {
        location <- which(as.integer(df[,1]) == i)
        corpus[i] <- paste(unname(df[location, 5]), sep = "", collapse = " ")
        parsedCorpus[i] <- paste(unname(df[location, 6]), sep = "", collapse = " ")
        identifier[i] <- paste(unname(df[location, 9])[1], "@", unname(df[location, 5])[1], "-", tail(unlist(strsplit(unname(df[location, 9])[length(unname(df[location, 9]))-1], ":", fixed = TRUE)), n = 1), "@", unname(df[location, 5])[length(unname(df[location, 5]))-1], sep = "")
      }
      corpus <- data.frame(as.character(identifier), as.character(corpus), as.character(parsedCorpus))
      names(corpus) <- c("identifier", "text", "parsed")
      withProgress(message = 'Save Binary...', value = 0, {
        file_name <- unlist(strsplit(as.character(corpus[1,1]), ":", fixed = TRUE))[4]
        file_name <- paste("./www/", file_name, "Treebank.rds", sep = "")
        saveRDS(corpus[,c(1,2)], file_name)
        file_name <- unlist(strsplit(as.character(corpus[1,1]), ":", fixed = TRUE))[4]
        file_name <- paste("./www/", file_name, "TreebankParsed.rds", sep = "")
        saveRDS(corpus[,c(1,3)], file_name)
        corpus[,c(1,2)]
      })
    })
  })
##### 2.2. Processing Morphology #######
  output$MorphUI <- renderUI({
    if (input$morph_method == "Morpheus API")
      return()
    selectInput("stemdic", label = "Choose StemDictionary", choices = dir("./www/")[grep(".rds", dir("./www/"))])
    })
  
  morph <- reactive({
    if (input$Morphgo == 0)
      return()
    if (input$morph_method == "Morpheus API")
      return(morpheus())
    if (input$morph_method == "Local StemDictionary")
      return(localStemDic())
    })
  
  localStemDic <- reactive({
    file_name <- input$stemdic
    file_name <- paste("./www/", file_name, sep = "")
    
    withProgress(message = 'Reading Texts', value = 0, {
      stem_dictionary <- readRDS(file_name)
    })
    
    ## Produce CSV Stem-Dictionary
    
    stem_dictionary_CSV <- vapply(stem_dictionary, 
                                  function(x){result <- paste(x, collapse = ";")
                                  return(result)
                                  },
                                  character(1))
    stem_dictionary_CSV <- data.frame(names(stem_dictionary_CSV), stem_dictionary_CSV)
    colnames(stem_dictionary_CSV) <- c("form", "lemmata")
    write.csv(stem_dictionary_CSV, file = "./www/stemdic.csv")
    return(stem_dictionary_CSV)
  })
  
  morpheus <- reactive({
    
    morpheusURL <- "https://services.perseids.org/bsp/morphologyservice/analysis/word?word="
    
    file_name <- input$morph_corpus
    file_name <- paste("./www/", file_name, sep = "")
    
    XMLpassage2 <-function(xdata){
      result <- xmlParse(xdata)
      temp.df <- as.data.frame(t(xpathSApply(result, "//*/hdwd", XMLminer)), stringsAsFactors = FALSE)
      as.vector(temp.df[['text']])}
    
    XMLminer <- function(x){
      xname <- xmlName(x)
      xattrs <- xmlAttrs(x)
      c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)}
    
#    parse_words <- function(x){
#      URLcontent <- x
#      if (URLcontent == "ServerError") {
#        lemma <- "NotFound"
#        return(lemma)}
#      else {
#        lemma <- if (is.null(XMLpassage2(URLcontent)) == TRUE) {
#         lemma <- "NotFound"
#          return(lemma)}
#        else {lemma <- tryCatch({XMLpassage2(URLcontent)},
#                                error = function(err) {
#                                  lemma <- "NotFound"
#                                  return(lemma)})
#        
#        lemma <- gsub("[0-9]", "", lemma)
#        lemma <- tolower(lemma)
#        lemma <- unique(lemma)
#        lemma <- paste(lemma, sep = "", collapse = ",")
#        if (nchar(lemma) == 0) {
#          lemma <- x
#          return(lemma)}
#        else {
#          return(lemma)
#        }
#        } #     }
#    }
    
    parsing <- function(x){
        word_form <- x
        withProgress(message = paste('Parse ', word_form, ": ", round((match(word_form, corpus_words)-1)/length(corpus_words)*100, digits=2), '%'), value = 0, {
      URL <- paste(morpheusURL, word_form, "&lang=lat&engine=morpheuslat", sep = "")

      URLcontent <- tryCatch({
        getURLContent(URL)}, 
        error = function(err)
        {tryCatch({
          Sys.sleep(0.1)
          getURLContent(URL)},
          error = function(err)
          {incProgress(0.1, detail = "Return original form")
            return(word_form)
          })
        })
      if (URLcontent == "ServerError") {
        lemma <- x
        incProgress(0.1, detail = "Return original form")
        return(lemma)}
      else {
        lemma <- if (is.null(XMLpassage2(URLcontent)) == TRUE) {
          lemma <- x
          incProgress(0.1, detail = "Return original form")
          return(lemma)}
        else {lemma <- tryCatch({XMLpassage2(URLcontent)},
                                error = function(err) {
                                  incProgress(0.1, detail = "Return original form")
                                  lemma <- x
                                  return(lemma)})
        
        lemma <- gsub("[0-9]", "", lemma)
        lemma <- tolower(lemma)
        lemma <- unique(lemma)
        if (nchar(lemma) == 0) {
          lemma <- x
          incProgress(0.1, detail = "Return original form")
          return(lemma)}
        else {
          incProgress(0.1, detail = paste(x, " is ", lemma))
          return(lemma)
        }
        }
      }})
    }
    
    withProgress(message = 'Reading Texts', value = 0, {
      corpus <- readRDS(file_name)
    })
    research_corpus <- corpus[,2]
    research_corpus <- factor(research_corpus)
    
    ### pre-processing:
    
    research_corpus <- tolower(research_corpus)  # force to lowercase
    research_corpus <- gsub("'", " ", research_corpus)  # remove apostrophes
    research_corpus <- gsub("-", "", research_corpus)  # remove hyphens, create composita
    # research_corpus <- gsub("v", "u", research_corpus) # normalise to 'u'
    # research_corpus <- gsub("j", "i", research_corpus) # normalise to 'i'
    
    research_corpus <- gsub("[[:punct:]]", " ", research_corpus)  # replace punctuation with space
    research_corpus <- gsub("[[:cntrl:]]", " ", research_corpus)  # replace control characters with space
    research_corpus <- gsub("^[[:space:]]+", "", research_corpus) # remove whitespace at beginning of documents
    research_corpus <- gsub("[[:space:]]+$", "", research_corpus) # remove whitespace at end of documents
    research_corpus <- gsub("[0-9]", "", research_corpus) #remove numbers
    
    ## produce dictionary for stemming:
    
    t1 <- Sys.time()
    
    ## tokenize on space and output as a list:
    doc.list <- strsplit(research_corpus, "[[:space:]]+")
    corpus_words <- unique(unlist(doc.list))
    corpus_words <- sort(corpus_words)
    
    ## stemming
    
    stem_dictionary <- sapply(corpus_words, parsing)
    file_name <- paste("./www/", "StemDic", ".rds", sep = "")
    saveRDS(stem_dictionary, file_name)
    ## Produce CSV Stem-Dictionary
    
    stem_dictionary_CSV <- vapply(stem_dictionary, 
                                  function(x){result <- paste(x, collapse = ";")
                                  return(result)
                                  },
                                  character(1))
    stem_dictionary_CSV <- data.frame(names(stem_dictionary_CSV), stem_dictionary_CSV)
    colnames(stem_dictionary_CSV) <- c("form", "lemmata")
    write.csv(stem_dictionary_CSV, file = "./www/stemdic.csv")
    return(stem_dictionary_CSV)
    return(stem_dictionary)
    # data.frame(stem_dictionary)
    # withProgress(message = paste('Produce Morphology URLs (', as.character(length(corpus_words)), ')', sep = " "), value = 0, {
    #  urls <- paste(morpheusURL, corpus_words, "&lang=lat&engine=morpheuslat", sep = "")
    #  batch_urls <- split(urls, ceiling(seq_along(urls)/1000))})
    # withProgress(message = paste('Produce StemDictionary (', as.character(length(corpus_words)), ')', sep = " "), value = 0, {
    #  output_list <- list()
    #  for (i in 1:length(batch_urls)) {
    #    t1 <- Sys.time()
    #    temp_vector <- getURI(batch_urls[[i]])
    #    temp_vector <- unname(unlist(sapply(temp_vector, parse_words)))
    #    output_list[[i]] <- temp_vector
    #    t2 <- Sys.time()
    #    time_needed <- t2 - t1
    #    incProgress(1/length(batch_urls), detail = paste("Fetched Batch ", i, " of ", length(batch_urls), ". Time needed: ", time_needed))
    #  }
     # lemmata <- unlist(output_list)
    #})
    
    # return(data.frame(corpus_words, urls, lemmata))
  })
  
  output$MorpCorpus <- renderDataTable({ # Print the result to the main panel
    if(!is.null(morph())) morph()
  })
  
##### 2.3. Output LDAvis #######
  
  output$topicmodels <- renderUI({
    getPage<-function() {
      return(tags$iframe(src = "./temp_vis/index.html"
                         , style="width:150%;",  frameborder="0"
                         ,id="iframe"
                         , height = "800px"))
    }
    getPage()})
  
  
##### 2.4. Processing TM #######
  
  output$topicmodelling <- renderDataTable({
    if (input$TMgo == 0)
      return()
    file_name <- input$tm_corpus
    file_name <- paste("./www/", file_name, sep = "")
    withProgress(message = 'Reading Texts', value = 0, {
      research_corpus <- readRDS(file_name)
    })
    stopword_corpus <- as.character(research_corpus[,2])
    output_names <- as.character(research_corpus[,1])
    research_corpus <- as.character(research_corpus[,2])
    
    
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
    
    # tokenize on space and output as a list:
    doc.list <- strsplit(research_corpus, "[[:space:]]+")
    all_words <- unlist(doc.list)
    # compute the table of terms:
    term.table <- table(all_words)
    term.table <- sort(term.table, decreasing = TRUE)
    
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
    stop_words <- as.character(as.data.frame(stop_words[1:input$stopnumber,])[,1])
    occurences <- input$occurrence
    del <- names(term.table) %in% stop_words | term.table < occurences
    term.table <- term.table[!del]
    vocab <- names(term.table)
    
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
    
    #Visualise
    serVis(json, out.dir = 'www/temp_vis', open.browser = FALSE)
    
    withProgress(message = 'Reading Texts', value = 0, {
      research_corpus <- readRDS(file_name)
    })
    
    withProgress(message = 'Generating Tables', value = 0, {
    # Tables
    
    ## reorder phi and theta according to JSON file
    
    new.order <- RJSONIO::fromJSON(json)$topic.order 
    phi <- phi[new.order,]
    theta <- theta[,new.order]
    
    ## generate topicnames
    
    phi.t <- t(phi)
    topicnames <- vector(mode="character", length=K)
    for (i in 1:K){
      topicnames[i] <- paste(rownames(head(phi.t[order(phi.t[,i],decreasing=TRUE),], n=7)), sep="", collapse="_")
      
    }
    
    #phi-table
    rownames(phi) <- topicnames
    phi <- t(phi)
    saveRDS(phi, file = "./www/temp_tab/phi.rds")
    write.csv(phi, file = "./www/temp_tab/phi.csv")
    
    #theta-table
    theta <- cbind(output_names, as.character(research_corpus[,2]), theta) 
    colnames(theta) <- c("identifier", "text", topicnames)
    saveRDS(theta, file = "./www/temp_tab/theta.rds")
    write.csv(theta, file = "./www/temp_tab/theta.csv")
    })
    
    research_corpus

  })
##### 2.5. Output Tables #######
##### 2.5.1. Output Theta Table #######
  
  output$theta <- renderDataTable({
    withProgress(message = 'Reading Texts', value = 0, {
      read.csv("./www/temp_tab/theta.csv", header = TRUE, sep = ",", quote = "\"")
    })
  })
##### 2.5.2. Output Phi Table #######
  
  output$phi <- renderDataTable({
    withProgress(message = 'Reading Texts', value = 0, {
      read.csv("./www/temp_tab/phi.csv", header = TRUE, sep = ",", quote = "\"")
    })
  })
  
}

##### 3. Start It Up #######

shinyApp(ui = ui, server = server)