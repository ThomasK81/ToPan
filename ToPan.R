##### 0.1, Libraries #######

library(shiny)
library(RCurl)
library(XML) #also install XML2
library(httr)
library(lda)
library(LDAvis)
library(data.table)

##### 0.2. Globals #######

CTS.Rep <- "http://cts.perseids.org/api/cts/?request=GetCapabilities"

##### 0.3. Functions #######

FetchCTSRep <- function(x) {
  xmlfile <- xmlTreeParse(x)
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
  return(urns)
}

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

##### 1. User Interface #######

ui <- navbarPage(theme = "bootstrap.min.css", div(img(src = "melete.png", height = "25"), "ToPān v.0.2"), windowTitle = "ToPān v.0.2 (beta)",
##### 1.0.1. Home #######
                 tabPanel("Home",
                          fluidRow(column(4, br(), div(img(src = "melete.png", height = "200"))),
                                   column(8, includeMarkdown("home.md")))),                 
                 tabPanel("Instructions",
                          sidebarLayout(sidebarPanel(br(), h6("Instructions"),
                                                     actionLink("data_link", "1. Entering the Data"), br(),
                                                     actionLink("morph_link", "2. Morphological Normalisation"), br(),
                                                     actionLink("tm_link", "3. Setting the TM Values"), br(),
                                                     actionLink("results_link", "4. Understanding the Results"), br(), br(),
                                                     actionLink("copyright_link", "Copyright Note")),
                                        mainPanel(htmlOutput("markdownfile")))),
##### 1.1. DATA INPUT #######                
                 navbarMenu("Data Input",
##### 1.1.1. CTS API INPUT #######
                            tabPanel("CTS API", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         uiOutput("CTSUI"),
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
##### 1.1.3. Server-Side RDS #######
                            tabPanel("Server-Side RDS",
                                     sidebarLayout(
                                       sidebarPanel(
                                         uiOutput("RDSUI"),
                                         actionButton("RDSgo", "Submit")
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
                                         radioButtons('label', 'Label (has to be column 3)',
                                                      c(Yes=TRUE,
                                                        No=FALSE),
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
                                       ))),
##### 1.1.6. 82XF INPUT #######
                            tabPanel("82XF",
                                     sidebarLayout(
                                       sidebarPanel(
                                         fileInput('file3', 'Choose 82XF File',
                                                   accept=c('.82XF', '.82xf')),
                                         actionButton("XFgo", "Submit")
                                       ),
                                       mainPanel(
                                         dataTableOutput("catalogue6")
                                       )))
                 ),
##### 1.2. Morphology Service Input #######
                 
                 tabPanel("Morphology Service",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput('morph_corpus', 'Corpus selection',
                                        accept=c('.rds')),
                              radioButtons("morph_method", label = "Method", choices = c("Morpheus API", "Local StemDictionary", "Server StemDictionary")),
                              uiOutput("MorphUI"),
                              actionButton("Morphgo", "Submit")
                            ),
                            mainPanel(
                              dataTableOutput("MorpCorpus")
                            ))),
##### 1.3. Stop Words #######
                 
                 tabPanel("Stop Words",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput('sw_corpus', 'SW Corpus', accept=c('.rds')),
                              sliderInput("stopnumber", label = "Number of Stopwords", min = 0, max = 400, value = 200),
                              textInput("add_stopwords", label = "Additional Stopwords", value = ""),
                              textInput("remove_stopwords", label = "Remove Words from Stopword list", value = ""),
                              actionButton("stopwordgo", "Submit")
                            ),
                            mainPanel(
                              dataTableOutput("stopwords")
                            ))),
                 
##### 1.4. Topic Modelling Input #######
                 
                 tabPanel("LDA TM",
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("ProcessTM"),
                              uiOutput("ProcessSW"),
                              sliderInput("occurrence", label = "Occurrence threshold", min = 1, max = 5, value = 3),
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
##### 1.5. Topic Modelling Visualisation Input #######
                 
                 tabPanel("LDAvis", 
                            uiOutput("VISUI"),
                            mainPanel(htmlOutput("topicmodels")
                            )
                 ),
##### 1.6. Topic Modelling Tables #######
                 
                 navbarMenu("LDA Tables", 
                            tabPanel("DocumentTopic (θ)",
                                     uiOutput("thetaUI"),
                                     mainPanel(dataTableOutput("theta"))),
                            tabPanel("TermTopic (φ)", 
                                     uiOutput("phiUI"),
                                     mainPanel(dataTableOutput("phi")))
                 ),
                 
##### 1.7. Explore #######
                 
                 navbarMenu("Explore",
                            tabPanel("Topics over IDs", mainPanel()),
                            tabPanel("Topics in Works", mainPanel()),
                            tabPanel("Most similar", mainPanel()),
                            tabPanel("Clusters", mainPanel())
                 ),
                 
##### 1.8. Downloads #######
                 
                 navbarMenu("Downloads",
                            tabPanel("Corpus",
                                     sidebarLayout(
                                       sidebarPanel(
                                         "INPUT SELECTION",
                                         uiOutput("dlcorpusUI"),
                                         actionButton("CorpusDownloadGo", "Preview"),
                                         downloadButton('downloadCorpus', 'Download')
                                       ),
                                       mainPanel(dataTableOutput("download_corpus")))),
                            tabPanel("Theta-Table",
                                     sidebarLayout(
                                       sidebarPanel(
                                         "INPUT SELECTION",
                                         uiOutput("dlthetaUI"),
                                         downloadButton('downloadtheta', 'Download')
                                       ),
                                       mainPanel(dataTableOutput("prevtheta"))))
                 )
)

##### 2. Server #######

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2)
  
##### 2.1. Catalogues #######
##### 2.1.1. Output CTS API Corpus #######  
  output$CTSUI <- renderUI({
    # urns <- FetchCTSRep(CTS.Rep)
    urns <- c("test1", "test2")
    selectInput("cts_urn", label = "CTS URN", choices = urns)
    })
  
  output$catalogue <- renderDataTable({
    if (input$apigo == 0)
      return()
    
    baseURL <- "http://cts.perseids.org/api/cts/?request=GetPassage&urn="
    reffURL <- "http://cts.perseids.org/api/cts/?request=GetValidReff&urn="
    requestURN <- input$cts_urn
    
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
    corpus.df <- data.frame(reffs, corpus)
    colnames(corpus.df) <- c("identifier", "text")
    write.csv(corpus.df, "./www/corpus.csv", row.names = FALSE)
    withProgress(message = 'Save Binary...', value = 0, {
      file_name <- unlist(strsplit(as.character(corpus.df[1,1]), ":", fixed = TRUE))[4]
      foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
      foldername <- paste("./www/data", foldername, sep = "/")
      dir.create(foldername, recursive = TRUE)
      file_name <- paste(foldername, "/", file_name, ".rds", sep = "")
      saveRDS(corpus.df, file_name)
    })
    corpus.df
  })
##### 2.1.2. Output Local Capitains API Corpus ####### 
  
  output$catalogue2 <- renderDataTable({
    if (input$CAPITAINSgo == 0)
      return()
    
    baseURL <- paste(input$api_url, "GetPassge&urn=", sep = "")
    reffURL <- paste(input$api_url, "GetValidReff&urn=", sep = "")
    requestURN <- input$api_cts_urn
    
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
    corpus.df <- data.frame(reffs, corpus)
    colnames(corpus.df) <- c("identifier", "text")
    write.csv(corpus.df, "./www/corpus.csv", row.names = FALSE)
    withProgress(message = 'Save Binary...', value = 0, {
      file_name <- unlist(strsplit(as.character(corpus.df[1,1]), ":", fixed = TRUE))[4]
      foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
      foldername <- paste("./www/data", foldername, sep = "/")
      dir.create(foldername, recursive = TRUE)
      file_name <- paste(foldername, "/", file_name, ".rds", sep = "")
      saveRDS(corpus.df, file_name)
    })
    corpus.df
  })
  
##### 2.1.3. Output Server-Side RDS #######
  output$RDSUI <- renderUI({
    ServerCorpora <- list.files(path = "./www/data", pattern = "*.rds", recursive = TRUE, full.names = TRUE)
    ServerCorpora <- ServerCorpora[which(grepl("Stopword", ServerCorpora) == FALSE)]
    names(ServerCorpora) <- sapply(strsplit(ServerCorpora, "/"), function(x) {x[length(x)]})
    selectInput("serverRDS", label = "Choose RDS file", choices = ServerCorpora)
    })
  
  output$catalogue3 <- renderDataTable({
    
    if (input$RDSgo == 0)
      return()
    inFile <- input$serverRDS
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      CSVcatalogue <- readRDS(inFile)
    })
    CSVcatalogue
  })
  
##### 2.1.4. Output CSV Corpus #######
  
  output$catalogue4 <- renderDataTable({
    
    if (input$CSVgo == 0)
      return()
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      CSVcatalogue <- fread(inFile$datapath, header = input$header, sep = input$sep)
    })
    colnames(CSVcatalogue) <- c('identifier', 'text')
    withProgress(message = 'Save Binary...', value = 0, {
      file_name <- unlist(strsplit(as.character(CSVcatalogue$identifier), ":", fixed = TRUE))[4]
      foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
      foldername <- paste("./www/data", foldername, sep = "/")
      dir.create(foldername, recursive = TRUE)
      file_name <- paste(foldername, "/", file_name, ".rds", sep = "")
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
        foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
        foldername <- paste("./www/data", foldername, sep = "/")
        dir.create(foldername, recursive = TRUE)
        file_name <- paste(foldername, "/", file_name, "Treebank.rds", sep = "")
        saveRDS(corpus[,c(1,2)], file_name)
        file_name <- unlist(strsplit(as.character(corpus[1,1]), ":", fixed = TRUE))[4]
        foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
        foldername <- paste("./www/data", foldername, sep = "/")
        dir.create(foldername, recursive = TRUE)
        file_name <- paste(foldername, "/", file_name, "TreebankParsed.rds", sep = "")
        saveRDS(corpus[,c(1,3)], file_name)
        corpus[,c(1,2)]
      })
    })
  })
  
##### 2.1.6. Output 82XF Corpus #######
  
  output$catalogue6 <- renderDataTable({
    
    if (input$XFgo == 0)
      return()
    inFile <- input$file3
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      CSVcatalogue <- fread(inFile$datapath, header = TRUE, sep = "#")
    })
    CSVcatalogue <- data.frame(CSVcatalogue$identifier, CSVcatalogue$passage)
    colnames(CSVcatalogue) <- c('identifier', 'text')
    withProgress(message = 'Save Binary...', value = 0, {
      file_name <- unlist(strsplit(as.character(CSVcatalogue[1,1]), ":", fixed = TRUE))[4]
      foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
      foldername <- paste("./www/data", foldername, sep = "/")
      dir.create(foldername, recursive = TRUE)
      file_name <- paste(foldername, "/", file_name, ".rds", sep = "")
      saveRDS(CSVcatalogue, file_name)
    })
    CSVcatalogue
  })
  
##### 2.2. Processing Morphology #######
  
  output$MorphUI <- renderUI({
    if (input$morph_method == "Morpheus API") {
      return(selectInput("morphlang", label = "Choose Languages", choices = c("Latin", "Greek", "Arabic"))) 
    }
    if (input$morph_method == "Server StemDictionary") {
      #### find filenames .rds
      return(selectInput("stemdic", label = "Choose StemDictionary", choices = list.files(path = "./www", pattern = "StemDic*.rds", recursive = TRUE, full.names = TRUE)))
    }
    fileInput('stemdic', 'Choose StemDictionary', accept=c('.rds'))
  })
  
  morph <- reactive({
    if (input$Morphgo == 0)
      return()
    if (input$morph_method == "Morpheus API")
      return(morpheus())
    if (input$morph_method == "Local StemDictionary")
      return(localStemDic())
    if (input$morph_method == "Server StemDictionary")
      return(serverStemDic())
  })
  
  serverStemDic <- reactive({
    
    inFile <- input$stemdic
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      stem_dictionary <- readRDS(inFile$datapath)
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
    
    ## Read in corpus
    
    inFile <- input$morph_corpus
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      corpus <- readRDS(inFile$datapath)
    })
    
    research_corpus <- corpus[,2]
    research_corpus <- as.character(research_corpus)
    identifier <- corpus[,1]
    identifier <- as.character(identifier)
    
    ### Normalise Corpus
    
    lemmatiser <- function(x){
      lemmatised <- stem_dictionary[[x]]
      return(lemmatised)}
    
    choose_lemma <- function(x){
      if (is.null(x))
        return(x)
      lemma <- names(which(NumberOccurrences[x]==max(NumberOccurrences[x])))
      if (length(lemma)==1) {return(lemma)
      }
      else {return (x[1])}
    }
    temp <- strsplit(research_corpus, " ")
    temp_correct <- list()
    for (i in 1:length(temp)) {
      temp_correct[[i]] <- sapply(temp[[i]], lemmatiser) 
    }
    NumberOccurrences <- table(unlist(temp_correct))
    
    corrected_corpus <- list()
    for (n in 1:length(temp_correct)) {
      temp_corrected <- list()
      counter <- n
      for (i in 1:length(temp_correct[[counter]])) {
        if (is.null(temp_correct[[counter]][[i]])) {
          temp_corrected[[i]] <- names(temp_correct[[counter]][i])
        } else {
          temp_corrected[[i]] <- choose_lemma(temp_correct[[counter]][[i]]) 
        }  
      }  
      corrected_corpus[[n]] <- temp_corrected
    }
    
    for (i in 1:length(corrected_corpus)) {
      corrected_corpus[[i]] <- paste(unlist(corrected_corpus[[i]]), collapse=" ")
    }
    research_corpus <- unlist(corrected_corpus)
    corrected_corpus_df <- data.frame(identifier, research_corpus)
    colnames(corrected_corpus_df) <- c('identifier', 'text')
    
    withProgress(message = 'Save Binary...', value = 0, {
      file_name <- unlist(strsplit(as.character(corrected_corpus_df[1,1]), ":", fixed = TRUE))[4]
      foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
      foldername <- paste("./www/data", foldername, sep = "/")
      dir.create(foldername, recursive = TRUE)
      file_name <- paste(foldername, "/", file_name, "-LocStemDicParsed.rds", sep = "")
      saveRDS(corrected_corpus_df, file_name)
    })
    
    return(corrected_corpus_df)
  })
  
  localStemDic <- reactive({
    
    inFile <- input$stemdic
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      stem_dictionary <- readRDS(inFile$datapath)
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
    
    ## Read in corpus
    
    inFile <- input$morph_corpus
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      corpus <- readRDS(inFile$datapath)
    })
    
    research_corpus <- corpus[,2]
    research_corpus <- as.character(research_corpus)
    identifier <- corpus[,1]
    identifier <- as.character(identifier)
    
    ### Normalise Corpus
    
    lemmatiser <- function(x){
      lemmatised <- stem_dictionary[[x]]
      return(lemmatised)}
    
    choose_lemma <- function(x){
      if (is.null(x))
        return(x)
      lemma <- names(which(NumberOccurrences[x]==max(NumberOccurrences[x])))
      if (length(lemma)==1) {return(lemma)
      }
      else {return (x[1])}
    }
    temp <- strsplit(research_corpus, " ")
    temp_correct <- list()
    for (i in 1:length(temp)) {
      temp_correct[[i]] <- sapply(temp[[i]], lemmatiser) 
    }
    NumberOccurrences <- table(unlist(temp_correct))
    
    corrected_corpus <- list()
    for (n in 1:length(temp_correct)) {
      temp_corrected <- list()
      counter <- n
      for (i in 1:length(temp_correct[[counter]])) {
        if (is.null(temp_correct[[counter]][[i]])) {
          temp_corrected[[i]] <- names(temp_correct[[counter]][i])
        } else {
          temp_corrected[[i]] <- choose_lemma(temp_correct[[counter]][[i]]) 
        }  
      }  
      corrected_corpus[[n]] <- temp_corrected
    }
    
    for (i in 1:length(corrected_corpus)) {
      corrected_corpus[[i]] <- paste(unlist(corrected_corpus[[i]]), collapse=" ")
    }
    research_corpus <- unlist(corrected_corpus)
    corrected_corpus_df <- data.frame(identifier, research_corpus)
    colnames(corrected_corpus_df) <- c('identifier', 'text')
    
    withProgress(message = 'Save Binary...', value = 0, {
      file_name <- unlist(strsplit(as.character(corrected_corpus_df[1,1]), ":", fixed = TRUE))[4]
      foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
      foldername <- paste("./www/data", foldername, sep = "/")
      dir.create(foldername, recursive = TRUE)
      file_name <- paste(foldername, "/", file_name, "-LocStemDicParsed.rds", sep = "")
      saveRDS(corrected_corpus_df, file_name)
    })
    
    return(corrected_corpus_df)
  })
  
  morpheus <- reactive({
    
    morpheusURL <- "https://services.perseids.org/bsp/morphologyservice/analysis/word?word="
    if (input$morphlang == "Latin") {
      langurl <- "&lang=lat&engine=morpheuslat"
    } else if (input$morphlang == "Greek") {
      langurl <- "&lang=grc&engine=morpheusgrc"
    } else if (input$morphlang == "Arabic") {
      langurl <- "&lang=ara&engine=aramorph"
    }
    
    XMLpassage2 <-function(xdata){
      result <- xmlParse(xdata)
      temp.df <- as.data.frame(t(xpathSApply(result, "//*/hdwd", XMLminer)), stringsAsFactors = FALSE)
      as.vector(temp.df[['text']])}
    
    XMLminer <- function(x){
      xname <- xmlName(x)
      xattrs <- xmlAttrs(x)
      c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)}
    
    parsing <- function(x){
      word_form <- x
      withProgress(message = paste('Parse ', word_form, ": ", round((match(word_form, corpus_words)-1)/length(corpus_words)*100, digits=2), '%'), value = 0, {
        URL <- paste(morpheusURL, word_form, langurl, sep = "")
        
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
    
    inFile <- input$morph_corpus
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      corpus <- readRDS(inFile$datapath)
    })
    
    research_corpus <- corpus[,2]
    research_corpus <- factor(research_corpus)
    identifier <- corpus[,1]
    identifier <- factor(identifier)
    
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
    
    ### Normalise Corpus
    
    lemmatiser <- function(x){
      lemmatised <- stem_dictionary[[x]]
      return(lemmatised)}
    
    choose_lemma <- function(x){
      if (is.null(x))
        return(x)
      lemma <- names(which(NumberOccurrences[x]==max(NumberOccurrences[x])))
      if (length(lemma)==1) {return(lemma)
      }
      else {return (x[1])}
    }
    temp <- strsplit(research_corpus, " ")
    temp_correct <- list()
    for (i in 1:length(temp)) {
      temp_correct[[i]] <- sapply(temp[[i]], lemmatiser) 
    }
    NumberOccurrences <- table(unlist(temp_correct))
    
    corrected_corpus <- list()
    for (n in 1:length(temp_correct)) {
      temp_corrected <- list()
      counter <- n
      for (i in 1:length(temp_correct[[counter]])) {
        if (is.null(temp_correct[[counter]][[i]])) {
          temp_corrected[[i]] <- names(temp_correct[[counter]][i])
        } else {
          temp_corrected[[i]] <- choose_lemma(temp_correct[[counter]][[i]]) 
        }  
      }  
      corrected_corpus[[n]] <- temp_corrected
    }
    
    for (i in 1:length(corrected_corpus)) {
      corrected_corpus[[i]] <- paste(unlist(corrected_corpus[[i]]), collapse=" ")
    }
    research_corpus <- unlist(corrected_corpus)
    corrected_corpus_df <- data.frame(identifier, research_corpus)
    colnames(corrected_corpus_df) <- c('identifier', 'text')
    
    withProgress(message = 'Save Binary...', value = 0, {
      file_name <- unlist(strsplit(as.character(corrected_corpus_df[1,1]), ":", fixed = TRUE))[4]
      foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
      foldername <- paste("./www/data", foldername, sep = "/")
      dir.create(foldername, recursive = TRUE)
      file_name <- paste(foldername, "/", file_name, "-MorphAPIParsed.rds", sep = "")
      saveRDS(corrected_corpus_df, file_name)
    })
    
    return(corrected_corpus_df)
  })
  
  output$MorpCorpus <- renderDataTable({ # Print the result to the main panel
    if(!is.null(morph())) morph()
  })
  
##### 2.3. Output LDAvis #######
  output$VISUI <- renderUI({
    ServerTM <- list.files(path = "./www", pattern = "index.html", recursive = TRUE, full.names = TRUE)
    ServerTM <- gsub("./www", "", ServerTM, fixed = TRUE)
    selectInput("TModel", label = "Choose TM", choices = ServerTM)
  })
  
  output$topicmodels <- renderUI({
    getPage<-function() {
      return(tags$iframe(src = input$TModel
                         , style="width:150%;",  frameborder="0"
                         ,id="iframe"
                         , height = "800px"))
    }
    getPage()})
  
##### 2.4. Stopwords #######
  output$stopwords <- renderDataTable({
    if (input$stopwordgo == 0)
      return()
    
    inFile <- input$sw_corpus
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      research_corpus <- readRDS(inFile)
    })
    identifier <- as.character(research_corpus[1,1])
    stopword_corpus <- as.character(research_corpus[,2])
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
    
    stop_words <- names(term.table2)
    rm(term.table2)
    stop_words <- stop_words[1:input$stopnumber]
    additional <- unlist(strsplit(input$add_stopwords, ",", fixed = TRUE))
    additional <- gsub("^[[:space:]]+", "", additional) # remove whitespace at beginning of documents
    additional <- gsub("[[:space:]]+$", "", additional) # remove whitespace at end of documents
    stop_words <- c(additional, stop_words)
    less <- unlist(strsplit(input$remove_stopwords, ",", fixed = TRUE))
    less <- gsub("^[[:space:]]+", "", less) # remove whitespace at beginning of documents
    less <- gsub("[[:space:]]+$", "", less) # remove whitespace at end of documents
    stop_words <- stop_words [! stop_words %in% less]
    
    withProgress(message = 'Save Binary...', value = 0, {
      file_name <- as.character(unlist(strsplit(identifier, ":", fixed = TRUE))[4])
      foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
      foldername <- paste("./www/data", foldername, sep = "/")
      dir.create(foldername, recursive = TRUE)
      file_name <- paste(foldername, "/", file_name, "-MF", input$stopnumber, "Stopword.rds", sep = "")
      saveRDS(stop_words, file_name)
    })
    
    data.frame(Index = c(1:length(stop_words)), Word = stop_words)
  })
  
##### 2.5. Processing TM #######
  output$ProcessTM <- renderUI({
    ServerTM <- list.files(path = "./www", pattern = ".rds", recursive = TRUE, full.names = TRUE)
    ServerTM <- ServerTM[which(grepl("Stopword", ServerTM) == FALSE)]
    names(ServerTM) <- sapply(strsplit(ServerTM, "/"), function(x) {x[length(x)]})
    selectInput("tm_corpus", label = "Choose Corpus", choices = ServerTM)
  })
  
  output$ProcessSW <- renderUI({
    ServerSW <- list.files(path = "./www", pattern = ".rds", recursive = TRUE, full.names = TRUE)
    ServerSW <- ServerSW[which(grepl("Stopword", ServerSW) == TRUE)]
    names(ServerSW) <- sapply(strsplit(ServerSW, "/"), function(x) {x[length(x)]})
    selectInput("stopwordlist", label = "Choose SW List", choices = ServerSW)
  })
  
  output$topicmodelling <- renderDataTable({
    if (input$TMgo == 0)
      return()
    
    inFile <- input$tm_corpus
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      research_corpus <- readRDS(inFile)
    })
    
    inFile <- input$stopwordlist
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      stop_words <- readRDS(inFile)
    })
    
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
    seed <- 73
    set.seed(seed)
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
    
    #Visualise and save
    inFile <- input$tm_corpus
    withProgress(message = 'Reading Texts', value = 0, {
      research_corpus <- readRDS(inFile)
    })
    
    folder <- inFile
    folder <- unlist(strsplit(folder, "/", fixed = TRUE))
    folder <- paste(folder[1:length(folder)-1], sep = "", collapse = "/")
    sw <- input$stopwordlist
    sw <- unlist(strsplit(unlist(strsplit(sw, "-", fixed = TRUE))[2], "Stop"))[1]
    folder <- paste(folder, "_", sw, "_K", K, "_alph", gsub(".", "", alpha, fixed = TRUE), "_eta", gsub(".", "", eta, fixed = TRUE), "_I", iterations, "_S", seed, sep = "")
    dir.create(paste(folder, "_tab", sep = ""), recursive = TRUE)
    visfolder <- paste(folder, "_vis", sep = "")
    dir.create(visfolder, recursive = TRUE)
    serVis(json, out.dir = visfolder, open.browser = FALSE)
    
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
      phi_folder <- paste(folder, "_tab/phi.rds", sep = "")
      phi_CSVfolder <- paste(folder, "_tab/phi.csv", sep = "")
      theta_folder <- paste(folder, "_tab/theta.rds", sep = "")
      theta_CSVfolder <- paste(folder, "_tab/theta.csv", sep = "")
      rownames(phi) <- topicnames
      phi <- t(phi)
      saveRDS(phi, file = phi_folder)
      write.csv(phi, file = phi_CSVfolder)
      
      #theta-table
      theta <- cbind(output_names, as.character(research_corpus[,2]), theta) 
      colnames(theta) <- c("identifier", "text", topicnames)
      saveRDS(theta, file = theta_folder)
      write.csv(theta, file = theta_CSVfolder)
    })
    
    research_corpus
    
  })
  
##### 2.6. Output Tables #######
##### 2.6.1. Output Theta Table #######
  output$thetaUI <- renderUI({
    ServerTheta <- list.files(path = "./www", pattern = "theta.csv", recursive = TRUE, full.names = TRUE)
    names(ServerTheta) <- sapply(strsplit(ServerTheta, "/"), function(x) {x[length(x)-1]})
    selectInput("ThetaTable", label = "Choose TM", choices = ServerTheta)
    })
  
  output$theta <- renderDataTable({
    inFile <- input$ThetaTable
    
    if (is.null(inFile))
      return(NULL)
    
    withProgress(message = 'Reading Texts', value = 0, {
      read.csv(inFile, header = TRUE, sep = ",", quote = "\"")
      })
    })
  
##### 2.6.2. Output Phi Table #######

  output$phiUI <- renderUI({
    ServerPhi <- list.files(path = "./www", pattern = "phi.csv", recursive = TRUE, full.names = TRUE)
    names(ServerPhi) <- sapply(strsplit(ServerPhi, "/"), function(x) {x[length(x)-1]})
    selectInput("PhiTable", label = "Choose TM", choices = ServerPhi)
    })

  output$phi <- renderDataTable({
    inFile <- input$PhiTable
    
    if (is.null(inFile))
      return(NULL)
    
    withProgress(message = 'Reading Texts', value = 0, {
      read.csv(inFile, header = TRUE, sep = ",", quote = "\"")
      })
    })
  
##### 2.6. Downloads #####
##### 2.6.1. Corpus #####
  output$dlcorpusUI <- renderUI({
    ServerCorpus <- list.files(path = "./www", pattern = ".rds", recursive = TRUE, full.names = TRUE)
    ServerCorpus <- ServerCorpus[which(grepl("Stopword", ServerCorpus) == FALSE)]
    ServerCorpus <- ServerCorpus[which(grepl("theta", ServerCorpus) == FALSE)]
    ServerCorpus <- ServerCorpus[which(grepl("phi", ServerCorpus) == FALSE)]
    names(ServerCorpus) <- sapply(strsplit(ServerCorpus, "/"), function(x) {x[length(x)]})
    selectInput("download_corpus", label = "Corpus", choices = ServerCorpus)
  })
  
  output$download_corpus <- renderDataTable({
    if (input$CorpusDownloadGo == 0)
      return()
    file_name <- input$download_corpus
    withProgress(message = 'Reading Texts', value = 0, {
      research_corpus <- readRDS(file_name)
    })
    identifier <- as.character(research_corpus[,1])
    passage <- as.character(research_corpus[,2])
    index <- c(1:length(identifier))
    download_corpus <- data.frame(identifier, passage, index)
    PrevUrn <- vector()
    NextUrn <- vector()
    for (i in 1:length(index)) {
      if(i-1 == 0) {
        PrevUrn[i] <- "NULL"
      } else {
        PrevUrn[i] <- identifier[which(index == i-1)]}
      if(i+1 > length(identifier)) {
        NextUrn[i] <- "NULL"
      } else {
        NextUrn[i] <- identifier[which(index == i+1)]}
    }
    
    download_corpus <- data.frame(identifier, PrevUrn, index, NextUrn, passage)
    colnames(download_corpus) <- c("Urn", "PrevUrn", "SequenceIndex", "NextUrn", "TextContent")
    download_corpus
  })
  
  output$downloadCorpus <- downloadHandler(
    filename = function() {
      paste("test", "82xf", sep = ".")
    },
    content = function(file) {
    file_name <- input$download_corpus
    withProgress(message = 'Reading Texts', value = 0, {
      research_corpus <- readRDS(file_name)
    })
    identifier <- as.character(research_corpus[,1])
    passage <- as.character(research_corpus[,2])
    index <- c(1:length(identifier))
    download_corpus <- data.frame(identifier, passage, index)
    PrevUrn <- vector()
    NextUrn <- vector()
    for (i in 1:length(index)) {
      if(i-1 == 0) {
        PrevUrn[i] <- "NULL"
      } else {
        PrevUrn[i] <- identifier[which(index == i-1)]}
      if(i+1 > length(identifier)) {
        NextUrn[i] <- "NULL"
      } else {
        NextUrn[i] <- identifier[which(index == i+1)]}
    }
    download_corpus <- data.frame(identifier, PrevUrn, index, NextUrn, passage)
    colnames(download_corpus) <- c("Urn", "PrevUrn", "SequenceIndex", "NextUrn", "TextContent")
    file_name <- unlist(strsplit(file_name, "/", fixed = TRUE))[3]
    file_name <- unlist(strsplit(file_name, ".", fixed = TRUE))[c(1,2)]
    file_name <- paste(file_name, sep = "", collapse = ".")
    file_name <- paste("./www/", file_name, ".82xf", sep = "")
    write.table(download_corpus, file, quote = TRUE, sep = "#", row.names = FALSE)
  })

##### 2.6.2. Phi-Tables #####

##### 2.6.3. Theta-Tables #####
  
  output$dlthetaUI <- renderUI({
    ServerTheta <- list.files(path = "./www", pattern = "theta.csv", recursive = TRUE, full.names = TRUE)
    names(ServerTheta) <- sapply(strsplit(ServerTheta, "/"), function(x) {x[length(x)-1]})
    selectInput("ThetaTableDL", label = "Choose TM", choices = ServerTheta)
  })
  
  output$prevtheta <- renderDataTable({
    inFile <- input$ThetaTableDL
    
    if (is.null(inFile))
      return(NULL)
    
    withProgress(message = 'Reading Texts', value = 0, {
      read.csv(inFile, header = TRUE, sep = ",", quote = "\"")
    })
  })
  
  output$downloadtheta <- downloadHandler(
    filename = function() {
      paste("test", "csv", sep = ".")
      },
    content = function(file) {
      inFile <- input$ThetaTableDL
      
      if (is.null(inFile))
        return(NULL)
      downloadtheta <- withProgress(message = 'Reading Texts', value = 0, {
        read.csv(inFile, header = TRUE, sep = ",", quote = "\"")
      })
      write.table(downloadtheta, file, quote = TRUE, sep = ",", row.names = FALSE)
      }
  )
  
##### 2.6.4. LDAvis Zip-file #####
  
}

##### 3. Start It Up #######

shinyApp(ui = ui, server = server)