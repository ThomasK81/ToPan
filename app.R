##### Log File

ToPan.log <- file("ToPan.log", open = "wt")
sink(ToPan.log , type = c("message"))

##### 0.1, Libraries #######

library(shiny)
library(RCurl)
library(XML) #also install xml2
library(xml2) 
library(httr)
library(lda)
library(LDAvis)
library(data.table)
library(stringr)
library(plyr)
library(ggplot2)
library(jsonlite)

##### 0.2. Functions #######


### Text cleaning function
preprocess_corpus <- function(x) {
  research_corpus <- tolower(x)  # force to lowercase
  research_corpus <- gsub("'", " ", research_corpus)  # remove apostrophes
  research_corpus <- gsub("-", "", research_corpus)  # remove hyphens
  research_corpus <- gsub("[[:punct:]]", " ", research_corpus)  # replace punctuation with space
  research_corpus <- gsub("[[:cntrl:]]", " ", research_corpus)  # replace control characters with space
  research_corpus <- trimws(research_corpus)
  research_corpus <-str_replace_all(research_corpus, "[\r\n]" , "")
  research_corpus <- gsub("^ *|(?<= ) | *$", "", research_corpus, perl = TRUE) # Remove multiple whitespace
  research_corpus <- gsub("[0-9]", "", research_corpus) #remove numbers
  return(research_corpus)
}

### GetCapabilities of CTS server to plain text
FetchCTSRep <- function(x) {
  input <- read_xml(x)
  incProgress(0.1, detail = "Retrieving identifiers...")
  urns <- xml_attrs(xml_find_all(xml_ns_strip(input), "//edition"))
  urns <- lapply(urns, "[[", "urn")
  urns <- unlist(urns)
  incProgress(0.1, detail = "Retrieving metadata")
  descriptions <- xml_text(xml_find_all(xml_ns_strip(input), "//edition/description"))
  # error handling because of faulty CTS api
  if (length(urns) == length(descriptions)) {
    names(urns) <- descriptions
  }
  return(urns)
}

##### Findings urns in CTS XML

fetch_cts_ids <- function(x, y) {
  incProgress(0.0001, detail = paste('Fetching IDs for', x, "from server...", sep = " "))
  urns <- xml_text(xml_find_all(xml_ns_strip(read_xml(paste(y, x, sep = ""))), "//urn"))
  return(urns)
}

##### Finding urns in CTS XML up to fourth level

check_cts_ids <- function(x, y) {
  incProgress(0.1, detail = paste("Checking reffs for", x, "from server...", sep = " "))
  URL <- paste(y, x, sep = "")
  if(http_error(URL) == TRUE) {
    return()
  } else {
    incProgress(0.1, detail = paste("Retrieving 1st level reffs for", x, "from server...", sep = " "))
    urns <- fetch_cts_ids(x, y)
  }
  if(http_error(paste(y, urns[1], sep = "")) == TRUE) {
    return(urns)
  } else {
    incProgress(0.1, detail = paste("Retrieving 2nd level reffs for", x, "from server...", sep = " "))
    urns <- lapply(urns, fetch_cts_ids, y = y)
    urns <- unlist(urns)
  }
  if(http_error(paste(y, urns[1], sep = "")) == TRUE) {
    return(urns)
  } else {
    incProgress(0.1, detail = paste("Retrieving 3rd level reffs for", x, "from server...", sep = " "))
    urns <- lapply(urns, fetch_cts_ids, y = y)
    urns <- unlist(urns)
  }
  if(http_error(paste(y, urns[1], sep = "")) == TRUE) {
    return(urns)
  } else {
    incProgress(0.1, detail = paste("Retrieving 4th level reffs for", x, "from server...", sep = " "))
    urns <- lapply(urns, fetch_cts_ids, y = y)
    urns <- unlist(urns)
    return(urns)
  }
}

##### Get clean plain text based on CTS XML using URNs

fetch_passage <- function(x, y){
  incProgress(0.0001, detail = paste('Reading', x, "from server...", sep = " "))
  passage <- xml_text(xml_find_all(xml_ns_strip(read_xml(paste(y, x, sep = ""))), "//passage"))
  passage <-trimws(passage)
  passage <-str_replace_all(passage, "[\r\n]" , "")
  passage <- gsub("^ *|(?<= ) | *$", "", passage, perl = TRUE)
  return(passage)
}

##### 1. User Interface #######

ui <- navbarPage(theme = "bootstrap.min.css", div(img(src = "melete.png", height = "25"), "ToPān v.0.3"), windowTitle = "ToPān v.0.3 (beta)",
##### 1.0.1. Home #######
                 tabPanel("Home",
                          fluidRow(column(4, br(), div(img(src = "melete.png", height = "200"))),
                                   column(8, includeMarkdown("home.md")))),
                  tabPanel("News",
                           fluidRow(column(4, br(), div(img(src = "melete.png", height = "200"))),
                                    column(8, uiOutput("markdownfile2")))),
                 tabPanel("Instructions",
                          sidebarLayout(sidebarPanel(br(), h5("Instructions"),
                            selectInput("section",
                                        label= "Section",
                                        choices= c("DataEntry", "Morph.Normalisation", "TM.Values", "Results", "Copyright"),
                                        selected= "DataEntry"
                            )),
                                        mainPanel(uiOutput("markdownfile")))),

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
                                         textInput("api_url", label = "API URL", value = "http://192.168.99.100:32778/api/cts?request="),
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
##### 1.1.4.2 CEX INPUT #######
tabPanel("CEX",
         sidebarLayout(
           sidebarPanel(
             fileInput('fileCEX', 'Choose CEX File',
                       accept=c('text/csv', 
                                'text/comma-separated-values,text/plain', 
                                '.cex',
                                '.CEX')),
             radioButtons('sepCEX', 'Separator',
                          c(Comma=',',
                            Hash='#',
                            Tab='\t'),
                          '#'),
             actionButton("CEXgo", "Submit")
           ),
           mainPanel(
             dataTableOutput("catalogueCEX")
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
                              uiOutput("MorphCorpusUI"),
                              radioButtons("morph_method", label = "Method", choices = c("Morpheus API", "LatMor", "Local StemDictionary", "Server StemDictionary")),
                              uiOutput("MorphUI"),
                              textInput("stemdicfm", label = "Name your dictionary", value = "stemdic", placeholder = "stemdic"),
                              actionButton("Morphgo", "Submit")
                            ),
                            mainPanel(
                              dataTableOutput("MorpCorpus")
                            ))),
##### 1.3. Stop Words #######
                 
                 tabPanel("Stop Words",
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("SWCorpusUI"),
                              numericInput("stopnumber", label = "Number of Stopwords", min = 0, max = 400, value = 200),
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
                              textInput("CTMfile", label = "Name your ctm-File", value = "", placeholder = "output_filename"),
                              numericInput("occurrence", label = "Occurrence threshold", min = 1, max = 5, value = 3),
                              numericInput("seed", label = "Seed", min = 1, max = 1000, value = 73),
                              numericInput("number_topics", label = "Number of Topics", min = 2, max = 200, value = 20),
                              sliderInput("alpha", label = "Alpha", min = 0.00, max = 1.00, value = 0.02),
                              sliderInput("eta", label = "Eta", min = 0.00, max = 1.00, value = 0.02),
                              numericInput("number_terms", label = "Number of Terms Shown", min = 15, max = 50, value = 25),
                              numericInput("iterations", label = "Iterations", min = 50, max = 5000, value = 500),
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
                            tabPanel("Topics over IDs",
                                     fluidRow(
                                       column(width = 4, class = "well",
                                              "INPUT SELECTION",
                                              uiOutput("ExpLDAIDUI", click = "LDAID_click"),
                                              uiOutput("ExpLDAIDUI2")),
                                       column(width = 8,
                                              fluidRow(
                                                column(width = 2,
                                                       plotOutput("ExpLDAID", height = 100,
                                                                  brush = brushOpts(
                                                                    id = "ExpLDAID_brush",
                                                                    resetOnNew = TRUE
                                                                    ))),
                                                column(width = 10,
                                                       plotOutput("ExpLDAID2", height = 400)
                                                       )))
                                              )),
                            tabPanel("Topic vs. Topic", 
                                     fluidRow(
                                       column(width = 4, class = "well",
                                              "INPUT SELECTION",
                                              uiOutput("ExpTVTUI", click = "LDAID_click"),
                                              uiOutput("ExpTVTUI2"),
                                              uiOutput("ExpTVTUI3")),
                                       column(width = 8,
                                              fluidRow(
                                                column(width = 6,
                                                       plotOutput("ExpTVT", height = 300,
                                                                  brush = brushOpts(
                                                                    id = "ExpTVT_brush",
                                                                    resetOnNew = FALSE
                                                                  ))),
                                                column(width = 6,
                                                       plotOutput("ExpTVT2", height = 300)
                                                )))
                                     ))
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
                            tabPanel("Phi-Table",
                                     sidebarLayout(
                                       sidebarPanel(
                                         "INPUT SELECTION",
                                         uiOutput("dlphiUI"),
                                         downloadButton('downloadphi', 'Download')
                                       ),
                                       mainPanel(dataTableOutput("prevphi")))),
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

##### 2.0. Instructions #######
  output$markdownfile <- renderUI({
    file <- switch(input$section,
                   DataEntry = "dataentry.md",
                   Morph.Normalisation = "morphologicalnormalisation.md",
                   TM.Values = "settingtmvalues.md",
                   Results = "understandingresults.md",
                   Copyright = "copyright.md",
                   stop("Unknown option")
    )
    includeMarkdown(file)
  })
  
  output$markdownfile2 <- renderUI({
    includeMarkdown("blog.md")
  })
  
##### 2.1. Catalogues #######
##### 2.1.1. Output CTS API Corpus #######  
  output$CTSUI <- renderUI({
    CTS.Rep <- "http://cts.dh.uni-leipzig.de/api/cts?request=GetCapabilities"
    withProgress(message = "Contacting server...", {
      urns <- FetchCTSRep(CTS.Rep)
    })
    print(which(is.na(urns)))
    selectInput("cts_urn", label = "CTS URN", choices = urns) 
    })
  
  output$catalogue <- renderDataTable({
    if (input$apigo == 0)
      return()
    
    baseURL <- "http://cts.dh.uni-leipzig.de/api/cts?request=GetPassage&urn="
    reffURL <- "http://cts.dh.uni-leipzig.de/api/cts?request=GetValidReff&urn="
    requestURN <- input$cts_urn
    
    withProgress(message = "Contacting server...", {
      reffs <- check_cts_ids(requestURN, reffURL)})
    withProgress(message = "Contacting server...", {
      corpus <- unlist(lapply(reffs, fetch_passage, y = baseURL))})
    corpus.df <- data.frame(reffs, corpus)
    
    colnames(corpus.df) <- c("identifier", "text")
    withProgress(message = 'Saving Binary...', value = 0, {
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
    
    reffs <- check_cts_ids(requestURN, reffURL)
    
    withProgress(message = "Contacting server...", value = 0, {
      corpus <- unlist(lapply(reffs, fetch_passage, y = baseURL))})
    corpus.df <- data.frame(reffs, corpus)
    
    colnames(corpus.df) <- c("identifier", "text")
    withProgress(message = 'Saving Binary...', value = 0, {
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
    ServerCorpora <- ServerCorpora[which(grepl("theta.rds", ServerCorpora, fixed = TRUE) == FALSE)]
    ServerCorpora <- ServerCorpora[which(grepl("phi.rds", ServerCorpora, fixed = TRUE) == FALSE)]
    ServerCorpora <- ServerCorpora[which(grepl("lda.rds", ServerCorpora, fixed = TRUE) == FALSE)]
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
    
    if (input$label == TRUE) {
      withProgress(message = 'Reading Texts', value = 0, {
        CSVcatalogue <- fread(inFile$datapath, header = input$header, sep = input$sep)
      })
      colnames(CSVcatalogue) <- c('identifier', 'text', 'label')
      withProgress(message = 'Saving Binary...', value = 0, {
        file_name <- unlist(strsplit(as.character(CSVcatalogue$identifier), ":", fixed = TRUE))[4]
        foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
        foldername <- paste("./www/data", foldername, sep = "/")
        dir.create(foldername, recursive = TRUE)
        file_name1 <- paste(foldername, "/", file_name, "_labelled.rds", sep = "")
        saveRDS(CSVcatalogue, file_name1)
        CSVlabels <- levels(factor(CSVcatalogue$label))
        incProgress(0.1, detail = "Produce data.frame")
        CSVcatalogue <- as.data.frame(CSVcatalogue)
        for (i in 1:length(CSVlabels)) {
          CSVlabel <- CSVlabels[i]
          incProgress(0.1, detail = paste("Export label", CSVlabel))
          CSVcatalogue_labelled <- CSVcatalogue[which(CSVcatalogue$label == CSVlabel),]
          file_name2 <- paste(foldername, "/", file_name, "_label_", CSVlabel,".rds", sep = "")
          saveRDS(CSVcatalogue_labelled, file_name2)
        }
        
      })
      return(CSVcatalogue)
    }
    withProgress(message = 'Reading Texts', value = 0, {
      CSVcatalogue <- fread(inFile$datapath, header = input$header, sep = input$sep)
    })
    colnames(CSVcatalogue) <- c('identifier', 'text')
    withProgress(message = 'Saving Binary...', value = 0, {
      file_name <- unlist(strsplit(as.character(CSVcatalogue$identifier), ":", fixed = TRUE))[4]
      foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
      foldername <- paste("./www/data", foldername, sep = "/")
      dir.create(foldername, recursive = TRUE)
      file_name <- paste(foldername, "/", file_name, ".rds", sep = "")
      saveRDS(CSVcatalogue, file_name)
    })
    CSVcatalogue
  })

##### 2.1.4.1 Output CEX Corpus #######
  
  output$catalogueCEX <- renderDataTable({
    if (input$CEXgo == 0)
      return()

    inFile <- input$fileCEX
    
    if (is.null(inFile))
      return(NULL)

    withProgress(message = 'Reading Texts', value = 0, {
      CSVcatalogue <- fread(inFile$datapath, sep = input$sepCEX, skip="#!ctsdata")
    })
    colnames(CSVcatalogue) <- c('identifier', 'text')
    withProgress(message = 'Saving Binary...', value = 0, {
      file_name <- unlist(strsplit(as.character(CSVcatalogue$identifier), ":", fixed = TRUE))[4]
      foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
      foldername <- paste("./www/data", foldername, sep = "/")
      dir.create(foldername, recursive = TRUE)
      file_name <- paste(foldername, "/CEX_", file_name, ".rds", sep = "")
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
      withProgress(message = 'Saving Binary...', value = 0, {
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
        parsed_corpus <- corpus[,c(1,3)]
        names(parsed_corpus) <- c("identifier", "text")
        saveRDS(parsed_corpus, file_name)
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
    CSVcatalogue <- data.frame(CSVcatalogue$Urn, CSVcatalogue$TextContent)
    colnames(CSVcatalogue) <- c('identifier', 'text')
    withProgress(message = 'Saving Binary...', value = 0, {
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
  
  output$MorphCorpusUI <- renderUI({
    ServerCorpora <- list.files(path = "./www/data", pattern = "*.rds", recursive = TRUE, full.names = TRUE)
    ServerCorpora <- ServerCorpora[which(grepl("Stopword", ServerCorpora) == FALSE)]
    ServerCorpora <- ServerCorpora[which(grepl("theta.rds", ServerCorpora, fixed = TRUE) == FALSE)]
    ServerCorpora <- ServerCorpora[which(grepl("phi.rds", ServerCorpora, fixed = TRUE) == FALSE)]
    ServerCorpora <- ServerCorpora[which(grepl("lda.rds", ServerCorpora, fixed = TRUE) == FALSE)]
    names(ServerCorpora) <- sapply(strsplit(ServerCorpora, "/"), function(x) {x[length(x)]})
    selectInput("morph_corpus", label = "Choose RDS file", choices = ServerCorpora)
  })
  
  output$MorphUI <- renderUI({
    if (input$morph_method == "Morpheus API") {
      return(selectInput("morphlang", label = "Choose Language", choices = c("Latin", "Greek"))) 
    }
    if (input$morph_method == "LatMor") {
      return(selectInput("latmorlang", label = "Only works for Latin", choices = c("Latin"))) 
    }
    if (input$morph_method == "Server StemDictionary") {
      #### find filenames .rds
      return(selectInput("stemdic", label = "Choose StemDictionary", choices = list.files(path = "./www", pattern = "StemDic*.rds", recursive = TRUE, full.names = TRUE)))
    }
    fileInput('stemdic', 'Choose StemDictionary', accept=c('.csv'))
  })
  
  morph <- reactive({
    if (input$Morphgo == 0)
      return()
    if (input$morph_method == "LatMor")
      return(latmor())
    if (input$morph_method == "Morpheus API")
      return(morpheus())
    if (input$morph_method == "Local StemDictionary")
      return(localStemDic())
    if (input$morph_method == "Server StemDictionary")
      return(serverStemDic())
  })
  
  serverStemDic <- reactive({
    
    req(input$stemdicfm)
    sdfm <- gsub("[^a-zA-Z0-9]", "", input$stemdicfm)
    sdfm <- paste0("./www/Dictionaries/", sdfm, ".csv")
    inFile <- input$stemdic
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      stem_dictionary <- readRDS(inFile)
    })
    
    ## Produce CSV Stem-Dictionary
    
    stem_dictionary_CSV <- vapply(stem_dictionary, 
                                  function(x){result <- paste(x, collapse = ";")
                                  return(result)
                                  },
                                  character(1))
    stem_dictionary_CSV <- data.frame(names(stem_dictionary_CSV), stem_dictionary_CSV)
    colnames(stem_dictionary_CSV) <- c("form", "lemmata")
    write.csv(stem_dictionary_CSV, file = sdfm)
    
    ## Read in corpus
    
    inFile <- input$morph_corpus
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      corpus <- readRDS(inFile)
    })
    
    research_corpus <- corpus$text
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
    
    withProgress(message = 'Saving Binary...', value = 0, {
      file_name <- unlist(strsplit(as.character(corrected_corpus_df[1,1]), ":", fixed = TRUE))[4]
      foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
      foldername <- paste("./www/data", foldername, sep = "/")
      dir.create(foldername, recursive = TRUE)
      file_name <- paste(foldername, "/", file_name, "-ServerStemDicParsed.rds", sep = "")
      saveRDS(corrected_corpus_df, file_name)
    })
    
    return(corrected_corpus_df)
  })
  
  localStemDic <- reactive({
    
    inFile <- input$stemdic
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      stem_dictionary <- fread(inFile$datapath)
    })
    temp_list <- strsplit(stem_dictionary$lemmata, ";", fixed = T)
    names(temp_list) <- stem_dictionary$form
    stem_dictionary <- temp_list
    rm(temp_list)
    
    inFile <- input$morph_corpus
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      corpus <- readRDS(inFile)
    })
    
    research_corpus <- corpus$text
    research_corpus <- as.character(research_corpus)
    research_corpus <- tolower(research_corpus)
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
    
    withProgress(message = 'Saving Binary...', value = 0, {
      file_name <- unlist(strsplit(as.character(corrected_corpus_df[1,1]), ":", fixed = TRUE))[4]
      foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
      foldername <- paste("./www/data", foldername, sep = "/")
      dir.create(foldername, recursive = TRUE)
      file_name <- paste(foldername, "/", file_name, "-LocStemDicParsed.rds", sep = "")
      saveRDS(corrected_corpus_df, file_name)
    })
    
    return(corrected_corpus_df)
  })
  
  latmor <- reactive({
    req(input$stemdicfm)
    sdfm <- gsub("[^a-zA-Z0-9]", "", input$stemdicfm)
    sdfm <- paste0("./www/Dictionaries/", sdfm, ".csv")
    
    inFile <- input$morph_corpus
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      corpus <- readRDS(inFile)
    })
    
    research_corpus <- corpus$text
    research_corpus <- factor(research_corpus)
    identifier <- corpus[,1]
    identifier <- factor(identifier)
    
    ### pre-processing:
    
    research_corpus <- preprocess_corpus(research_corpus)
    
    
    ## produce dictionary for stemming:
    
    t1 <- Sys.time()
    
    ## tokenize on space and output as a list:
    doc.list <- strsplit(research_corpus, "[[:space:]]+")
    corpus_words <- unique(unlist(doc.list))
    corpus_words <- sort(corpus_words)
    
    ## stemming
    parsing2 <- function(x){
      word_form <- x
      withProgress(message = paste('Parsing ', word_form, ": ", round((match(word_form, corpus_words)-1)/length(corpus_words)*100, digits=2), '%'), value = 0, {
        batch_command <- paste0("echo ", word_form," | fst-infl ./LatMor/latmor.a")
        lemmata <- system(batch_command, intern = T)
        lemmata <- lemmata[2:length(lemmata)]
        if(length(grep("no result", lemmata)) != 0) {
          lemmata <- word_form
          incProgress(0.1, detail = "Return original form")
          return(lemmata)
        } 
        else {
          lemmata <- gsub("<.*$", "", lemmata)
          lemmata <- tolower(lemmata)
          lemmata <- unique(lemmata)
          incProgress(0.1, detail = paste(x, " is ", lemmata))
          return(lemmata)
        }
      })
    }
    
    stem_dictionary <- sapply(corpus_words, parsing2)
    file_name <- paste("./www/", "LatMorStemDic", ".rds", sep = "")
    saveRDS(stem_dictionary, file_name)
    
    ## Produce CSV Stem-Dictionary
    
    stem_dictionary_CSV <- vapply(stem_dictionary, 
                                  function(x){result <- paste(x, collapse = ";")
                                  return(result)
                                  },
                                  character(1))
    stem_dictionary_CSV <- data.frame(names(stem_dictionary_CSV), stem_dictionary_CSV)
    colnames(stem_dictionary_CSV) <- c("form", "lemmata")
    write.csv(stem_dictionary_CSV, file = sdfm)
    
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
    
    withProgress(message = 'Saving Binary...', value = 0, {
      file_name <- unlist(strsplit(as.character(corrected_corpus_df[1,1]), ":", fixed = TRUE))[4]
      foldername <- paste(unlist(strsplit(file_name, ".", fixed = TRUE)), sep = "", collapse = "/")
      foldername <- paste("./www/data", foldername, sep = "/")
      dir.create(foldername, recursive = TRUE)
      file_name <- paste(foldername, "/", file_name, "-LatMorParsed.rds", sep = "")
      saveRDS(corrected_corpus_df, file_name)
    })
    
    return(corrected_corpus_df)
  })
  
  morpheus <- reactive({
    
    req(input$stemdicfm)
    sdfm <- gsub("[^a-zA-Z0-9]", "", input$stemdicfm)
    sdfm <- paste0("./www/Dictionaries/", sdfm, ".csv")
    
    if (input$morphlang == "Latin") {
      morpheusURL <- "http://morph.perseids.org/analysis/word?lang=lat&engine=morpheuslat&word="
    } else if (input$morphlang == "Greek") {
      morpheusURL <- "http://morph.perseids.org/analysis/word?lang=grc&engine=morpheusgrc&word="
    }
  
    inFile <- input$morph_corpus
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      corpus <- readRDS(inFile)
    })
    
    research_corpus <- corpus$text
    research_corpus <- factor(research_corpus)
    identifier <- corpus$identifier
    identifier <- factor(identifier)
    
    ### pre-processing:
    
    research_corpus <- preprocess_corpus(research_corpus)
    
    
    ## produce dictionary for stemming:
    
    t1 <- Sys.time()
    
    ## tokenize on space and output as a list:
    doc.list <- strsplit(research_corpus, "[[:space:]]+")
    corpus_words <- unique(unlist(doc.list))
    corpus_words <- sort(corpus_words)
    
    ## stemming
    parsing <- function(x){
      word_form <- x
      withProgress(message = paste('Parsing ', word_form, ": ", round((match(word_form, corpus_words)-1)/length(corpus_words)*100, digits=2), '%'), value = 0, {
        URL <- URLencode(paste0(morpheusURL, word_form))
        morph <- fromJSON(URL)
        if (is.vector(morph$RDF$Annotation$Body$rest$entry$dict$hdwd)) {
          lemmata <- morph$RDF$Annotation$Body$rest$entry$dict$hdwd$`$`
        } else {
          lemmata <- morph$RDF$Annotation$Body$rest$entry$dict$hdwd[,2]
        }
        lemmata <- gsub("[0-9]", "", lemmata)
        lemmata <- tolower(lemmata)
        lemmata <- unique(lemmata)
        if (length(lemmata) == 0) {
          lemmata <- x
          incProgress(0.1, detail = "Return original form")
          return(lemmata)}
        else {
          incProgress(0.1, detail = paste(x, " is ", lemmata))
          return(lemmata)
        }
      })
    }
    
    stem_dictionary <- sapply(corpus_words, parsing)
    file_name <- paste("./www/", "MorpheusStemDic", ".rds", sep = "")
    saveRDS(stem_dictionary, file_name)
    ## Produce CSV Stem-Dictionary
    
    stem_dictionary_CSV <- vapply(stem_dictionary, 
                                  function(x){result <- paste(x, collapse = ";")
                                  return(result)
                                  },
                                  character(1))
    stem_dictionary_CSV <- data.frame(names(stem_dictionary_CSV), stem_dictionary_CSV)
    colnames(stem_dictionary_CSV) <- c("form", "lemmata")
    write.csv(stem_dictionary_CSV, file = sdfm)
    
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
    
    withProgress(message = 'Saving Binary...', value = 0, {
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
    selectInput("TModel", label = "Choose TM", choices = ServerTM, width = "100%")
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
  output$SWCorpusUI <- renderUI({
    ServerCorpora <- list.files(path = "./www/data", pattern = "*.rds", recursive = TRUE, full.names = TRUE)
    ServerCorpora <- ServerCorpora[which(grepl("Stopword", ServerCorpora) == FALSE)]
    ServerCorpora <- ServerCorpora[which(grepl("theta.rds", ServerCorpora, fixed = TRUE) == FALSE)]
    ServerCorpora <- ServerCorpora[which(grepl("phi.rds", ServerCorpora, fixed = TRUE) == FALSE)]
    ServerCorpora <- ServerCorpora[which(grepl("lda.rds", ServerCorpora, fixed = TRUE) == FALSE)]
    names(ServerCorpora) <- sapply(strsplit(ServerCorpora, "/"), function(x) {x[length(x)]})
    selectInput("sw_corpus", label = "Choose RDS file", choices = ServerCorpora)
  })
  
  output$stopwords <- renderDataTable({
    if (input$stopwordgo == 0)
      return()
    
    inFile <- input$sw_corpus
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      research_corpus <- readRDS(inFile)
    })
    identifier <- as.character(research_corpus$identifier)
    stopword_corpus <- as.character(research_corpus$text)
    stopword_corpus <- preprocess_corpus(stopword_corpus)
    
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
    
    withProgress(message = 'Saving Binary...', value = 0, {
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
    ServerTM <- ServerTM[which(grepl("theta.rds", ServerTM, fixed = TRUE) == FALSE)]
    ServerTM <- ServerTM[which(grepl("phi.rds", ServerTM, fixed = TRUE) == FALSE)]
    ServerTM <- ServerTM[which(grepl("lda.rds", ServerTM, fixed = TRUE) == FALSE)]
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
    
    req(input$CTMfile)
    CTMfilename <- gsub("[^a-zA-Z0-9]", "", input$CTMfile)
    TMvariables <- paste0("K", input$number_topics, "_alpha", gsub("[^a-zA-Z0-9]", "", input$alpha), "_eta", gsub("[^a-zA-Z0-9]", "", input$eta), "_I", input$iterations, "_S", input$seed)
    CTMfilename <- paste0("./www/CTM/", CTMfilename, "-", TMvariables, ".ctm")

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
    withProgress(message = 'Reading Stopwords', value = 0, {
      stop_words <- readRDS(inFile)
    })
    
    output_names <- as.character(research_corpus$identifier)
    research_corpus <- as.character(research_corpus$text)

    withProgress(message = "Normalise IDs...", {output_names <- gsub("^[[:space:]]+", "", output_names) # remove whitespace at beginning of documents
    output_names <- gsub("[[:space:]]+$", "", output_names) # remove whitespace at end of documents
    output_names <- gsub("[[:space:]]+", " ", output_names) # remove multiple whitespace
    output_names <- trimws(output_names)})
    
    withProgress(message = "Prepare topic modelling...", {
      research_corpus <- preprocess_corpus(research_corpus)
      # tokenize on space and output as a list:
      doc.list <- strsplit(research_corpus, "[[:space:]]+")
      all_words <- unlist(doc.list)
      # compute the table of terms:
      term.table <- table(all_words)
      term.table <- sort(term.table, decreasing = TRUE)
      occurences <- input$occurrence
      del <- names(term.table) %in% stop_words | term.table < occurences
      stopwords <- term.table[del]
      stopwords <- names(stopwords)
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
      })
    
    # Fit the model:
    withProgress(message = "Modelling (this may take a while)...", {
      seed <- input$seed
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
    })
    
    withProgress(message = "Structuring results...", {
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
    })
    
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
    ldafolder <- paste(folder, "_mod", sep = "")
    dir.create(ldafolder, recursive = TRUE)
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
      lda_folder <- paste(folder, "_mod/lda.rds", sep = "")
      rownames(phi) <- topicnames
      phi <- t(phi)
      saveRDS(phi, file = phi_folder)
      write.csv(phi, file = phi_CSVfolder)
      
      #theta-table
      theta <- cbind(output_names, as.character(research_corpus$text), theta) 
      colnames(theta) <- c("identifier", "text", topicnames)
      saveRDS(theta, file = theta_folder)
      saveRDS(fit, file = lda_folder)
      write.csv(theta, file = theta_CSVfolder)
      
      #ctm_file
      temp <- as.character(research_corpus$text)
      sink(CTMfilename)
      cat("#!TM_variables")
      cat("\n")
      cat(TMvariables)
      cat("\n")
      cat("Occurrence:", input$occurrence)
      cat("\n\n")
      cat("#!ctsdata")
      cat("\n")
      for(i in 1:length(output_names)){
        cat(paste0(output_names[i], "#", temp[i]))
        cat("\n")
      }
      cat("\n")
      cat("#!stop_words")
      cat("\n") 
      cat(stopwords)
      cat("\n\n")
      cat("#!word_index")
      cat("\n")
      cat(vocab)
      cat("\n\n")
      cat("#!assignments")
      cat("\n")
      for(i in 1:length(output_names)){
        cat(paste0(output_names[i], "#", paste(fit$assignments[[i]], collapse = ",")))
        cat("\n")
      }
      cat("\n")
      cat("#!theta")
      cat("\n")
      sink()
      write.table(theta, append = T, file = CTMfilename, row.names = F, quote = F, sep = "#")
      sink(CTMfilename, append = T)
      cat("\n")
      cat("#!phi")
      cat("\n")
      cat("token#")
      sink()
      write.table(phi, append = T, file = CTMfilename, quote = F, sep = "#")
      sink(CTMfilename, append = T)
      cat("\n")
      sink()
    })
    
    research_corpus
    
  })
  
##### 2.6. Output Tables #######
##### 2.6.1. Output Theta Table #######
  output$thetaUI <- renderUI({
    ServerTheta <- list.files(path = "./www", pattern = "theta.csv", recursive = TRUE, full.names = TRUE)
    names(ServerTheta) <- sapply(strsplit(ServerTheta, "/"), function(x) {x[length(x)-1]})
    selectInput("ThetaTable", label = "Choose TM", choices = ServerTheta, width = "100%")
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
    selectInput("PhiTable", label = "Choose TM", choices = ServerPhi, width = "100%")
    })

  output$phi <- renderDataTable({
    inFile <- input$PhiTable
    
    if (is.null(inFile))
      return(NULL)
    
    withProgress(message = 'Reading Texts', value = 0, {
      read.csv(inFile, header = TRUE, sep = ",", quote = "\"")
      })
    })
  
##### 2.7. Explore #####
##### 2.7.1. Topics over ID #####
  
  output$ExpLDAIDUI <- renderUI({
    ServerLDA <- list.files(path = "./www", pattern = "theta.rds", recursive = TRUE, full.names = TRUE)
    names(ServerLDA) <- sapply(strsplit(ServerLDA, "/"), function(x) {x[length(x)-1]})
    selectInput("LDAID", label = "Choose TM", choices = ServerLDA)
  })
  
  output$ExpLDAIDUI2 <- renderUI({
    inFile <- input$LDAID
    
    if (is.null(inFile))
      return(NULL)
    
    theta.frame <- readRDS(inFile)
    max_value <- ncol(theta.frame) - 2
    sliderInput("LDAIDTopic", label = "Topic", min = 1, max = max_value, value = 1)
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$ExpLDAID <- renderPlot({
    inFile <- input$LDAID
    inNumber <- input$LDAIDTopic
    
    if (is.null(inFile))
      return(NULL)
    
    if (is.null(inNumber))
      return(NULL)
    
    theta.frame <- readRDS(inFile)
    theta.frame <- as.data.frame(theta.frame)
    topic <- inNumber + 2
    theta.frame[,topic] <- as.numeric(levels(theta.frame[,topic]))[theta.frame[,topic]]
    theta.frame <- theta.frame
    ggplot(data = theta.frame, aes(c(1:length(identifier)), theta.frame[,topic])) + 
    labs(title = "Zoom", x = "", y = "") + 
    geom_col()
  })
  
  output$ExpLDAID2 <- renderPlot({
    inFile <- input$LDAID
    inNumber <- input$LDAIDTopic
    
    if (is.null(inFile))
      return(NULL)
    
    if (is.null(inNumber))
      return(NULL)
    
    theta.frame <- readRDS(inFile)
    theta.frame <- as.data.frame(theta.frame)
    topic <- inNumber + 2
    theta.frame <- theta.frame
    theta.frame[,topic] <- as.numeric(levels(theta.frame[,topic]))[theta.frame[,topic]]
    ggplot(data = theta.frame, aes(c(1:length(identifier)), theta.frame[,topic])) + 
      labs(title = paste("Topic:", names(theta.frame)[topic]), x = "Section", y = "Topic Value") + 
      geom_col() + coord_cartesian(xlim = ranges$x)
  })
  
  observe({
    brush <- input$ExpLDAID_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

##### 2.7.2. Topic vs Topic #####
  
  output$ExpTVTUI <- renderUI({
    ServerLDA <- list.files(path = "./www", pattern = "theta.rds", recursive = TRUE, full.names = TRUE)
    names(ServerLDA) <- sapply(strsplit(ServerLDA, "/"), function(x) {x[length(x)-1]})
    selectInput("TVT", label = "Choose TM", choices = ServerLDA)
  })
  
  output$ExpTVTUI2 <- renderUI({
    inFile <- input$TVT
    
    if (is.null(inFile))
      return(NULL)
    
    theta.frame <- readRDS(inFile)
    max_value <- ncol(theta.frame) - 2
    sliderInput("TVTTopic", label = "Sort by Topic", min = 1, max = max_value, value = 1)
  })
  
  output$ExpTVTUI3 <- renderUI({
    inFile <- input$TVT
    
    if (is.null(inFile))
      return(NULL)
    
    theta.frame <- readRDS(inFile)
    max_value <- ncol(theta.frame) - 2
    sliderInput("TVTTopic2", label = "Compare Topic", min = 1, max = max_value, value = 1)
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$ExpTVT <- renderPlot({
    inFile <- input$TVT
    inNumber <- input$TVTTopic
    
    if (is.null(inFile))
      return(NULL)
    
    if (is.null(inNumber))
      return(NULL)
    
    theta.frame <- readRDS(inFile)
    theta.frame <- as.data.frame(theta.frame)
    topic <- inNumber + 2
    theta.frame[,topic] <- as.numeric(levels(theta.frame[,topic]))[theta.frame[,topic]]
    theta.frame <- theta.frame[with(theta.frame, order(-theta.frame[,topic])), ]
    ggplot(data = theta.frame, aes(c(1:length(identifier)), theta.frame[,topic])) + 
      labs(title = paste("Topic:", names(theta.frame)[topic]), x = "Section", y = "Topic Value") + 
      geom_col() + coord_cartesian(xlim = ranges$x)
  })
  
  output$ExpTVT2 <- renderPlot({
    inFile <- input$TVT
    inNumber <- input$TVTTopic
    inNumber2 <- input$TVTTopic2
    
    if (is.null(inFile))
      return(NULL)
    
    if (is.null(inNumber))
      return(NULL)
    
    if (is.null(inNumber2))
      return(NULL)
    
    if (inNumber2 == inNumber)
      return(NULL)
    
    theta.frame <- readRDS(inFile)
    theta.frame <- as.data.frame(theta.frame)
    topic <- inNumber + 2
    topic2 <- inNumber2 + 2
    theta.frame[,topic] <- as.numeric(levels(theta.frame[,topic]))[theta.frame[,topic]]
    theta.frame <- theta.frame[with(theta.frame, order(-theta.frame[,topic])), ]
    theta.frame[,topic2] <- as.numeric(levels(theta.frame[,topic2]))[theta.frame[,topic2]]
    ggplot(data = theta.frame, aes(c(1:length(identifier)), theta.frame[,topic2])) + 
      labs(title = paste("Topic:", names(theta.frame)[topic2]), x = "Section", y = "Topic Value") + 
      geom_col() + coord_cartesian(xlim = ranges$x)
  })
  
  observe({
    brush <- input$ExpTVT_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
##### 2.8. Downloads #####
##### 2.8.1. Corpus #####
  output$dlcorpusUI <- renderUI({
    ServerCorpus <- list.files(path = "./www", pattern = "*.rds", recursive = TRUE, full.names = TRUE)
    ServerCorpus <- ServerCorpus[which(grepl("Stopword", ServerCorpus) == FALSE)]
    ServerCorpus <- ServerCorpus[which(grepl("theta.rds", ServerCorpus, fixed = TRUE) == FALSE)]
    ServerCorpus <- ServerCorpus[which(grepl("phi.rds", ServerCorpus, fixed = TRUE) == FALSE)]
    ServerCorpus <- ServerCorpus[which(grepl("lda.rds", ServerCorpus, fixed = TRUE) == FALSE)]
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
    identifier <- as.character(research_corpus$identifier)
    passage <- as.character(research_corpus$text)
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
    identifier <- as.character(research_corpus$identifier)
    passage <- as.character(research_corpus$text)
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

##### 2.8.2. Phi-Tables #####
  
  output$dlphiUI <- renderUI({
    ServerPhi <- list.files(path = "./www", pattern = "phi.csv", recursive = TRUE, full.names = TRUE)
    names(ServerPhi) <- sapply(strsplit(ServerPhi, "/"), function(x) {x[length(x)-1]})
    selectInput("ThetaPhiDL", label = "Choose TM", choices = ServerPhi)
  })
  
  output$prevphi <- renderDataTable({
    inFile <- input$ThetaPhiDL
    
    if (is.null(inFile))
      return(NULL)
    
    withProgress(message = 'Reading Texts', value = 0, {
      read.csv(inFile, header = TRUE, sep = ",", quote = "\"")
    })
  })
  
  output$downloadphi <- downloadHandler(
    filename = function() {
      paste("test", "csv", sep = ".")
    },
    content = function(file) {
      inFile <- input$ThetaPhiDL
      
      if (is.null(inFile))
        return(NULL)
      downloadphi <- withProgress(message = 'Reading Texts', value = 0, {
        read.csv(inFile, header = TRUE, sep = ",", quote = "\"")
      })
      write.table(downloadphi, file, quote = TRUE, sep = ",", row.names = FALSE)
    }
  )

##### 2.8.3. Theta-Tables #####
  
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
  
##### 2.8.4. LDAvis Zip-file #####
  
}

##### 3. Start It Up #######

shinyApp(ui = ui, server = server)