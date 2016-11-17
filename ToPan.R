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

##### 0.4. Preprocessing of CTS API inventory #######

urns <- FetchCTSRep(CTS.Rep)
metadata <- data.frame(urns)

##### 1. User Interface #######

ui <- navbarPage(theme = "bootstrap.min.css", div(img(src = "melete.png", height = "25"), "ToPān v.0.1"), windowTitle = "ToPān v.0.1",
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
                                                   accept=c('text/csv', 
                                                            'text/comma-separated-values,text/plain', 
                                                            '.82XF', '.82xf')),
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
                              radioButtons("morph_method", label = "Method", choices = c("Morpheus API", "Local StemDictionary")),
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
                              fileInput('tm_corpus', 'TM Corpus', accept=c('.rds')),
                              fileInput('stopwordlist', 'SW List', accept=c('.rds')),
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
                          # Sidebar with a slider input for the number of bins
                          mainPanel(htmlOutput("topicmodels"))
                 ),
                 ##### 1.6. Topic Modelling Tables #######
                 
                 navbarMenu("LDA Tables", 
                            tabPanel("DocumentTopic (θ)", mainPanel(dataTableOutput("theta"))),
                            tabPanel("TermTopic (φ)", mainPanel(dataTableOutput("phi")))
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
                                         selectInput("download_corpus", label = "Corpus", choices = dir("./www/")[grep(".rds", dir("./www/"))]),
                                         actionButton("CorpusDownloadGo", "Download")
                                       ),
                                       mainPanel(dataTableOutput("download_corpus"))))
                 )
)

##### 2. Server #######

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=30*1024^2)
  
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
  
  }

##### 3. Start It Up #######

shinyApp(ui = ui, server = server)