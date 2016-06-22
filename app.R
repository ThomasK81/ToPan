library(shiny)
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

ui <- navbarPage(theme = "bootstrap.min.css", div(img(src = "melete.png", height = "25"), "ToPān v.0.1"), windowTitle = "ToPān v.0.1",
                 tabPanel("Home", mainPanel(includeMarkdown("home.md"))),
                 
                 navbarMenu("Data Input",
                            tabPanel("CTS API", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("cts_urn", label = "CTS URN", choices = urns),
                                         checkboxInput("morphology", "Morphological Normalisation (currently only for Latin and Greek)", TRUE),
                                         submitButton("Submit")
                                         ),
                                       mainPanel(
                                         dataTableOutput("catalogue")
                                       )
                                       )),
                            tabPanel("Local CAPITainS", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         textInput("api_url", label = "API URL", value = ""),
                                         selectInput("cts_urn", label = "CTS URN", choices = urns),
                                         checkboxInput("morphology", "Morphological Normalisation (currently only for Latin and Greek)", TRUE),
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
                                         checkboxInput("morphology", "Morphological Normalisation (currently only for Latin and Greek)", TRUE),
                                         submitButton("Submit")
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
                              textInput("add_stopwords", label = "Additional Stopwords", value = "christchurch earthquake"),
                              sliderInput("number_topics", label = "Number of Topics", min = 2, max = 25, value = 15),
                              sliderInput("number_terms", label = "Number of Terms Shown", min = 15, max = 50, value = 25),
                              sliderInput("iterations", label = "Iterations", min = 500, max = 5000, value = 500),
                              submitButton("Submit")
                            ),
                            mainPanel())),
                 
                 tabPanel("LDAvis", 
                          # Sidebar with a slider input for the number of bins
                          mainPanel(htmlOutput("topicmodels"))
                          ),
                 
                 navbarMenu("LDA Tables", 
                            tabPanel("DocumentTopic (θ)", mainPanel(includeMarkdown("home.md"))),
                            tabPanel("TermTopic (φ)", mainPanel(includeMarkdown("home.md")))
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
      result <- xmlParse(xdata)
      result <- as.data.frame(t(xpathSApply(result, "//*/tei:body", XMLminer)), stringsAsFactors = FALSE)[[1]]
      result <- gsub("\n", "", result, fixed = FALSE)
      result <- gsub("\t", "", result, fixed = FALSE)
      result}
    
    withProgress(message = 'Fetch Texts', value = 0, {
    urls <- paste(baseURL, reffs, sep = "")
    batch_urls <- split(urls, ceiling(seq_along(urls)/100))
    output_list <- list()
    for (i in 1:length(batch_urls)) {
      counter <- 0
      temp_vector <- getURIAsynchronous(batch_urls[[i]])
      while(length(which(temp_vector == "")) > 0) {
        print(paste("Fetch rest of batch-request ", as.character(i), "/", as.character(length(batch_urls)), sep="")); 
        temp_vector[which(temp_vector == "")] <- getURIAsynchronous(batch_urls[[i]][which(temp_vector == "")]);
        counter <- counter+1; if (counter == 3) {temp_vector[which(temp_vector == "")] <- "NotRetrieved"}}
      temp_vector[which(temp_vector != "NotRetrieved")] <- unlist(lapply(temp_vector[which(temp_vector != "NotRetrieved")], XMLpassage1))
      output_list[[i]] <- temp_vector
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
  }

shinyApp(ui = ui, server = server)