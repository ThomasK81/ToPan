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
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    print(input$bins)
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$catalogue <- renderDataTable({
    read.csv("./www/corpus.csv", header = TRUE, sep = ",", quote = "\"")
  })
  }

shinyApp(ui = ui, server = server)