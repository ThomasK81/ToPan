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
                 tabPanel("Home", 
                          sidebarPanel(
                            sliderInput("bins",
                                        "Number of bins:",
                                        min = 1,
                                        max = 50,
                                        value = 30)
                          ),
                          mainPanel(
                   plotOutput("distPlot")
                 )
                          ),
                 navbarMenu("Data Input",
                            tabPanel("CTS API", 
                                     fluidPage(
                                       fluidRow(
                                         column(6, "Text Input & Processing",
                                           selectInput("cts_urn", label = "CTS URN", choices = urns),
                                           checkboxInput("morphology", "Morphological Normalisation (currently only for Latin and Greek)", TRUE),
                                           textInput("add_stopwords", label = "Additional Stopwords", value = "")
                                         ),
                                         column(6, "Topic Modelling",
                                                sliderInput("number_topics", label = "Number of Topics", min = 2, max = 25, value = 15),
                                                sliderInput("number_terms", label = "Number of Terms Shown", min = 15, max = 50, value = 25),
                                                sliderInput("iterations", label = "Iterations", min = 500, max = 5000, value = 500),
                                                submitButton("Submit")
                                         )
                                         ))
                            ),
                            tabPanel("Local CAPITainS", 
                                     fluidPage(
                                       sidebarLayout(
                                         sidebarPanel(
                                           textInput("api_url", label = "API URL", value = ""),
                                           selectInput("cts_urn", label = "CTS URN", choices = urns),
                                           checkboxInput("morphology", "Morphological Normalisation (currently only for Latin and Greek)", TRUE),
                                           textInput("add_stopwords", label = "Additional Stopwords", value = ""),
                                           sliderInput("number_topics", label = "Number of Topics", min = 2, max = 25, value = 15),
                                           sliderInput("number_terms", label = "Number of Terms Shown", min = 15, max = 50, value = 25),
                                           sliderInput("iterations", label = "Iterations", min = 500, max = 5000, value = 500),
                                           submitButton("Submit")
                                         ),
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("Search Results", tableOutput("catalogue")),
                                             tabPanel("Download Data", helpText(   
                                               a("Click Here to Download the Search Results",     href="/www/catalogue/catalogue.csv"),
                                               a("Click Here to Download the Search Results",     href="/www/catalogue/catalogue.csv")
                                             ))
                                           ))))
                            ),
                            tabPanel("DNZ API",
                                     fluidPage(
                                       sidebarLayout(
                                         sidebarPanel(
                                           textInput("search_text", label = "Search Text", value = "christchurch+earthquake"),
                                           textInput("api_key", label = "DNZ API Key", value = ""),
                                           radioButtons("collection", label = "Collection", 
                                                        choices = list("ATL Cartoon Descriptions" = 1, "Index NZ Abstracts" = 2, "no search" = 3
                                                        ), selected = 3),
                                           textInput("date", label = "Year", value = "2011"),
                                           textInput("add_stopwords", label = "Additional Stopwords", value = "christchurch earthquake"),
                                           sliderInput("number_topics", label = "Number of Topics", min = 2, max = 25, value = 15),
                                           sliderInput("number_terms", label = "Number of Terms Shown", min = 15, max = 50, value = 25),
                                           sliderInput("iterations", label = "Iterations", min = 500, max = 5000, value = 500),
                                           submitButton("Submit")
                                         ),
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("Search Results", tableOutput("catalogue")),
                                             tabPanel("Download Data", helpText(   
                                               a("Click Here to Download the Search Results",     href="/www/catalogue/catalogue.csv"),
                                               a("Click Here to Download the Search Results",     href="/www/catalogue/catalogue.csv")
                                             ))
                                           ))))
                            ),
                            tabPanel("CSV",
                                     fluidPage(
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
                                           textInput("add_stopwords", label = "Additional Stopwords", value = ""),
                                           sliderInput("number_topics", label = "Number of Topics", min = 2, max = 25, value = 15),
                                           sliderInput("number_terms", label = "Number of Terms Shown", min = 15, max = 50, value = 25),
                                           sliderInput("iterations", label = "Iterations", min = 500, max = 5000, value = 500),
                                           submitButton("Submit")
                                         ),
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("Search Results", tableOutput("catalogue")),
                                             tabPanel("Download Data", helpText(   
                                               a("Click Here to Download the Search Results",     href="/www/catalogue/catalogue.csv"),
                                               a("Click Here to Download the Search Results",     href="/www/catalogue/catalogue.csv")
                                             ))
                                           ))))
                            )
                 ),
                 tabPanel("Read",
                          mainPanel(
                            htmlOutput("catalogue")
                          )),
                 tabPanel("LDAvis", 
                          # Sidebar with a slider input for the number of bins
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("bins",
                                          "Number of bins:",
                                          min = 1,
                                          max = 50,
                                          value = 30)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                              plotOutput("distPlot")
                            )
                          )),
                 navbarMenu("About", 
                            tabPanel("About ToPān"),
                            tabPanel("About Me")
                 )
)

server <- function(input, output, session) {
  
  output$topicmodels <- renderUI({
    getPage<-function() {
      return(includeHTML("~/OneDrive/GithubProjects/TopicModellingR/DH2016/www/temp_vis/index.html"))
    }
    getPage()})
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    print(input$bins)
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })}

shinyApp(ui = ui, server = server)