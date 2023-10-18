library(shiny)
options(shiny.maxRequestSize = 9 * 1024^2)
source("setup.R")  # Assuming "setup.R" contains the necessary functions


ui <- fluidPage(
  titlePanel("Convert CSV files to Qualtrics-compatible TXT format"),
  sidebarLayout(
    sidebarPanel(
      fileInput('qna', 'Choose Q&A file to upload', accept = c('.csv')),
      tags$hr(),
      p('Make sure the .csv file uploaded has the same column names as the sample:',
        a(href = '/Users/yasminyu/Library/CloudStorage/OneDrive-HKUST/000_GitHub/000_SOP/Qualtrics/data/23Fall_qna.csv', 'Sample_qna')
      ),
      fileInput('blocks', 'Choose Blocks file to upload', accept = c('.csv')),
      tags$hr(),
      p('Make sure the .csv file uploaded has the same column names as the sample:',
        a(href = '/Users/yasminyu/Library/CloudStorage/OneDrive-HKUST/000_GitHub/000_SOP/Qualtrics/data/23Fall_blocks.csv', 'Sample_blocks')
      )
    ),
    mainPanel(
      # textOutput('text'),
      downloadButton('downloadData', 'Download')
    )
  )
)


server <- function(input, output, session) {
  converted_text <- reactive({

    qna <- read.csv(input$qna$datapath) %>% t()
    blocks <- read.csv(input$blocks$datapath)
    
    if (!is.null(qna) && !is.null(blocks)) {
      complete_survey(qna, blocks)
    }
    else {
      print("Missing")
    }
  })
  
  # output$text <- renderText({
  #   converted_text()
  # })
  
  output$downloadData <- downloadHandler(
    filename = function() { "converted_text.txt" },
    content = function(file) {
      writeLines(converted_text(), file)
    }
  )
}

shinyApp(ui, server)