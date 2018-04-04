
library(shiny)
library(shinyAce)
library(DiagrammeR)
library(datamodelr)

get_graph <- function(x, rankdir = "RL", view_type = "all") {

  ret <- ""
  try({
    dm <- dm_read_yaml(text = x)
    graph <- dm_create_graph(dm, rankdir = rankdir, view_type = view_type)
    ret <- graph$dot_code
  })
  ret
}

# Set the default YAML to example.yml
f_name <- system.file("samples/example.yml", package = "datamodelr")
default_yaml <- paste0(readLines(f_name), collapse   = "\n")

ui = shinyUI(fluidPage(
  tags$head(tags$script(src = "message-handler.js")),
  fluidRow(
    column(2, HTML("<h2>data<b>model</b>r</h2>"))
  ),
  fluidRow(
    column(4,
           aceEditor("ace", mode = "yaml", value=default_yaml, height = 600, fontSize = 15)
    ),
    column(6,
           grVizOutput('diagram', height = 600)
    ),
    column(2,
           selectInput("view_type", label = "Show columns",  choices = list(
             "All columns" = "all",
             "Keys only" = "keys_only",
             "No columns" = "title_only"), selected = "RL"),
           selectInput("rankdir", label = "Graph direction",  choices = list(
             "Right-left" = "RL",
             "Bottom-top" = "BT",
             "Left-right" = "LR",
             "Top-bottom" = "TB"), selected = "RL"),
           hr(),
           downloadButton(outputId = 'downloadData', label = 'Download SVG'),
           downloadButton(outputId = 'downloadDataPng', label = 'Download PNG')
    )
  ),
  HTML('<footer>
    Powered by:
    <a href = "https://github.com/bergant/datamodelr">datamodelr</a> - Data model diagrams in R
    | <a href = "http://rich-iannone.github.io/DiagrammeR/">DiagrammeR</a> - Create graph diagrams and flowcharts using R
    | <a href = "http://shiny.rstudio.com/">shiny</a> - A web application framework for R
    | <a href = "http://trestletech.github.io/shinyAce/">shinyAce</a> - Integrating the <a href="http://ace.c9.io">Ace Editor</a> with Shiny
    </footer>'
  )
))

server = function(input, output, session){

  output$diagram <- renderGrViz({
    grViz(
      get_graph(input$ace, rankdir = input$rankdir, view_type = input$view_type)
    )

  })
  output$downloadData <- downloadHandler(
    filename = "export.svg",
    content = function(file) {
      dm <- dm_read_yaml(text = input$ace)
      graph <- dm_create_graph(dm, rankdir = input$rankdir, view_type = input$view_type)
      dm_export_graph(graph, file, file_type = "svg");
    },
    contentType = "image/svg+xml"
  )
  output$downloadDataPng <- downloadHandler(
    filename = "export.png",
    content = function(file) {
      dm <- dm_read_yaml(text = input$ace)
      graph <- dm_create_graph(dm, rankdir = input$rankdir, view_type = input$view_type)
      dm_export_graph(graph, file, file_type = "png");
    },
    contentType = "image/png"
  )
}


shinyApp(ui = ui, server = server)
