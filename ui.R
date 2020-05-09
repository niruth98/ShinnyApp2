shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  ),
  titlePanel("Genetic Data"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("checkGroup", label = h4("Models"),
                         choices = list("Random Forest" = 1, "SVM" = 2),selected = 1),
      sliderInput("Features", h4("Number of Features:"),
                  min = 10, max = 100,
                  value = 10, step = 1),
      numericInput('sim',h4('Number of Simulations'),25,min = 10,max = 75),
      actionButton(inputId = "plot",
                   label = h5("Run"))


    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel('Model/(s) Performance',
                 shiny::plotOutput(outputId = 'lineplot'),
                 shiny::plotOutput(outputId = 'combined')
        ),


        tabPanel('PCA',
                 shiny::plotOutput(outputId = 'pca')),
        tabPanel('Selected Genes',DT::dataTableOutput('ex'))

      )
    )
  )
))
