library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme=shinytheme("cerulean"),

    titlePanel("Word prediction App"),

    sidebarLayout(
        sidebarPanel(
            textInput("text", label = ('Enter some text here'), value = ''),
            tableOutput('table'),
            sliderInput('sliderN', 'Maximum number of word branches on graph',
                        min = 1,  max = 15,  value = 5),
            sliderInput('sliderLength', 'Maximum length of word chain on graph',
                        min = 4,  max = 20,  value = 10)
        ),

        mainPanel(
            h4("SBO algorithm graph:"),
            plotOutput("wordChain", width='100%', height='700px')
        )
    )
))
