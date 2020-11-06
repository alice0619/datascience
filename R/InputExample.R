library(shiny)

ui <- fluidPage(

    titlePanel("INPUTS"),

    fluidRow(
        column(4, 
               h3("checkboxInput"), # title
               checkboxInput(inputId = "checkbox", 
                             label = "Choice A", # label of check box
                             value = TRUE)
               ), 
        column(4, 
               checkboxGroupInput(inputId = "checkboxGroupInput", 
                                  label = h3("checkboxGroupInput"), # title
                                  choices = list("Choice 1" = 1, 
                                                 "Choice 2" = 2, 
                                                 "Choice 3" = 3), 
                                  selected = 1, 
                                  inline = TRUE)
               ), 
        column(4, 
               dateInput(inputId = "date", 
                         label = h3("dateInput"), 
                         value = "2016-01-01", 
                         startview = "year")
               ), 
    ), 
    
    fluidRow(
        column(4, 
               dateRangeInput("dateRangeInput", 
                              h3("dateRangeInput"))
               ), 
        column(4, 
               textInput("text", h3("textInput"), value = "Enter text...")
               ), 
        column(4, 
               numericInput("num", h3("numericInput"), value = 1, step = 3)
               ), 
    ), 
    
    fluidRow(
        column(4, 
               selectInput("select", h3("selectInput"), 
                           choices = list("Choice 1" = 1, 
                                          "Choice 2" = 2, 
                                          "Choice 3" = 3, 
                                          "Choice 4" = 4), 
                           selected = 1)
               ),
        column(4, 
               sliderInput("sliderInput1", h3("sliderInput1"), 
                           min = 0, max = 100, value = 50), 
               sliderInput("sliderInput2", h3("sliderInput2"), 
                           min = 0, max = 100, value = c(25, 75), # range of input
                           step = 3, 
                           animate = TRUE)
               )
    )
)

server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)

