library(shiny)
library(dplyr)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("Investing Scenerios"),
    
    # Application title
    titlePanel("Title"),
    
    # Sidebar with a slider input
    fluidRow(
        column(3,
               numericInput("init",
                           "Initial amount:",
                           min = 0,
                           value = 1000),
               sliderInput("yrs",
                           "Number of years:",
                           min = 0,
                           max = 50,
                           value = 10,
                           step = 1)
        ),
        column(3, 
               numericInput("annual",
                           "Periodic contribution:",
                           min = 0,
                           value = 360),
               selectInput("type", 
                           "Contribution as the end of each:", 
                           choices = c("year" = 0, 
                             "month" = 1),
                           selected = "year")
        ),
        column(3, 
               sliderInput("return",
                           "Avg annual return:",
                           min = 0,
                           max = 20,
                           value = 10,
                           step = 0.1), 
               sliderInput("volatility",
                           "Avg annual volatility:",
                           min = 1,
                           max = 20,
                           value = 18,
                           step = 0.1)
        ), 
        column(3, 
               numericInput("repetitions",
                           label = "Number of simulations:",
                           min = 1,
                           max = 5000,
                           value = 50), 
               numericInput("seed",
                            "Random seed:",
                            value = 12345))
    ),
    
    # timeline plot
    h4("Timelines"),
    plotOutput("timeline"),
    
    # table of returns
    h4("Balances"),
    verbatimTextOutput("table")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$timeline <- renderPlot({
        
        # Defining functions:
        # 1) Future Value Function
        #' @title future_value()
        #' @description compute future value of an investment
        #' @param amount, rate, years
        #' @return PV(1+r)^t
        future_value <- function(amount, rate, type) {
            if (type == 1) {
                FV <- amount*(1+rate/12)^(12)
            } else {
                FV <- amount*(1+rate)
            }
            return(FV)
        }
        
        # 2) Creating an empty dataframe for efficiency
        #' @title createEmptyDf()
        #' @description create an empty dataframe
        #' @param nrow, ncol, colnames
        #' @return nrow x ncol dataframe
        createEmptyDf <- function(nrow, ncol, colnames) {
            data.frame(matrix(vector(), nrow, ncol, dimnames = list(c(), colnames)))
        }
        
        # data frame set-up definitions
        contribution <- input$annual
        period <- seq(from = 1, to = input$yrs)
        rep <- seq(from = 1, to = input$repetitions)
        r <- input$return / 100
        volat <- input$volatility / 100
        
        # defining data frames, for loop to input data into df
        columns <- if (input$type == 0) { 
            c("Year", "Simulation", "Value")
        } else {
                c("Month", "Simulation", "Value")
            }
        df <- createEmptyDf(input$yrs*input$repetitions, 3, columns)
        
        
        set.seed(input$seed)
        count <- 1
        for (i in 1:length(rep)) { 
            amount <- input$init
            sims <- paste("Simulation", i, sep = "")
            df[count, 1] <- 0
            df[count, 2] <- sims
            df[count, 3] <- amount
            for (t in 1:length(period)) {
                rate <- rnorm(1, mean = r, sd = volat)
                df[count+t, 1] <- t
                df[count+t, 2] <- sims 
                df[count+t, 3] <- future_value(amount, rate, input$type) + contribution
                amount <- df[count+t, 3]
            }
            count <- count + input$yrs + 1
        }
        
        df[,1] <- as.integer(df[,1])
        
        if (input$type == 0) {
            g <- ggplot(data = df) + 
                geom_line(aes(x = Year, y = Value, group = Simulation), alpha = 0.1) + 
                stat_summary(aes(x = Year, y = Value, colour = "Mean"), fun.y = "mean", geom = "line") +
                stat_summary(aes(x = Year, y = Value, colour = "25%"), geom = "line", fun.y = "quantile", fun.args = list(probs = .25), linetype = 2) + 
                stat_summary(aes(x = Year, y = Value, colour = "Median"), geom = "line", fun.y = "quantile", fun.args = list(probs = .50), linetype = 2) +
                stat_summary(aes(x = Year, y = Value, colour = "75%"), geom = "line", fun.y = "quantile", fun.args = list(probs = .75), linetype = 2) +
                ggtitle("Simulations of investing balance") +
                xlab("Years") + ylab("Values($)") +
                theme_light() + 
                scale_colour_manual(name="Line Color", 
                    values=c("25%"="blue", "Median"="darkgreen", "Mean"="red", "75%"="orange"))
        } else {
            g <- ggplot(data = df) + 
                geom_line(aes(x = Month, y = Value, group = Simulation), alpha = 0.1) + 
                stat_summary(aes(x = Month, y = Value, colour = "Mean"), fun.y = "mean", geom = "line") +
                stat_summary(aes(x = Month, y = Value, colour = "25%"), geom = "line", fun.y = "quantile", fun.args = list(probs = .25), linetype = 2) + 
                stat_summary(aes(x = Month, y = Value, colour = "Median"), geom = "line", fun.y = "quantile", fun.args = list(probs = .50), linetype = 2) +
                stat_summary(aes(x = Month, y = Value, colour = "75%"), geom = "line", fun.y = "quantile", fun.args = list(probs = .75), linetype = 2) +
                ggtitle("Simulations of investing balance") +
                xlab("Months") + ylab("Values($)") +
                theme_light() + 
                scale_colour_manual(name="Line Color", 
                                    values=c("25%"="blue", "Median"="darkgreen", "Mean"="red", "75%"="orange"))
        }
        print(g)
    })

    
    
    # Stats Table
    output$table <- renderPrint({
        # Defining functions:
        # 1) Future Value Function
        #' @title future_value()
        #' @description compute future value of an investment
        #' @param amount, rate, years
        #' @return PV(1+r)^t
        future_value <- function(amount, rate, type) {
            if (type == 1) {
                FV <- amount*(1+rate/12)^(12)
            } else {
                FV <- amount*(1+rate)
            }
            return(FV)
        }
        
        # 2) Creating an empty dataframe for efficiency
        #' @title createEmptyDf()
        #' @description create an empty dataframe
        #' @param nrow, ncol, colnames
        #' @return nrow x ncol dataframe
        createEmptyDf <- function(nrow, ncol, colnames) {
            data.frame(matrix(vector(), nrow, ncol, dimnames = list(c(), colnames)))
        }
        
        # data frame set-up definitions
        contribution <- input$annual
        period <- seq(from = 1, to = input$yrs)
        rep <- seq(from = 1, to = input$repetitions)
        r <- input$return / 100
        volat <- input$volatility / 100
        
        # defining data frames, for loop to input data into df
        columns <- if (input$type == 0) {
            c("Year")
        } else {
            c("Month")
        }
        df <- createEmptyDf(input$yrs+1, 1, columns)
        df[,1] <- seq(from = 0, to = input$yrs)
        
        set.seed(input$seed)
        for (i in 1:length(rep)) {
            amount <- input$init
            sims <- rep(0, input$yrs)
            sims[1] <- input$init
            for (t in 1:length(period)) {
                rate <- rnorm(1, mean = r, sd = volat)
                sims[t+1] <- future_value(amount, rate, input$type) + contribution
                amount <- sims[t+1]
            }
            column_name <- paste("Simulation", i, sep = "")
            df <- df %>% mutate(!!column_name := sims)
        }
        end_amount <- unlist(df[input$yrs+1, -1])
        print(df)
        
        cat("\nQuantile of end amount\n")
        quantiles <- quantile(end_amount, probs = seq(0, 1, by = 0.1))
        pd <- data.frame(quantiles)
        print(pd)
        
        cat("\nAverage end amount: \n")
        print(mean(end_amount))
        
        cat("\nMedian end amount: \n")
        print(median(end_amount))
        
        cat("\nStandard deviation of end amount: \n")
        print(sd(end_amount))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
