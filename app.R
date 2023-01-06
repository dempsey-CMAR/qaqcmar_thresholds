
library(data.table)
library(dplyr)
library(here)
library(shiny)
library(sensorstrings)
library(stringr)
library(qaqcmar)
library(tidyr)

deployment <- "Borgles Island 2019-05-30"

dat <- fread(paste0(here("data"), "/", deployment, ".csv"))

vars <- dat %>%
  ss_pivot_longer() %>%
  distinct(variable) %>%
  arrange(variable)

qc_tests <- c("climatology", "grossrange", "spike")

#sensors <- sort(unique(dat$sensor_type))

thresholds <- threshold_tables$climatology_table %>%
  pivot_longer(cols = c("season_min", "season_max"), names_to = "threshold") %>%
  mutate(threshold = str_remove(threshold, pattern = "season_"),
         threshold = paste(season, threshold, sep = "_")) %>%
  mutate(qc_test = "climatology") %>%
  select(-season) %>%
  bind_rows(
    threshold_tables$grossrange_table %>%
      pivot_longer(cols = sensor_min:user_max, names_to = "threshold") %>%
      mutate(qc_test = "grossrange")
  ) %>% bind_rows(
    threshold_tables$spike_table %>%
      pivot_longer(cols = c("spike_high", "spike_low"), names_to = "threshold") %>%
      mutate(qc_test = "spike")
  )




#dat_qc <- qc_test_all(dat)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Quality Control"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("variable", "Variable", choices = vars$variable),

            selectInput("qc_test", "QC Test", choices = qc_tests),

            conditionalPanel(
              condition = "input.qc_test == 'grossrange'",
              checkboxGroupInput("sensors", "Sensor(s)", choices = sensors)
            ),

            uiOutput("sensor_boxes"),
            uiOutput("dynamic_ui")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$sensor_boxes <- renderUI({

    sensors <- dat %>%
      ss_pivot_longer() %>%
      filter(variable == input$variable) %>%
      distinct(sensor_type) %>%
      arrange()

    checkboxGroupInput("sensors", "Sensor(s)", choices = sensors$sensor_type)
  })

  output$dynamic_ui <- renderUI({
   # req(mydata3)

    thresh_display <- thresholds %>%
      filter(
        qc_test == input$qc_test,
        variable == input$variable,
        sensor_type %in% input$sensors
      )

    selectInput("check", label = "check", choices = thresh_display$value)
  })

    # ui_elems <- purrr::map(my_cols, ~{
    #   if (class(mydata3[[.x]]) %in% c("factor", "character")){
    #     output <- textInput(
    #       inputId = paste("input", .x, sep = "_"),
    #       label = .x,
    #       value = NULL
    #     )
    #   } else if (class(mydata3[[.x]]) %in% c("integer", "numeric")){
    #     output <- numericInput(
    #       inputId = paste("input", .x, sep = "_"),
    #       label = .x,
    #       value = NULL
    #     )
    #   } else output <- NULL
    #
    #   return(output)
    # })
    #
    # return(tagList(ui_elems))
 # })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x))

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application
shinyApp(ui = ui, server = server)
