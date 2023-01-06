
library(data.table)
library(dplyr)
library(here)
library(shiny)
library(sensorstrings)
library(stringr)
library(qaqcmar)
library(tidyr)
library(purrr)

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

            uiOutput("sensor_boxes"),
            uiOutput("dynamic_ui")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)


server <- function(input, output) {

  output$sensor_boxes <- renderUI({

    if (input$qc_test == "grossrange") {
      sensors <- dat %>%
        ss_pivot_longer() %>%
        filter(variable == input$variable) %>%
        distinct(sensor_type) %>%
        arrange()

      checkboxGroupInput(
        "sensors", "Sensor(s)",
        choices = sensors$sensor_type,
        selected = sensors$sensor_type
      )
    }
  })

  output$dynamic_ui <- renderUI({
    # req(mydata3)

    # something going wrong with filtering - might need to change this
    thresh_display <- thresholds %>%
      filter(
        qc_test == input$qc_test,
        variable == input$variable,
        sensor_type %in% input$sensors
      )

    ui_elems <- list(NULL)
    for (i in seq_along(1:nrow(thresh_display))) {

      ui_elems[[i]] <- numericInput(
        thresh_display$threshold[i],
        thresh_display$threshold[i],
        value = thresh_display$value[i]
      )
    }

    return(tagList(ui_elems))

    # selectInput("check", label = "check", choices = thresh_display$value)
  })

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
