# https://community.rstudio.com/t/creating-multiple-numeric-input-according-to-the-variables-of-an-uploaded-dataset/12293

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
      mutate(qc_test = "grossrange", threshold = paste0(sensor_type, "_", threshold))
  ) %>% bind_rows(
    threshold_tables$spike_table %>%
      pivot_longer(cols = c("spike_high", "spike_low"), names_to = "threshold") %>%
      mutate(qc_test = "spike")
  )


#dat_qc <- qc_test_all(dat)


# Define UI
ui <- fluidPage(

  # Application title
  titlePanel("Quality Control"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      style = "height: 90vh; overflow-y: auto;",

      actionButton(
        "qc_plot", "Apply Flags", icon("flag"),
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      br(),
      br(),

      selectInput("variable", "Variable", choices = vars$variable),

      selectInput("qc_test", "QC Test", choices = qc_tests),

      uiOutput("sensor_boxes"),
      uiOutput("dynamic_ui")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    ),

    fluid = TRUE
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

    # filter for thresholds of interest
    thresh_display <- thresholds %>%
      filter(qc_test == input$qc_test, variable == input$variable)

    if (input$qc_test == "grossrange") {
      thresh_display <- filter(thresh_display, sensor_type %in% input$sensors)
    }

    # make numeric inputs for thresholds of interest
    ui_elems <- list(NULL)
    for (i in 1:nrow(thresh_display)) {

      ui_elems[[i]] <- numericInput(
        inputId = thresh_display$threshold[i],
        label = thresh_display$threshold[i],
        value = thresh_display$value[i],
        width = '50%'
      )
    }

    # make a grid layout for the threshold input boxes
    fluidPage(

      ui_grid <- list(NULL),

      fluidRow(

        for(j in seq(1, length(ui_elems), 2)) {

          ui_grid[[j]] <- column(6, ui_elems[[j]], ui_elems[[j+1]])

        }
      ),

      return(tagList(ui_grid))
    )

    I# uncomment this if decide to not use the grid layout
    # return(tagList(ui_elems))
  })


  observeEvent(input$qc_plot, {

    output$distPlot <-  renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = runif(30, 1, 30)[1])

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white',
           xlab = 'Waiting time to next eruption (in mins)',
           main = 'Histogram of waiting times')
    })
  })

}

# Run the application
shinyApp(ui = ui, server = server)
