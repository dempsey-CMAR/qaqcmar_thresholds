# https://community.rstudio.com/t/creating-multiple-numeric-input-according-to-the-variables-of-an-uploaded-dataset/12293

library(data.table)
library(dplyr)
library(DT)
library(here)
library(purrr)
library(shiny)
library(sensorstrings)
library(stringr)
library(qaqcmar)
library(tidyr)


# perhaps move this to script in /R folder --------------------------------

deployment <- "Borgles Island 2019-05-30"

dat <- fread(paste0(here("data"), "/", deployment, ".csv"))

vars <- dat %>%
  ss_pivot_longer() %>%
  distinct(variable) %>%
  arrange(variable)

qc_tests <- c("climatology", "grossrange", "spike")


# gr_table <- threshold_tables %>%
#   filter(qc_test == "grossrange") %>%
#   select(-qc_test, -season) %>%
#   mutate(threshold = str_remove(threshold, "am_|hobo_|vr2ar_")) %>%
#   pivot_wider(names_from = "threshold", values_from = "threshold_value")
#
# cl_table <- threshold_tables %>%
#   filter(qc_test == "climatology") %>%
#   select(-qc_test, -sensor_type) %>%
#   mutate(
#     threshold = str_replace(threshold, "winter|spring|summer|fall", "season")
#   ) %>%
#   pivot_wider(names_from = "threshold", values_from = "threshold_value")
#
# dat_qc <-  dat %>%
#   qc_test_all(
#     qc_tests = "grossrange",
#     climatology_table = cl_table,
#     grossrange_table = gr_table
#     # seasons_table = qaqcmar::threshold_tables$seasons_table
#   )
#
#


# user interface ----------------------------------------------------------
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

      uiOutput("sensor_ui"),
      uiOutput("thresh_ui")
    ),

    # Show a plot of the generated distribution
    mainPanel(

      tabsetPanel(
        type = "tabs",
        tabPanel("Threshold Table", DTOutput("qc_thresh_table")),
        tabPanel("QC Data", DTOutput("qc_dat"))
      )
    ),

    # for reactive numeric inputs (for thresholds)
    fluid = TRUE
  )
)


# server -----------------------------------------------------------------
server <- function(input, output) {

  # reactive values
  thresh_default <- reactive({
    # filter for thresholds of interest
    thresh_default <- threshold_tables %>%
      filter(qc_test == input$qc_test, variable == input$variable)

    if (input$qc_test == "grossrange") {
      thresh_default <- filter(thresh_default, sensor_type %in% input$sensors)
    }

    return(thresh_default)
  })

  # reactive user interface
  output$sensor_ui <- renderUI({

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

  output$thresh_ui <- renderUI({

    validate(
      need(!(input$variable == "sensor_depth_measured_m" & input$qc_test == "climatology"),
      "No thresholds available for selected Variable and QC Test")
    )

    # make numeric inputs for thresholds of interest
    ui_elems <- list(NULL)
    for (i in 1:nrow(thresh_default())) {

      ui_elems[[i]] <- numericInput(
        inputId = thresh_default()$threshold[i],
        label = thresh_default()$threshold[i],
        value = thresh_default()$threshold_value[i],
        width = '50%'
      )
    }

    # make a grid layout for the threshold input boxes
    fluidPage(

      ui_grid <- list(NULL),

      fluidRow(

        # might be able to change the order by filling in the left col
        # and then the right by making the indices with seq
        for(j in seq(1, length(ui_elems), 2)) {

          ui_grid[[j]] <- column(6, ui_elems[[j]], ui_elems[[j+1]])

        }
      ),

      return(tagList(ui_grid))
    )

    # uncomment this if decide to not use the grid layout
    # return(tagList(ui_elems))
  })

  # make a table of default and user-input threshold values to use for qc flag
  qc_thresh <- eventReactive(input$qc_plot, {

    qc_thresh <- thresh_default()

    for (k in 1:nrow(qc_thresh)) {

      qc_thresh$threshold_value[k] <- input[[qc_thresh$threshold[k]]]
    }

    # for joining with dat
    qc_thresh <- qc_thresh %>%
      mutate(
        threshold = str_replace(threshold, "winter|spring|summer|fall", "season"),
        threshold = str_remove(threshold, "am_|hobo_|vr2ar_")
      )


    return(qc_thresh)
  })

  output$qc_thresh_table <- ({
    renderDT({datatable(qc_thresh())})
  })


  observeEvent(input$qc_plot, {

    qc_thresh <- qc_thresh() #%>%
      # mutate(
      #   threshold = str_replace(threshold, "winter|spring|summer|fall", "season"),
      #   threshold = str_remove(threshold, "am_|hobo_|vr2ar_")
      # )

    qc_dat <- dat %>%
      ss_pivot_longer() %>%
      filter(variable == input$variable) %>%
      ss_pivot_wider() %>%
      qc_test_all(
        qc_tests = input$qc_test,
        climatology_table = qc_thresh,
        grossrange_table = qc_thresh,
        spike_table = qc_thresh
    )

    output$qc_dat <- ({
      renderDT({datatable(qc_dat)})
    })
  })


  # observeEvent(input$qc_plot, {
  #
  #   output$distPlot <-  renderPlot({
  #     # generate bins based on input$bins from ui.R
  #     x    <- faithful[, 2]
  #     bins <- seq(min(x), max(x), length.out = runif(30, 1, 30)[1])
  #
  #     # draw the histogram with the specified number of bins
  #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
  #          xlab = 'Waiting time to next eruption (in mins)',
  #          main = 'Histogram of waiting times')
  #   })
  # })

}

# Run the application
shinyApp(ui = ui, server = server)
