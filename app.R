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
library(plotly)


# perhaps move this to script in /R folder --------------------------------

deployment <- "Borgles Island 2019-05-30"

dat <- fread(paste0(here("data"), "/", deployment, ".csv"))

vars <- dat %>%
  ss_pivot_longer() %>%
  distinct(variable) %>%
  arrange(variable)

qc_tests <- c("climatology", "grossrange", "spike")


# user interface ----------------------------------------------------------
ui <- fluidPage(

  # Application title
  titlePanel("Quality Control"),

  # Reactive sidebar
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

    # for reactive numeric inputs (for thresholds)
    fluid = TRUE,

    # Main panels
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("QC Plot", plotlyOutput("qc_flags")),
        tabPanel("Threshold Table", DTOutput("qc_thresh_table")),
        tabPanel("QC Data", DTOutput("qc_dat"))
      )
    )
  )
)

# server -----------------------------------------------------------------
server <- function(input, output) {

  # default threshold values
  thresh_default <- reactive({
    # filter for thresholds of interest
    thresh_default <- threshold_tables %>%
      filter(qc_test == input$qc_test, variable == input$variable)

    if (input$qc_test == "grossrange") {
      thresh_default <- filter(thresh_default, sensor_type %in% input$sensors)
    }

    return(thresh_default)
  })

  # provide check boxes to select sensor(s) of interest, if threshold depends on sensor
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

  # provide numeric inputs for thresholds of interest
  output$thresh_ui <- renderUI({

    validate(
      need(!(input$variable == "sensor_depth_measured_m" &
               input$qc_test == "climatology"),
           "No thresholds available for selected Variable and QC Test")
    )

    # make numeric inputs
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

    # display so can verify that it updates with user inputs
    output$qc_thresh_table <- ({
      renderDT({datatable(qc_thresh)})
    })

    return(qc_thresh) # <- use this table for applying qc flags
  })

  # final qc table - display so can verify that it updates with user inputs
  # updates when "Apply Flags" button is clickec
  # output$qc_thresh_table <- ({
  #   renderDT({datatable(qc_thresh())})
  # })

  # apply flags to data
  observeEvent(input$qc_plot, {

    qc_thresh <- qc_thresh()

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

    output$qc_flags <- ({
      p <- qc_plot_flags(qc_dat, vars = input$variable, qc_tests = input$qc_test)
      renderPlotly(
          ggplotly(p[[input$variable]][[input$qc_test]])
      )
    })


  })




}

# Run the application
shinyApp(ui = ui, server = server)
