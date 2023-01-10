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

#sensors <- sort(unique(dat$sensor_type))

thresh_default_all <- threshold_tables$climatology_table %>%
  pivot_longer(cols = c("season_min", "season_max"), names_to = "threshold") %>%
  mutate(threshold = str_remove(threshold, pattern = "season_"),
         threshold = paste(season, threshold, sep = "_")) %>%
  mutate(qc_test = "climatology") %>%
  select(-season) %>%
  bind_rows(
    threshold_tables$grossrange_table %>%
      pivot_longer(cols = sensor_min:user_max, names_to = "threshold") %>%
      mutate(
        sensor_type = if_else(sensor_type == "vemco", "vr2ar", sensor_type),
        qc_test = "grossrange", threshold = paste0(sensor_type, "_", threshold),
        threshold = str_replace(threshold, "aquameasure", "am")
      )
  ) %>% bind_rows(
    threshold_tables$spike_table %>%
      pivot_longer(cols = c("spike_high", "spike_low"), names_to = "threshold") %>%
      mutate(qc_test = "spike")
  ) %>%
  mutate(sensor_type = if_else(sensor_type == "vemco", "vr2ar", sensor_type))


#dat_qc <- qc_test_all(dat)



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
      DTOutput("thresh_final")
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
    thresh_default <- thresh_default_all %>%
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
        value = thresh_default()$value[i],
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
  thresh_qc <- eventReactive(input$qc_plot, {

    thresh_inc <- thresh_default()$threshold # thresholds included

    thresh_input <- list(NULL)
    for (k in seq_along(thresh_inc)) {

      thresh_input[[k]] <- data.frame(
        threshold = thresh_inc[k],      # threshold name
        value = input[[thresh_inc[k]]]  # input value
      )
    }

    thresh_qc <- map_df(thresh_input, rbind)

    return(thresh_qc)
  })


  observeEvent(input$qc_plot, {

    output$thresh_final <- renderDT({
      datatable(thresh_qc())
    })
  })

  # observeEvent(input$qc_plot, {
  #
  #  x <- thresh %>%
  #     mutate(threshold = str_remove(threshold, "am_|hobo_|vr2ar_")) %>%
  #     pivot_wider(names_from = "threshold", values_from = "value")
  #
  #  dat_qc <- dat %>%
  #    qc_test_all(
  #      qc_tests = input$qc_test,
  #
  #
  #
  #
  #      )
  #
  #
  #
  #
  #
  # })


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
