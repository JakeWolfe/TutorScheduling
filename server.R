library(shiny)
library("datasets")
library("readr")
library("phonTools")
library("lpSolve")
library("plyr")
library("DT")

source("ScheduleMainTutors.R")

function(input, output) {
  
  observeEvent(input$makeSchedule, {
    
    if(is.null(signUpschedule)||is.null(tutorHours)||is.null(classes)||is.null(dailyHours)){
      showModal(modalDialog(
        title = "Error Creating Schedule",
        "Please enter all files before creating schedule"
      ))
      return()
    }

    schedules <- getSchedule(input$signUpSchedule, input$tutorHours, input$classes, 
                            input$dailyHours, input$timeOpenSlider[1], input$timeOpenSlider[2], 6)
  
    
    if (is.null(schedules)) {
      showModal(modalDialog(
        title = "Error Creating Schedule",
        "Schedule was unable to be made, check you files and increase number of hours and try again"
      ))
      return()
    }
    
    numHours = input$timeOpenSlider[2] - input$timeOpenSlider[1]
    
    output$mondaySchedule <- DT::renderDataTable(
      schedules[[1]],
      options = list(scrollX = TRUE, pageLength = numHours, callback = I(
        'function(row, data) { $("td", row).css("text-align", "center"); }'
      ))
    )
    
    output$tuesdaySchedule <- DT::renderDataTable(
      schedules[[2]],
      options = list(scrollX = TRUE, pageLength = numHours, callback = I(
        'function(row, data) { $("td", row).css("text-align", "center"); }'
      ))
    )
    
    output$wednesdaySchedule <- DT::renderDataTable(
      schedules[[3]],
      options = list(scrollX = TRUE, pageLength = numHours, callback = I(
        'function(row, data) { $("td", row).css("text-align", "center"); }'
      ))
    )
    
    output$thursdaySchedule <- DT::renderDataTable(
      schedules[[4]],
      options = list(scrollX = TRUE, pageLength = numHours, callback = I(
        'function(row, data) { $("td", row).css("text-align", "center"); }'
      ))
    )
    
    output$fridaySchedule <- DT::renderDataTable(
      schedules[[5]],
      options = list(scrollX = TRUE, pageLength = numHours, callback = I(
        'function(row, data) { $("td", row).css("text-align", "center"); }'
      ))
    )
    
    output$saturdaySchedule <- DT::renderDataTable(
      schedules[[6]],
      options = list(scrollX = TRUE, pageLength = numHours, callback = I(
        'function(row, data) { $("td", row).css("text-align", "center"); }'
      ))
    )
    
    output$summary <- DT::renderDataTable(
      schedules$Stats$HourCounts,
      options = list(scrollX = TRUE, rowname=FALSE, pageLength = numHours, callback = I(
        'function(row, data) { $("td", row).css("text-align", "center"); }'
      ))
    )
    
    output$readableSchedule <- DT::renderDataTable(
      schedules$readableSchedule,
      extensions = c('Buttons'),
      escape = FALSE,
      options = list(
        pageLength = numHours,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    
  })
  
  dailyHours <- reactive({
    dailyHours <- c()
    file1 <- input$dailyHours
    if(is.null(file1)){return()}
    read.csv(file = inFile$datapath, header = TRUE)
  })
  
  signUpschedule <- reactive({
    signUpschedule <- c()
    file1 <- input$signUpschedule
    if(is.null(file1)){return()}
    read.csv(file = inFile$datapath, header = TRUE)
  })
  
  tutorHours <- reactive({
    tutorHours <- c()
    file1 <- input$tutorHours
    if(is.null(file1)){return()}
    read.csv(file = inFile$datapath, header = TRUE)
  })
  
  classes <- reactive({
    classes <- c()
    file1 <- input$classes
    if(is.null(file1)){return()}
    read.csv(file = inFile$datapath, header = TRUE)
  })
  
  output$range <- renderPrint({ input$timeOpenSlider })
}