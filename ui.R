library(shiny)

fluidPage(
  
  # Application title.
  titlePanel("Tutoring Center Schedule"),
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view. The helpText function is
  # also used to include clarifying text. Most notably, the
  # inclusion of a submitButton defers the rendering of output
  # until the user explicitly clicks the button (rather than
  # doing it immediately when inputs change). This is useful if
  # the computations required to render output are inordinately
  # time-consuming.
  sidebarLayout(
    sidebarPanel(
      
      fileInput("dailyHours", "Daily Hours",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      
      fileInput("signUpSchedule", "Tutor Preferences",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      
      fileInput("tutorHours", "Tutor Hours",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      
      fileInput("classes", "Tutor Classes Taken",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      
      sliderInput("timeOpenSlider", label = h3("Hours Open: Military Time"), min = 0, 
                  max = 24, value = c(10, 21)),
    
      actionButton("makeSchedule", "Generate Schedule")
    ),
    
    # Show a summary of the dataset and an HTML table with the
    # requested number of observations. Note the use of the h4
    # function to provide an additional header above each output
    # section.
    mainPanel(
      
      tabsetPanel(
        tabPanel('Readable Schedule',
                 DT::dataTableOutput("readableSchedule")),
        # tabPanel('Monday Schedule',
        #          DT::dataTableOutput("mondaySchedule")),
        # tabPanel('Tuesday Schedule',
        #          DT::dataTableOutput("tuesdaySchedule")),
        # tabPanel('Wednesday Schedule',
        #          DT::dataTableOutput("wednesdaySchedule")),
        # tabPanel('Thursday Schedule',
        #          DT::dataTableOutput("thursdaySchedule")),
        # tabPanel('Friday Schedule',
        #          DT::dataTableOutput("fridaySchedule")),
        # tabPanel('Saturday Schedule',
        #          DT::dataTableOutput("saturdaySchedule")),
        tabPanel('Schedule Statistics',
                 DT::dataTableOutput("summary"))
      )
      
      
    )
  )
  
)