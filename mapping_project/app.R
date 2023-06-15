source("map_utils.R", local = F)

### Create user interface
ui <- fluidPage(
  titlePanel("Mapping Project"),
  tabsetPanel(
    tabPanel("Maps", 
             fluidRow(
               br(),
               column(2, 
                      selectInput("inputcountry", label = "Select country", choices = c("USA"))
               ),
               column(2,
                      uiOutput("level_input")
               ),
               column(2,
                      uiOutput("state_input")
               ),
               column(2,
                      uiOutput("county_input")
               ),
               column(2,
                      uiOutput("var_input")
               ),
             ),
             fluidRow(
               column(12,
                      align = "center",
                      h3(textOutput("title")))
             ),
             fluidRow(
               column(12,
                      align = "center",
                      shinycssloaders::withSpinner(
                        plotOutput("plot")
                      )
               )
             )
    )
  )
)

### Set up server side
server <- function(input, output, session) {
  
  # creates dropdown for level of analysis depending on country and level
  leveloptions <- reactiveValues(value = c("", ""))
  
  observeEvent(input$inputcountry, {
      leveloptions$value[1] <- "US States"
      leveloptions$value[2] <- "Counties within a state"
      leveloptions$value[3] <- "Subdivisions within a county"
  })
  
  output$level_input <- renderUI({
    req(input$inputcountry)
    selectInput("inputlevel", "Select level of analysis", 
                c(leveloptions$value)
    )
  })
  
  output$county_input <- renderUI({
    req(input$level_input)
    if (req(input$inputlevel) == "Subdivisions within a county") {
      
      selectInput("county", "Select county", 
                  c("Los Angeles")
      )
    }
  })
  
  # creates dropdown for variables depending on country and level of analysis 
  variableoptions <- reactiveValues(value = c(""))
  
  observeEvent(input$inputcountry, { # determine options
      variableoptions$value[1] <- "Median household income"
      variableoptions$value[2] <- "Median gross rent per bedroom"
    # }
  })
  
  # creates dropdown for states if county-level is chosen
  output$state_input <- renderUI({
    req(input$inputcountry == "USA")
    if (req(input$inputlevel) == "Counties within a state" | req(input$inputlevel) == "Subdivisions within a county") {
      
      selectInput("inputstate", "Select state", 
                  c("California")
      )
    }
  })
  
  # creates dropdown for counties if subdivision-level is chosen
  output$county_input <- renderUI({
    req(input$inputcountry == "USA")
    if (req(input$inputlevel) == "Subdivisions within a county") {
      
      selectInput("inputcounty", "Select county", 
                  c("Los Angeles")
      )
    }
  })
  
  output$var_input <- renderUI({
    req(input$inputcountry, input$inputlevel)
    selectInput("inputvariable", "Select variable", 
                c("", variableoptions$value)
    )
  })
  
  # converts variable input into variable names
  inputvariable <- reactive({
    variables$varname[which(variables$text == input$inputvariable)]
  })
  
  # download necessary files
  #### to do: only download new data when needed!
  shpfile <- reactive({
    req(input$inputcountry, input$inputlevel) 
    get_shapefiles(country = input$inputcountry, level = input$inputlevel, state = input$inputstate, county = input$inputcounty)
  })
  
  # create object for title
  outcome <- reactiveVal()
  output$title <- renderText({
    req(input$inputvariable)
    outcome(c(paste0(input$inputlevel, "-level variation in ", input$inputvariable)))
    outcome()
  })
  
  output$plot <- renderPlot({
    # only start doing something when input is given
    req(input$inputcountry, input$inputlevel, input$inputvariable) 
    
    map <- create_map(shp = shpfile(), 
                      varname = inputvariable(),
                      outcome_name = input$inputvariable,
                      level = input$inputlevel)
    map
  },
  res = 96)
  
}


shinyApp(ui, server)
