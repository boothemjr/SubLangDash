#### Sub Reader ####
# import libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(SnowballC)
library(tidytext)
library(readtext)
library(stopwords)

# set DATA_DIR
DATA_DIR <- paste0(getwd(), "/data/subs/")

# read in subs
subs <-
  readtext(paste0(DATA_DIR, "*.txt"))

# change episode names
subs$doc_id = sub("[^-]*- ", "", as.character(subs$doc_id))
subs$doc_id = sub(" -.*$", "", as.character(subs$doc_id))

# tokenize text
tokenized <- unnest_tokens(subs, word, text)

# create stopwords dataframe
stopdf <- as.data.frame(stopwords("es"))

# change column name
stopdf <- rename(stopdf, "stops" = "stopwords(\"es\")")

# build df of extra stop words unique to this series (eg. names, English words)
# TODO convert to table for each tv show
newstopdf <-
  data.frame(
    "stops" = c(
      "oh",
      "si",
      "ok",
      "bien",
      "hola",
      "bueno",
      "ross",
      "phoebe",
      "bob",
      "rachel",
      "alan"
    )
  )

# add new stop words to df
stopdf <- rbind(stopdf, newstopdf)

# remove stop words
tokenized <- anti_join(tokenized, stopdf, by = c("word" = "stops"))

# TODO truncate to stems - will require reading base word
# tokenized <- wordStem(tokenized[2], language = "spanish")

# create list of episodes for selection
episodeList <- tokenized %>%
  select(doc_id) %>%
  unique()

### DASHBOARD UI ####
ui <-
  dashboardPage(
    dashboardHeader(title = "Language Learning Dashboard"),
    dashboardSidebar(
      sidebarMenu(
        id = "sidebarmenu",
        # TV Show Selection
        menuItem(
          "TV Shows",
          tabName = "show",
          icon = icon("tv",
                      lib = "font-awesome")
        ),
        conditionalPanel(
          condition = "input.sidebarmenu == 'show'",
          selectInput(
            "show",
            label = "Select Show:",
            choices = "Friends",
            selected = "Friends"
          )
        ),
        # Episode Selection
        menuItem(
          "Episodes",
          tabName = "episode",
          icon = icon("tv",
                      lib = "font-awesome")
        ),
        conditionalPanel(
          condition = "input.sidebarmenu == 'episode'",
          selectInput("episode",
                      label = "Select Episode:",
                      choices = episodeList)
        ),
        # Student Information
        menuItem(
          "Student Progress",
          tabName = "student",
          icon = icon("user-graduate",
                      lib = "font-awesome")
        ),
        conditionalPanel(
          condition = "input.sidebarmenu == 'student'",
          checkboxGroupInput("epsViewed", label = "Episodes Viewed",
                             choices = episodeList$doc_id),
          numericInput(
            "epsViewedNum",
            "Episodes Viewed",
            0,
            min = 0,
            step = 1
          )
        )
      )
    ),
    dashboardBody(tabItems(
      tabItem(tabName = "show",
              fluidRow(
                box(
                  title = "TV Show",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("plot1",
                             height = 700),
                  width = 12,
                  height = "800"
                )
              )),
      
      tabItem(tabName = "episode",
              fluidRow(
                box(
                  title = "Episode",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("plot2",
                             height = 700),
                  width = 12,
                  height = "800"
                )
              )),
      
      tabItem(tabName = "student",
              hr(),
              fluidRow(column(
                3, verbatimTextOutput("plot3")
              )))
    ))
  )
### DASHBOARD SERVER ####
server <- function(input, output) {
  # plot occurrences for each word by series
  output$plot1 <- renderPlot({
    tokenized %>%
      count(word, sort = TRUE)  %>%
      mutate(word = reorder(word, n)) %>%
      head(20) %>%
      ggplot(aes(word, n)) +
      geom_bar(stat = "identity",
               fill = "darkred",
               colour = "black")
  })
  
  # TODO plot occurrences for each word by season
  
  # plot occurrences for each word by episode
  output$plot2 <- renderPlot({
    tokenized %>%
      filter(doc_id == input$episode) %>%
      count(word, sort = TRUE)  %>%
      mutate(word = reorder(word, n)) %>%
      head(20) %>%
      ggplot(aes(word, n)) +
      geom_bar(stat = "identity",
               fill = "darkred",
               colour = "black")
  })
  
  # plot occurrences based off student selection
  # if(is.null(input$epsViewed)){
  #   output$plot3 <- renderPrint({
  #     "No Input"
  #   })
  # }
  # else{
  #   output$plot3 <- renderPrint({
  #     "Valid Input"
  #   })
  # }
  
  output$plot3 <- renderPrint({
    ifelse(
      is.null(input$epsViewed),
      "No Input",
      tokenized %>%
        filter(doc_id == input$epsViewed) %>%
        count(word, sort = TRUE) %>%
        mutate(word = reorder(word, n))
    )
  })
}
### DASHBOARD APP ####
shinyApp(ui, server)