## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(sidebarMenu(
            menuItem("Recommend by Genre", tabName = "genre", icon = icon("film")),
            menuItem("Recommend by Rating", tabName = "rating", icon = icon("bomb"))
          )),

          dashboardBody(includeCSS("css/movies.css"),
              tabItems(
                tabItem(tabName = "genre",
                        fluidRow(
                          box(width = 12, title = "Step 1: Select your favorite genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                              selectInput("genre_choice",
                                          label="Choose genre from list",
                                          choices = list("Action","Adventure", "Animation", "Children's",
                                                         "Comedy", "Crime", "Documentary", "Drama",
                                                         "Fantasy", "Film-Noir", "Horror", "Musical",
                                                         "Mystery", "Romance", "Sci-Fi", "Thriller",
                                                         "War", "Western"), selected = 1)
                              )
                          ),
                        fluidRow(
                          useShinyjs(),
                          box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "Step 2: Discover movies you might like",
                            br(),
                            withBusyIndicatorUI(
                              actionButton("genre_btn", "Click here to get your recommendations", class = "btn-warning")
                            ),
                            br(),
                            tableOutput("genre_results")
                          )
                        )
                      ),
                tabItem(tabName = "rating",
                        fluidRow(
                          box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                              div(class = "rateitems",
                                  uiOutput('ratings')
                              )
                          )
                        ),
                        fluidRow(
                          useShinyjs(),
                          box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "Step 2: Discover movies you might like",
                            br(),
                            withBusyIndicatorUI(
                              actionButton("ratings_btn", "Click here to get your recommendations", class = "btn-warning")
                            ),
                            br(),
                            tableOutput("ratings_results")
                          )
                        )
                )
              )
          )
    )
)