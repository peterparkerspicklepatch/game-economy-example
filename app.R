library(DT)
library(shiny)
library(plotly)
library(readxl)
library(tidyverse)

#setwd("/Users/phillipblack 1/Downloads/")
island_economy <- read_excel("20180112 - Game economy modelling test - dataset.xlsx")
island_economy_lookup <- read_excel("20180112 - Game economy modelling test - dataset.xlsx", 2)

island_economy_lookup <- island_economy_lookup %>% rename("resource_long" = "...1")

island_economy_gather <-
  island_economy_lookup %>%
  gather(-resource_long, key = 'Type', value = 'Value')

base_input <-
island_economy_gather %>%
  replace_na(list(Value = 0)) %>%
  group_by(resource_long) %>%
  summarise(total = sum(Value)) %>%
  filter(total == 0) %>%
  mutate(type = 'base_input') %>%
  select(resource_long, type)

#eh, manual is fine. This can scale if fed a list.
island_economy_long <-
  island_economy_gather %>%
  add_row(resource_long = c("Blueberry", "Coconut", "Pomegranate"),  Value = "base_input") %>%
  filter(!is.na(Value)) %>%
  select("Output" = "resource_long", "Input" = "Type", "Amount" = "Value")

ui <- fluidPage(

navbarPage("Understanding an Island Economy",

  # Give the page a title
  tabPanel("Relationships",

  # Generate a row with a sidebar
  sidebarLayout(

    # Define the sidebar with one input
    sidebarPanel(

      selectizeInput("resource",
                     "Resource",
                      choices = unique(island_economy$`Resource name`),
                      multiple = T,
                      selected = c('Cotton', 'Board', 'Fishing Net')),
      hr("Add resources to explore above.")
    ),

    mainPanel(
      p("Question: Build a tool to visualize and possibly edit the relationship between resources."),
      p("I made some basic bar charts and data tables. Nothing spectacular. With more time I'd work on adding resource production trees and creating reactive cells on the data tables"),
      br(),
      tabsetPanel(
      tabPanel(p(icon("bar-chart"), "Visualize Production"),
        h4('Resource Comparison', align = 'Left'),
        plotlyOutput("barPlot", width = '90%'),
        h4('Resource Cost', align = 'Left'),
        plotlyOutput("barPlot2", width = '90%')
      ),
      tabPanel(p(icon("table"), "Data Tables"),
               p("A user can double click to edit values. Unfortunately, this is not reflected on the charts. While possible, it was outside time constraints."),
               br(),
               h4("Resource Comparison", align = 'Left'),
               dataTableOutput(outputId = 'dTable'),
               h4("Resource Cost Lookup", align = 'Left'),
               dataTableOutput(outputId = 'dTable2')
    )
  )
  )
)
),
  tabPanel("Time to build",

  # Generate a row with a sidebar
  sidebarLayout(

    # Define the sidebar with one input
    sidebarPanel(

      selectInput("resource_drop",
                     "Resource",
                      choices = unique(island_economy$`Resource name`),
                      selected = c('Spyglass'))
    ),
    mainPanel(
          p("Question: Build a tool that can use the input data to calculate the total time to build a resource (from the basic
ingredients all the way to the final product)"),
          br(),
          p("Some of the production chain was unclear. If 1 Cotton can only produce 1 Cotton, how does one make the Cotton to begin with?
            In this case, I assumed Cotton was the 'Base Resource' of production."),
          br(),
          p("Below I calculate the total resources needed for each input and tally it as a running sum.
            The time to complete each input was a function of
            Total Time for Resource = Needed/Produced Per Cycle * Min Per Cycle then rounded upward."),
          h4("Resource Cost Lookup", align = 'Left'),
          dataTableOutput('currentResult')

      )
  )
),
  tabPanel("Collection Estimation",

  # Generate a row with a sidebar
  sidebarLayout(

    # Define the sidebar with one input
    sidebarPanel(

      selectInput("estimate",
                     "Resource",
                      choices = unique(island_economy$`Resource name`),
                      selected = c('Spyglass')),

             numericInput("sessions",
                     "Sessions Per Day",
                     2,
                     min = 1,
                     max = 200)

    ),
    mainPanel(
          p("Build a tool that can estimate how many sessions would a player need to complete a certain object. Please
explicitly state your assumptions on playing behavior (eg: sessions per day, session length, etc)"),
          p("I didn't think this was the most useful question, rather I explored the number of session days to build a resource.
            With some assumptions around sessions days to real time days (i.e. how many days in a week does a player play)
             this can back into days to produce the resource."),
          br(),
          p("This was a simple model wherein my theory was that how many sessions days is dependent on how many collection sessions
            the resource asks of the player. A collection session is where the players 'clears out' all the resources in a app open to app close."),
          h4("Collection Sessions in Production Chain", align = 'Left'),
          p("Here, I just show number of 'nodes' the resource asks of the player.
            For Spyglass, the player 'kicks' off build order at node 2, then returns to kick off node 1 and finally node 0."),
          br(),
          p("With more time I would add a 'savings' account to account for extra resources not used in the resource build."),
          p("A big assumption was that a player could only kick off one cycle per resource. This means a player could not build 3x cotton cycles at once, but must wait for one to complete before starting another."),
          dataTableOutput('estimate'),
          h4("Estimation", align = 'Left'),
          dataTableOutput('estimate2')

      )
  )
  ),
     tabPanel("Other Questions",

    mainPanel(
      h4("Bonus question: let’s assume now that producing a Fishing Net has a 30% chance of failure (you wait the
time, lose the cotton and don’t get a fishing net). How would you change the models?"),
      br(),
      p("If this was the case, the resources produced from a production cycle become the expected resources produced from a cycle.
        expected[resources_from_one_cycle] = resources_from_one_cycle*prob_of_success.

        Wherein, Total Time for Resource = (Needed/expected[Produced Per Cycle]) * Min Per Cycle
        "),
      br(),
      h4("Collection Sessions in Production Chain", align = 'Left'),
      br(),
      p("What else would you use this dataset for? Are there other visualizations or models you think would be
interesting to look at or build? Looking at your current model, are there any issues you would flag up to the
game designer?"),
      br(""),
      tags$ol(
        tags$li("Without question a tree diagram showing all of the nodes of production for a given resource."),
        tags$li("Bonus points if the model can 'prune' parts of the production tree and recalculate production times."),
        tags$li("My model also doesn't take into account excess production which matters over the long-run as
players build up excess base resources. This drives down collection sessions and sessions needed to produce a given output."),
        tags$li("The session day estimation is primative, layering in a couple more assumptions could greatly improve prediction accuarcy. Benchmarking against King's internal games is the best test."),
        tags$li("Being able to build a basket of goods (3x Spyglass, 2x Fishing Goods), and calculating the total session days required is another area of development."),
        tags$li("Being able to add a new resource in the model and well adding required inputs is useful for 'building out the game'."),
        tags$li("I'd spend more time thinking about the number of unique resources needed for an output. More unique resources could increase UX costs for the player as they might have to collect from different parts of the game.")
      ),
      br(""),
      h4("Time Spent: 8 Hours")

  )
)
)
)

# Define a server for the Shiny app
server <- function(input, output) {

  # Fill in the spot we created for a plot
  output$barPlot <- renderPlotly({

    island_economy %>%
    gather(-`Resource name`, key = 'Type', value = 'Value') %>%
    filter(`Resource name` %in% input$resource) %>%
    ggplot(aes(x = `Resource name`, y = Value, fill = Type)) +
        geom_bar(stat = 'identity') +
        facet_wrap(~Type) +
        theme(strip.text.x = element_text(size = 5.5)) +
        labs(x = "Resource name", y = "Amount") +
        theme(legend.position = "none")
  })

  # Fill in the spot we created for a plot
  output$barPlot2 <- renderPlotly({
    island_economy_lookup %>%
    gather(-resource_long, key = 'Type', value = 'Value') %>%
    filter(resource_long %in% input$resource, !is.na(Value)) %>%
    ggplot(aes(x = Type, y = Value)) +
        geom_bar(stat = 'identity') +
        facet_wrap(~resource_long, scales = "free") +
        labs(x = "Cost by Resource", y = "Amount")
  })

    # Render island_economy_long table
  output$dTable <- renderDataTable({
        island_economy %>%
        filter(`Resource name` %in% input$resource) %>%
        DT::datatable(editable = TRUE)
    })

  output$dTable2 <- renderDataTable({
        island_economy_lookup %>%
        gather(-resource_long, key = 'Type', value = 'Value') %>%
        filter(resource_long %in% input$resource, !is.na(Value)) %>%
        rename("Resource name" = "resource_long", "Resource Required Produce" = "Type", "Cost" = "Value") %>%
        DT::datatable(editable = TRUE)
    })

  output$currentResult <- renderDataTable({
        Output = input$resource_drop
        answer <- data.frame()
        newParent <- data.frame(Output)

        currentResult <-
          island_economy_long %>%
          inner_join(newParent, by = c("Output" = "Output")) %>%
          select(Input)

         newParent <- currentResult
         answer <- dplyr::bind_rows(answer, newParent)

         repeat{
           currentResult <-
            island_economy_long %>%
            inner_join(newParent, by = c("Output" = "Input")) %>%
            select(Input)
         if(identical(currentResult, newParent) != TRUE){
           newParent <- currentResult
           answer <- dplyr::bind_rows(answer, newParent)
         } else {
            break
         }
           }

         answer %>%
           group_by(Input) %>%
           summarise(Needed = n()) %>%
           left_join(island_economy, by = c("Input" = "Resource name")) %>%
           mutate(`Cycles` = ceiling(Needed/`Number of resources produced`),
                  `Total Time for Resource` = Needed/`Number of resources produced`*`Minutes to produce the resource`,
                  `Running Total Time` = cumsum(`Total Time for Resource`)) %>%
           select(Input, Needed, `Number of resources produced in Single Cycle` =
                  `Number of resources produced`, `Cycles`, `Min Per Cycle` = `Minutes to produce the resource`,
                  `Total Time for Resource`, `Running Total Time`) %>%
           DT::datatable(rownames = FALSE, options = list(dom = 't',ordering = F))
                 })

  output$estimate <- renderDataTable({
        Output = input$estimate
        answer <- data.frame()
        newParent <- data.frame(Output)
        collection_session <- 0

        currentResult <-
          island_economy_long %>%
          inner_join(newParent, by = c("Output" = "Output")) %>%
          select(Input)

         newParent <- currentResult
         answer <-
           dplyr::bind_rows(answer, newParent) %>%
           mutate(collection_session = collection_session)

         repeat{
           currentResult <-
            island_economy_long %>%
            inner_join(newParent, by = c("Output" = "Input")) %>%
            select(Input)
         if(identical(currentResult, newParent) != TRUE){
           newParent <- currentResult
           collection_session <- collection_session + 1
           newParent_c <- newParent %>% mutate(collection_session = collection_session)
           answer <- dplyr::bind_rows(answer, newParent_c)
         } else {
            break
         }
           }

           answer %>%
           left_join(island_economy, by = c("Input" = "Resource name")) %>%
           select(Input, collection_session) %>%
           DT::datatable(rownames = FALSE, options = list(dom = 't',ordering = F))
                 })

   output$estimate2 <- renderDataTable({
        Output = input$estimate
        answer <- data.frame()
        newParent <- data.frame(Output)
        collection_session <- 0

        currentResult <-
          island_economy_long %>%
          inner_join(newParent, by = c("Output" = "Output")) %>%
          select(Input)

         newParent <- currentResult
         answer <-
           dplyr::bind_rows(answer, newParent) %>%
           mutate(collection_session = collection_session)

         repeat{
           currentResult <-
            island_economy_long %>%
            inner_join(newParent, by = c("Output" = "Input")) %>%
            select(Input)
         if(identical(currentResult, newParent) != TRUE){
           newParent <- currentResult
           collection_session <- collection_session + 1
           newParent_c <- newParent %>% mutate(collection_session = collection_session)
           answer <- dplyr::bind_rows(answer, newParent_c)
         } else {
            break
         }
           }

           answer %>%
           left_join(island_economy, by = c("Input" = "Resource name")) %>%
           select(Input, collection_session) %>%
           summarise(`Session Days to Produce Object` = n_distinct(collection_session)/input$sessions) %>%
           DT::datatable(rownames = FALSE, options = list(dom = 't',ordering = F))


                 })


}

shinyApp(ui, server)
