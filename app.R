library(shiny)

# Ensure the 'www' directory exists for GIFs
if (!dir.exists("www")) dir.create("www")

# Source the holiday-specific functions
source("holiday_christmas.R")
source("holiday_hanukkah.R")

# Define UI for the Shiny app
ui <- navbarPage(
  "Data Card Generator",

  # First page: Home page
  tabPanel(
    "Home",
    fluidPage(
      titlePanel("Data Card Generator"),
      div(
        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 75vh;",
        textInput("name", "Enter your first name:", value = "", width = "300px"),
        selectInput("holiday", "Choose a holiday:", choices = c("Christmas", "Hanukkah"), selected = "Christmas", width = "300px"),
        div(style = "height: 20px;"),
        actionButton("submit", "Generate Greeting", style = "width: 150px;")
      )
    )
  ),

  # Second page: Card page
  tabPanel(
    "Card",
    fluidPage(
      div(
        style = "display: flex; align-items: center; justify-content: flex-start; gap: 10px; padding: 10px;",
        actionButton("goBack", "Clear", class = "btn-primary"),
        downloadButton("downloadGIF", "Download", class = "btn-success")
      ),
      div(
        style = "display: flex; justify-content: center; padding-top: 20px;",
        uiOutput("gifOutput")  # Dynamically display the GIF
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$submit, {
    # Generate the greeting text
    name <- input$name
    holiday <- input$holiday
    if (name == "") {
      name <- "Friend"  # Default if no name is provided
    }
    greeting_text <- paste("Happy", holiday, name, "from the DVA team!")

    # Generate the GIF based on the selected holiday
    if (holiday == "Christmas") {
      generate_christmas_plot(greeting_text, output_file = "www/holiday_card.gif")
    } else if (holiday == "Hanukkah") {
      generate_hanukkah_plot(greeting_text, output_file = "www/holiday_card.gif")
    }

    # Switch to the Card page
    updateNavbarPage(session, "Data Card Generator", selected = "Card")
  })

  # Display the GIF dynamically
  output$gifOutput <- renderUI({
    if (file.exists("www/holiday_card.gif")) {
      tags$img(src = "holiday_card.gif", alt = "Holiday Greeting", style = "width: 600px; height: 500px;")
    } else {
      tags$p("No card generated yet.", style = "color: red; text-align: center;")
    }
  })

  # Clear the GIF and go back to the Home page
  observeEvent(input$goBack, {
    if (file.exists("www/holiday_card.gif")) file.remove("www/holiday_card.gif")  # Clean up file
    updateNavbarPage(session, "Data Card Generator", selected = "Home")
  })

  # Download handler for the GIF
  output$downloadGIF <- downloadHandler(
    filename = function() {
      "holiday_card.gif"
    },
    content = function(file) {
      file.copy("www/holiday_card.gif", file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
