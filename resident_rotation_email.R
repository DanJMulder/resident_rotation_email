## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##   Shiny App for Rapid Resident Peds GI Rotation Email Creation                            ##
##   Written by Daniel Mulder, March 2022                                                    ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## Preamble
# This script does not constitute medical advice and is only to be used for the purposes of learning or preparing personal templates
# This script contains no real medical information, any information contained within is fictional example information

# Load required packages ----
library(shiny) # for interactive web application framework
library(tidyverse) # for basic data organization
library(glue) # for gluing together text

#2022-2023 common block dates
blocks <- tibble(
  block_number = 1:13,
  start_date = ymd("2022-07-01", "2022-08-02", "2022-08-30", "2022-09-27", "2022-10-25", "2022-11-22", "2022-12-20", "2023-01-17", "2023-02-14", "2023-03-14", "2023-04-11", "2023-05-09", "2023-06-06"),
  end_date = ymd("2022-08-01", "2022-08-29", "2022-09-26", "2022-10-24", "2022-11-21", "2022-12-19", "2023-01-16", "2023-02-13", "2023-03-13", "2023-04-10", "2023-05-08", "2023-06-05", "2023-06-30")
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Upcoming Pediatric GI Rotation"),
    tags$b(strong("(for copy/paste into outlook)")),
    br(),
    br(),
    
    # Select the components
    tabsetPanel(type = "tabs",
                tabPanel("Input",
                         br(),
                         textInput("resident", "Resident First Name:"),
                         selectInput("block", width = '50%', "Block Number", blocks$block_number),
                         tagAppendAttributes(textOutput("block_dates"), style = "white-space:pre-wrap;"),
                         br(),
                         dateInput("clinic_date_1", "Clinic Date (YYYY-MM-DD):", value = today()),
                         dateInput("clinic_date_2", "Clinic Date (YYYY-MM-DD):", value = today()),
                         dateInput("scoping_date_1", "Scoping Date (YYYY-MM-DD):", value = today()),
                         dateInput("scoping_date_2", "Scoping Date (YYYY-MM-DD):", value = today()),
                         ),
                
                tabPanel("Text Output (for copy/paste)",
                         br(),
                         tagAppendAttributes(textOutput("full_note"), style = "white-space:pre-wrap;")
                         )
                )
    )
  
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # these calculations update the clinic and scoping dates on the rotation depending on the block selected
  # the dates can also be updated manually
  observe({
    
    first_clinic_date <- ceiling_date(blocks$start_date[as.numeric(input$block)], "week") + 1
    updateDateInput(session, "clinic_date_1", value = first_clinic_date)
    
    second_clinic_date <- ceiling_date(blocks$start_date[as.numeric(input$block)], "week") + 15
    updateDateInput(session, "clinic_date_2", value = second_clinic_date)
    
    first_scoping_date <- ceiling_date(blocks$start_date[as.numeric(input$block)], "week") + 3
    updateDateInput(session, "scoping_date_1", value = first_scoping_date)
    
    second_scoping_date <- ceiling_date(blocks$start_date[as.numeric(input$block)], "week") + 17
    updateDateInput(session, "scoping_date_2", value = second_scoping_date)
    
  })
  
  # this chunk pulls the start/end dates out of the "blocks" tibble at the top of the script to assist the user with selecting the clinic/scoping dates
  blockDates <- reactive({
    
    start_date_selection <- blocks$start_date[as.numeric(input$block)]
    
    start_date_text <- glue(
      paste(wday(start_date_selection, label = TRUE, abbr = FALSE)),
      ", ",
      paste(month(start_date_selection, label = TRUE, abbr = FALSE)),
      " ", paste0(mday(start_date_selection)))
    
    end_date_selection <- blocks$end_date[as.numeric(input$block)]
    
    end_date_text <- glue(
      paste(wday(end_date_selection, label = TRUE, abbr = FALSE)),
      ", ",
      paste(month(end_date_selection, label = TRUE, abbr = FALSE)),
      " ", paste0(mday(end_date_selection)))
    
    return(glue("(Block Dates: ", start_date_text, " to ", end_date_text, ")"))
    
  })
  
  output$block_dates <- renderPrint(blockDates())
  
  
  formData <- reactive({
    
    # function for creating a test string for a sepecific date in the "Monday June 27" format
    smart_date_text <- function(date_input) {
      glue(
        paste(wday(date_input, label = TRUE, abbr = FALSE)),
        " ",
        paste(month(date_input, label = TRUE, abbr = FALSE)),
        " ",
        paste0(mday(date_input)))
    }
    
    # pull the start/end dates of the rotation out of the "blocks" tibble at the top of this script to use in the body of the text
    start_date_selection <- smart_date_text(blocks$start_date[as.numeric(input$block)])
    end_date_selection <- smart_date_text(blocks$end_date[as.numeric(input$block)])
    
    return(glue("Hello ", input$resident, ",",
                "\n",
                "\n",
                paste0("I see you are on the GI/endo rotation with me from ", start_date_selection, " to ", end_date_selection, ". We are looking forward to having you join!"),
                "\n",
                "\n",
                "Is there a good time on the first day of your rotation that we can meet and talk about rotation objectives and logistics and such? Please let me know.",
                "\n",
                "\n",
                "I am understanding of the challenge of the shared nature of the rotation with the endo team and I assure you it has not been an issue going back and forth so far. Wherever the learning opportunities are, I will try to get you there.",
                "\n",
                "\n",
                "My clinics while you are on rotation are ", smart_date_text(input$clinic_date_1), " and ", smart_date_text(input$clinic_date_2), ". My clinics run all day at COPC. All clinic notes if typed must be sent to me within 72 hours (we can talk more about this in person). Please send patient info only through kingstonhsc.ca email addresses.",
                "\n",
                "\n",
                "I am scoping ", smart_date_text(input$scoping_date_1), " and ", smart_date_text(input$scoping_date_2), " in the KGH OR, which is usually scheduled to start at noon but often starts earlier if the OR cases from the morning are completed early. It is good for you to see at least one upper GI endoscopy and one colonoscopy during the rotation just so you have a sense of what the procedure entails for your future practice.",
                "\n",
                "\n",
                "We will do brief focused teaching sessions when you are available for about 30 minutes.",
                "\n",
                "\n",
                "Please do not hesitate to ask anytime if you have any questions.",
                "\n",
                "\n",
                "Looking forward to working together! Email/text/call anytime, my cell is 343-580-0154.",
                "\n",
                "\n",
                "Dan"
                )
           )
    })
  
  output$full_note <- renderPrint(formData())
  
}

# Run the application 
shinyApp(ui = ui, server = server)
