#
# This application predicts whether a passenger would have suvivied the Titanic wreckage.
#

library(shiny)

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Titanic Survivor Predictor"),
   
   # Sidebar for image 
   sidebarLayout(
      sidebarPanel(
        img(src = "survivor.png", height = 250, width = 250)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h4('This application predicts whether the given passenger would have survived the Titanic wreckage.'),
        img(src = "line.png", height = 25, width = 600),
        p('Please provide the following details about the passenger and then click Submit button.'),
        br(),
        selectInput("pclass", "Class travelled by the passenger: ", c("First Class" = 1, "Second Class" = 2, "Third Class" = 3)),
        radioButtons("sex", "Sex of the passenger: ", c("Female" = 1, "Male" = 2), inline = TRUE),
        sliderInput("age", 'Age of the passenger: ', 30, min=0, max=80, step=1),
        submitButton("Submit"),
        img(src = "line.png", height = 100, width = 600),
        verbatimTextOutput("prediction_text")
      )
   )
)

# Define server for prediction
server <- function(input, output) {
   output$prediction_text <- renderText({
     pass_class <- as.numeric(input$pclass)
     pass_sex <- ifelse(input$sex == 1, "female", "male")
     pass_age_group <- ifelse(input$age <= 18, "minor", "major")
     new_pass <- data.frame(Pclass = pass_class, Sex = pass_sex, AgeGroup = pass_age_group)
     new_pass$Sex <- factor(new_pass$Sex, levels = c("female", "male"))
     new_pass$AgeGroup <- factor(new_pass$AgeGroup, levels = c("major", "minor"))
     library(randomForest)
     load("titanic_survivor_predictor.rda")
     std_text <- "It is predicted that the passenger will "
     prediction <- predict(fit, new_pass, OOB = TRUE, type = "response")
     prediction_text <- ifelse(prediction == "0", paste0(std_text, "not survive."), paste0(std_text, "survive."))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
