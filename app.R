library(shiny)
library(ggplot2)
library(caret)
library(randomForest)
# Load iris dataset
data(iris)

# Split dataset into training and testing sets
set.seed(1234)
trainIndex <- createDataPartition(iris$Species, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
train <- iris[trainIndex,]
test <- iris[-trainIndex,]

# Train a random forest model
set.seed(4321)
rf_model <- train(Species ~ ., data = train, method = "rf")

# Define UI
ui <- fluidPage(
  titlePanel("Iris Species Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("sepal_length", "Sepal Length", min = 0, max = 10, value = 5.1),
      sliderInput("sepal_width", "Sepal Width", min = 0, max = 10, value = 3.5),
      sliderInput("petal_length", "Petal Length",  min = 0, max = 10, value = 1.4),
      sliderInput("petal_width", "Petal Width", min = 0, max = 10, value = 0.2),
      actionButton("submit_button", "Submit"),
      br(),
      actionButton("random_button", "Display")
    ),
    
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("prediction")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Generate random input
  random_input <- eventReactive(input$random_button, {
    list(
      sepal_length = runif(1, 0, 10),
      sepal_width = runif(1, 0, 10),
      petal_length = runif(1, 0, 10),
      petal_width = runif(1, 0, 10)
    )
  })
  
  # Render plot
  output$plot <- renderPlot({
    ggplot(data = iris, aes(x = Sepal.Length + Sepal.Width, y = Petal.Length + Petal.Width, 
                            color = Species)) +
      geom_point(size = 3) +
      geom_point(aes(x = input$sepal_length + input$sepal_width, y = input$petal_length + input$petal_width), 
                 color = "red", size = 5) +
      geom_point(aes(x = random_input()$sepal_length, y = random_input()$sepal_width), 
                 color = "green", size = 5, shape = 4) +
      labs(title = "Iris Species by Sepal Dimensions",
           x = "Sepal Length + Sepal Width",
           y = "Petal Length + Petal Width") +
      theme_minimal()
  })
  
  
  
  # Make prediction on new data
  new_data <- eventReactive(input$submit_button, {
    data.frame(Sepal.Length = input$sepal_length,
               Sepal.Width = input$sepal_width,
               Petal.Length = input$petal_length,
               Petal.Width = input$petal_width)
  })
  
  output$prediction <- renderPrint({
    prediction <- predict(rf_model, new_data())
    paste("The predicted species is", prediction)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
