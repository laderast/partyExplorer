library(shiny)
library(ggplot2)
library(dplyr)
library(party)
library(data.table)
library(here)
library(palmerpenguins)
library(rintrojs)

###Put Data processing steps here
###This is a test using the iris dataset
###You can load any data here, just name it "dataset"

data(penguins)

dataset <- penguins
 outcomeVar <- "species"
 #dataset <- dataset %>% mutate(sepalRatio=Sepal.Length/Sepal.Width, petalRatio = Petal.Length/Petal.Width)
 
source("debounce.R")

##Build test and train sets of data
trainIndex <- sample(nrow(dataset), size = floor(0.75 * nrow(dataset)))
trainDataset <- dataset[trainIndex,]
testDataset <- dataset[-trainIndex,]

##Find covariate names
covariateNames <- setdiff(colnames(dataset), outcomeVar) 

##Don't modify anything below here, or app won't work properly.
#get the variable types
varClass <- sapply(dataset, class)

#separate the variables into each type
categoricalVars <- names(varClass[varClass == "factor"])
numericVars <- names(varClass[varClass %in% c("numeric", "integer")])


ui <- navbarPage(
  
  # Application title
  "Palmer Penguins",
  # Sidebar with a slider input for number of bins
  tabPanel("Tree Explorer",
           rintrojs::introjsUI(),
           actionButton("help", "Help", icon = icon("circle-question")),
           introBox(
           selectInput("covariates", "Click Box to Add Covariates", 
                       choices = covariateNames, multiple=TRUE),
           data.step = 1,
           data.intro = "<p>Welcome to the Palmer Penguin dataset. We're going to try to predict the
           Species of Penguin (Adelie, Chinstrap, Gentoo) using a conditional inference decision tree using the {party} package.
           Click the selector box to start adding a feature to the model."
            ),
           fluidRow(
            introBox( 
             column(width=9,plotOutput("cartTree", click="mouse_click")),
             
             data.step = 2,
             data.intro = "This is the resulting decision tree. 
             The decision tree shows the different classes that we are trying to predict (penguin species),
             and you can see the decision cutoff that separates each branch. At the bottom are the different bins defined by each decision cutoff."
             ),
             column(width=3, introBox(tableOutput("confusionMatrix"), data.step = 3,
                    data.intro = "This is the confusion matrix. You can see how the tree predicts versus the truth."))
             
            
           ),
           fluidRow(
             column(
           introBox(
           img(src="penguins.jpg", alt="Penguin Squad from Shirokuma Cafe", width = 400),
           data.step=4, data.intro = "This is a drawing of Adelie, Gentoo, and Chinstrap Penguins from one of my favorite anime, <a href='https://www.crunchyroll.com/polar-bear-cafe'>Shirokuma Cafe.</a>"
           ), width=6),
           column(
           introBox(
           img(src="penguinsan.gif", alt = "Penguin-san from Shirokuma Cafe freaking out about driving", width=400),
           data.step=5, data.intro = "This is a gif from Shirokuma Cafe."), width= 6
           ))
           #uiOutput("groupUI"),
           #verbatimTextOutput("groupSummary")#,
           #verbatimTextOutput("cartNode")
  ),
  tabPanel("Compare Nodes",
           introBox(
           selectInput("compareVar", "Select Covariate to Compare", 
                       choices=numericVars)),
           plotOutput("compareViolin")
  ),
  
  tabPanel("Test Group Accuracy",
           introBox(
           verbatimTextOutput("testResponse"))
  )
  
  
  #add group comparison?
  #add evaluate on test data?
)



server <- shinyServer(function(input, output, session) {
  
  #trainData is a "reactive" dataset - you can add live filtering criteria here
  #to use this reactive, notice you have to use trainData() rather than just trainData
  #in this expression
  trainData <- reactive({
    trainDataset
  })
  
  testData <- reactive({
    testDataset
  })
  
  treeObj <- reactive({ 
    #might want to use party (ctree()) instead - don't have to prune
    
    if(is.null(input$covariates)){
      return(NULL)
      #covariateString <- covariateNames[1]
    }else{
      inputString <- debounce(input$covariates, 1000)
      
      #inputString <- input$covariates
      
      covariateString <- 
        paste(inputString(), collapse = " + ")
    }
    formulaString <- paste(outcomeVar, "~", covariateString)
    
    print(formulaString)
    #trainTree <- tree(Species ~ ., data=dataset)
    
    trainTree <- ctree(as.formula(formulaString), data=trainData())
    
    return(trainTree)
  })
  
  output$cartTree <- renderPlot({
    ##levels of interactivity
    #examineGroups: output summary for group
    if(is.null(treeObj())){return(NULL)}
    
    
    plot(treeObj())
    
  })
  
  trainMembership <- reactive({
    if(is.null(treeObj())){return(NULL)}
    return(where(treeObj()))
  })
  
  testMembership <- reactive({
    if(is.null(treeObj())){return(NULL)}
    return(where(treeObj(), newdata=testData()))
  })
  
  
  output$confusionMatrix <- renderTable({
    if(is.null(treeObj())){return(NULL)}
    
    out <- data.frame(predict=predict(treeObj()), truth=trainData()[[outcomeVar]])
    #print(head(out))
    tab <- table(out)
    tab
  })
  
  output$cartNode <- renderText({
    if(is.null(input$mouse_click)){return(NULL)}
    print(input$mouse_click)
  })
  
  output$groupUI <- renderUI({
    if(is.null(treeObj())){return(NULL)}
    
    membership <- trainMembership()
    membership <- sort(membership)
    choices <- unique(membership)
    
    out <- list()
    out <- list(out, selectInput("groupNumber", "Select Terminal Node to Summarize", 
                                 choices=membership, selected=membership[1]))
    
    return(tagList(out))
    
  })
  
  output$compareViolin <- renderPlot({
    
    if(is.null(trainMembership())){ return(NULL)}
    
    membership <- factor(trainMembership())
    outData <- trainData() %>% 
      mutate(membership = membership) 
    
    ggplot(outData, aes_string(x="membership", y=input$compareVar, fill="membership")) + geom_violin()
    
  })
  
  output$groupSummary <- renderPrint({
    if(is.null(input$groupNumber)){return(NULL)}
    if(is.null(treeObj())){return(NULL)}
    
    membership <- trainMembership()
    
    groupNum <- input$groupNumber
    groupData <- trainData() %>% mutate(membership = membership) %>% 
      dplyr::filter(membership == groupNum) %>% select(-membership)
    
    print(summary(groupData))
    
  })
  
  output$groupTable <- renderTable({
    membership <- trainMembership()
    table(membership, trainData()[[outcomeVar]])
  })
  
  output$testResponse <- renderPrint({
    
    if(is.null(treeObj())){return(NULL)}
    
    predictions <- predict(treeObj(), newdata=testData())
    outTab <- table(predictions, testData()[[outcomeVar]])
    
    accuracy <- sum(diag(outTab))/sum(outTab) * 100
    accuracy <- signif(accuracy, digits = 4)
    
    print(paste0("Accuracy of Model: ", accuracy, "%"))
    print(outTab)
    
  })
  
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Previous",
                                               "skipLabel"="Done"))
  )
  
})

shinyApp(ui = ui, server = server)
