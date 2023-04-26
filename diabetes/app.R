library(shiny)
library(randomForest)
library(caret)
library(ggplot2)
library(shinythemes)
library(pROC)
library(magrittr)
library(dplyr)
library(tidyr)
library(MLmetrics)

# Load the trained model
model <- readRDS("model.rds")
df_diabetes = read.csv("diabetes.csv")

#for visualziation
#list_col_names = colnames(df_diabetes)
#ist_cols_names = list_cols_names[! list_cols_names %in% c('Diabetes_012')]

TestSet <-read.csv("testing.csv",header = TRUE)
TestSet<-TestSet[,-1]

# Define UI for app
ui <- navbarPage(theme = shinytheme("cerulean"),
  
  #Introduction
  #________________________________________________________________________________________________________________________________________
  title = "Diabetes Predictor",
  tabPanel("Dataset",
           tags$h2("Diabetes Health indicator Dataset"),
           br(),
           tags$p("For this project, I chose this dataset as I wanted to create an application that can server real life purpose."),
           br(),
           tags$p("This app uses a random forest model to predict whether a person has diabetes or not based on a set of input variables."),
           br(),
           tags$p("The input variables consist of Age, Sex, High Blood Pressure, High Cholestrol levels,if they have had any heart diseases or an heart attack, do they do enough physical activity, 
           do they reguarly consume fruits and vegetables, do they consumer alchohol frequently and get some idea about their general health and physical health", style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
           br(),
           tags$p("Diabetes Health indicator Dataset as the following columns"),
           br(),
           tableOutput("data_table"),
           br(),
           tags$p("Out of these, the best features were selected based on the visuzaliation that we will see in the next pages."),
           br(),
           tags$a("Diabetes Health Indicators Dataset", href = "https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset"),
           tags$span(" from Kaggle"),
           br(),
          

  p(em("Developed by"),
    br("Nitaant Jay Vyas"),
    style="text-align:center; font-family: times"),
  ),
  
  #Visualization_____________________________________________________________
  tabPanel(
    "Distribution of Data",
    fluidPage(
      fluidRow(
        column(
          width = 12,
          offset = 0,
          style = "background-color: #f5f5f5; border: 1px solid #ccc; border-radius: 3px; padding: 10px;",
          selectInput(
            "var",
            "Select a variable:",
            choices = colnames(df_diabetes)[2:22],
            selected = colnames(df_diabetes)[2]
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          offset = 0,
          style = "background-color: #fff; border: 1px solid #ccc; border-radius: 3px; padding: 10px;",
          plotOutput("plot")
        )
      )
    )
  ),
  
  
  #Algorithm
  #________________________________________________________________________________________________________________________
  tabPanel("Random Forest Model",
           tags$h2("Random Forest"),
           tags$p("Random forest is a type of supervised learning algorithm used for both classification and regression tasks. It is an ensemble learning method that operates by constructing multiple decision trees at training time and outputting the class that is the mode of the classes (classification) or mean prediction (regression) of the individual trees."),
           tags$p("Each decision tree is built using a subset of the training data and a random subset of the input variables. By averaging over multiple trees, random forest helps to reduce overfitting and improves the accuracy of predictions on new data."),
           tags$h3("The model could be represented mathematically as:"),
           tags$p("P(diabetes | HighBP, HighChol, HeartDiseaseorAttack, PhysActivity, Fruits, Veggies, HvyAlcoholConsump, GenHlth, PhysHlth, Sex, Age)

where P(diabetes) is the probability of the individual having diabetes given their other features.",
           style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
           tags$p("The optimal values of these hyperparameters are typically found using cross-validation techniques, such as grid search or random search."),
           br(),"These paremeters were chosen because the patients have an idea about these features and as there were 22 features originally, it took an extremely long time to compute.",
           tags$h3("Random forest is a good choice for this particular diabetes prediction system for several reasons:"),
           #tags$li("The dataset may have imbalanced classes, with more individuals without diabetes than with diabetes. Random forest can handle imbalanced classes by adjusting the weights of the classes."),
           tags$ul(
            
             tags$li("The dataset may have imbalanced classes, with more individuals without diabetes than with diabetes. Random forest can handle imbalanced classes by adjusting the weights of the classes."),
             tags$li("Random forest can handle interactions between features and non-linear relationships between features and the outcome variable."),
             tags$li("Random forest is generally robust to outliers and missing data."),
        
           
           style="text-align:justify;color:black;background-color:#B3E6B3;padding:15px;border-radius:10px"),
           
  
  
          
  ),
  #Model
  tabPanel("Model Performance",
          
                 tabPanel("Model Summary",
                          h3("Model Summary"),
                          verbatimTextOutput("model_summary"),
                          h3("Accuracy:"),
                          verbatimTextOutput("accuracy"),
                          h3("Number of Trees: "),
                          verbatimTextOutput("ntree"),
                          #h3("Precision:"),
                          #verbatimTextOutput("precision"),
                          #h3("Recall:"),
                          #verbatimTextOutput("recall"),
                          #h#3("f1 Score:"),
                          #verbatimTextOutput("f1_score")
                 
                          style="text-align:justify;color:black;background-color:  #B3CCE6;padding:15px;border-radius:10px" )
            
  ),
  
  

  
  #Predictor Page
  #______________________________________________________________________________________________________________________________________________
    tabPanel("Predictor",
  sidebarLayout(
    sidebarPanel(
     sliderInput("Age", "Age:", min = 0, max = 90, value = 30),
      selectInput("Sex", "Sex:", choices = c("Male", "Female")),
      selectInput("HighBP", "High Blood Pressure:", choices = c("No", "Yes")),
      selectInput("HighChol", "High Cholesterol:", choices = c("No", "Yes")),
      selectInput("HeartDiseaseorAttack", "Heart Disease or Attack:", choices = c("No", "Yes")),
      selectInput("PhysActivity", "Physical Activity:", choices = c("No", "Yes")),
      selectInput("Fruits", "Fruits:", choices = c("No", "Yes")),
      selectInput("Veggies", "Vegetables:", choices = c("No", "Yes")),
      selectInput("HvyAlcoholConsump", "Heavy Alcohol Consumption:", choices = c("No", "Yes")),
      sliderInput("GenHlth", "General Health:", min = 1, max = 5, value = 4),
      sliderInput("PhysHlth", "Physical Health:", min = 0, max = 30, value = 5),
      br(),
      actionButton("submit", "Submit")
      #actionButton("reset", "Reset")
    ),
    mainPanel(
      h3("Prediction Results:"),
      verbatimTextOutput("prediction"),
      tags$ul(
        style = "list-style-type:none;",
        wellPanel(
          style = "background-color: #f5f5f5; border: 1px solid #ccc; border-radius: 3px; padding: 10px;",
          tags$li("Age : Enter your age")
        ),
        wellPanel(
          style = "background-color: #f5f5f5; border: 1px solid #ccc; border-radius: 3px; padding: 10px;",
          tags$li("Sex : Choose what Sex are you")
        ),
        wellPanel(
          style = "background-color: #f5f5f5; border: 1px solid #ccc; border-radius: 3px; padding: 10px;",
          tags$li("High Blood Pressure : Yes if you have high blood")
        ),
        wellPanel(
          style = "background-color: #f5f5f5; border: 1px solid #ccc; border-radius: 3px; padding: 10px;",
          tags$li("High Cholestrol: Yes if you have high Cholestrol")
        ),
        wellPanel(
          style = "background-color: #f5f5f5; border: 1px solid #ccc; border-radius: 3px; padding: 10px;",
          tags$li("Heart Disease or Attack : If you have experienced either of those select yes")
        ),
        wellPanel(
          style = "background-color: #f5f5f5; border: 1px solid #ccc; border-radius: 3px; padding: 10px;",
          tags$li("Physical Activity : Are you physically active?")
        ),
        wellPanel(
          style = "background-color: #f5f5f5; border: 1px solid #ccc; border-radius: 3px; padding: 10px;",
          tags$li("Fruits : Do you have any fruits every day?")
        ),
        wellPanel(
          style = "background-color: #f5f5f5; border: 1px solid #ccc; border-radius: 3px; padding: 10px;",
          tags$li("Vegetables : Do you have any vegetables every day")
        ),
        wellPanel(
          style = "background-color: #f5f5f5; border: 1px solid #ccc; border-radius: 3px; padding: 10px;",
          tags$li("Heavy Alcohol Consumption : Do you consume alcohol multiple times a week?")
        ),
        wellPanel(
          style = "background-color: #f5f5f5; border: 1px solid #ccc; border-radius: 3px; padding: 10px;",
          tags$li("General Health : Rate your General Health from 1 to 5, 1 being the best")
        ),
        wellPanel(
          style = "background-color: #f5f5f5; border: 1px solid #ccc; border-radius: 3px; padding: 10px;",
          tags$li("Physical Health : Have you had any physical issues in the last 30 days?")
        )
      ),
        tags$p(
          "",
          tags$b("Based on the input provided by the user we can easily predict if they have diabetes already or have high chance of having it the near future."),
          ""
        )
        
      )
    )
    ),
  tabPanel("Description",
          
                      p(strong("What data you collected?"),
                        br(),
                        "The dataset was downloaded from Kaggle.",
                        tags$a("Diabetes Health Indicators Dataset", href = "https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset"),
                        br(),
                        "This dataset includes features relating to diabetes", 
                        br(),
                        br(),
                        "This dataset consists of 21 features and the target variable, Diabetes column. 
                                     There are about 250K data instances and a file size of 22mb.", 
                        
                        br(),
                        br(),
                        "Following are the column distributions:",
                        
                        br(),
                        br(),
                        "1. TARGET column: ",
                        br(),
                        "    Whether the Patient has Diabetes, Borderlien diabetes or No diabetes",
                        
                        br(),
                        br(),
                        "2. CATEGORICAL columns: ",
                        br(),
                        "    2.1. What is their Age.",
                        br(),
                        "    2.2. What Sex are they?",
                        br(),
                        "    2.3. Do they have High Blood Pressure?",
                        br(),
                        "    2.4. Do they have high Cholestrol Levels?",
                        br(),
                        "    2.5. Have they had their cholestrol checked recently?",
                        br(),
                        "    2.6. What is their BMI?",
                        br(),
                        "    2.7. Do they smoke frequently? ",
                        br(),
                        "    2.8. Have they ever had a stroke?",
          
                        br(),
                        "2.9 Have they had a heart disease or heart attack? ",
                        br(),
                        "  2.10 Do they have access to healthcare?",
                        br(),
                        "  2.11 How is their General Health?",
                        br(),
                        "   2.12 How is they Mental Health?",
                        br(),
                        "   2.13 How is their Physical Health?",
                        br(),
                        "   2.14 Do they have difficult walking",
                        br(),
                        "   2.15 What is their education?",
                        br(),
                        "   2.16 Do they have high income?",
                        br(),
                        "    2.17 Do they Consume fruits regularly?",
                        br(),
                        "    2.18 Do they consumer vegetables regularly?",
                        br(),
                        "    2.19 Do they consumer alcohol regularly?",
                        br(),
                        "    2.20How much physical activity they did in the last 30 days",
                        
                        br(),
                        "2.21Can they afford doctor?",
                        br(),
                        style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                      br(),
                      
                      p(strong("Why this topic is interesting or important to you?"),
                        br(),
                        "I always have had great interest in healthcare and want to work in a field closely related to it. ",
                        br(),
                        "I have known a lot of people who have had undiagnosed diabetes from a young age and would like to create an opportunity for someone to work towards early diagnosis of it.",
    
                        
                        br(),
                        "One of the reasons for choosing this dataset is that it is an unbalanced dataset and I wanted to experiment with how good random forest algorithm is when considering multilevel classifcation of an imbalanced dataset.",
                        
                        br(),
                        "Machine Learning can be an extremely useful tool in diagnosing illnesses. Aside from this illness, there are many other diseases such as Alzheimer's, heart failures, different types of cancer and pneumonia which are already being diagnosed using Machine Learning. ",
                        
                        br(),
                        br(),
                        
                        style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
                      ),
                      br(),
                      
                      p(strong("How did you analyze the data?"),
                        br(),
                        "Following the Data Science Project Cycle, I started with a problem and the possible solutions for it. After brainstorming for a few days, I finalized this dataset.",
                        br(),

                        "I created a dynamic graph that would take in the categorical features and plot them against the three classes : Diabetes, No Diabetes, And Borderline Diabetes.",
                        br(),
                        "I finalized the best features after considering the graphs as well as considering from a users point of view. Features such as BMI, Income, Education even though usefull were not as important as other features and hence were eliminated.",
                        br(),
                        style="text-align:justify;color:black;background-color:#E6B3B3;padding:15px;border-radius:10px"),
                      br(),
                      
                      
                      p(strong("What did you find in the data?"),
  
                        br(),
                        "Following are the multiple observations regarding this dataset:",
                        br(),
                        br(),
                        "1. Most of the Diabetic Patients Have never had a heart disease or a stroke.",
                        br(),
          
                        "2. There are more number of non Diabetics having higher BP and high Cholestrol than diabetics",
                        
                        br(),
                        "3.After being diagnosed with diabetes, people become more concerned about their health and get regular checkups which include cholestrol check and regular doctor checkups.",
                        br(),
                        "4.Most of the Diabetics refrain from physical activity and generaly have 0 to none physical activity ",
                        br(),
                        "5.Their mental health, physical health and general health is however not bad.",
                        br(),
                        "6. There are more female diabetics than male.",
                        br(),
                        "7. There are large number of teenagers and young adult who have diabetes which gradually decreases and than increases as they get older.",
                        
                        style="text-align:justify;color:black;background-color:#B3E6B3;padding:15px;border-radius:10px"),
                      
                      width=8),
                    br()
           )
           
           
         

           
 

#_______________________________________________________________________________________________________________________________________________________


# Define server logic for app
server <- function(input, output) {
  # Create data frame from user input
  input_data <- reactive({
    data.frame(
      Age = input$Age,
      Sex = ifelse(input$Sex == "Male", 1, 0),
      HighBP = ifelse(input$HighBP == "Yes", 1, 0),
      HighChol = ifelse(input$HighChol == "Yes", 1, 0),
      HeartDiseaseorAttack = ifelse(input$HeartDiseaseorAttack == "Yes", 1, 0),
      PhysActivity = ifelse(input$PhysActivity == "Yes", 1, 0),
      Fruits = ifelse(input$Fruits == "Yes", 1, 0),
      Veggies = ifelse(input$Veggies == "Yes", 1, 0),
      HvyAlcoholConsump = ifelse(input$HvyAlcoholConsump == "Yes", 1, 0),
      GenHlth = input$GenHlth,
      PhysHlth = input$PhysHlth
    )
  })
  
  
  
  
  # Dataset Page
  # Print the first 10 rows of the datset on the intro page
  output$data_table <- renderTable({
    head(data, n = 10)
  })

  
  
  #Predictor Page Server
  #_______________________________________________________________________________________________________________________________________________
  # Make the prediction using the trained model
  prediction <- eventReactive(input$submit, {
    predict(model, input_data())
  })
  
  # Return the prediction to the UI
  output$prediction <- renderPrint({
    paste("Based on the information you provided, the predicted diabetes status is:", prediction())
  })

  
  
#Visualization
 # ______________________________________________________________________________________________________________________________________________________________
  output$plot <- renderPlot({
    df_plot <- df_diabetes %>%
      count(!!sym(input$var), Diabetes_012) %>%
      pivot_wider(names_from = Diabetes_012, values_from = n, values_fill = 0) %>%
      mutate_at(vars(-all_of(input$var)), as.integer) %>%
      mutate(!!sym(input$var) := as.character(!!sym(input$var)))
    
    ggplot(df_plot, aes_string(x = input$var)) +
      geom_bar(aes(y = `0`, fill = "No Diabetes"), stat = "identity", position = position_dodge()) +
      geom_bar(aes(y = `1`, fill = "Borderline Diabetes"), stat = "identity", position = position_dodge()) +
      geom_bar(aes(y = `2`, fill = "Diabetes"), stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = c("#F8766D", "#00BFC4", "#619CFF"), name = "Diabetes") +
      labs(x = input$var, y = "Count", title = " Distribution by Diabetes",
           subtitle = paste("No Diabetes:", sum(df_plot$`0`),
                            "| Borderline Diabetes:", sum(df_plot$`1`),
                            "| Diabetes:", sum(df_plot$`2`))) +
      scale_x_discrete(labels = c("No", "Yes")) +
      theme_minimal()
  })
  #Modelling___________________________________________________________________________________________

  pretrained_model <- readRDS("model.rds")
  test_set = read.csv("testing.csv")
  output$model_summary <- renderPrint({
    summary(pretrained_model)
  })
 
  output$ntree <- renderText({
    ntree <- model$forest$ntree
    paste("Number of trees in the model:", ntree)
  })
  # Render accuracy output
  output$accuracy <- renderText({
    pred <- predict(pretrained_model, newdata = test_set, type = "response")
    acc <- mean(pred == test_set$Diabetes_012)
    paste0("Accuracy: ", round(acc * 100, 2), "%")
  })
  output$precision <- renderText({
    pred <- predict(pretrained_model, newdata = test_set, type = "response")
    precision <- MLmetrics::Precision(pred, test_set$Diabetes_012, positive = "Diabetes")
    precision2 <- MLmetrics::Precision(pred, test_set$Diabetes_012, positive = "No diabetes")
    precision3 <- MLmetrics::Precision(pred, test_set$Diabetes_012, positive = "Borderline diabetes")
    macro_precision <- mean(c(precision, precision2, precision3))
    paste0("Macro-averaged Precision: ", round(macro_precision * 100, 2), "%")
  })
  output$recall <- renderText({
    pred <- predict(pretrained_model, newdata = test_set, type = "response")
    recall <- recall(pred, test_set$Diabetes_012, positive = "Diabetes")
    macro_recall <- macroAvg(recall)
    paste0("Macro-averaged Recall: ", round(macro_recall * 100, 2), "%")
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

