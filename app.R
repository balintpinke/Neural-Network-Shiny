#libraries
library(shiny)
library(shinydashboard)
library(neuralnet)
library(DT)
library(shinyWidgets)

#data
df <- read.csv2("NHL_cleaned.csv")

#check for NAs
apply(df, 2, function(x) sum(is.na(x)))
df = df[!is.na(df$Salary),]



#train neural network for predicting salary

ui <- dashboardPage(
  dashboardHeader(title = "Neural network Demo"),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu()),
  dashboardBody(

    box(title="Input parameters", width = 6, 
        column(width = 6,
    numericInput("training_proportion", "Training data proportion", 0.7, min=0.01, max = 0.99, step = 0.01)
    ),
        column(width = 6,
    numericInput("test_proportion", "Test data proportion", 0.3, min=0.01, max = 0.99, step = 0.01)
    ),
        column(width = 6,
    numericInput("threshold", "A numeric value specifying the threshold for the partial derivatives of the error function as stopping criteria", 0.1, min=0.01, max = 0.99, step = 0.01)
    ),
        column(width = 6,
    numericInput("hidden_one_layer", "The number of hidden neurons in one layer", 3, min=0, max = 7, step = 1)
    ),
        column(width = 6,
    numericInput("repetition", "The number of repetitions for the neural network's training", 3, min=1, max = 100, step = 1)
    ),
        column(width = 6,
    numericInput("stepmax", "Maximum steps for the training of the neural network", 100000, min=10000, max = 1000000, step = 1000)
    ),
    column(width = 6,
    pickerInput("cov_variables",  "Choose covariates", choices=names(df)[!names(df) %in% "Salary"], selected=names(df)[!names(df) %in% "Salary"], multiple = TRUE,  
                options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "None...",
                `select-all-text` = "Choose all",
                `none-selected-text` = "Choose variables"
    ))
    )
    
    ),
    box(title="Demo data", width = 6,
        dataTableOutput("table")
    ),
  
    actionButton("nn_start", "Start NN"),
    dataTableOutput("nn_table"),
    dataTableOutput("orig_pred_table"),
    dataTableOutput("cor_table"),
    plotOutput("nn_plot")
  )
)

server <- function(input, output) { 
  
  #demo data
  output$table <- renderDataTable(datatable(df, options = list(
                                                pageLength = 5,
                                                scrollX = 200,
                                                deferRender = TRUE,
                                                scroller = TRUE)))

                                     
  
  nn_list <- eventReactive(input$nn_start, {
    
    #normalization
    df=data.frame(scale(df))
    #training and test data
    order = sample(2, nrow(df), replace = TRUE, prob=c(input$training_proportion, input$test_proportion))
    # order = sample(2, nrow(df), replace = TRUE, prob=c(0.7, 0.3))
    train_df = df[order == 1,]
    test_df = df[order == 2,]
    
    # paste formula
    nn_formula = input$cov_variables
    nn_formula <- paste(nn_formula, collapse=' + ')
    nn_formula <- paste('Salary ~', nn_formula)
    nn_formula <- as.formula(nn_formula)
    
    threshold=input$threshold
    hidden=input$hidden_one_layer
    rep=input$repetition
    stepmax=input$stepmax
    # threshold=0.1
    # hidden=1
    # rep=3
    # stepmax=100000
    # 
      
    neural_network <- neuralnet(Salary ~ Age + Goal + Assist + Points + Plus_minus, 
                              # nn_formula,
                                data = train_df,
                                hidden = hidden,
                                threshold = threshold,
                                rep = rep,
                                stepmax = stepmax,
                                lifesign = "full")
    
    df <- data.frame(neural_network[["result.matrix"]])
    
    model_results <- predict(neural_network, test_df[1:8])
    orig_pred=data.frame(cbind(test_df$Salary, model_results))
    # predicted_strength <- model_results$net.result
    cor=data.frame(cor(model_results, test_df$Salary))

    list=list(df1 = df, df2 = orig_pred, df3 = cor)
    list
    # print(class(list[['df3']]))
    
    })
  
  
  
  output$nn_table <- renderDataTable(nn_list()[['df1']])
  
  output$orig_pred_table <- renderDataTable(nn_list()[['df2']])
  
  output$cor_table <- renderDataTable(nn_list()[['df3']])
    
  # output$nn_plot <- renderPlot(nn_list()[['df3']])
  
  }

shinyApp(ui, server)
