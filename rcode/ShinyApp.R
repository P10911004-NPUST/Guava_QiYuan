if(!require(shiny)){install.packages("shiny")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(agricolae)){install.packages("agricolae")}
if(!require(car)){install.packages("car")}
if(!require(FSA)){install.packages("FSA")}

se <- function(x){
  sd(x, na.rm = T)/sqrt(length(x))
}

mean2 <- function(x){
  mean(x, na.rm = T)
}

ui <- fluidPage(
  titlePanel(h1("ANOVA")),
  
  fluidRow(
    column(4, fileInput("upload", "Upload a (*.csv) file", accept = ".csv")),
    column(4, selectInput("response", "Response variable (Y):", choices = "NULL")),
    column(4, selectInput("treat", "Treatment variable (X):", choices = "NULL"))
  ),
  
  tags$hr(),
  
  fluidRow(
    column(6, plotOutput("count")),
    column(6, plotOutput("hist"))
  ),
  
  tags$hr(),
  
  fluidRow(
    column(3, textOutput("missing")),
    column(4, textOutput("shapiro")),
    column(5, textOutput("levene"))
  ),
  
  tags$hr(),
  
  fluidRow(
    sidebarPanel(
      verbatimTextOutput("pre_hoc"),
      verbatimTextOutput("post_hoc"),
      width = 6
    ),
    column(6, plotOutput("boxplot"))
  ),
  
  
  tags$hr(),
  
  fluidRow(
    column(12, dataTableOutput("desc_table")),
  ),
  
  
  tags$hr(),
  
  dataTableOutput("table"),
  
) 

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 10*1024^2)
  
  inFile <- reactive({input$upload})
  rawdata <- reactive({
    read_csv(inFile()$datapath, locale = locale(encoding = 'UTF-8'))
    })
  
  response <- reactive({
    y <- rawdata()[[input$response]]
    y <- y[which(y >= 0 | y < 0)]
    y
    })
  
  treat <- reactive({
    x <- rawdata()[[input$treat]]
    x <- x[which(rawdata()[[input$response]] >= 0 | 
                   rawdata()[[input$response]] < 0)]
    })
  
  df <- reactive({
    df0 <- data.frame(y = response(),
                      x = treat())
    df0
  })
  
  lin_mod <- reactive({
    aov(response() ~ as_factor(treat()))
    })
  
  desc_data <- reactive({
    df() |> 
    group_by(x) |> 
    summarise(mean_y = mean2(y),
              se_y = se(y),
              count_y = length(na.omit(y)))
    })
  
  missing_value <- reactive({
    length(rawdata()[[input$response]]) > length(response())
  })
  
  shapiro <- reactive({
    shapiro.test(response())$p.value
  })
  
  levene <- reactive({
    leveneTest(lin_mod())[1, 3]
  })
  
  pre_hoc <- reactive({
    if(shapiro() > 0.05){
      pre_hoc2 <- oneway.test(response() ~ treat(), var.equal = (levene() > 0.05))
    }else{
      pre_hoc2 <- kruskal.test(response() ~ treat())
    }
    pre_hoc2
  })
  
  post_hoc <- reactive({
    df <- data.frame(x = as_factor(treat()),
                     y = response())
    lin_mod <- aov(y ~ x, data = df)
    
    if(pre_hoc()$p.value < 0.05 & shapiro() < 0.05){
      post_hoc2 <- dunnTest(y ~ as_factor(x), data = df, method = "bonferroni")$res
      post_hoc2["P.adj"] <- round(post_hoc2["P.adj"], 4)
      post_hoc2 <- as.data.frame(post_hoc2) |> 
        arrange(P.adj)
    }else{
      if(pre_hoc()$p.value < 0.05 & shapiro() > 0.05 & levene() < 0.05){
        post_hoc2 <- scheffe.test(lin_mod, "x")$groups
      }else{
        if(pre_hoc()$p.value < 0.05 & shapiro() > 0.05 & levene() > 0.05){
          post_hoc2 <- LSD.test(lin_mod, "x", p.adj = "none")$groups
        }else{
          post_hoc2 <- scheffe.test(lin_mod, "x")$groups
        }
      }
    }
    post_hoc2
  })
  
  observeEvent(input$upload, {
    updateSelectInput(session, "response", choices = colnames(rawdata()))
    updateSelectInput(session, "treat", choices = colnames(rawdata()))
  })
  
  output$table <- renderDataTable(
    rawdata(),  options = list(pageLength = 5)
  )
  
  output$count <- renderPlot({
    barplot(table(treat()), xlab = names(rawdata()[input$treat]), ylab = "Cases")
  })
  
  output$hist <- renderPlot({
    hist(response(), xlab = names(rawdata()[input$response]), main = NULL)
  })
  
  output$boxplot <- renderPlot({
    boxplot(response() ~ treat(), 
            xlab = names(rawdata()[input$treat]),
            ylab = names(rawdata()[input$response]))
  })
  
  output$missing <- renderText({
    paste0("Missing value(s): ", missing_value())
  })
  
  output$shapiro <- renderText({
    if(shapiro() > 0.05){
      shapiro2 <- "Pass"
    }else{
      shapiro2 <- "Fail"
    }
    paste0("Shapiro-Wilk test (Normality): ", shapiro2)
  })
  
  output$levene <- renderText({
    if(levene() > 0.05){
      levene2 <- "Pass"
    }else{
      levene2 <- "Fail"
    }
    paste0("Levene's test (Homogeneity of variance): ", levene2)
  })

  output$desc_table <- renderDataTable(
    desc_data(),  options = list(pageLength = 5)
  )
  
  
  output$pre_hoc <- renderPrint({
    pre_hoc()
  })
  
  output$post_hoc <- renderPrint({
    post_hoc()
  })
}

shinyApp(ui, server)
