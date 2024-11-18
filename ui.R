# ui.R
ui <- fluidPage(
  titlePanel("LLM-E 😺"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range",
                     "Select Date Range:",
                     start = min(dummy_data$date),
                     end = max(dummy_data$date)),
      
      selectInput("product",
                  "Banking Product:",
                  choices = c("All", unique(dummy_data$product))),
      
      hr(),
      
      # Product Summary
      h4("Product Summary"),
      verbatimTextOutput("product_summary"),
      
      hr(),
      
      # Claude Assistant Section
      h4("Banking Product Advisor"),
      textAreaInput("user_query", 
                    "Ask about product performance:",
                    placeholder = "e.g., 'Which product has the highest customer satisfaction?' or 'Compare interest paid across products'",
                    height = "100px"),
      actionButton("send_query", "Ask  😺", 
                   class = "btn-primary"),
      
      # Loading spinner for AI response
      conditionalPanel(
        condition = "input.send_query % 2 == 1 && !output.ai_response",
        tags$div(
          class = "text-center mt-3",
          tags$div(
            class = "spinner-border text-primary",
            tags$span(class = "visually-hidden", "Loading...")
          ),
          tags$p("Getting insights...", class = "mt-2")
        )
      ),
      
      # Display AI Response
      uiOutput("ai_response")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Product Performance",
                 fluidRow(
                   column(6, plotOutput("balance_trend")),
                   column(6, plotOutput("customer_metrics"))
                 ),
                 hr(),
                 DTOutput("product_comparison")),
        
        tabPanel("Transaction Analysis",
                 plotOutput("transaction_patterns"),
                 DTOutput("transaction_summary")),
        
        tabPanel("Customer Insights",
                 fluidRow(
                   column(6, plotOutput("age_distribution")),
                   column(6, plotOutput("satisfaction_plot"))
                 )),
        
        tabPanel("AI Analysis",
                 textAreaInput("analysis_query", 
                               "Request Product Analysis:",
                               placeholder = "e.g., 'Compare performance metrics across products' or 'Analyze customer demographics and satisfaction'",
                               height = "100px"),
                 actionButton("run_analysis", "Generate Analysis", 
                              class = "btn-primary"),
                 
                 # Loading spinner for analysis
                 conditionalPanel(
                   condition = "input.run_analysis > 0 && !output.analysis_result",
                   tags$div(
                     class = "text-center mt-3",
                     tags$div(
                       class = "spinner-border text-primary",
                       tags$span(class = "visually-hidden", "Loading...")
                     ),
                     tags$p("Generating analysis...", class = "mt-2")
                   )
                 ),
                 
                 # Results section
                 div(style = "margin-top: 20px",
                     uiOutput("analysis_result"))
        )
      )
    )
  ),
  
  # Add required CSS for spinners
  tags$head(
    tags$style(HTML("
      .spinner-border {
        display: inline-block;
        width: 2rem;
        height: 2rem;
        vertical-align: text-bottom;
        border: 0.25em solid currentColor;
        border-right-color: transparent;
        border-radius: 50%;
        animation: spinner-border .75s linear infinite;
      }
      @keyframes spinner-border {
        to { transform: rotate(360deg); }
      }
    "))
  )
)