# server.R
server <- function(input, output, session) {
  # Filtered Data
  filtered_data <- reactive({
    data <- dummy_data %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2])
    
    if (input$product != "All") {
      data <- data %>% filter(product == input$product)
    }
    
    data
  })
  
  # Product Summary
  output$product_summary <- renderText({
    data <- filtered_data()
    
    if (input$product == "All") {
      # Summary for all products
      summary_stats <- data %>%
        summarise(
          total_balance = sum(last(balance)),
          total_customers = sum(unique(customer_count)),
          avg_satisfaction = mean(customer_satisfaction) * 100,
          total_interest = -sum(amount[transaction_type == "interest"]),
          total_fees = -sum(amount[transaction_type == "fee"])
        )
      
      paste0(
        "Total Balance: $", format(summary_stats$total_balance, big.mark=","), "\n",
        "Total Customers: ", format(summary_stats$total_customers, big.mark=","), "\n",
        "Avg Satisfaction: ", round(summary_stats$avg_satisfaction, 1), "%\n",
        "Interest Paid: $", format(summary_stats$total_interest, big.mark=","), "\n",
        "Fees Collected: $", format(summary_stats$total_fees, big.mark=",")
      )
    } else {
      # Summary for specific product
      product_data <- product_characteristics %>%
        filter(product_name == input$product)
      
      summary_stats <- data %>%
        summarise(
          current_balance = last(balance),
          customer_count = last(customer_count),
          satisfaction = last(customer_satisfaction) * 100,
          total_interest = -sum(amount[transaction_type == "interest"]),
          total_fees = -sum(amount[transaction_type == "fee"])
        )
      
      paste0(
        "Current Balance: $", format(summary_stats$current_balance, big.mark=","), "\n",
        "Number of Customers: ", format(summary_stats$customer_count, big.mark=","), "\n",
        "Interest Rate: ", product_data$interest_rate, "%\n",
        "Customer Satisfaction: ", round(summary_stats$satisfaction, 1), "%\n",
        "Interest Paid: $", format(summary_stats$total_interest, big.mark=","), "\n",
        "Fees Collected: $", format(summary_stats$total_fees, big.mark=","), "\n",
        "Min Balance Req.: $", format(product_data$min_balance, big.mark=","), "\n",
        "Monthly Fee: $", product_data$monthly_fee
      )
    }
  })
  
  # Balance Trend Plot
  output$balance_trend <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = balance, color = product)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Balance Trends by Product",
           x = "Date",
           y = "Balance ($)") +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme(legend.position = "bottom")
  })
  
  # Customer Metrics Plot
  output$customer_metrics <- renderPlot({
    filtered_data() %>%
      group_by(product) %>%
      summarise(
        customers = last(customer_count),
        satisfaction = last(customer_satisfaction)
      ) %>%
      gather(metric, value, -product) %>%
      ggplot(aes(x = product, y = value, fill = metric)) +
      geom_col(position = "dodge") +
      theme_minimal() +
      labs(title = "Customer Metrics by Product",
           x = "Product",
           y = "Value") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Product Comparison Table
  output$product_comparison <- renderDT({
    filtered_data() %>%
      group_by(product) %>%
      summarise(
        `Total Balance` = scales::dollar(last(balance)),
        `Customer Count` = format(last(customer_count), big.mark = ","),
        `Avg Transaction` = scales::dollar(mean(abs(amount))),
        `Interest Paid` = scales::dollar(-sum(amount[transaction_type == "interest"])),
        `Fees Collected` = scales::dollar(-sum(amount[transaction_type == "fee"])),
        `Satisfaction` = scales::percent(last(customer_satisfaction))
      ) %>%
      datatable(options = list(pageLength = 5))
  })
  
  # Transaction Patterns Plot
  output$transaction_patterns <- renderPlot({
    filtered_data() %>%
      group_by(product, transaction_type) %>%
      summarise(total_amount = sum(abs(amount)), .groups = "drop") %>%
      ggplot(aes(x = product, y = total_amount, fill = transaction_type)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Transaction Patterns by Product",
           x = "Product",
           y = "Total Amount ($)") +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Transaction Summary Table
  output$transaction_summary <- renderDT({
    filtered_data() %>%
      group_by(product, transaction_type) %>%
      summarise(
        `Transaction Count` = n(),
        `Total Amount` = scales::dollar(sum(abs(amount))),
        `Average Amount` = scales::dollar(mean(abs(amount))),
        .groups = "drop"
      ) %>%
      datatable(options = list(pageLength = 10))
  })
  
  # Age Distribution Plot
  output$age_distribution <- renderPlot({
    filtered_data() %>%
      group_by(product) %>%
      summarise(avg_age = last(avg_customer_age)) %>%
      ggplot(aes(x = product, y = avg_age, fill = product)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Average Customer Age by Product",
           x = "Product",
           y = "Age") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
  })
  
  # Satisfaction Plot
  output$satisfaction_plot <- renderPlot({
    filtered_data() %>%
      group_by(product) %>%
      summarise(satisfaction = last(customer_satisfaction)) %>%
      ggplot(aes(x = product, y = satisfaction, fill = product)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Customer Satisfaction by Product",
           x = "Product",
           y = "Satisfaction Score") +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
  })
  
  # AI Response State
  ai_response <- reactiveVal(NULL)
  
  # Sidebar Claude Assistant Logic
  observeEvent(input$send_query, {
    req(input$user_query)
    
    # Reset previous response while waiting
    ai_response(NULL)
    
    # Get current filtered data
    current_data <- filtered_data()
    
    # Calculate useful statistics
    stats_summary <- current_data %>%
      group_by(product) %>%
      summarise(
        total_balance = scales::dollar(last(balance)),
        customer_count = format(last(customer_count), big.mark = ","),
        satisfaction = scales::percent(last(customer_satisfaction)),
        interest_paid = scales::dollar(-sum(amount[transaction_type == "interest"])),
        fees_collected = scales::dollar(-sum(amount[transaction_type == "fee"])),
        avg_age = round(mean(avg_customer_age), 1)
      )
    
    # Prepare detailed context about the current data
    data_context <- sprintf(
      "You are a banking product analyst providing insights in a clear, conversational manner. Here's the current data:\n
      Date Range: %s to %s
      Products Analyzed: %s
      
      Key Metrics by Product:
      %s
      
      Important Notes:
      - All amounts should be formatted with $ and commas
      - Use percentages for satisfaction scores
      - Focus on meaningful trends and patterns
      - Provide specific recommendations when relevant
      - Keep responses concise and focused
      - Don't provide code, just insights in clear text",
      format(min(current_data$date), "%B %d, %Y"),
      format(max(current_data$date), "%B %d, %Y"),
      ifelse(input$product == "All", "All Products", input$product),
      paste(capture.output(print(stats_summary)), collapse = "\n")
    )
    
    # Create prompt for conversational analysis
    conversation_prompt <- paste0(
      "Based on the banking products data above, please provide a clear, conversational response to this question:\n\n",
      input$user_query,
      "\n\nFormat as a natural conversation, highlighting key insights and using proper formatting for numbers. Do not include any code in your response."
    )
    
    # Get Claude's response
    tryCatch({
      response <- chat$chat(paste(data_context, conversation_prompt))
      
      # Update response with formatted HTML
      ai_response(
        paste0(
          "<div style='white-space: pre-wrap; padding: 10px;'>",
          gsub("\n", "<br>", response),
          "</div>"
        )
      )
    }, error = function(e) {
      ai_response(
        "<div class='text-danger'>Sorry, I encountered an error analyzing the banking data. Please try again.</div>"
      )
    })
  })
  
  # AI Analysis Logic for the Analysis Tab
  observeEvent(input$run_analysis, {
    req(input$analysis_query)
    
    # Get current filtered data
    current_data <- filtered_data()
    
    # Prepare data context with strict instructions
    data_context <- paste0(
      "You are an R code generator for banking analysis. CRITICAL REQUIREMENTS:\n",
      "1. Return ONLY executable R code\n",
      "2. NO explanations, comments, or markdown\n",
      "3. NO text before or after the code\n",
      "\nAvailable data in 'current_data' dataframe:\n",
      "- date: transaction date\n",
      "- product: banking product name\n",
      "- transaction_type: deposit/withdrawal/interest/fee\n",
      "- amount: transaction amount\n",
      "- balance: running balance\n",
      "- customer_count: number of customers\n",
      "- avg_customer_age: average age of customers\n",
      "- customer_satisfaction: satisfaction score (0-1)\n",
      "\nRequired libraries are already loaded:\n",
      "- ggplot2, dplyr, scales\n",
      "\nFormatting requirements:\n",
      "- Use scales::dollar_format() for currency\n",
      "- Use scales::percent_format() for percentages\n",
      "- Use theme_minimal() for plots\n"
    )
    
    # Create prompt for analysis
    analysis_prompt <- paste0(
      data_context,
      "\n\nGenerate ONLY R code for this analysis: ", input$analysis_query,
      "\n\nIMPORTANT: Respond with ONLY the R code, nothing else. No explanations or text.")
    
    # Get response from Claude
    tryCatch({
      # Show loading message
      output$analysis_result <- renderUI({
        tags$div(class = "alert alert-info",
                 "Generating analysis...")
      })
      
      response <- chat$chat(analysis_prompt)
      
      # Simplified response cleaning
      clean_code <- response %>%
        gsub("```[rR]?|```", "", .) %>%
        gsub("Here's the code:|The code:|Here's the analysis:|R code:", "", .) %>%
        gsub("This code will.*$", "", .) %>%
        trimws()
      
      if (nchar(clean_code) > 0) {
        # Try parsing the code first to validate it
        tryCatch({
          parsed_code <- parse(text = clean_code)
          
          # Create a new environment for code execution
          env <- new.env(parent = globalenv())
          env$current_data <- current_data
          
          # Execute the code
          result <- eval(parsed_code, envir = env)
          
          # Render the result
          output$analysis_result <- renderUI({
            tagList(
              # Show the generated code first
              tags$div(class = "alert alert-secondary",
                       style = "font-family: monospace;",
                       "Generated code:",
                       tags$pre(clean_code)),
              
              # Then show the result
              if (inherits(result, "ggplot")) {
                plotOutput("analysis_plot")
              } else if (inherits(result, "data.frame")) {
                DTOutput("analysis_table")
              } else {
                tags$pre(capture.output(print(result)))
              }
            )
          })
          
          # Create the actual outputs
          if (inherits(result, "ggplot")) {
            output$analysis_plot <- renderPlot({
              result
            })
          } else if (inherits(result, "data.frame")) {
            output$analysis_table <- renderDT({
              result
            })
          }
          
        }, error = function(e) {
          output$analysis_result <- renderUI({
            tags$div(class = "alert alert-danger",
                     "Invalid R code generated. Please try rephrasing your request.",
                     tags$br(),
                     tags$code(as.character(e)))
          })
        })
      } else {
        output$analysis_result <- renderUI({
          tags$div(class = "alert alert-warning",
                   "No valid code was generated. Please try rephrasing your request.")
        })
      }
    }, error = function(e) {
      output$analysis_result <- renderUI({
        tags$div(class = "alert alert-danger",
                 "Error generating analysis. Please try again.",
                 tags$br(),
                 tags$code(as.character(e)))
      })
    })
  })
  
  # Display AI Response
  output$ai_response <- renderUI({
    if (is.null(ai_response())) {
      return(NULL)
    }
    HTML(ai_response())
  })
}