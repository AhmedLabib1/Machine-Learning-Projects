install.packages("shiny")

library(tidyverse)
library(ggplot2)
library(arules)
library(shiny)
library(shinythemes)

########################################################################

# function to visualize compare cash and credit totals
compare_cash_credit <- function (data){
  
  # create data frame 
  V1_DATA <- data.frame(paymentType = c("Cash", "Credit"),
                        Totals = c(4957, 4878))
  
  # set colors of cash and credit
  bar_colors <- c("#1f77b4", "#ff7f0e")
  
  # create barplot
  barplot(height = V1_DATA$Totals,
          names.arg = V1_DATA$paymentType, 
          main = "Comparison of Cash and Credit Totals",
          xlab = "Payment Type", ylab = "Total",
          col = bar_colors,
          ylim = c(0, 6000))
  
  # Add legend
  legend("topright", 
         legend = unique(data$paymentType),
         fill = bar_colors,
         cex = 1.7)
  
  # Add grid lines
  grid()
}

########################################################################

# function to visualize Compare each age and sum of total spending
compare_age_total <- function(data){
  
  age_total <- aggregate(total ~ age, data = data, FUN = sum)
  
  
  # par(mar = c(bottom, left, top, right)) ==> adjust the margins
  par(mar = c(5, 5, 3, 2))
  
  # create scatter plot
  plot(age_total$age, 
       age_total$total, 
       xlab = "Age", 
       ylab = "Total Spending", 
       xlim = c(20, 60),
       main = "Age vs Total Spending",
       col = "blue",
       pch = 20)   
  
}

########################################################################

# function to visualize each city total spending and arrange it by total descending
city_total_spending <- function(data){
  
  city_total <- aggregate(total ~ city, data = data, FUN = sum)
  
  # Order the cities by total spending in descending order
  city_total <- city_total[order(city_total$total, decreasing = TRUE), ]
  
  # Convert city to factor and reorder levels based on total spending
  city_total$city <- factor(city_total$city, levels = city_total$city)
  
  # par(mar = c(bottom, left, top, right)) ==> adjust the margins
  par(mar = c(6, 5, 3, 2))
  
  barplot(height = city_total$total,
          names.arg = city_total$city,
          main = "each city total spending",
          col = "skyblue",
          border = "black",
          las = 2)
}

########################################################################

#function to compare Display the distribution of total spending
total_spending_distribution <- function(data){
  
  hist(data$total,
       main = "Distribution of Total Spending",
       xlab = "Total Spending",
       ylab = "Frequency",
       col = "skyblue", 
       border = "black")
  
}

################################################################################

dashboard <- function() {
  
  ui <- fluidPage(
    theme=shinythemes::shinytheme("sandstone"),
    titlePanel("Grocery Dataset Analysis"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Choose CSV File"),
        width="auto",
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Cleaned Data", 
                   fluidRow(
                     column(width = 6, align = "left",
                            actionButton("clean", "Clean Data"),
                            downloadButton("download", "Download Cleaned Data"),
                            style = "margin-top: 10px;"
                     ),
                   ),
                   fluidRow(
                     tableOutput("cleaned_table")
                   ),
                   
                   width="auto"
          ),
          tabPanel("Data Visualization",
                   fluidRow(
                     column(width = 12,
                            plotOutput("barplot1"))
                   ),
                   fluidRow(
                     column(width = 12,
                            plotOutput("scatterplot"))
                   ),
                   fluidRow(
                     column(width = 12,
                            plotOutput("barplot2"))
                   ),
                   fluidRow(
                     column(width = 12,
                            plotOutput("hist"))
                   ),
                   width="auto"
          ),
          tabPanel("Clustering",
                   sidebarPanel(
                     width = "auto",
                     sliderInput("clusters",
                                 "Number of clusters:" ,
                                 min = 2,
                                 max = 4,
                                 step = 1,
                                 value = 2),
                     actionButton("clustersCalc", "Claculate Clusters Grouping")),
                   style = "margin-top: 10px;",
                   mainPanel(
                     fluidRow(
                       column(width = 8, plotOutput("plot")),
                       column(width = 4, tableOutput("table"))
                     )
                   ),
                   
                   width="auto"
          ),
          tabPanel("Association",
                   sidebarPanel(
                     sliderInput(
                       "minSupport",
                       "Minimum Support:",
                       min = 0.001,
                       max = 1,
                       value = 0.001,
                       step = 0.001
                     ),
                     sliderInput(
                       "minConfidence",
                       "Minimum Confidence:",
                       min = 0.001,
                       max = 1,
                       value = 0.001,
                       step = 0.001
                     ),
                     actionButton("generateButton", "Generate Rules"),
                     style = "margin-top: 10px;"
                   ),
                   mainPanel(
                     fluidRow(
                       column(width = 6,  # Adjusted column width
                              h4("Number of Rules Generated:"),
                              verbatimTextOutput("numRules"),
                              h4("Generated Rules:"),
                              verbatimTextOutput("rules")
                       ),
                       column(width = 6,  # Adjusted column width
                              plotOutput("frequencyPlot")
                       )
                     )
                   )
          )
        )
      )
    )
  )
  
  # Define server
  server <- function(input, output) {
    
    cleaned_data <- eventReactive(input$clean, {
      req(input$file)
      data <- read.csv(input$file$datapath)
      
      # Ensure 'items' column exists
      if (!"items" %in% colnames(data)) {
        stop("The 'items' column is missing from the dataset.")
      }
      
      # Remove duplicate rows
      data <- unique(data)
      
      # Remove rows with NA values
      data <- na.omit(data)
      
      return(data)
    })
    
    
    ##############################################################################
    
    # Output cleaned data as a table
    output$cleaned_table <- renderTable({
      req(is.data.frame(cleaned_data()))
      cleaned_data()
    })
    
    ##############################################################################
    
    # Download handler for cleaned data CSV
    output$download <- downloadHandler(
      filename = function() {
        "cleaned_data.csv"
      },
      content = function(file) {
        write.csv(cleaned_data(), file, row.names = FALSE)
      }
    )
    
    ##############################################################################
    
    output$barplot1 <- renderPlot({
      compare_cash_credit(cleaned_data())
    })
    
    ##############################################################################
    
    output$scatterplot <- renderPlot({
      compare_age_total(cleaned_data())
    })
    
    ##############################################################################
    
    output$barplot2 <- renderPlot({
      city_total_spending(cleaned_data())
    })
    
    ##############################################################################
    
    output$hist <- renderPlot({
      total_spending_distribution(cleaned_data())
      
    })
    
    ##############################################################################
    
    clustered_data <- eventReactive(input$clustersCalc,{
      req(cleaned_data())
      cleaned <- cleaned_data()
      selected_columns <- cleaned[, c("customer", "age", "total")]
      summarized_data <- selected_columns %>% 
        group_by(customer, age) %>% 
        summarise(total_spending = sum(total), .groups = "drop") %>% 
        collect()
      return(summarized_data)
    })
    
    ##############################################################################
    
    output$table <- renderTable({
      req(clustered_data(),input$clustersCalc)
      clusters <- as.integer(input$clusters)
      if (is.null(clusters) || input$clusters == "") {
        return("Number of clusters has not been defined yet.")
      } else {
        k_means <- kmeans(clustered_data()[, c("age", "total_spending")], centers = clusters)
        final_data <- clustered_data() %>% mutate(cluster_number = k_means$cluster[match(age, clustered_data()$age)])
        final_data
      }
    })
    
    ##############################################################################
    
    output$plot <- renderPlot({
      # Perform k-means clustering
      clusters <- as.integer(input$clusters)
      k_means <- kmeans(clustered_data()[, c("age", "total_spending")], centers = clusters)
      
      
      # Add cluster labels to the data
      final_data <- clustered_data() %>% 
        mutate(cluster_number = k_means$cluster[match(age, clustered_data()$age)])
      
      # Generate a set of colors for the clusters
      cluster_colors <- rainbow(clusters)
      
      # Create a scatter plot of age vs. total spending, colored by cluster
      plot(final_data$age, final_data$total_spending, col = cluster_colors[final_data$cluster_number],
           xlab = "Age", ylab = "Total Spending",
           main = "Clusters of Customers Based on Age and Total Spending",
           pch = 15, cex = 1.2)  # Increase point size
      
      # Add legend with the same set of colors
      legend("topright", legend = unique(final_data$cluster_number),
             col = cluster_colors, pch = 15, title = "Cluster",
             cex = 0.8, bg = "white", border = "black")  # Customize legend appearance
    })
    
    
    ##############################################################################
    
    # Reactive function to generate association rules based on user input
    association_rules <- eventReactive(input$generateButton, {
      req(input$minSupport, input$minConfidence, cleaned_data())
      data <- cleaned_data()
      minSup <- input$minSupport
      minConf <- input$minConfidence
      
      # Convert the list of transactions to a transactions object
      transactions_list <- lapply(data$items, function(x) unlist(strsplit(x, ",")))
      transactions <- as(transactions_list, "transactions")
      
      # Set options for transactions (e.g., delimiter, format)
      options(digits = 2)
      options(datapath = "")
      options(read.transactions.check = FALSE) # Suppresses warning
      
      # Generate association rules
      rules <- apriori(
        transactions,
        parameter = list(
          support = minSup,
          confidence = minConf,
          minlen = 2
        )
      )
      
      return(rules)
    })
    
    
    # Render the number of rules generated
    output$numRules <- renderText({
      req(association_rules())
      paste("Total rules generated:", length(association_rules()))
    })
    
    # Render the generated rules
    output$rules <- renderPrint({
      req(association_rules())
      if (!is.null(association_rules())) {
        inspect(association_rules())
      } else {
        print("No rules generated yet.")
      }
    })
    
    
    # Render the frequency plot
    output$frequencyPlot <- renderPlot({
      req(cleaned_data(), input$generateButton, association_rules())
      data <- cleaned_data()
      if (!is.null(association_rules())) {
        transactions_list <- lapply(data$items, function(x) unlist(strsplit(x, ",")))
        transactions <- as(transactions_list, "transactions")
        
        # Generate item frequencies
        item_freq <- itemFrequency(transactions)
        top_items <- sort(item_freq, decreasing = TRUE)[1:10]
        
        # Plot the barplot with vertical x-axis labels
        barplot(top_items, las = 2, main = "Item Frequency", xlab = "Items", ylab = "Frequency", col = "skyblue")
      }
    })
    # Render the frequency plot
    output$frequencyPlot <- renderPlot({
      req(cleaned_data(), input$generateButton, association_rules())
      
      data <- cleaned_data()
      
      # Ensure 'items' column is present in the data
      if (!"items" %in% colnames(data)) {
        stop("The 'items' column is missing from the dataset.")
      }
      
      # Convert 'items' column to list of transactions
      transactions_list <- lapply(data$items, function(x) unlist(strsplit(x, ",")))
      
      # Check if the transaction list is not empty
      if (length(transactions_list) == 0) {
        stop("No transactions available.")
      }
      
      # Convert the list of transactions to a 'transactions' object
      transactions <- as(transactions_list, "transactions")
      
      # Calculate item frequencies
      item_freq <- itemFrequency(transactions)
      
      # Check if item_freq contains values
      if (length(item_freq) == 0) {
        stop("No items found in the transactions.")
      }
      
      # Get top 10 most frequent items
      top_items <- sort(item_freq, decreasing = TRUE)[1:10]
      
      # Plot the barplot with vertical x-axis labels
      barplot(top_items, las = 2, main = "Item Frequency", 
              xlab = "Items", ylab = "Frequency", col = "skyblue")
    })
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
}
dashboard()
