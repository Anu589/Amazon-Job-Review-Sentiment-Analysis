# Load required libraries (ensure all libraries are installed)
library(shinydashboard)
library(shiny)
library(wordcloud2)
library(ggplot2)
library(leaflet)
library(knitr)
library(readr)
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(tm)
library(RColorBrewer)
library(sf)
library(maps)
library(dplyr)
library(reshape2)
library(syuzhet)
library(lubridate)
library(tidygeocoder)
library(stm)
library(cowplot)
library(ggmap)
library(networkD3)
library(plotly)
library(jsonlite)
library(purrr)
library(httr)

# Read the dataset
world_cities=read.csv("worldcities.csv")
reviews_data <- read.csv("Amazon_job_reviews_(USA_ India)2008-2020.csv")
reviews_data$Date <- as.Date(reviews_data$Date, format = "%d %b %Y")

reviews_data$Location <- str_replace_all(reviews_data$Location, setNames(
  c('Bawal', 'Dwarka', 'Nagpur, Maharashtra, Maharashtra', 'Sangrur', 
    'Srinagar, Jammu and Kashmir, Jammu and Kashmir', 'Varanasi', 
    'Hyderabad', 'Aligarh, Uttar Pradesh, Uttar Pradesh', 'Hyderabad', 
    'Hapur', 'Ghatkopar', 'Campinas, Sao Paulo, Sao Paulo', 'Baran', 
    'Manesar', 'Malur, Karnataka, Karnataka', 'Monterrey, Nuevo Lean, Nuevo Lean', 
    'Kanpur, Uttar Pradesh, Uttar Pradesh', 'Kashipur, Uttaranchal, Uttaranchal', 
    'Kurali', 'Irinjalakuda', 'Agra, Uttar Pradesh, Uttar Pradesh', 'Am Purwa', 
    'Asansol, West Bengal, West Bengal', 'Azamgarh'), 
  c('Bāwal', 'Dwārka', 'Nāgpur, Maharashtra, Maharashtra', 'Sangrūr', 
    'Srīnagar, Jammu and Kashmir, Jammu and Kashmir', 'Vāranāsi', 
    'Hyderābād', 'Alīgarh, Uttar Pradesh, Uttar Pradesh', 'HyderÄbÄd', 
    'Hāpur', 'Ghātkopar', 'Campinas, São Paulo, São Paulo', 'Bārān', 
    'Mānesar', 'Mālūr, Karnataka, Karnataka', 'Monterrey, Nuevo León, Nuevo León', 
    'Kānpur, Uttar Pradesh, Uttar Pradesh', 'Kāshīpur, Uttaranchal, Uttaranchal', 
    'Kūrāli', 'Irinjālakuda', 'Āgra, Uttar Pradesh, Uttar Pradesh', 'Ām Purwa', 
    'Āsansol, West Bengal, West Bengal', 'Āzamgarh')))

reviews_data$CEO.Approval = tolower(reviews_data$CEO.Approval)
reviews_data$Recommended = tolower(reviews_data$Recommended)
reviews_data$Business.Outlook = tolower(reviews_data$Business.Outlook)

# Clean missing data for 'Location' and 'Position'
reviews_data <- reviews_data[!is.na(reviews_data$Location) & reviews_data$Location != "NaN", ]
reviews_data <- reviews_data[!is.na(reviews_data$Position) & reviews_data$Position != "NaN", ]

# Correctly converting the date format
reviews_data$Date <- as.Date(reviews_data$Date, format = "%d %b %Y")

# Fill missing values for ratings with column means
reviews_data <- reviews_data %>%
  mutate(
    `Work.Life.Balance` = ifelse(is.na(`Work.Life.Balance`), mean(`Work.Life.Balance`, na.rm = TRUE), `Work.Life.Balance`),
    `Career.Opportunities` = ifelse(is.na(`Career.Opportunities`), mean(`Career.Opportunities`, na.rm = TRUE), `Career.Opportunities`),
    `Compensation.and.Benefits` = ifelse(is.na(`Compensation.and.Benefits`), mean(`Compensation.and.Benefits`, na.rm = TRUE), `Compensation.and.Benefits`),
    `Senior.Management` = ifelse(is.na(`Senior.Management`), mean(`Senior.Management`, na.rm = TRUE), `Senior.Management`),
    `Culture.and.Values` = ifelse(is.na(`Culture.and.Values`), 0, `Culture.and.Values`),
    `Diversity.and.Inclusion` = ifelse(is.na(`Diversity.and.Inclusion`), 0, `Diversity.and.Inclusion`),
  )

# Sentiment Analysis Preparation
sentiment_data <- data.frame(reviews_data$pros, reviews_data$cons, reviews_data$Comment.for.company, reviews_data$advice.to.Management)
colnames(sentiment_data) <- c("pros", "cons", "Comment.for.company", "Advice.to.Management")

summary(sentiment_data)
# Replace empty values with placeholders
sentiment_data$pros[is.na(sentiment_data$pros) | sentiment_data$pros == ""] <- "No pros provided"
sentiment_data$cons[is.na(sentiment_data$cons) | sentiment_data$cons == ""] <- "No cons provided"
sentiment_data$Comment.for.company[is.na(sentiment_data$Comment.for.company) | sentiment_data$Comment.for.company == ""] <- "No Comment for company provided"



# Convert text to lowercase
sentiment_data$pros <- tolower(sentiment_data$pros)
sentiment_data$cons <- tolower(sentiment_data$cons)
sentiment_data$Comment.for.company <- tolower(sentiment_data$Comment.for.company)
sentiment_data$Advice.to.Management <- tolower(sentiment_data$Advice.to.Management)

scored_Advice.to.Management=get_sentiment(sentiment_data$Advice.to.Management)
scored_sentiment= data.frame(get_sentiment(sentiment_data$pros), get_sentiment(sentiment_data$cons), get_sentiment(sentiment_data$Comment.for.company))

colnames(scored_sentiment) = c("pros", "cons", "Comment.for.company")

summary(sentiment_data)

#Create a Combined DataFrame for Sentiment Scores
sentiment_long <- data.frame(
  date = reviews_data$Date,
  pros = scored_sentiment$pros,
  cons = scored_sentiment$cons,
  Comment.for.company= scored_sentiment$Comment.for.company
)

# Step 3: Create a Year Column for Grouping
sentiment_long$year <- year(sentiment_long$date)  # Extract the year

# Step 4: Summarize Data by Year
sentiment_summary <- sentiment_long %>%
  group_by(year) %>%
  summarise(
    avg_pros = mean(pros, na.rm = TRUE),
    avg_cons = mean(cons, na.rm = TRUE),
    avg_Comment.for.company= mean(Comment.for.company, na.rm = TRUE)
  )

# Step 5: Reshape the Data to Long Format for Plotting
sentiment_long_format <- sentiment_summary %>%
  pivot_longer(cols = c(avg_pros, avg_cons,avg_Comment.for.company), 
               names_to = "type", 
               values_to = "sentiment_score")
summary(reviews_data)

reviews_data$Year <- as.integer(format(as.Date(reviews_data$Date, format = "%d-%m-%Y"), "%Y"))

reviews_data$Current.employee <- as.logical(reviews_data$Current.employee)

reviews_data$Former.employee <- as.logical(reviews_data$Former.employee)
###################################################
# Step 3: Extract the city names from the 'Location' column
reviews_data$Location <- str_extract(reviews_data$Location, "^[^,]+")

# Step 4: Count the number of employees in each city
employee_count_by_city <- reviews_data %>%
  filter(!is.na(Location)) %>%  # Remove rows where city is NA
  count(Location)  # Count the number of employees per city

# Step 5: Match city names from reviews_data with the city_ascii in the city_data
matched_data <- employee_count_by_city %>%
  left_join(world_cities, by = c("Location" = "city_ascii"))

# Step 6: Create a new dataframe with city, employee count, latitude, and longitude
final_data <- matched_data %>%
  select(city, n, lat, lng)  # 'n' represents the employee count, adjust as needed for naming

# Step 7: View the resulting dataframe
head(final_data)

####################################################################
#UI of the Dashboard
ui <- dashboardPage(
  dashboardHeader(
    title = "Amazon Job Review Analytics (2008-2020)",
    titleWidth = 300
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "About", icon = icon("info-circle")),
      menuItem("Wordcloud", tabName = "wordcloud", icon = icon("cloud"),
               menuSubItem("Positions", tabName = "positions_wordcloud", icon = icon("briefcase")),
               menuSubItem("Pros", tabName = "pros_wordcloud", icon = icon("thumbs-up")),
               menuSubItem("Cons", tabName = "cons_wordcloud", icon = icon("thumbs-down")),
               menuSubItem("Comment for Company", tabName = "comment_for_company_wordcloud", icon = icon("comment-dots"))
      ),
      menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("smile")),
      
      menuItem("Rating Visualizations", tabName = "rating_visualizations", icon = icon("chart-bar"),
               menuSubItem("Ratings Chart", tabName = "ratings_chart", icon = icon("bar-chart")),
               menuSubItem("Correlation Heatmap", tabName = "correlation_heatmap", icon = icon("fire"))
      ),
      
      menuItem("Organizational Health", tabName = "Organizational_Health", icon = icon("users-cog")),
      menuItem("Advice to Management", tabName="advice_to_company_insights", icon=icon("lightbulb")),
      menuItem("Map", tabName = "location_of_employees", icon = icon("globe-americas"))
    )
  ),
  
  dashboardBody(
    
    tabItems(  # Wrap everything inside tabItems here
      
      # Introduction Tab
      tabItem(
        tabName = "About", 
        h2("Introduction to the Dataset"),
        p("This application analyzes employee reviews at Amazon from 2008 to 2020. The dataset includes a wide range of variables, providing insights into employee satisfaction, organizational health, and employee advice to company. The key features of the dataset are as follows:"),
        tags$ul(
          tags$li("A total of 29,495 rows of employee reviews."),
          tags$li("Geographic distribution predominantly from India and the USA.")
        ),
        h2("Dataset Description"),
        p("The table below provides a detailed description of the columns in the dataset:"),
        DT::dataTableOutput("dataset_description")
      ),
      
      # Wordcloud Tabs
      tabItem(tabName = "positions_wordcloud", 
              h2("Positions Wordcloud"),
              p("This word cloud displays the most frequent job positions mentioned in the reviews, giving insight into the roles most commonly associated with Amazon."),
              wordcloud2Output("positions_wordcloud_output")),
      
      tabItem(tabName = "pros_wordcloud", 
              h2("Pros Wordcloud"),
              p("This word cloud visualizes the most commonly mentioned positive aspects of working at Amazon, helping identify key strengths as highlighted by employees."),
              wordcloud2Output("pros_wordcloud_output")),
      
      tabItem(tabName = "cons_wordcloud", 
              h2("Cons Wordcloud"),
              p("This word cloud represents the most frequent negative comments or challenges faced by employees at Amazon, shedding light on areas for improvement."),
              wordcloud2Output("cons_wordcloud_output")),
      
      tabItem(tabName = "comment_for_company_wordcloud", 
              h2("Comments for Company Wordcloud"),
              p("This word cloud highlights the most frequently used words in employee comments regarding Amazon, revealing general sentiments and feedback about the company."),
              wordcloud2Output("comment_wordcloud_output")),
      
      # Sentiment Analysis Tab
      tabItem(tabName = "sentiment", 
              fluidRow(
                box(title = "Sentiment Analysis of Reviews", width = 12, solidHeader = TRUE, status = "primary",
                    p("This visualization provides an overview of the sentiment of pros, cons and comment for company, aggregating them individually over the years. The stacked bar chart for pros and cons and the trend line for comment for company, helps in understanding the general mood of employees based on their feedback."),
                    plotOutput("sentiment_overall_plot"))
              )
      ),
      
      # Ratings Chart Tab
      tabItem(tabName = "ratings_chart",
              fluidRow(
                box(
                  title = "Time-Series Ratings Analysis", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  p("This chart displays the average ratings given by employees over time(in years) across various categories, helping track trends in employee satisfaction."),
                  selectInput(
                    "rating_category", 
                    label = "Select a Rating Category:",
                    choices = c(
                      "Overall.rating", 
                      "Work.Life.Balance",
                      "Culture.and.Values",
                      "Diversity.and.Inclusion",
                      "Career.Opportunities",
                      "Compensation.and.Benefits",
                      "Senior.Management"
                    ),
                    selected = "Overall.rating"
                  ),
                  selectInput(
                    "employee_status", 
                    label = "Select Employee Status:",
                    choices = c("Current Employee", "Former Employee"),
                    selected = "Current Employee"
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Time-Series of Ratings", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  plotOutput("rating_time_series")
                )
              )
      ),
      
      # Correlation Heatmap Tab
      tabItem(tabName = "correlation_heatmap",
              fluidRow(
                box(
                  title = "Correlation Heatmap", 
                  width = 12, 
                  solidHeader = TRUE, 
                  status = "primary",
                  p("This heatmap visualizes the correlations between different rating categories. It helps identify which aspects of the workplace are related to each other in the eyes of employees."),
                  plotOutput("heatmap")
                )
              )
      ),
      
      # Organizational Health Tab
      tabItem(tabName = "Organizational_Health",
              fluidRow(
                box(title = "Organizational Health", width = 12, solidHeader = TRUE, status = "primary",
                    p("This radial chart provides an overview of Amazon's organizational health by showing employee satisfaction across various dimensions such as CEO Approval, Business Approval and Recommendation."),
                    selectInput(
                      "year_selected",
                      label = "Select Year:",
                      choices = unique(year(reviews_data$Date)),  # Assuming 'Year' column exists
                      selected = 2008
                    ),
                    plotlyOutput("radial_chart")
                )
              )
      ),
      tabItem(tabName = "advice_to_company_insights",
              h2("Insights from Advice to Management"),
              p("This section shows key insights based on the advice provided to the management by employees."),
              
              # First, create a fluidRow for the wordcloud and sentiment analysis, 
              # but set them to be full-width (stacked vertically)
              fluidRow(
                # Table for Advice to Management
                column(
                  width = 12,  # Ensure this takes the full width of the row
                  box(
                    title = "Top 20 Advice to Management",  # Change title to reflect table
                    width = NULL,  # This ensures the box uses full available width
                    solidHeader = TRUE, 
                    status = "primary",
                    p("This table shows the most frequent advice provided by employees to management."),
                    tableOutput("advice_wordcloud_output")  # This will display the table instead of word cloud
                  )
                ),                
                
                # Sentiment Analysis for Advice to Management
                column(
                  width = 12,  # Ensure this also takes the full width of the row
                  box(
                    title = "Sentiment Analysis of Advice to Management Over Time", 
                    width = NULL,  # This ensures the box uses full available width
                    solidHeader = TRUE, 
                    status = "primary",
                    p("This chart shows the sentiment of advice to management over time, helping track how employee feedback has evolved."),
                    plotOutput("sentiment_over_time_plot")  # Replace with your actual plot output ID
                  )
                )
              )
      ),
      
      
      
      # Map Tab
      tabItem(tabName = "location_of_employees",
              fluidRow(
                box(title = "Map of Employee Locations", width = 12, solidHeader = TRUE, status = "primary",
                    p("This map visualizes the geographic distribution of Amazon employees based on the locations mentioned in their reviews. It helps identify areas with significant employee presence."),
                    leafletOutput("map"),  # Note the comma here
                    plotOutput("combined_map")  # Corrected with a comma before it
                )
              )
      )
    )
  )
)



server <- function(input, output, session) {
  
  # Dataset description table
  output$dataset_description <- DT::renderDataTable({
    # Data frame with column names and descriptions
    dataset_description <- data.frame(
      Column = c(
        "ID", "Date", "Location", "Position", "Comments for Company",
        "Overall Ratings", "Work-Life Balance", "Culture & Values", 
        "Diversity & Inclusion", "Career Opportunities", 
        "Compensation and Benefits", "Senior Management", 
        "CEO Approval", "Recommendation Status", "Business Outlook",
        "Employment Type", "Timeline", "Pros", "Cons", 
        "Advice to Management", "Review URL"
      ),
      Description = c(
        "Unique Identifier",
        "Date of Review ",
        "Geographic Location ",
        "Job Title ",
        "Employee Feedback ",
        "Overall Job Satisfaction ",
        "Work-Life Balance Rating ",
        "Culture and Values Rating ",
        "Diversity and Inclusion Rating ",
        "Career Opportunities Rating ",
        "Compensation Rating ",
        "Senior Management Rating ",
        "Approval Rating for CEO ",
        "Employee Recommendation ",
        "Business Outlook Rating ",
        "Current or Former Employee ",
        "Length of Employment ",
        "Positive Feedback ",
        "Negative Feedback ",
        "Suggestions ",
        "Link to Full Review "
      ),
      Type= c(
        "(integer)",
        "(date)",
        "(character)",
        "(character)",
        "(character)",
        "(numeric)",
        "(numeric)",
        "(numeric)",
        "(numeric)",
        "(numeric)",
        "(numeric)",
        "(numeric)",
        "(numeric)",
        "(character)",
        "(numeric)",
        "(character)",
        "(character)",
        "(character)",
        "(character)",
        "(character)",
        "(character)"
      )
    )
    
    # Render the table as a DataTable
    DT::datatable(
      dataset_description,
      options = list(pageLength = 10, autoWidth = TRUE,order=list()),
      rownames = FALSE
    )
  })
  
  output$rating_time_series <- renderPlot({
    # Filter data based on employee status (Current or Former)
    filtered_data <- reviews_data %>%
      filter(Current.employee == (input$employee_status == "Current Employee"), 
             !is.na(Year))  # Ensure Year is not NA
    
    # Check if the selected rating category exists in the data
    selected_category <- input$rating_category
    
    # Ensure the column exists and is numeric
    if (selected_category %in% colnames(reviews_data)) {
      
      # Group by Year and calculate the average for the selected rating category
      filtered_data <- filtered_data %>%
        group_by(Year) %>%
        summarise(Avg_Rating = mean(get(selected_category), na.rm = TRUE)) %>%
        ungroup()  # Ungroup the result
      
      # Check if filtered data is not empty
      if (nrow(filtered_data) > 0) {
        
        # Time-series plot
        ggplot(filtered_data, aes(x = Year, y = Avg_Rating)) +
          geom_line(color = "blue", size = 1) +
          geom_point(color = "red", size = 2) +
          labs(
            title = paste("Time-Series Analysis of", selected_category, "for", input$employee_status),
            x = "Year",
            y = "Average Rating"
          ) +
          scale_x_continuous(
            breaks = seq(min(filtered_data$Year, na.rm = TRUE), max(filtered_data$Year, na.rm = TRUE), by = 1)
          ) +
          theme_minimal()
      } else {
        # If filtered_data is empty, return a message
        ggplot() + 
          geom_text(aes(x = 1, y = 1, label = "No data available for this selection"), size = 6) +
          theme_void()
      }
      
    } else {
      # If the selected category is not valid, return a message or empty plot
      ggplot() + 
        geom_text(aes(x = 1, y = 1, label = "Invalid Rating Category"), size = 6) +
        theme_void()
    }
  })
  
  output$radial_chart <- renderPlotly({
    
    # Reformat the data: assign scores to categorical responses
    reviews_data <- reviews_data %>%
      mutate(
        CEO.Approval.Score = case_when(
          CEO.Approval == "yes" ~ 1,   # Yes -> 1
          CEO.Approval == "no" ~ 0,    # No -> 0
          CEO.Approval == "may be" ~ 0.5, # Maybe -> 0.5
          TRUE ~ NA_real_              # Handle missing or unexpected values
        ),
        Recommended.Score = case_when(
          Recommended == "yes" ~ 1,   
          Recommended == "no" ~ 0,    
          Recommended == "may be" ~ 0.5, 
          TRUE ~ NA_real_              
        ),
        Business.Outlook.Score = case_when(
          Business.Outlook == "yes" ~ 1,   
          Business.Outlook == "no" ~ 0,    
          Business.Outlook == "may be" ~ 0.5, 
          TRUE ~ NA_real_              
        )
      )
    
    # Calculate proportions for each response type in each category
    proportions_data <- reviews_data %>%
      filter(Year == input$year_selected) %>%
      group_by(Year) %>%
      summarise(
        CEO.Yes = mean(CEO.Approval == "yes", na.rm = TRUE),
        CEO.No = mean(CEO.Approval == "no", na.rm = TRUE),
        CEO.Maybe = mean(CEO.Approval == "may be", na.rm = TRUE),
        Rec.Yes = mean(Recommended == "yes", na.rm = TRUE),
        Rec.No = mean(Recommended == "no", na.rm = TRUE),
        Rec.Maybe = mean(Recommended == "may be", na.rm = TRUE),
        Biz.Yes = mean(Business.Outlook == "yes", na.rm = TRUE),
        Biz.No = mean(Business.Outlook == "no", na.rm = TRUE),
        Biz.Maybe = mean(Business.Outlook == "may be", na.rm = TRUE)
      )
    
    # Reshape data for stacking
    radial_data <- data.frame(
      Category = rep(c("CEO Approval", "Recommended", "Business Outlook"), each = 3),
      Response = rep(c("Yes", "No", "Maybe"), times = 3),
      Value = c(
        proportions_data$CEO.Yes, proportions_data$CEO.No, proportions_data$CEO.Maybe,
        proportions_data$Rec.Yes, proportions_data$Rec.No, proportions_data$Rec.Maybe,
        proportions_data$Biz.Yes, proportions_data$Biz.No, proportions_data$Biz.Maybe
      )
    )
    
    # Assign specific color names for responses
    color_mapping <- c("Yes" = 'blue', "No" = 'red', "Maybe" = 'green')
    
    # Create a stacked radial chart
    plot_ly(
      radial_data,
      r = ~Value,
      theta = ~Category,
      type = 'barpolar',
      color = ~Response,  # Automatically creates a legend
      colors = color_mapping,  # Define color mapping
      marker = list(
        opacity = 0.6  # Adjust transparency
      )
    ) %>%
      layout(
        title = paste("Review Breakdown Year -", input$year_selected),
        polar = list(
          radialaxis = list(showticklabels = TRUE, ticks = "", range = c(0, 1)), # Normalize to [0, 1]
          angularaxis = list(showline = TRUE)
        ),
        legend = list(title = list(text = "Responses")),  # Add legend title
        showlegend = TRUE
      )
  })
  
  output$positions_wordcloud_output <- renderWordcloud2({
    positions_df <- reviews_data %>% count(Position) 
    wordcloud2(positions_df)
  })
  
  output$pros_wordcloud_output <- renderWordcloud2({
    pros_df <- sentiment_data%>%
      filter(pros != "#name?") %>%
      count(pros) %>%
      filter(n > 1)
    wordcloud2(pros_df)
  })
  
  output$cons_wordcloud_output <- renderWordcloud2({
    cons_df <- sentiment_data %>%
      filter(cons != "#name?") %>%  # Exclude rows where cons is "#name?"
      count(cons) %>%
      filter(n > 1)  # Keep only rows with count greater than 1
    
    wordcloud2(cons_df)
  })
  
  
  output$comment_wordcloud_output <- renderWordcloud2({
    comments_df <- sentiment_data%>%
      filter(Comment.for.company != "#name?")%>%
      count(Comment.for.company) %>%
      filter(n > 2)
    wordcloud2(comments_df)
  })
  
  output$sentiment_overall_plot <- renderPlot({
    ggplot(sentiment_summary, aes(x = year)) +
      geom_bar(aes(y = avg_pros, fill = "Pros"), stat = "identity", alpha = 0.7, color = "black") + 
      geom_bar(aes(y = avg_cons, fill = "Cons"), stat = "identity", alpha = 0.7, color = "black") + 
      geom_line(aes(y = avg_Comment.for.company, color = "Comment.for.company Trend"), size = 1.2) +
      geom_point(aes(y = avg_Comment.for.company, color = "Comment.for.company Trend"), size = 2) +
      scale_fill_manual(values = c("Pros" = "#C1FFC1", "Cons" = "#FF6A6A")) +
      scale_color_manual(values = c("Comment.for.company Trend" = "grey")) +
      scale_y_continuous(name = "Average Sentiment Score") +
      labs(title = "Sentiment Analysis: Pros and Cons Over Time", 
           x = "Year", fill = "Category", color = "Trend") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  output$heatmap <- renderPlot({
    # Select relevant columns and remove NA values
    data <- reviews_data %>%
      select(`Overall.rating`, `Work.Life.Balance`, `Career.Opportunities`, 
             `Compensation.and.Benefits`, `Senior.Management`, `Culture.and.Values`) %>%
      na.omit()
    
    # Calculate correlation matrix
    cor_matrix <- cor(data, use = "complete.obs")
    
    # Melt the correlation matrix into a long format
    cor_melted <- reshape2::melt(cor_matrix)
    
    # Plot the correlation heatmap
    ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                           midpoint = 0, limit = c(-1, 1), name = "Correlation") +
      theme_minimal() +
      labs(title = "Correlation Heatmap", x = "", y = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  # Create a word cloud for the advice to management
  output$advice_wordcloud_output <- renderTable({
    # Preprocess the text data for advice count
    advice_data <- reviews_data %>%
      filter(!is.na(advice.to.Management)) %>%
      filter(!tolower(advice.to.Management) %in% c("na","nan", "none", "#name?", "nothing", "n/a","no"," ", "","none","nothing","nil","nothing")) %>%
      mutate(advice_cleaned = tolower(advice.to.Management),  # Convert to lowercase
             advice_cleaned = gsub("[[:punct:]]", "", advice_cleaned)) %>%  # Remove punctuation
      group_by(advice_cleaned) %>%
      tally(sort = TRUE) %>%
      top_n(20, n)  # Select top 20 most frequent advice
    
    # Return the table of advice and their counts
    advice_data
  })
  
  
  
  
  # Create sentiment analysis over time for advice to management
  output$sentiment_over_time_plot <- renderPlot({
    # Preprocess the sentiment analysis
    reviews_data <- reviews_data %>%
      filter(!is.na(advice.to.Management)) %>%
      mutate(Date = ymd(Date),  # Ensure Date is in Date format
             sentiment_score = get_sentiment(advice.to.Management)) %>%
      group_by(year = year(Date)) %>%
      summarise(average_sentiment = mean(sentiment_score, na.rm = TRUE))
    
    # Plot the sentiment over time
    ggplot(reviews_data, aes(x = year, y = average_sentiment)) +
      geom_line() +
      geom_point() +
      labs(title = "Sentiment Analysis of Advice to Management Over Time",
           x = "Year", y = "Average Sentiment") +
      scale_x_continuous(breaks = seq(2008, 2020, by = 1), labels = as.character(seq(2008, 2020, by = 1))) +  # Fix year labels
      theme_minimal()
  })
  
  
  
  output$map <- renderLeaflet({
    # Define color scale based on employee count
    color_scale <- colorBin(palette = "Reds", domain = final_data$n, bins = 5)
    
    # Create the leaflet map
    leaflet() %>%
      # Add the world map (countries' borders)
      addProviderTiles("CartoDB.Positron") %>%
      
      # Add tiny dots for employee locations
      addCircleMarkers(
        data = final_data,  # Use the data with city, employee count, lat, and lng
        lng = ~lng, lat = ~lat,  # Longitude and Latitude for employee locations
        radius = 2,  # Small radius for the dots
        color = ~color_scale(n),  # Color the dots based on employee count
        fillOpacity = 0.7,  # Transparency of the dot
        stroke = FALSE,  # No border around the circle marker
        popup = ~paste("<b>City:</b>", city, "<br><b>Employees:</b>", n)  # Display city and employee count on click
      ) %>%
      
      # Add a legend to describe the color scale (employee count)
      addLegend(
        position = "bottomright",
        pal = color_scale,
        values = final_data$n,
        title = "Employee Count",
        opacity = 0.7
      )
  })
  
}

shinyApp(ui, server)