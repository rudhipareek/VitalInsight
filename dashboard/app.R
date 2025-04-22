
library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(plotly)
library(DT)
library(shinyBS)
library(shinyWidgets)
library(shinydashboard)

# nolint start
# Load data
data <- read.csv("data/diabetes_young_adults_india.csv")

# Print column names to check available variables
print("Column names:")
print(colnames(data))
print("First few rows:")
print(head(data))

# Check if Blood_Glucose column exists, if not, don't use it in visualizations
has_blood_glucose <- "Blood_Glucose" %in% colnames(data)

# Define color palette
diabetes_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6")

# Define the risk calculation function with gender-specific BMI
calculate_diabetes_risk <- function(age, bmi, family_history, physical_activity, smoking, gender) {
  # Base risk score
  risk_score <- 0
  
  # Age factor (higher risk with increasing age)
  age_factor <- if(age < 25) {
    1
  } else if(age >= 25 && age < 35) {
    2
  } else {
    3
  }
  
  # BMI factor (higher risk with higher BMI) - gender specific
if(gender == "Male") {
  bmi_factor <- if(bmi < 18.5) {
    1  # Underweight
  } else if(bmi >= 18.5 && bmi < 25) {
    2  # Normal weight
  } else if(bmi >= 25 && bmi < 30) {
    3  # Overweight
  } else if(bmi >= 30 && bmi < 35) {
    4  # Obesity Class I
  } else if(bmi >= 35 && bmi < 40) {
    5  # Obesity Class II
  } else {
    6  # Obesity Class III
  }
} else { # Female - using same BMI categories for scientific accuracy
  bmi_factor <- if(bmi < 18.5) {
    1  # Underweight
  } else if(bmi >= 18.5 && bmi < 25) {
    2  # Normal weight
  } else if(bmi >= 25 && bmi < 30) {
    3  # Overweight
  } else if(bmi >= 30 && bmi < 35) {
    4  # Obesity Class I
  } else if(bmi >= 35 && bmi < 40) {
    5  # Obesity Class II
  } else {
    6  # Obesity Class III
  }
}
  
  # Family history is a major factor
  family_factor <- if(family_history) 3 else 0
  
  # Physical activity is protective - refined scale
  activity_factor <- switch(physical_activity,
                         "Sedentary" = 3,
                         "low" = 2,
                         "Moderate" = 1,
                         "High" = 0)
  
  # Smoking increases risk
  smoking_factor <- if(smoking) 2 else 0
  
  # Calculate total risk score
  risk_score <- age_factor + bmi_factor + family_factor + activity_factor + smoking_factor
  
  # Translate to risk category
  risk_category <- if(risk_score <= 4) {
    "Low Risk"
  } else if(risk_score > 4 && risk_score <= 8) {
    "Moderate Risk"
  } else if(risk_score > 8 && risk_score <= 11) {
    "High Risk"
  } else {
    "Very High Risk"
  }
  
  # Risk percentage (approximate)
  max_score <- 14 # Maximum possible score
  risk_percentage <- round((risk_score / max_score) * 100)
  
  # Return both category and percentage
  return(list(
    category = risk_category,
    percentage = risk_percentage,
    score = risk_score
  ))
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      .title-box {
        background-color: #2c3e50;
        color: white;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 5px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      }
      .box-container {
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
        padding: 20px;
        margin-bottom: 20px;
        background-color: white;
      }
      .nav-tabs {
        margin-bottom: 15px;
        border-bottom: 2px solid #3498db;
      }
      .nav-tabs > li > a {
        font-weight: 500;
        color: #2c3e50;
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
        border-top: 2px solid #3498db;
        border-bottom: 0;
        font-weight: bold;
      }
      .risk-low {
        color: #2ecc71;
        font-weight: bold;
      }
      .risk-moderate {
        color: #f39c12;
        font-weight: bold;
      }
      .risk-high {
        color: #e74c3c;
        font-weight: bold;
      }
      .risk-very-high {
        color: #c0392b;
        font-weight: bold;
      }
      .progress {
        height: 25px;
        margin-bottom: 10px;
        border-radius: 10px;
      }
      .progress-bar-low {
        background-color: #2ecc71;
      }
      .progress-bar-moderate {
        background-color: #f39c12;
      }
      .progress-bar-high {
        background-color: #e74c3c;
      }
      .progress-bar-very-high {
        background-color: #c0392b;
      }
      .chart-explanation {
        background-color: #f8f9fa;
        padding: 10px;
        border-left: 4px solid #3498db;
        margin-top: 10px;
        font-size: 14px;
        color: #2c3e50;
      }
      .insight-box {
        background-color: #eaf2f8;
        border-left: 4px solid #3498db;
        padding: 10px;
        margin-top: 15px;
        font-style: italic;
      }
      .section-title {
        border-bottom: 2px solid #ecf0f1;
        padding-bottom: 8px;
        color: #2c3e50;
        font-weight: 500;
      }
      .sidebar-section {
        margin-bottom: 25px;
      }
      .info-icon {
        color: #3498db;
        margin-left: 5px;
      }
      .stats-box {
        text-align: center;
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 15px;
      }
      .stats-number {
        font-size: 24px;
        font-weight: bold;
        color: #3498db;
      }
      .stats-label {
        font-size: 14px;
        color: #7f8c8d;
      }
      .recommendations-box {
        background-color: #e8f8f5;
        border-radius: 5px;
        padding: 15px;
      }
      .info-tooltip {
        font-size: 12px;
        cursor: help;
      }
    "))
  ),
  
  div(class = "title-box",
      titlePanel("Diabetes in Young Adults - India"),
      p("Interactive dashboard for analysis of diabetes risk factors, prevalence, and health metrics among young adults in India")
  ),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar-section",
        h4("Data Filters", class = "section-title"),
        # Only include region filter if Region column exists
        if("Region" %in% colnames(data)) {
          pickerInput("region", "Select Region:",
                    choices = c("All", unique(data$Region)), 
                    selected = "All",
                    options = list(`live-search` = TRUE,
                                  `actions-box` = TRUE),
                    multiple = FALSE)
        },
        sliderInput("ageRange", "Age Range:",
                  min = min(data$Age), max = max(data$Age),
                  value = c(min(data$Age), max(data$Age)),
                  step = 1,
                  round = TRUE),
        prettyCheckboxGroup("diabetesTypes", "Diabetes Types:",
                          choices = unique(data$Diabetes_Type),
                          selected = unique(data$Diabetes_Type),
                          status = "primary",
                          animation = "smooth")
      ),
      
      div(class = "sidebar-section",
        h4("About This Dashboard", class = "section-title"),
        p("This dashboard explores diabetes trends among young adults in India through:"),
        tags$ul(
          tags$li("Prevalence by type and region"),
          tags$li("Health metrics and correlations"),
          tags$li("Lifestyle impact and recommendations"),
          tags$li("Interactive tools for risk estimation and health planning")
        ),
        p("Based on a study of young adults in India to support informed health choices and diabetes care.")
    ),

      
      div(class = "stats-box",
        uiOutput("totalCasesBox")
      ),
      
      width = 3
    ),

     # plot description, Key insights, text  
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", 
          div(class = "box-container",
            h4("Diabetes Distribution", class = "section-title"),
            fluidRow(
              column(6, #piechart 
                plotlyOutput("diabetesPieChart", height = "300px"),
                div(class = "chart-explanation",
                  p("This pie chart visualizes the proportion of various diagnosed diabetes types within the selected group. It highlights the prevalence of each type based on case count")
                )
              ),
              #region chart 
              if("Region" %in% colnames(data)) {
                column(6, 
                  plotlyOutput("regionBarChart", height = "300px"),
                  div(class = "chart-explanation",
                    p("Stacked bar chart showing the number of diabetes cases by type across different regions, excluding individuals without diabetes.")
                  )
                )
              } else {
                column(6, 
                  plotlyOutput("ageHistogram", height = "300px"),
                  div(class = "chart-explanation",
                    p("Histogram showing the distribution of age among individuals with different types of diabetes")
                  )
                )
              }
            ),
            hr(),
            h4("Summary Statistics", class = "section-title"),
            div(class = "insight-box",
              p("This table summarizes key statistics for each diabetes type, including average BMI, age, HbA1c, blood sugar levels, and percentage with family history.")
            ),
            DTOutput("summaryTable")
          )
        ),
        
        tabPanel("Health Metrics",
          div(class = "box-container",
            h4("BMI Distribution by Diabetes Type", class = "section-title"),
            plotlyOutput("bmiViolinPlot", height = "350px"),
            div(class = "chart-explanation",
              p("Violin plot showing BMI distribution by diabetes type. The width of each violin represents the density of data points, showing the full range and concentration of BMI values.")
            ),
            hr(),
            h4("Health Metrics Correlations", class = "section-title"),
            fluidRow(
              column(6, 
                plotlyOutput("hba1cBoxPlot", height = "300px"),
                div(class = "chart-explanation",
                  p("Box plot showing HbA1c levels by diabetes type with clinical threshold lines. The red line (6.5%) indicates the diabetes diagnostic threshold.")
                )
              ),
              column(6, 
                plotlyOutput("bloodSugarPlot", height = "300px"),
                div(class = "chart-explanation",
                  p("Blood sugar levels across different diabetes types, showing the relationship between fasting glucose levels and diabetes diagnosis.")
                )
              )
            ),
            hr(),
            h4("Health Metrics by Age Group", class = "section-title"),
            plotlyOutput("ageGroupMetrics", height = "350px"),
            div(class = "insight-box",
              p("Key Insight: Health metrics vary significantly across age groups, with older individuals showing higher HbA1c and BMI values, especially in Type 2 diabetes cases.")
            )
          )
        ),
        
        tabPanel("Lifestyle Analysis",
          div(class = "box-container",
            h4("Physical Activity and Diabetes", class = "section-title"),
            plotlyOutput("activityPlot", height = "350px"),
            div(class = "chart-explanation",
              p("This chart shows the relationship between physical activity levels and diabetes metrics (case counts and HbA1c levels). Regular physical activity is associated with better insulin sensitivity and glucose metabolism.")
            ),
            hr(),
            h4("Dietary Impact on Diabetes", class = "section-title"),
            plotlyOutput("lifestyleHeatmap", height = "350px"),
            div(class = "chart-explanation",
              p("Heatmap showing how different combinations of dietary habits and fast food intake affect average HbA1c levels and diabetes prevalence.")
            ),
            hr(),
            h4("Sleep and Stress Impact", class = "section-title"),
            plotlyOutput("lifestyleFactorsPlot", height = "350px"),
            div(class = "chart-explanation",
              p("Heatmap displaying diabetes rates based on combinations of sleep duration and stress levels, with higher risk regions shown in red.")
            ),
            hr(),
            h4("Lifestyle Recommendations", class = "section-title"),
            div(class = "recommendations-box",
              h4("Evidence-Based Recommendations"),
              tags$ul(
                tags$li(strong("Physical Activity:"), " Aim for 150 minutes of moderate-intensity activity per week (30 minutes, 5 days/week)"),
                tags$li(strong("Diet:"), " Focus on whole grains, lean proteins, and vegetables; limit processed foods and sugary beverages"),
                tags$li(strong("Sleep:"), " 7-8 hours of quality sleep helps regulate glucose metabolism and appetite hormones"),
                tags$li(strong("Stress Management:"), " Regular stress reduction practices like meditation can improve insulin sensitivity and reduce diabetes risk")
              )
            )
          )
        ),
        
        tabPanel("Risk Calculator",
          div(class = "box-container",
            h3("Diabetes Risk Assessment Calculator", class = "section-title"),
            p("This evidence-based tool provides an estimate of diabetes risk based on personal health factors. Please enter your information below:"),
            hr(),
            fluidRow(
              column(6,
                numericInput("calc_age", "Age:",
                           value = 25, min = 18, max = 40),
                numericInput("calc_bmi", "BMI:",
                           value = 23, min = 15, max = 50, step = 0.1),
                selectInput("calc_gender", "Gender:",
                          choices = c("Male", "Female"),
                          selected = "Male"),
                bsTooltip("calc_gender", "BMI risk thresholds differ by gender", 
                         placement = "right", trigger = "hover")
              ),
              column(6,
                checkboxInput("calc_family", "Family History of Diabetes",
                            value = FALSE),
                selectInput("calc_activity", "Physical Activity Level:",
                          choices = c("Sedentary", "Low", "Moderate", "High"),
                          selected = "Moderate"),
                bsTooltip("calc_activity", 
                         "Sedentary: <30 min/week, Low: 30-60 min/week, Moderate: 60-150 min/week, High: >150 min/week", 
                         placement = "left", trigger = "hover"),
                checkboxInput("calc_smoking", "Current Smoker",
                            value = FALSE),
                br(),
                actionButton("calculate", "Calculate Risk", 
                           class = "btn-primary btn-lg btn-block")
              )
            ),
            hr(),
            conditionalPanel(
              condition = "input.calculate",
              h4("Your Diabetes Risk Assessment:", class = "section-title"),
              uiOutput("riskCategory"),
              uiOutput("riskProgress"),
              hr(),
              h4("Risk Factors Explained:", class = "section-title"),
              uiOutput("riskExplanation"),
              hr(),
              h4("Personalized Recommendations:", class = "section-title"),
              uiOutput("recommendations")
            )
          )
        ),
        tabPanel("Physical Activity Planner",
            div(class = "box-container",
                h3("Physical Activity Planner", class = "section-title"),
                p("Plan your exercise routine to help manage blood glucose levels and reduce diabetes risk."),
                hr(),
                fluidRow(
                column(4,
                    h4("Your Profile", class = "section-title"),
                    numericInput("weight", "Weight (kg):", value = 70, min = 30, max = 200),
                    selectInput("fitness_level", "Current Fitness Level:",
                            choices = c("Beginner", "Intermediate", "Advanced"),
                            selected = "Beginner"),
                    selectInput("exercise_goal", "Exercise Goal:",
                            choices = c("Weight Management", "Blood Sugar Control", "Overall Health", "Cardiovascular Fitness"),
                            selected = "Blood Sugar Control"),
                    hr(),
                    h4("Weekly Schedule", class = "section-title"),
                    sliderInput("days_per_week", "Exercise days per week:", 
                            min = 1, max = 7, value = 3, step = 1),
                    numericInput("minutes_per_session", "Minutes per session:", 
                            value = 30, min = 10, max = 120, step = 5),
                    actionButton("generate_plan", "Generate Exercise Plan", 
                            class = "btn-primary btn-block")
                ),
                column(8,
                    conditionalPanel(
                    condition = "input.generate_plan",
                    h4("Your Personalized Exercise Plan", class = "section-title"),
                    div(class = "recommendations-box",
                        uiOutput("exercise_plan_summary")
                    ),
                    hr(),
                    h4("Weekly Schedule", class = "section-title"),
                    tableOutput("exercise_schedule"),
                    hr(),
                    h4("Estimated Health Benefits", class = "section-title"),
                    div(class = "chart-explanation",
                        plotlyOutput("exercise_benefits_plot", height = "250px")
                    ),
                    div(class = "insight-box",
                        uiOutput("exercise_insights")
                    )
                    )
                ))
            )
        ),

        tabPanel("Nutrition Analyzer",
          div(class = "box-container",
            h3("Indian Cuisine Diabetes-Friendly Nutrition Analyzer", class = "section-title"),
            p("Analyze traditional Indian meals for diabetes management. Enter food items to calculate nutritional content and get personalized recommendations."),
            hr(),
            fluidRow(
              column(6,
                div(class = "box-container", style = "margin-top: 0;",
                  h4("Build Your Meal", class = "section-title"),
                  selectizeInput("food_item", "Select Indian Dish:",
                              choices = NULL, # Will be populated from server
                              options = list(`create-item` = TRUE, 
                                            placeholder = "Search or enter custom dish")),
                  numericInput("serving_size", "Serving Size (grams):",
                            value = 100, min = 10, max = 1000, step = 10),
                  div(style = "display: flex; justify-content: space-between; margin-top: 15px;",
                    actionButton("add_food", "Add to Meal", 
                              class = "btn btn-primary", icon = icon("plus")),
                    actionButton("clear_meal", "Clear Meal", 
                              class = "btn btn-danger", icon = icon("trash"))
                  ),
                  hr(),
                  h4("Current Meal Composition", class = "section-title"),
                  
                  # Set a fixed height for the table with scrolling
                  div(style = "max-height: 350px; overflow-y: auto; margin-bottom: 20px;",
                    DTOutput("meal_table")
                  )
                )
              ),
              column(6,
                div(class = "box-container", style = "margin-top: 0; padding-bottom: 0;",
                  tabsetPanel(
                    id = "nutritionTabs",
                    tabPanel("Summary",
                      h4("Macronutrient Distribution", class = "section-title"),
                      plotlyOutput("nutrition_chart", height = "250px"),
                      hr(),
                      h4("Glycemic Impact Assessment", class = "section-title"),
                      plotlyOutput("glycemic_gauge", height = "200px"),
                      div(class = "chart-explanation",
                        p("The glycemic load gauge shows the estimated impact of your meal on blood glucose levels.")
                      )
                    ),
                    tabPanel("Micronutrients",
                      h4("Micronutrient Analysis", class = "section-title"),
                      plotlyOutput("micronutrient_chart", height = "350px"),
                      div(class = "insight-box",
                        p("Micronutrients are essential for overall health and can impact diabetes management.")
                      )
                    ),
                    tabPanel("Recommendations",
                      div(class = "recommendations-box",
                        h4("Personalized Diabetes Management Recommendations"),
                        uiOutput("nutrition_recommendations")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  # reactive filtered dataset
  filtered_data <- reactive({
    result <- data
    
    # filter by region 
    if("Region" %in% colnames(data) && input$region != "All") {
      result <- result %>% filter(Region == input$region)
    }
    
    # Filter by age range
    result <- result %>% 
      filter(Age >= input$ageRange[1] & Age <= input$ageRange[2]) %>%
      filter(Diabetes_Type %in% input$diabetesTypes)
    
    return(result)
  })

  
  # reactive for total cases count
  output$totalCasesBox <- renderUI({
    total_cases <- nrow(filtered_data())
    
    tagList(
      div(class = "stats-number", total_cases),
      div(class = "stats-label", "Total Cases")
    )
  })
  
  # Overview tab plots
  # Distribution of Diabetes Types Piechart
  output$diabetesPieChart <- renderPlotly({
  diabetes_counts <- filtered_data() %>%
    group_by(Diabetes_Type) %>%
    summarise(Count = n())

  plot_ly(
    diabetes_counts,
    labels = ~Diabetes_Type,
    values = ~Count,
    type = 'pie',
    hole = 0.45,
    marker = list(
      colors = diabetes_colors,
      line = list(color = '#FFFFFF', width = 2)
    ),
    textinfo = 'percent',
    textposition = 'inside',
    insidetextfont = list(size = 14, color = '#FFFFFF'),
    hoverinfo = 'text',
    text = ~paste0(Diabetes_Type, ": ", Count, " cases")
  ) %>%
    layout(
      title = list(
        text = "Distribution of Diabetes Types",
        font = list(size = 18),
        x = 0.02
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "v",
        x = 1.05,
        y = 0.9,
        font = list(size = 12),
        bgcolor = "rgba(0,0,0,0)"
      ),
      margin = list(l = 20, r = 120, t = 60, b = 20)
    )
})


# 2. Regional breakdown - showing diabetes prevalence by region
output$regionBarChart <- renderPlotly({
  region_data <- filtered_data() %>%
    group_by(Region, Diabetes_Type) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    filter(Diabetes_Type != "None")

  plot_ly(
    region_data,
    x = ~Region,
    y = ~Count,
    color = ~Diabetes_Type,
    type = "bar",
    colors = diabetes_colors,
    hoverinfo = 'text',
    text = ~paste0("<b>", Region, "</b><br>", Diabetes_Type, ": ", Count, " cases")
  ) %>%
    layout(
      title = list(
        text = "Diabetes Distribution by Region",
        font = list(size = 18),
        x = 0
      ),
      xaxis = list(
        title = "",
        tickangle = -30,
        tickfont = list(size = 12)
      ),
      yaxis = list(
        title = "Number of Cases",
        titlefont = list(size = 14),
        gridcolor = "rgba(200,200,200,0.2)"
      ),
      barmode = 'stack',
      legend = list(
        orientation = "v",
        x = 1.05,
        y = 1,
        font = list(size = 12)
      ),
      margin = list(l = 60, r = 130, t = 60, b = 100)
    )
})



# 3. Summary table with more relevant metrics
output$summaryTable <- renderDT({
  summary_data <- filtered_data() %>%
    group_by(Diabetes_Type) %>%
    summarise(
      "Cases" = n(),
      "Avg. BMI" = round(mean(BMI, na.rm = TRUE), 1),
      "Avg. Age" = round(mean(Age, na.rm = TRUE), 1),
      "Avg. HbA1c" = round(mean(HbA1c, na.rm = TRUE), 1),
      "Avg. Blood Sugar" = round(mean(Fasting_Blood_Sugar, na.rm = TRUE), 1),
      "% Family History" = round(mean(Family_History_Diabetes == "Yes", na.rm = TRUE) * 100, 1)
    )
  
  datatable(summary_data, 
           options = list(
             pageLength = 5,
             dom = 't',
             ordering = TRUE,
             autoWidth = TRUE
           ),
           rownames = FALSE,
           class = 'cell-border stripe') %>% 
    formatStyle(columns = names(summary_data), 
               backgroundColor = "#f8f9fa",
               borderRadius = "5px")
})

  
  # Health Metrics tab plots
  # 1. BMI distribution visualization with violin plot
output$bmiViolinPlot <- renderPlotly({
    plot_ly(filtered_data(), 
          x = ~Diabetes_Type, 
          y = ~BMI, 
          split = ~Gender,
          type = "violin", 
          box = list(visible = TRUE),
          meanline = list(visible = TRUE),
          color = ~Diabetes_Type,
          colors = diabetes_colors) %>%
    layout(title = list(text = "BMI Distribution by Diabetes Type and Gender",
                        font = list(size = 16),
                        y = 0.95),
           xaxis = list(title = list(text = "Diabetes Type",
                                    font = list(size = 12),
                                    standoff = 15)),
           yaxis = list(title = list(text = "BMI",
                                     font = list(size = 12),
                                     standoff = 15)),
           violinmode = "group",
           legend = list(orientation = "h", y = -0.3),
           margin = list(l = 60, r = 30, t = 60, b = 120))
})

# 2. HbA1c Box Plot
output$hba1cBoxPlot <- renderPlotly({
  plot_ly(filtered_data(), 
          y = ~HbA1c, 
          color = ~Diabetes_Type, 
          type = "box",
          colors = diabetes_colors,
          boxpoints = "outliers",
          jitter = 0.3,
          pointpos = -1.8) %>%
    layout(title = list(text = "HbA1c Levels by Diabetes Type",
                       font = list(size = 16),
                       y = 0.95),
           xaxis = list(title = list(text = "Diabetes Type",
                                    font = list(size = 12),
                                    standoff = 15)),
           yaxis = list(title = list(text = "HbA1c (%)",
                                    font = list(size = 12),
                                    standoff = 15)),
           legend = list(orientation = "h", y = -0.4),
           margin = list(l = 60, r = 30, t = 60, b = 120),
           shapes = list(
             # Diabetes HbA1c reference line
             list(
               type = "line", 
               x0 = -0.5, 
               x1 = length(unique(filtered_data()$Diabetes_Type)) - 0.5, 
               y0 = 6.5, 
               y1 = 6.5, 
               line = list(color = '#a71818', width = 2, dash = 'dash')
             ),
             # Prediabetes reference line
             list(
               type = "line", 
               x0 = -0.5, 
               x1 = length(unique(filtered_data()$Diabetes_Type)) - 0.5, 
               y0 = 5.7, 
               y1 = 5.7, 
               line = list(color = '#243193', width = 2, dash = 'dash')
             )
           ),
           annotations = list(
             list(
               x = length(unique(filtered_data()$Diabetes_Type)) - 0.5,
               y = 6.5,
               text = "Diabetes ≥ 6.5%",
               showarrow = FALSE,
               xanchor = "right",
               yanchor = "bottom",
               font = list(size = 10, color = "#a71818")
             ),
             list(
               x = length(unique(filtered_data()$Diabetes_Type)) - 0.5,
               y = 5.7,
               text = "Prediabetes ≥ 5.7%",
               showarrow = FALSE,
               xanchor = "right",
               yanchor = "bottom",
               font = list(size = 10, color = "#243193")
             )
           ))
})

# 3. Blood Sugar Plot
output$bloodSugarPlot <- renderPlotly({
  plot_ly(filtered_data(), 
          x = ~Diabetes_Type, 
          y = ~Fasting_Blood_Sugar, 
          color = ~Diabetes_Type,
          colors = diabetes_colors,
          type = "box",
          boxpoints = "outliers",
          jitter = 0.3,
          pointpos = -1.8) %>%
    layout(title = list(text = "Fasting Blood Sugar Levels",
                       font = list(size = 16),
                       y = 0.95),
           xaxis = list(title = list(text = "Diabetes Type",
                                    font = list(size = 12),
                                    standoff = 15)),
           yaxis = list(title = list(text = "Fasting Blood Sugar (mg/dL)",
                                    font = list(size = 12),
                                    standoff = 15)),
           legend = list(orientation = "h", y = -0.4),
           margin = list(l = 70, r = 30, t = 60, b = 120),
           shapes = list(
             # Normal blood sugar reference line
             list(
               type = "line", 
               x0 = -0.5, 
               x1 = length(unique(filtered_data()$Diabetes_Type)) - 0.5, 
               y0 = 100, 
               y1 = 100, 
               line = list(color = 'green', width = 2, dash = 'dash')
             ),
             # Prediabetes reference line
             list(
               type = "line", 
               x0 = -0.5, 
               x1 = length(unique(filtered_data()$Diabetes_Type)) - 0.5, 
               y0 = 126, 
               y1 = 126, 
               line = list(color = 'red', width = 2, dash = 'dash')
             )
           ),
           annotations = list(
             list(
               x = length(unique(filtered_data()$Diabetes_Type)) - 0.5,
               y = 100,
               text = "Normal < 100 mg/dL",
               showarrow = FALSE,
               xanchor = "right",
               yanchor = "bottom",
               font = list(size = 10, color = "green")
             ),
             list(
               x = length(unique(filtered_data()$Diabetes_Type)) - 0.5,
               y = 126,
               text = "Diabetes ≥ 126 mg/dL",
               showarrow = FALSE,
               xanchor = "right",
               yanchor = "bottom",
               font = list(size = 10, color = "red")
             )
           ))
})

# 4. Age Group Metrics Plot
output$ageGroupMetrics <- renderPlotly({
  library(RColorBrewer)

  # Create age groups
  age_data <- filtered_data() %>%
    mutate(Age_Group = case_when(
      Age < 20 ~ "< 20",
      Age >= 20 & Age < 25 ~ "20-24",
      Age >= 25 & Age < 30 ~ "25-29",
      Age >= 30 & Age < 35 ~ "30-34",
      Age >= 35 ~ "35+"
    )) %>%
    mutate(Age_Group = factor(Age_Group, levels = c("< 20", "20-24", "25-29", "30-34", "35+"))) %>%
    group_by(Age_Group, Diabetes_Type) %>%
    summarise(
      Avg_BMI = mean(BMI, na.rm = TRUE),
      Avg_HbA1c = mean(HbA1c, na.rm = TRUE),
      Avg_BloodSugar = mean(Fasting_Blood_Sugar, na.rm = TRUE),
      Count = n(),
      .groups = 'drop'
    )
  
  # Nice modern color palette
  diabetes_colors <- RColorBrewer::brewer.pal(n = length(unique(age_data$Diabetes_Type)), name = "Set2")

  # Scatter Plot
  plot_ly() %>%
    add_trace(
      data = age_data, 
      x = ~Age_Group, y = ~Avg_HbA1c, 
      type = 'scatter', mode = 'lines+markers',
      color = ~Diabetes_Type, colors = diabetes_colors,
      name = ~paste(Diabetes_Type, " - HbA1c"),
      text = ~paste("Age Group:", Age_Group, "<br>Avg HbA1c:", round(Avg_HbA1c, 2), "%"),
      hoverinfo = 'text',
      line = list(width = 3),
      marker = list(size = 9, symbol = "circle")
    ) %>%
    add_trace(
      data = age_data, 
      x = ~Age_Group, y = ~Avg_BMI, 
      type = 'scatter', mode = 'lines+markers',
      color = ~Diabetes_Type, colors = diabetes_colors,
      name = ~paste(Diabetes_Type, " - BMI"),
      text = ~paste("Age Group:", Age_Group, "<br>Avg BMI:", round(Avg_BMI, 1)),
      hoverinfo = 'text',
      line = list(width = 3, dash = 'dot'),
      marker = list(size = 9, symbol = "diamond"),
      yaxis = "y2"
    ) %>%
    layout(
      title = list(
        font = list(size = 20, family = "Arial"),
        x = 0.05
      ),
      xaxis = list(
        title = "Age Group",
        tickfont = list(size = 12),
        titlefont = list(size = 14),
        showgrid = FALSE
      ),
      yaxis = list(
        title = "Avg HbA1c (%)",
        titlefont = list(size = 14),
        tickfont = list(size = 12),
        gridcolor = 'rgba(200,200,200,0.3)'
      ),
      yaxis2 = list(
        title = "Avg BMI",
        overlaying = "y",
        side = "right",
        titlefont = list(size = 14),
        tickfont = list(size = 12),
        showgrid = FALSE
      ),
      legend = list(
        orientation = "v",
        x = 1.08,
        y = 1.1,
        font = list(size = 12),
        xanchor = "left",
        bordercolor = "lightgray",
        borderwidth = 1
      ),
      margin = list(l = 60, r = 160, t = 80, b = 60),
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 12),
        bordercolor = "#ccc"
      )
    )
})
  
  # Lifestyle tab plots - improved with more accurate data
  # Physical Activity Impact on Diabetes
  output$activityPlot <- renderPlotly({
  activity_data <- filtered_data() %>%
    group_by(Physical_Activity_Level, Diabetes_Type) %>%
    summarise(Count = n(), 
              Avg_HbA1c = mean(HbA1c, na.rm = TRUE),
              .groups = 'drop') %>%
    # focus only on actual diabetes cases- removed none
    filter(Diabetes_Type != "None")
  
  # Order the activity levels properly
  activity_data$Physical_Activity_Level <- factor(activity_data$Physical_Activity_Level, 
                                                levels = c("Sedentary", "Moderate", "Active"))
  
  # Plot with dual y-axis (count bars and HbA1c line)
  plot_ly() %>%
    add_bars(data = activity_data, 
            x = ~Physical_Activity_Level, 
            y = ~Count, 
            color = ~Diabetes_Type, 
            colors = diabetes_colors,
            name = ~paste(Diabetes_Type, "Count")) %>%
    add_trace(data = activity_data, 
             x = ~Physical_Activity_Level, 
             y = ~Avg_HbA1c, 
             color = ~Diabetes_Type,
             colors = diabetes_colors,
             type = 'scatter', 
             mode = 'lines+markers',
             yaxis = 'y2',
             marker = list(size = 10),
             line = list(width = 3),
             name = ~paste(Diabetes_Type, "HbA1c")) %>%
    layout(title = list(text = "Physical Activity Impact on Diabetes", font = list(size = 16)),
           xaxis = list(title = "Physical Activity Level",
                        categoryorder = "array", 
                        categoryarray = c("Sedentary", "Moderate", "Active")),
           yaxis = list(title = "Number of Cases", side = "left"),
           yaxis2 = list(title = "Average HbA1c", side = "right", overlaying = "y"),
           legend = list(orientation = "h", y = -0.3),
           margin = list(l = 50, r = 50, t = 50, b = 120),
           barmode = "group")
})

# 2. lifestyle heatmap with dietary habits and diabetes metrics
  output$lifestyleHeatmap <- renderPlotly({
    lifestyle_data <- filtered_data() %>%
      group_by(Dietary_Habits, Fast_Food_Intake) %>%
      summarise(
        Average_HbA1c = mean(HbA1c, na.rm = TRUE),
        Diabetes_Rate = mean(Diabetes_Type != "None", na.rm = TRUE) * 100,
        .groups = 'drop'
      )
    
    # Convert Fast_Food_Intake to factor with appropriate labels
    lifestyle_data$Fast_Food_Intake_Label <- factor(
      lifestyle_data$Fast_Food_Intake,
      levels = 0:5,
      labels = c("None", "Very Low", "Low", "Medium", "High", "Very High")
    )
    
    #heatmap showing diet impact on diabetes
    plot_ly(lifestyle_data, 
            x = ~Fast_Food_Intake_Label, 
            y = ~Dietary_Habits, 
            z = ~Average_HbA1c, 
            type = "heatmap",
            colorscale = "RdBu",
            reversescale = TRUE, # Lower HbA1c (blue) is better
            colorbar = list(title = "Avg HbA1c"),
            hoverinfo = "text",
            text = ~paste("Fast Food: ", Fast_Food_Intake_Label, 
                        "<br>Diet: ", Dietary_Habits, 
                        "<br>Avg HbA1c: ", round(Average_HbA1c, 1),
                        "<br>Diabetes Rate: ", round(Diabetes_Rate, 1), "%")) %>%
      layout(title = list(text = "Impact of Dietary Habits on HbA1c Levels", font = list(size = 16)),
            xaxis = list(title = "Fast Food Consumption", 
                          categoryorder = "array",
                          categoryarray = c("None", "Very Low", "Low", "Medium", "High", "Very High")),
            yaxis = list(title = "Overall Dietary Habits",
                        categoryorder = "array",
                        categoryarray = c("Healthy", "Moderate", "Unhealthy")))
  })

  # 3.Sleep and stress impact on diabetes
  
  output$lifestyleFactorsPlot <- renderPlotly({
    # Categorize sleep hours
    sleep_data <- filtered_data() %>%
      mutate(Sleep_Category = case_when(
        Sleep_Hours < 6 ~ "< 6 hrs",
        Sleep_Hours >= 6 & Sleep_Hours < 8 ~ "6-8 hrs",
        Sleep_Hours >= 8 ~ "> 8 hrs"
      )) %>%
      # Categorize stress levels
      mutate(Stress_Category = case_when(
        Stress_Level <= 3 ~ "Low Stress",
        Stress_Level <= 6 ~ "Moderate Stress",
        TRUE ~ "High Stress"
      )) %>%
      # Ensure proper factor levels
      mutate(
        Sleep_Category = factor(Sleep_Category, levels = c("< 6 hrs", "6-8 hrs", "> 8 hrs")),
        Stress_Category = factor(Stress_Category, levels = c("Low Stress", "Moderate Stress", "High Stress"))
      ) %>%
      # Group by both categories
      group_by(Sleep_Category, Stress_Category) %>%
      summarise(
        Count = n(),
        Diabetes_Rate = mean(Diabetes_Type != "None") * 100,
        .groups = 'drop'
      )
      
    # heatmap 
    plot_ly(sleep_data, 
            x = ~Sleep_Category, 
            y = ~Stress_Category,
            z = ~Diabetes_Rate, 
            type = "heatmap",
            colorscale = "RdYlGn_r",  # Red for high risk, green for low
            text = ~paste("Sleep: ", Sleep_Category,
                        "<br>Stress: ", Stress_Category,
                        "<br>Diabetes Rate: ", round(Diabetes_Rate, 1), "%",
                        "<br>Count: ", Count),
            hoverinfo = "text") %>%
      layout(title = list(text = "Sleep and Stress Impact on Diabetes Rate", font = list(size = 16)),
            xaxis = list(title = "Sleep Duration"),
            yaxis = list(title = "Stress Level"))
  })
    
  # Risk Calculator outputs - with gender-specific BMI assessment
  risk_results <- eventReactive(input$calculate, {
    calculate_diabetes_risk(
      age = input$calc_age,
      bmi = input$calc_bmi,
      family_history = input$calc_family,
      physical_activity = input$calc_activity,
      smoking = input$calc_smoking,
      gender = input$calc_gender
    )
  })
  
  output$riskCategory <- renderUI({
    risk <- risk_results()
    risk_class <- switch(risk$category,
                       "Low Risk" = "risk-low",
                       "Moderate Risk" = "risk-moderate",
                       "High Risk" = "risk-high",
                       "Very High Risk" = "risk-very-high")
    
    tagList(
      div(style = "font-size: 22px;", 
          "Your risk category: ", 
          span(class = risk_class, risk$category, 
              " (", risk$percentage, "%)"))
    )
  })
  
  output$riskProgress <- renderUI({
    risk <- risk_results()
    risk_bar_class <- switch(risk$category,
                          "Low Risk" = "progress-bar-low",
                          "Moderate Risk" = "progress-bar-moderate",
                          "High Risk" = "progress-bar-high",
                          "Very High Risk" = "progress-bar-very-high")
    
    tagList(
      div(class = "progress",
        div(class = paste("progress-bar", risk_bar_class),
            role = "progressbar",
            style = paste0("width: ", risk$percentage, "%;"),
            )
      )
    )
  })
  
  output$riskExplanation <- renderUI({
    risk <- risk_results()
    
    risk_factors <- list()
    
    # Age factor
    age_text <- if(input$calc_age < 25) {
      "Your age (under 25) is associated with lower diabetes risk."
    } else if(input$calc_age >= 25 && input$calc_age < 35) {
      "Your age group (25-34) has a moderate risk for diabetes."
    } else {
      "Your age group (35+) has an increased risk for diabetes."
    }
    risk_factors <- c(risk_factors, age_text)
    
    # BMI factor - gender specific
    
if(input$calc_gender == "Male" || input$calc_gender == "Female") {
  bmi_text <- if(input$calc_bmi < 18.5) {
    "Your BMI indicates you are underweight, which may have health implications but is generally associated with lower diabetes risk."
  } else if(input$calc_bmi >= 18.5 && input$calc_bmi < 25) {
    "Your BMI is in a healthy range, which is associated with lower diabetes risk."
  } else if(input$calc_bmi >= 25 && input$calc_bmi < 30) {
    "Your BMI indicates you are overweight, which increases diabetes risk."
  } else if(input$calc_bmi >= 30 && input$calc_bmi < 35) {
    "Your BMI indicates obesity (class I), which significantly increases diabetes risk."
  } else if(input$calc_bmi >= 35 && input$calc_bmi < 40) {
    "Your BMI indicates obesity (class II), which greatly increases diabetes risk."
  } else {
    "Your BMI indicates severe obesity (class III), which is strongly associated with diabetes risk."
  }
}
    risk_factors <- c(risk_factors, bmi_text)
    
    # Family history
    if(input$calc_family) {
      risk_factors <- c(risk_factors, "Family history of diabetes significantly increases your risk. First-degree relatives with diabetes can increase your risk by 2-6 times.")
    }
    
    # Physical activity 
    activity_text <- switch(input$calc_activity,
                         "Sedentary" = "Sedentary lifestyle (minimal physical activity) significantly increases diabetes risk by reducing insulin sensitivity.",
                         "Low" = "Low physical activity (30-60 minutes/week) increases diabetes risk. Research shows at least 150 minutes per week is recommended.",
                         "Moderate" = "Moderate physical activity (60-150 minutes/week) helps reduce diabetes risk by improving insulin sensitivity.",
                         "High" = "Regular high physical activity (150+ minutes/week) significantly reduces diabetes risk by up to 60% through improved glucose metabolism and weight management.")
    risk_factors <- c(risk_factors, activity_text)
    
    # Smoking
    if(input$calc_smoking) {
      risk_factors <- c(risk_factors, "Smoking increases the risk of type 2 diabetes by approximately 30-40% according to research, by causing inflammation and oxidative stress.")
    }
    
    # Generate HTML list with better styling
    tags$ul(
      lapply(risk_factors, function(factor) {
        tags$li(factor, style = "margin-bottom: 10px;")
      }),
      style = "list-style-type: disc; padding-left: 20px;"
    )
  })
  
  output$recommendations <- renderUI({
    risk <- risk_results()
    
    # Base recommendations
    recs <- list(
      "Maintain a balanced diet rich in whole grains, lean proteins, vegetables, and fruits with a low glycemic index",
      "Stay hydrated by drinking water regularly throughout the day and limit sugary beverages"
    )
    
    # BMI-specific recommendations - gender specific
    if(input$calc_gender == "Male") {
      if(input$calc_bmi >= 22) {
        recs <- c(recs, paste0("Consider working with a healthcare provider on a weight management plan to achieve a BMI below 22, which is optimal for Asian males"))
      }
    } else { # Female
      if(input$calc_bmi >= 21) {
        recs <- c(recs, paste0("Consider working with a healthcare provider on a weight management plan to achieve a BMI below 21, which is optimal for Asian females"))
      }
    }
    
    # Activity recommendations
    if(input$calc_activity == "Sedentary") {
      recs <- c(recs, "Start with short daily walks (10-15 minutes) and gradually increase to 30 minutes of moderate activity 5 days per week")
    } else if(input$calc_activity == "Low") {
      recs <- c(recs, "Increase physical activity to at least 150 minutes of moderate-intensity exercise per week (e.g., 30 minutes on 5 days)")
    } else if(input$calc_activity == "Moderate") {
      recs <- c(recs, "Maintain current physical activity and consider adding 2-3 days of strength training exercises per week")
    }
    
    # Smoking recommendations
    if(input$calc_smoking) {
      recs <- c(recs, "Consider a smoking cessation program to reduce your diabetes risk - quitting smoking can improve insulin sensitivity within 8 weeks")
    }
    
    # Family history recommendations
    if(input$calc_family) {
      recs <- c(recs, "Get regular screenings for diabetes (at least annually), especially given your family history")
      recs <- c(recs, "Consider genetic counseling to better understand your inherited risk factors")
    }
    
    # High risk specific recommendations
    if(risk$category %in% c("High Risk", "Very High Risk")) {
      recs <- c(recs, 
               "Consult with a healthcare provider for formal diabetes screening including HbA1c and fasting glucose tests",
               "Consider meeting with a dietitian for personalized nutrition advice focused on low glycemic index foods",
               "Monitor for early symptoms of diabetes including increased thirst, frequent urination, and unexplained fatigue")
    }
    
    # Generate HTML list with better styling
    tags$div(
      class = "recommendations-box",
      tags$ul(
        lapply(recs, function(rec) {
          tags$li(rec, style = "margin-bottom: 10px;")
        }),
        style = "list-style-type: disc; padding-left: 20px;"
      )
    )
  })
  
  # Dynamic BMI thresholds explanation based on selected gender
  observe({
  if(input$calc_gender == "Male" || input$calc_gender == "Female") {
    updateSliderInput(session, "calc_bmi",
                    label = paste0("BMI: (Underweight < 18.5, Normal 18.5-24.9, Overweight 25-29.9, Obese > 30)"))
  }
  })
  
# Physical Activity Planner outputs
  observe({
    # Default recommendations based on fitness level
    if(input$fitness_level == "Beginner") {
      updateSliderInput(session, "days_per_week", value = 3)
      updateNumericInput(session, "minutes_per_session", value = 30)
    } else if(input$fitness_level == "Intermediate") {
      updateSliderInput(session, "days_per_week", value = 4)
      updateNumericInput(session, "minutes_per_session", value = 45)
    } else {
      updateSliderInput(session, "days_per_week", value = 5)
      updateNumericInput(session, "minutes_per_session", value = 60)
    }
  })

  # Generate exercise plan based on user inputs
  exercise_plan <- eventReactive(input$generate_plan, {
    # Calculate more accurate calorie burn estimates based on MET values
    # MET = Metabolic Equivalent of Task
    get_met_value <- function(fitness_level, exercise_type) {
      met_values <- list(
        "Beginner" = list(
          "Cardio" = 4.0,
          "Strength" = 3.5,
          "Flexibility" = 2.5,
          "HIIT" = 6.0
        ),
        "Intermediate" = list(
          "Cardio" = 6.0,
          "Strength" = 5.0,
          "Flexibility" = 3.0,
          "HIIT" = 6.0
        ),
        "Advanced" = list(
          "Cardio" = 8.0,
          "Strength" = 6.5,
          "Flexibility" = 3.5,
          "HIIT" = 10.0
        )
      )
      return(met_values[[fitness_level]][[exercise_type]])
    }
    
    # Function to calculate calories burned
    calculate_calories <- function(weight_kg, met, duration_min) {
      # Formula: calories = MET × weight(kg) × duration(hours)
      return(met * weight_kg * (duration_min/60))
    }
    
    # Improved exercise database with better progression, proper sets/reps, and timing
    exercise_database <- list(
      "Weight Management" = list(
        "Beginner" = list(
          "Cardio" = list(
            "Walking (brisk pace)" = list(description = "30 min at 3-4 mph pace", met = 3.8),
            "Stationary cycling (light)" = list(description = "25 min at low resistance, 60-65% MHR", met = 4.0),
            "Swimming (leisure)" = list(description = "20 min easy pace with breaks as needed", met = 5.0),
            "Water walking" = list(description = "25 min in chest-deep water", met = 4.5)
          ),
          "Strength" = list(
            "Bodyweight circuit" = list(description = "2-3 sets × 10-12 reps: squats, knee push-ups, chair dips, glute bridges", met = 3.5),
            "Resistance band workout" = list(description = "2 sets × 12-15 reps: band rows, shoulder press, lateral raises, bicep curls", met = 3.5),
            "Dumbbell basics" = list(description = "Light weights: 2 sets × 12-15 reps: goblet squats, chest press, bent-over rows", met = 3.0)
          ),
          "Flexibility" = list(
            "Basic stretching routine" = list(description = "10-15 min: Hold each stretch 20-30 sec, focus on hamstrings, hip flexors, shoulders", met = 2.5),
            "Beginner yoga" = list(description = "20 min focus on basic poses (cat-cow, child's pose, downward dog)", met = 2.5),
            "Gentle mobility exercises" = list(description = "15 min: 10 reps each of neck rolls, shoulder circles, hip circles, ankle rotations", met = 2.0)
          ),
          "HIIT" = list(
            "Beginner intervals" = list(description = "20 min: 30 sec easy exercise, 90 sec recovery × 8 rounds", met = 6.0),
            "Step intervals" = list(description = "15 min: 20 sec step ups, 40 sec marching in place × 15 rounds", met = 5.5),
            "Walk-jog intervals" = list(description = "20 min: 1 min brisk walk, 2 min regular pace × 7 rounds", met = 5.0)
          )
        ),
        "Intermediate" = list(
          "Cardio" = list(
            "Power walking/light jogging" = list(description = "30 min: alternate 3 min brisk walking, 2 min jogging × 6 rounds", met = 6.0),
            "Cycling (moderate)" = list(description = "35 min at moderate resistance, 65-75% MHR with 5 min warmup/cooldown", met = 6.5),
            "Elliptical trainer" = list(description = "30 min: 5 min warmup, 20 min 70-75% MHR, 5 min cooldown", met = 5.5),
            "Swimming laps" = list(description = "30 min: alternate 50m freestyle, 50m breaststroke, 30 sec rest", met = 6.0)
          ),
          "Strength" = list(
            "Dumbbell full-body workout" = list(description = "3 sets × 10-12 reps: squats, lunges, chest press, rows, shoulder press, tricep extensions", met = 5.0),
            "Circuit training" = list(description = "3 rounds × 8 exercises: 40 sec work/20 sec rest (squat jumps, push-ups, dumbbell rows, etc.)", met = 5.5),
            "Supersets routine" = list(description = "3 rounds of 4 supersets: pair pushing/pulling exercises, 12 reps each, 60 sec rest between sets", met = 5.0)
          ),
          "Flexibility" = list(
            "Dynamic stretching" = list(description = "15 min: 10 reps each of leg swings, arm circles, torso twists, walking lunges", met = 3.0),
            "Yoga flow" = list(description = "25 min vinyasa flow with sun salutations and warrior sequences", met = 3.5),
            "Pilates basics" = list(description = "25 min core focus: planks, bridges, leg circles, the hundred", met = 3.5)
          ),
          "HIIT" = list(
            "Tabata intervals" = list(description = "20 min: 20 sec high intensity, 10 sec rest × 8 rounds, 1 min recovery between exercises", met = 8.0),
            "Cardio circuit" = list(description = "25 min: 45 sec work, 15 sec rest × 5 exercises × 4 rounds", met = 7.5),
            "Bodyweight HIIT" = list(description = "30 min: 30 sec high intensity, 30 sec rest × 10 exercises × 3 rounds", met = 7.0)
          )
        ),
        "Advanced" = list(
          "Cardio" = list(
            "Running intervals" = list(description = "35 min: 1 min sprint (85-90% effort), 1 min jog (60% effort) × 12-15 rounds", met = 9.0),
            "High-intensity cycling" = list(description = "40 min: 5 min warmup, 30 sec sprint/90 sec recovery × 10, 15 min threshold, 5 min cooldown", met = 8.5),
            "Rowing machine" = list(description = "30 min: 5 min warmup, 30 sec all-out/90 sec moderate × 10, 5 min cooldown", met = 8.5),
            "Stair climbing" = list(description = "30 min: alternating 2 min high intensity, 1 min moderate pace", met = 9.0)
          ),
          "Strength" = list(
            "Heavy weight training" = list(description = "4 sets × 6-8 reps compound movements: deadlifts, squats, bench press, rows, overhead press", met = 6.5),
            "HIIT with weights" = list(description = "3 rounds of complex: 6 exercises × 45 sec work/15 sec rest, 2 min recovery between rounds", met = 8.0),
            "Advanced bodyweight" = list(description = "4 sets × 8-12 reps: pull-ups, dips, pistol squats, plyometric push-ups, L-sits", met = 6.0)
          ),
          "Flexibility" = list(
            "Advanced yoga" = list(description = "45 min power yoga with inversions, arm balances, and deep stretching", met = 4.0),
            "Deep stretching" = list(description = "30 min: PNF stretching techniques, hold each position 45-60 sec", met = 3.0),
            "Dynamic mobility" = list(description = "20 min complex movement patterns with focus on thoracic spine, hips and ankles", met = 3.5)
          ),
          "HIIT" = list(
            "Pyramid intervals" = list(description = "40 min: work intervals increasing 30/45/60/90 sec with equal recovery × 3 rounds", met = 9.5),
            "Advanced circuit" = list(description = "45 min: 50 sec work/10 sec transition × 6 exercises × 4 rounds with compound movements", met = 10.0),
            "CrossFit-style WOD" = list(description = "30 min AMRAP (As Many Rounds As Possible) of 5 compound exercises", met = 10.0)
          )
        )
      ),
      "Blood Sugar Control" = list(
        "Beginner" = list(
          "Cardio" = list(
            "Post-meal walking" = list(description = "15-20 min walk within 30 min after eating major meals", met = 3.5),
            "Light cycling" = list(description = "20 min steady pace, 55-65% MHR", met = 4.0),
            "Water aerobics" = list(description = "25 min gentle movements, focus on continuous motion", met = 4.0)
          ),
          "Strength" = list(
            "Resistance band training" = list(description = "2 sets × 12-15 reps: focus on major muscle groups (chest, back, legs, arms)", met = 3.0),
            "Light dumbbell circuit" = list(description = "2 sets × 12-15 reps: 1-3 lb weights for upper body, 5-8 lb for lower body", met = 3.5),
            "Chair exercises" = list(description = "20 min seated and standing exercises: chair squats, seated rows, calf raises", met = 2.8)
          ),
          "Flexibility" = list(
            "Gentle yoga" = list(description = "20 min: focus on breathing and holding poses for 20-30 sec each", met = 2.5),
            "Tai chi basics" = list(description = "20 min simple movements with focus on balance and breathing", met = 3.0),
            "Standing stretches" = list(description = "15 min full body routine focusing on major muscle groups", met = 2.5)
          ),
          "HIIT" = list(
            "Very light intervals" = list(description = "15 min: 20 sec moderate effort, 100 sec recovery × 5 rounds", met = 5.0),
            "Walk-based intervals" = list(description = "20 min: 1 min brisk walk, 2 min casual pace × 7 rounds", met = 4.5),
            "Chair-supported intervals" = list(description = "15 min: 30 sec stand-sit transitions, 90 sec recovery × 6 rounds", met = 4.0)
          )
        ),
        "Intermediate" = list(
          "Cardio" = list(
            "Interval walking" = list(description = "30 min: 2 min brisk pace (70-75% MHR), 1 min recovery pace × 10 rounds", met = 5.0),
            "Swimming" = list(description = "30 min continuous swimming, alternating strokes every 50-100m", met = 6.0),
            "Dance workout" = list(description = "25 min moderate intensity with 5 min warmup and cooldown", met = 5.5)
          ),
          "Strength" = list(
            "Moderate weight circuit" = list(description = "3 sets × 10-12 reps: 8 exercises rotating between upper/lower body", met = 5.0),
            "Bodyweight supersets" = list(description = "3 sets × 12-15 reps: pair pushing/pulling exercises with minimal rest", met = 4.5),
            "Resistance training" = list(description = "3 sets × 10-12 reps: focus on large muscle groups with 60-90 sec rest", met = 5.0)
          ),
          "Flexibility" = list(
            "Yoga flow" = list(description = "30 min with breath work: sun salutations and balanced sequences", met = 3.0),
            "Pilates" = list(description = "25 min core and stability focus with controlled breathing", met = 3.5),
            "Dynamic flexibility" = list(description = "20 min active stretching: 10-12 reps of movement patterns", met = 3.0)
          ),
          "HIIT" = list(
            "Moderate intervals" = list(description = "25 min: 30 sec moderate-high effort, 90 sec active recovery × 10 rounds", met = 7.0),
            "Circuit intervals" = list(description = "30 min: 40 sec work, 20 sec rest × 6 exercises × 3 rounds", met = 6.5),
            "Cardio-strength blend" = list(description = "25 min alternating 1 min cardio, 1 min strength × 12 rounds", met = 6.0)
          )
        ),
        "Advanced" = list(
          "Cardio" = list(
            "HIIT cardio" = list(description = "30 min: 30 sec high intensity (85-90% MHR), 30 sec active recovery × 15 rounds", met = 8.0),
            "Trail running" = list(description = "35 min varied terrain with hill climbs and technical sections", met = 9.0),
            "Rowing intervals" = list(description = "30 min: 1 min hard (85% effort), 1 min moderate (65% effort) × 15 rounds", met = 8.5)
          ),
          "Strength" = list(
            "Progressive weight training" = list(description = "4 sets × 6-12 reps: increasing weight each set, 90-120 sec rest", met = 6.0),
            "Advanced circuit training" = list(description = "3 rounds of 10 stations: 50 sec work, 10 sec transition, 2 min between rounds", met = 7.0),
            "Functional fitness" = list(description = "4 sets × 8-10 reps: complex movements combining multiple joints/planes", met = 6.5)
          ),
          "Flexibility" = list(
            "Power yoga" = list(description = "45 min advanced sequence with challenging poses and transitions", met = 4.0),
            "Advanced Pilates" = list(description = "35 min with equipment (reformer, chair, or small apparatus)", met = 3.8),
            "Mobility flow" = list(description = "25 min dynamic patterns focusing on joint control and range", met = 3.5)
          ),
          "HIIT" = list(
            "Advanced intervals" = list(description = "35 min: 30 sec sprint/max effort, 30 sec recovery × 20 rounds", met = 9.0),
            "Complex conditioning" = list(description = "40 min: 4 exercises back-to-back × 5 rounds, 2 min recovery between rounds", met = 8.5),
            "Metabolic resistance" = list(description = "30 min: heavy weights with minimal rest, 6 exercises × 5 rounds", met = 8.0)
          )
        )
      ),
      "Overall Health" = list(
        "Beginner" = list(
          "Cardio" = list(
            "Brisk walking" = list(description = "30 min moderate pace (3-3.5 mph)", met = 3.8),
            "Stationary bike" = list(description = "25 min light resistance (RPE 3-4/10)", met = 4.0),
            "Swimming (leisure)" = list(description = "25 min easy pace with breaks as needed", met = 5.0)
          ),
          "Strength" = list(
            "Bodyweight basics" = list(description = "2 sets × 10-12 reps: squats, wall push-ups, assisted lunges, glute bridges", met = 3.0),
            "Light resistance training" = list(description = "2 sets × 12-15 reps: major muscle groups with light weights/bands", met = 3.5),
            "Stability exercises" = list(description = "2 sets × 30-60 sec holds: planks, wall sits, single-leg balances", met = 2.8)
          ),
          "Flexibility" = list(
            "Basic stretching" = list(description = "15 min full body: hold each stretch 20-30 sec, 2-3 reps per stretch", met = 2.5),
            "Gentle yoga" = list(description = "20 min foundation poses with focus on breathing and alignment", met = 2.5),
            "Joint mobility" = list(description = "15 min range of motion exercises for all major joints", met = 2.0)
          ),
          "HIIT" = list(
            "Intro intervals" = list(description = "15 min: 20 sec moderate effort, 100 sec recovery × 5 rounds", met = 5.0),
            "Light cardio circuit" = list(description = "20 min: alternate 45 sec easy exercise, 75 sec rest × 10 rounds", met = 4.5),
            "Beginner metabolic" = list(description = "20 min: 30 sec light effort, 90 sec very light effort × 10 rounds", met = 4.0)
          )
        ),
        "Intermediate" = list(
          "Cardio" = list(
            "Jogging" = list(description = "30 min steady pace (5-6 mph or comfortable conversation pace)", met = 7.0),
            "Cycling" = list(description = "35 min moderate intensity (RPE 5-7/10)", met = 6.0),
            "Swimming" = list(description = "30 min mixed strokes with minimal rest", met = 6.0)
          ),
          "Strength" = list(
            "Moderate weight training" = list(description = "3 sets × 10-12 reps: full body workout with moderate weights", met = 5.0),
            "Circuit training" = list(description = "3 rounds × 8 exercises: 45 sec work/15 sec rest", met = 5.5),
            "Bodyweight progression" = list(description = "3 sets × 10-12 reps: push-ups, bodyweight squats, dips, inverted rows", met = 4.5)
          ),
          "Flexibility" = list(
            "Yoga" = list(description = "30 min intermediate sequence with flowing transitions", met = 3.0),
            "Pilates" = list(description = "25 min core focus with proper breathing techniques", met = 3.5),
            "Active stretching" = list(description = "20 min dynamic movement patterns through full ROM", met = 3.0)
          ),
          "HIIT" = list(
            "Mixed intervals" = list(description = "25 min: 40 sec moderate-high intensity, 80 sec recovery × 10 rounds", met = 7.0),
            "Cardio-strength circuit" = list(description = "30 min: alternate 1 min cardio, 1 min strength × 15 rounds", met = 6.5),
            "Tempo training" = list(description = "30 min: 3 min moderate, 2 min hard, 1 min easy × 5 rounds", met = 6.0)
          )
        ),
        "Advanced" = list(
          "Cardio" = list(
            "Running" = list(description = "40 min with varying pace (intervals of tempo, hills, recovery)", met = 8.0),
            "HIIT training" = list(description = "35 min: structured intervals with work:rest ratio of 1:1 or 2:1", met = 8.5),
            "Cycling (vigorous)" = list(description = "45 min with hills/sprints (RPE 7-9/10 during hard efforts)", met = 8.0)
          ),
          "Strength" = list(
            "Advanced weight training" = list(description = "4 sets × 8-10 reps: split routine focusing on specific muscle groups", met = 6.5),
            "Plyometrics" = list(description = "3 sets × 8-10 reps: box jumps, jump squats, clap push-ups, bounds", met = 7.0),
            "Olympic lifting" = list(description = "5 sets × 3-5 reps: clean & jerk, snatch variations with proper form", met = 7.0)
          ),
          "Flexibility" = list(
            "Power yoga" = list(description = "45 min advanced practice with challenging poses and flows", met = 4.0),
            "Advanced stretching" = list(description = "30 min deep stretches with PNF techniques", met = 3.0),
            "Movement patterns" = list(description = "30 min complex mobility working multiple planes of motion", met = 3.5)
          ),
          "HIIT" = list(
            "Advanced intervals" = list(description = "35 min: multiple work:rest ratios (30:30, 45:15, 60:20) × 5 rounds", met = 9.0),
            "Complex conditioning" = list(description = "40 min: 5 exercises × 6 rounds with minimal rest", met = 8.5),
            "Sport-specific HIIT" = list(description = "45 min: mimic sport movements at high intensity with controlled recovery", met = 8.0)
          )
        )
      ),
      "Cardiovascular Fitness" = list(
        "Beginner" = list(
          "Cardio" = list(
            "Brisk walking" = list(description = "30 min moderate pace with proper arm swing", met = 3.8),
            "Stationary bike" = list(description = "25 min gradually increasing resistance (RPE 3-5/10)", met = 4.0),
            "Swimming (leisure)" = list(description = "25 min alternating between strokes and resting as needed", met = 5.0),
            "Step exercises" = list(description = "20 min basic stepping patterns on flat surface or low step", met = 4.0)
          ),
          "Strength" = list(
            "Bodyweight circuit" = list(description = "2 sets × 12-15 reps: exercises that elevate heart rate", met = 3.5),
            "Light cardio resistance" = list(description = "2 sets × 15-20 reps: light weights with minimal rest", met = 3.5),
            "Step-based exercises" = list(description = "2 sets × 12-15 reps: step-ups, elevated lunges, box taps", met = 3.5)
          ),
          "Flexibility" = list(
            "Recovery stretching" = list(description = "15 min focusing on muscle groups used during cardio", met = 2.5),
            "Gentle yoga" = list(description = "20 min with focus on breathing capacity", met = 2.5),
            "Joint mobility" = list(description = "15 min for ankles, knees, hips to support cardio activities", met = 2.0)
          ),
          "HIIT" = list(
            "Walk-jog intervals" = list(description = "20 min: 1 min brisk walk, 2 min slower walk × 7 rounds", met = 5.0),
            "Step intervals" = list(description = "15 min: 30 sec faster stepping, 90 sec regular pace × 7 rounds", met = 4.5),
            "Cardio alternations" = list(description = "20 min: alternate 1 min each between two low-impact exercises", met = 4.5)
          )
        ),
        "Intermediate" = list(
          "Cardio" = list(
            "Jogging intervals" = list(description = "35 min: 3 min jog (70-75% MHR), 1 min walk × 8-9 rounds", met = 7.0),
            "Cycling (moderate hills)" = list(description = "40 min with varied resistance (RPE 5-7/10)", met = 6.5),
            "Swimming laps" = list(description = "30 min continuous with minimal wall rest", met = 7.0),
            "Cardio machines" = list(description = "35 min rotating between 2-3 different machines", met = 6.0)
          ),
          "Strength" = list(
            "Cardio-strength circuit" = list(description = "3 sets × 12-15 reps: strength exercises with minimal rest", met = 5.5),
            "Endurance training" = list(description = "3 sets × 15-20 reps: lighter weights, higher repetitions", met = 5.0),
            "Hill training" = list(description = "30 min incline walking/jogging (4-8% grade)", met = 6.0)
          ),
          "Flexibility" = list(
            "Dynamic stretching" = list(description = "20 min active movement to increase range of motion", met = 3.0),
            "Recovery yoga" = list(description = "25 min with breathwork to enhance oxygen efficiency", met = 3.0),
            "Foam rolling" = list(description = "20 min myofascial release focusing on legs and back", met = 2.5)
          ),
          "HIIT" = list(
            "Pyramid intervals" = list(description = "30 min: increasing work periods (30/45/60/75/60/45/30 sec)", met = 7.5),
            "Tempo intervals" = list(description = "30 min: 3 min threshold pace, 2 min recovery × 6 rounds", met = 7.0),
            "Mixed cardio circuit" = list(description = "30 min: 3 different cardio modalities × 10 min each", met = 6.5)
          )
        ),
        "Advanced" = list(
          "Cardio" = list(
            "Speed intervals" = list(description = "40 min: 1 min sprint (85-90% MHR), 1 min jog (60-65% MHR) × 15-20 rounds", met = 9.0),
            "Hill sprints" = list(description = "35 min: 30-45 sec uphill effort, jog down recovery × 10-12 rounds", met = 10.0),
            "Advanced cycling" = list(description = "45 min with high resistance intervals (RPE 8-9/10)", met = 8.5),
            "Cardio cross-training" = list(description = "50 min multi-machine circuit with minimal transition time", met = 8.0)
          ),
          "Strength" = list(
            "Metabolic conditioning" = list(description = "40 min circuit: 45 sec work/15 sec rest × 8 exercises × 4 rounds", met = 8.0),
            "Endurance strength" = list(description = "3 sets × 15-20 reps: compound movements with 30 sec rest", met = 5.5),
            "CrossFit-style workout" = list(description = "30 min AMRAP or EMOM format with compound movements", met = 8.5)
          ),
          "Flexibility" = list(
            "Active recovery" = list(description = "25 min mobility and stretching targeting performance limiters", met = 3.0),
            "Yoga for athletes" = list(description = "30 min targeted stretching for runners/cyclists", met = 3.5),
            "Advanced mobility" = list(description = "25 min dynamic movement patterns for performance enhancement", met = 3.5)
          ),
          "HIIT" = list(
            "VO2max intervals" = list(description = "40 min: 2-4 min at 90-95% effort, equal recovery × 6-8 rounds", met = 10.0),
            "Lactate threshold sessions" = list(description = "45 min with 5-8 min sustained hard efforts (RPE 8/10)", met = 9.0),
            "Advanced fartlek" = list(description = "40 min: unpredictable intervals of varying length and intensity", met = 8.5)
          )
        )
      )
    )
    
    # Get exercise database for the selected goal and fitness level
    selected_exercises <- exercise_database[[input$exercise_goal]][[input$fitness_level]]
    
    # Define days of week for schedule
    days_of_week <- c("Monday", "Wednesday", "Friday", "Tuesday", "Thursday", "Saturday", "Sunday")
    
    # Select required number of days
    selected_days <- days_of_week[1:input$days_per_week]
    
    # Create intelligent exercise distribution
    # For balanced training, distribute exercise types based on goal
    exercise_distribution <- list()
    
    if(input$exercise_goal == "Weight Management") {
      exercise_distribution <- list(
        "Cardio" = 0.35,  
        "Strength" = 0.35,
        "Flexibility" = 0.1,
        "HIIT" = 0.20
      )
    } else if(input$exercise_goal == "Blood Sugar Control") {
      exercise_distribution <- list(
        "Cardio" = 0.30,  
        "Strength" = 0.40, 
        "Flexibility" = 0.15,
        "HIIT" = 0.15
      )
    } else if(input$exercise_goal == "Overall Health") {
      exercise_distribution <- list(
        "Cardio" = 0.35,  
        "Strength" = 0.35, 
        "Flexibility" = 0.20,
        "HIIT" = 0.15  
      )
    } else if(input$exercise_goal == "Cardiovascular Fitness") {
      exercise_distribution <- list(
        "Cardio" = 0.40,  
        "Strength" = 0.30, 
        "Flexibility" = 0.15,  
        "HIIT" = 0.15 
      )
    }
    
    # Calculate number of days for each type based on distribution
    days_count <- list(
      "Cardio" = round(input$days_per_week * exercise_distribution[["Cardio"]]),
      "Strength" = round(input$days_per_week * exercise_distribution[["Strength"]]),
      "Flexibility" = round(input$days_per_week * exercise_distribution[["Flexibility"]]),
      "HIIT" = round(input$days_per_week * exercise_distribution[["HIIT"]])
    )

    # Ensure we have at least one day for each if possible
    if(sum(unlist(days_count)) < input$days_per_week) {
      # Add remaining days to cardio
      days_count[["Cardio"]] <- days_count[["Cardio"]] + (input$days_per_week - sum(unlist(days_count)))
    } else if(sum(unlist(days_count)) > input$days_per_week) {
      excess <- sum(unlist(days_count)) - input$days_per_week
      if(days_count[["Flexibility"]] > excess) {
        days_count[["Flexibility"]] <- days_count[["Flexibility"]] - excess
      } else {
        excess_remaining <- excess - days_count[["Flexibility"]]
        days_count[["Flexibility"]] <- max(0, days_count[["Flexibility"]] - excess)
        
        if(days_count[["HIIT"]] > excess_remaining) {
          days_count[["HIIT"]] <- days_count[["HIIT"]] - excess_remaining
        } else {
          excess_remaining <- excess_remaining - days_count[["HIIT"]]
          days_count[["HIIT"]] <- max(0, days_count[["HIIT"]])
          days_count[["Strength"]] <- days_count[["Strength"]] - excess_remaining
        }
      }
    }
    
    # Create exercise types array based on distribution
    exercise_types <- c(
      rep("Cardio", days_count[["Cardio"]]),
      rep("Strength", days_count[["Strength"]]),
      rep("Flexibility", days_count[["Flexibility"]]),
      rep("HIIT", days_count[["HIIT"]])
    )
    
    # If we need more exercise types to match selected days
    if(length(exercise_types) < input$days_per_week) {
      additional_types <- rep(c("Cardio", "Strength","HIIT"), input$days_per_week)[1:(input$days_per_week - length(exercise_types))]
      exercise_types <- c(exercise_types, additional_types)
    }
    
    # Shuffle exercise types for better distribution through the week
    if(input$days_per_week > 1) {
      exercise_types <- sample(exercise_types, input$days_per_week)
    }
    
    # Create exercise schedule
    exercise_schedule <- data.frame(
      Day = selected_days,
      Exercise_Type = exercise_types,
      Duration = rep(input$minutes_per_session, input$days_per_week),
      stringsAsFactors = FALSE
    )
    
    # Select specific exercises for each day, avoid repeating the same exercise
    selected_exercise_names <- list()
    selected_exercise_details <- list()
    selected_exercise_mets <- list()
    
    for(i in 1:nrow(exercise_schedule)) {
      type <- exercise_schedule$Exercise_Type[i]
      available_exercises <- selected_exercises[[type]]
      exercise_names <- names(available_exercises)
      
      # Avoid repeating exercises when possible
      if(type %in% names(selected_exercise_names)) {
        used_exercises <- selected_exercise_names[[type]]
        available_names <- setdiff(exercise_names, used_exercises)
        
        # If we've used all exercises of this type, reset selection
        if(length(available_names) == 0) {
          available_names <- exercise_names
        }
        
        selected <- sample(available_names, 1)
        selected_exercise_names[[type]] <- c(selected_exercise_names[[type]], selected)
      } else {
        selected <- sample(exercise_names, 1)
        selected_exercise_names[[type]] <- selected
      }
      
      # Store the exercise details
      selected_exercise_details[[i]] <- available_exercises[[selected]]$description
      selected_exercise_mets[[i]] <- available_exercises[[selected]]$met
    }
    
    # Add exercises and calculated calories to schedule
    exercise_schedule$Specific_Exercise <- unlist(selected_exercise_names)
    exercise_schedule$Exercise_Details <- unlist(selected_exercise_details)
    
    # Calculate calories burned more accurately for each session
    exercise_schedule$Calories_Burned <- mapply(function(met, duration) {
      round(calculate_calories(input$weight, met, duration))
    }, unlist(selected_exercise_mets), exercise_schedule$Duration)
    
    # Calculate weekly calorie burn
    weekly_cal_burn <- sum(exercise_schedule$Calories_Burned)
    
    # Calculate scientific benefits after 3 months based on research
    # Blood sugar reduction based on exercise frequency and intensity
    blood_sugar_improvement <- 0
    if(input$exercise_goal == "Blood Sugar Control") {
      # Based on research showing 10-20% improvement in blood glucose control with regular exercise
      blood_sugar_improvement <- 8 + (input$days_per_week * 1.5) + (as.numeric(factor(input$fitness_level, levels = c("Beginner", "Intermediate", "Advanced"))) * 2)
    } else {
      blood_sugar_improvement <- 5 + (input$days_per_week * 1.2) + (as.numeric(factor(input$fitness_level, levels = c("Beginner", "Intermediate", "Advanced"))) * 1.5)
    }
    
    # Cardiovascular improvement (VO2 max increase %)
    cardio_improvement <- 0
    if(input$exercise_goal == "Cardiovascular Fitness") {
      cardio_improvement <- 8 + (input$days_per_week * 2) + (as.numeric(factor(input$fitness_level, levels = c("Beginner", "Intermediate", "Advanced"))) * 1)
    } else {
      cardio_improvement <- 5 + (input$days_per_week * 1.5) + (as.numeric(factor(input$fitness_level, levels = c("Beginner", "Intermediate", "Advanced"))) * 0.5)
    }
    
    # Weight management (more conservative and scientifically based)
    weight_change <- 0
    if(input$exercise_goal == "Weight Management") {
      # Base on 3500 kcal deficit = 1 lb weight loss (0.45 kg)
      # Calculate 12 weeks of deficit at current exercise rate
      total_deficit <- weekly_cal_burn * 12
      estimated_weight_loss <- total_deficit / 7700  # Approximate kcal per kg
      
      # Cap at reasonable amount for 3 months based on starting weight
      max_safe_loss <- input$weight * 0.06  # Max 6% in 3 months
      weight_change <- min(estimated_weight_loss, max_safe_loss)
    } else {
      # For non-weight focused goals, estimate modest changes
      total_deficit <- weekly_cal_burn * 12
      estimated_weight_loss <- total_deficit / 7700 * 0.7  # Accounting for compensation in diet
      weight_change <- min(estimated_weight_loss, input$weight * 0.03)  # Max 3% in 3 months
    }
    
    # Additional health benefits
    health_benefits <- list(
      "Blood Pressure" = paste0("-", round(2 + (input$days_per_week * 0.5)), " mm Hg (systolic)"),
      "Resting Heart Rate" = paste0("-", round(3 + (input$days_per_week * 0.7)), " bpm"),
      "Sleep Quality" = paste0("+", round(10 + (input$days_per_week * 2)), "%")
    )
    
    # Compile all benefits
    benefits <- list(
      "Weight" = round(input$weight - weight_change, 1),
      "Weight_Change" = round(weight_change, 1),
      "Blood_Sugar_Reduction" = round(blood_sugar_improvement),
      "Cardio_Fitness_Improvement" = round(cardio_improvement),
      "Additional" = health_benefits
    )
    
    # Create weekly summary for different exercise types
    weekly_summary <- list(
      "Cardio" = paste0(days_count[["Cardio"]], " days, ", 
                      days_count[["Cardio"]] * input$minutes_per_session, " total minutes"),
      "Strength" = paste0(days_count[["Strength"]], " days, ", 
                        days_count[["Strength"]] * input$minutes_per_session, " total minutes"),
      "Flexibility" = paste0(days_count[["Flexibility"]], " days, ", 
                          days_count[["Flexibility"]] * input$minutes_per_session, " total minutes"),
      "HIIT" = paste0(days_count[["HIIT"]], " days, ", 
                days_count[["HIIT"]] * input$minutes_per_session, " total minutes")                    
    )
    
    return(list(
      schedule = exercise_schedule,
      weekly_cal_burn = weekly_cal_burn,
      benefits = benefits,
      weekly_summary = weekly_summary,
      weekly_exercise = selected_exercises
    ))
  })

  # Output for exercise plan summary
  output$exercise_plan_summary <- renderUI({
    plan <- exercise_plan()
    total_weekly_minutes <- input$days_per_week * input$minutes_per_session
    
    # Create a more professional summary
    div_style <- "margin-bottom: 15px;"
    
    # Generate appropriate recommendations text based on goal
    recommendation_text <- if(input$exercise_goal == "Blood Sugar Control") {
      "This plan balances cardio, strength, and flexibility exercises to optimize glucose metabolism. Research shows consistent exercise can reduce HbA1c by 0.5-0.7% over 3-6 months."
    } else if(input$exercise_goal == "Weight Management") {
      "This plan focuses on a sustainable mix of cardio and strength training to maximize calorie burn while preserving muscle mass, which helps maintain metabolic rate during weight loss."
    } else if(input$exercise_goal == "Cardiovascular Fitness") {
      "This plan emphasizes progressive cardio training with supporting strength work to improve heart health, endurance, and VO2 max, with research showing 10-15% improvement in 12 weeks."
    } else {
      "This balanced plan incorporates all exercise components to improve overall health markers including blood pressure, cholesterol levels, and mental wellbeing."
    }
    
    # Create weekly exercise distribution summary
    weekly_distribution <- paste(
      "<strong>Weekly distribution:</strong>",
      paste0("<span style='color:#3498db'>", plan$weekly_summary$Cardio, " of cardio</span>"), 
      paste0("<span style='color:#e74c3c'>", plan$weekly_summary$Strength, " of strength</span>"),
      paste0("<span style='color:#2ecc71'>", plan$weekly_summary$Flexibility, " of flexibility</span>"),
      paste0("<span style='color:#9b59b6'>", plan$weekly_summary$HIIT, " of HIIT</span>"),
      sep = " | "
    )
    
    tagList(
      div(style = div_style,
          HTML(paste0("<strong>Total weekly exercise:</strong> ", total_weekly_minutes, " minutes <br>",
                    "<strong>Estimated calories burned per week:</strong> ", round(plan$weekly_cal_burn), " calories <br>")),
          HTML(paste0("<strong>Level:</strong> ", input$fitness_level, " | ",
                    "<strong>Goal:</strong> ", input$exercise_goal))
      ),
      div(style = div_style,
          HTML(weekly_distribution)
      ),
      div(style = div_style,
          p(recommendation_text)
      )
    )
  })

  # Output for exercise schedule table (continued)
  output$exercise_schedule <- renderTable({
    exercise_plan()$schedule %>%
      select(Day, Exercise_Type, Specific_Exercise, Exercise_Details, Duration, Calories_Burned) %>%
      rename(
        "Day of Week" = Day,
        "Type" = Exercise_Type,
        "Exercise" = Specific_Exercise,
        "Description" = Exercise_Details,
        "Minutes" = Duration,
        "Est. Calories" = Calories_Burned
      )
  }, striped = TRUE, bordered = TRUE, align = "c")

  # Output for exercise benefits plot
  output$exercise_benefits_plot <- renderPlotly({
    plan <- exercise_plan()
    
    # Create data frame for estimated benefits
    benefits_data <- data.frame(
      Metric = c("Blood Sugar Level<br>Reduction (mg/dL)", 
                "Cardiovascular<br>Fitness Improvement (%)", 
                "Est. Weight<br>Reduction (kg)"),
      Value = c(
        plan$benefits$Blood_Sugar_Reduction,
        plan$benefits$Cardio_Fitness_Improvement,
        plan$benefits$Weight_Change
      )
    )

    # Create horizontal bar chart with improved styling
    plot_ly(
      benefits_data,
      x = ~Value,
      y = ~Metric,
      type = "bar",
      orientation = "h",
      marker = list(
        color = c("#e74c3c", "#3498db", "#2ecc71"),
        line = list(color = "rgba(0,0,0,0)", width = 1)
      ),
      text = ~Value,
      textposition = "outside",
      hoverinfo = "text",
      hovertext = ~paste(Metric, ": ", Value)
    ) %>%
      layout(
        title = list(text = "Estimated 3-Month Benefits", y = 0.95),
        xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE),
        yaxis = list(title = "", categoryorder = "array", categoryarray = rev(benefits_data$Metric)),
        margin = list(l = 20, r = 30, t = 40, b = 30),
        showlegend = FALSE
      )
  })

  # Output for exercise insights
  output$exercise_insights <- renderUI({
    plan <- exercise_plan()
    
    # Generate evidence-based insights based on goal and fitness level
    primary_insight <- if(input$exercise_goal == "Blood Sugar Control") {
      if(input$fitness_level == "Beginner") {
        "Research shows even light activity can reduce post-meal blood glucose by 30-40mg/dL. Your plan emphasizes consistent activity throughout the week to improve insulin sensitivity."
      } else if(input$fitness_level == "Intermediate") {
        "Studies indicate that alternating between cardio and resistance training can optimize glycemic control. Your plan incorporates both with a focus on moderate-intensity activities that research links to improved insulin function."
      } else {
        "At your advanced fitness level, research supports incorporating higher-intensity intervals which have been shown to improve insulin sensitivity for up to 48 hours post-exercise. Your plan leverages this effect with strategic high-intensity sessions."
      }
    } else if(input$exercise_goal == "Weight Management") {
      if(input$fitness_level == "Beginner") {
        "For sustainable weight management, your plan focuses on gradually building exercise consistency. Studies show that consistent moderate activity of 150+ minutes weekly helps maintain weight loss long-term."
      } else if(input$fitness_level == "Intermediate") {
        "Research indicates that combining cardio with progressive resistance training optimizes body composition during weight management. Your plan balances both to preserve muscle mass while creating a calorie deficit."
      } else {
        "At your advanced level, your plan incorporates metabolic conditioning and higher-intensity intervals shown in studies to maximize calorie expenditure both during and after exercise through EPOC (excess post-exercise oxygen consumption)."
      }
    } else if(input$exercise_goal == "Cardiovascular Fitness") {
      if(input$fitness_level == "Beginner") {
        "Your plan establishes the foundation for cardiovascular health with activities that gradually increase heart rate. Research shows even moderate-intensity exercise can reduce cardiovascular disease risk by 14-20%."
      } else if(input$fitness_level == "Intermediate") {
        "To improve cardiorespiratory fitness, your plan incorporates interval training that research shows can increase VO₂ max by 15-20% over 12 weeks when performed consistently."
      } else {
        "Your advanced plan leverages research on polarized training - combining high-intensity intervals with targeted recovery sessions - which elite athletes use to optimize cardiovascular performance and VO₂ max improvement."
      }
    } else {
      if(input$fitness_level == "Beginner") {
        "Your plan focuses on establishing fundamental movement patterns and building consistency. Research shows that achieving 150 minutes of moderate activity weekly provides up to 80% of total possible health benefits from exercise."
      } else if(input$fitness_level == "Intermediate") {
        "For overall health optimization, your plan balances cardiorespiratory, muscular strength, and mobility work - a combination shown in research to improve multiple biomarkers simultaneously."
      } else {
        "At your advanced level, your diverse plan targets all components of fitness with progressive overload - research indicates this comprehensive approach maximizes longevity benefits and functional capacity."
      }
    }
    
    # Create actionable tips based on goal and fitness level
    actionable_tip <- if(input$exercise_goal == "Blood Sugar Control") {
      if(input$fitness_level == "Beginner") {
        "Tip: Time your walks for 30-45 minutes after meals when possible - research shows this timing can reduce post-meal glucose spikes by up to 30%."
      } else if(input$fitness_level == "Intermediate") {
        "Tip: Consider performing brief (10-minute) bouts of resistance exercises before meals, which studies suggest can improve post-meal glucose handling."
      } else {
        "Tip: Monitor recovery between high-intensity sessions - research indicates overtraining can temporarily reduce insulin sensitivity. Aim for at least 48 hours between intense sessions."
      }
    } else if(input$exercise_goal == "Weight Management") {
      if(input$fitness_level == "Beginner") {
        "Tip: Focus on consistency over intensity. Research shows those who maintain 90% adherence to planned exercise sessions have 3× better long-term results than those with inconsistent patterns."
      } else if(input$fitness_level == "Intermediate") {
        "Tip: Consider tracking heart rate during workouts to optimize intensity. Studies show maintaining heart rate at 70-85% of maximum during cardio sessions maximizes fat utilization."
      } else {
        "Tip: Research suggests periodizing your training intensity with planned 'deload' weeks (reduced intensity) every 4-6 weeks improves long-term adherence and prevents plateaus."
      }
    } else if(input$exercise_goal == "Cardiovascular Fitness") {
      if(input$fitness_level == "Beginner") {
        "Tip: Use the 'talk test' to gauge intensity - you should be able to speak in short sentences but not sing. Research shows this corresponds to optimal training zones for cardiovascular improvement."
      } else if(input$fitness_level == "Intermediate") {
        "Tip: Consider incorporating hill workouts or incline training once weekly - research indicates this efficiently builds cardiac stroke volume and VO₂ max."
      } else {
        "Tip: Research supports including some longer, steady-state sessions (60+ minutes) in addition to intervals to build cardiovascular endurance and improve mitochondrial density."
      }
    } else {
      if(input$fitness_level == "Beginner") {
        "Tip: Focus on proper form before increasing intensity. Studies show mastering movement patterns first reduces injury risk by up to 60% and improves long-term progress."
      } else if(input$fitness_level == "Intermediate") {
        "Tip: Track your resting heart rate over time - research shows a reduction of 10+ bpm correlates with significant improvements in cardiorespiratory fitness and overall health."
      } else {
        "Tip: Consider incorporating skill-based activities (e.g., dance, martial arts, sports) which research links to improved neuroplasticity and cognitive function alongside physical benefits."
      }
    }
    
    # Create specialized tip based on specific health needs
    diabetes_tip <- if(input$exercise_goal == "Blood Sugar Control") {
      "Monitor your blood glucose before and after exercise sessions to understand your body's response patterns. Many people notice improved glucose levels for 24-48 hours following activity."
    } else {
      "Remember that consistent exercise improves insulin sensitivity over time, which helps regulate blood glucose levels and may reduce medication needs (always consult your healthcare provider)."
    }
    
    # Create mental health benefit note
    mental_health_note <- "Beyond physical benefits, this exercise plan can improve mood, reduce anxiety, enhance sleep quality, and boost cognitive function through increased brain-derived neurotrophic factor (BDNF) production."
    
    tagList(
      p(primary_insight, style = "font-weight: 500;"),
      hr(style = "margin: 10px 0;"),
      p(actionable_tip),
      p(diabetes_tip),
      p(mental_health_note, style = "font-style: italic; margin-top: 10px;")
    )
  })

  # Output for additional health information
  output$additional_health_benefits <- renderUI({
    plan <- exercise_plan()
    
    # Calculate additional metrics
    additional_benefits <- plan$benefits$Additional
    
    tagList(
      h4("Additional Health Benefits", class = "section-title"),
      div(class = "row",
          div(class = "col-md-4",
              div(class = "benefit-box",
                  strong("Blood Pressure"),
                  p(additional_benefits$`Blood Pressure`),
                  p("Systolic reduction after 3 months", class = "small-text")
              )
          ),
          div(class = "col-md-4",
              div(class = "benefit-box",
                  strong("Resting Heart Rate"),
                  p(additional_benefits$`Resting Heart Rate`),
                  p("Reduction in beats per minute", class = "small-text")
              )
          ),
          div(class = "col-md-4",
              div(class = "benefit-box",
                  strong("Sleep Quality"),
                  p(additional_benefits$`Sleep Quality`),
                  p("Estimated improvement", class = "small-text")
              )
          )
      ),
      p("Note: Individual results may vary. These estimates are based on clinical research on similar exercise programs.", class = "disclaimer-text")
    )
  })




  # Load Indian food nutrition dataset
  food_database <- reactive({
    df <- read.csv("data/Indian_Food_Nutrition_Processed.csv")
    # Print original column names for debugging
    print("Original column names:")
    print(names(df))
    
    # Clean the column names properly
    clean_names <- tolower(gsub("\\.", "_", gsub(" ", "_", names(df))))
    names(df) <- clean_names
    
    # Print cleaned column names for debugging
    print("Cleaned column names:")
    print(names(df))
    
    return(df)  
    })
   
  # Add this after your food_database reactive
  observe({
    req(food_database())
    print("First few rows of food database:")
    print(head(food_database()))
  })
  
  # Initialize meal data with the correct columns
  meal_data <- reactiveVal(data.frame(
    dish_name = character(),
    serving_size = numeric(),
    calories = numeric(),
    carbs = numeric(),
    protein = numeric(),
    fats = numeric(),
    free_sugar = numeric(),
    fibre = numeric(),
    sodium = numeric(),
    calcium = numeric(),
    iron = numeric(),
    vitamin_c = numeric(),
    folate = numeric(),
    stringsAsFactors = FALSE
  ))

 
  # Update food item choices
  observe({
    req(food_database())
    req("dish_name" %in% names(food_database()))
    dish_choices <- sort(unique(food_database()$dish_name))
    print(paste("Found", length(dish_choices), "unique dishes"))
    updateSelectizeInput(session, "food_item",
                        choices = dish_choices,
                        server = TRUE)
  })

observeEvent(input$add_food, {
  req(input$food_item, input$serving_size)
  
  # Debug selected food
  print(paste("Selected food:", input$food_item))
  print(paste("Serving size:", input$serving_size))
  
  # Check if food exists in database
  food_exists <- input$food_item %in% food_database()$dish_name
  print(paste("Food exists in database:", food_exists))
  
  if(food_exists) {
    # Get food info from database
    food_info <- food_database()[food_database()$dish_name == input$food_item, ]
    
    # Debug food_info
    print(paste("Number of rows in food_info:", nrow(food_info)))
    print("Column names in food_info:")
    print(names(food_info))
    
    
    if(nrow(food_info) > 0) {
      # Scale nutrients based on serving size
      serving_ratio <- input$serving_size / 100
      
      # Print values for debugging
      print("Values for new food item:")
      print(paste("Calories:", food_info$calories_kcal))
      print(paste("Carbs:", food_info$carbohydrates_g))
      
      # Create new food item
      new_food <- data.frame(
      dish_name = input$food_item,
      serving_size = input$serving_size,
      calories = round(as.numeric(food_info[1, "calories__kcal_"]) * serving_ratio, 1),
      carbs = round(as.numeric(food_info[1, "carbohydrates__g_"]) * serving_ratio, 1),
      protein = round(as.numeric(food_info[1, "protein__g_"]) * serving_ratio, 1),
      fats = round(as.numeric(food_info[1, "fats__g_"]) * serving_ratio, 1),
      free_sugar = round(as.numeric(food_info[1, "free_sugar__g_"]) * serving_ratio, 1),
      fibre = round(as.numeric(food_info[1, "fibre__g_"]) * serving_ratio, 1),
      sodium = round(as.numeric(food_info[1, "sodium__mg_"]) * serving_ratio, 1),
      calcium = round(as.numeric(food_info[1, "calcium__mg_"]) * serving_ratio, 1),
      iron = round(as.numeric(food_info[1, "iron__mg_"]) * serving_ratio, 1),
      vitamin_c = round(as.numeric(food_info[1, "vitamin_c__mg_"]) * serving_ratio, 1),
      folate = round(as.numeric(food_info[1, "folate__µg_"]) * serving_ratio, 1)
    )
      
      # Add to meal data
      meal_data(rbind(meal_data(), new_food))
    } else {
      # If we found the food in the database but got zero rows
      print("ERROR: Food found in database but returned zero rows")
      # Fall back to default values
      new_food <- data.frame(
        dish_name = input$food_item,
        serving_size = input$serving_size,
        calories = 200,
        carbs = 25,
        protein = 8,
        fats = 10,
        free_sugar = 2,
        fibre = 3,
        sodium = 300,
        calcium = 50,
        iron = 1.5,
        vitamin_c = 5,
        folate = 20
      )
      
      # Add to meal data
      meal_data(rbind(meal_data(), new_food))
    }
  } else {
    # Custom food with default values for Indian cuisine
    print("Creating custom food item with default values")
    new_food <- data.frame(
      dish_name = input$food_item,
      serving_size = input$serving_size,
      calories = 200,
      carbs = 25,
      protein = 8,
      fats = 10,
      free_sugar = 2,
      fibre = 3,
      sodium = 300,
      calcium = 50,
      iron = 1.5,
      vitamin_c = 5,
      folate = 20
    )
    
    # Add to meal data
    meal_data(rbind(meal_data(), new_food))
  }
})

  # Clear meal
  observeEvent(input$clear_meal, {
    meal_data(data.frame(
      dish_name = character(),
      serving_size = numeric(),
      calories = numeric(),
      carbs = numeric(),
      protein = numeric(),
      fats = numeric(),
      free_sugar = numeric(),
      fibre = numeric(),
      sodium = numeric(),
      calcium = numeric(),
      iron = numeric(),
      vitamin_c = numeric(),
      folate = numeric(),
      stringsAsFactors = FALSE
    ))
  })

  # Render meal table
  output$meal_table <- renderDT({
    req(meal_data())
    # Rename columns for display
    display_data <- meal_data()
    colnames(display_data)[colnames(display_data) == "dish_name"] <- "Dish"
    colnames(display_data)[colnames(display_data) == "serving_size"] <- "Serving (g)"
    colnames(display_data)[colnames(display_data) == "calories"] <- "Calories"
    colnames(display_data)[colnames(display_data) == "carbs"] <- "Carbs (g)"
    colnames(display_data)[colnames(display_data) == "protein"] <- "Protein (g)"
    colnames(display_data)[colnames(display_data) == "fats"] <- "Fats (g)"
    
    datatable(display_data[, c("Dish", "Serving (g)", "Calories", "Carbs (g)", 
                            "Protein (g)", "Fats (g)")],
              options = list(
                pageLength = 5,
                dom = 't',
                ordering = TRUE,
                scrollX = TRUE,  # Enable horizontal scrolling if needed
                scrollY = "280px", 
                scroller = TRUE 
              ),
              rownames = FALSE) %>%
          formatStyle(columns = names(display_data[, c("Dish", "Serving (g)", "Calories", 
                                                        "Carbs (g)", "Protein (g)", "Fats (g)")]), 
                        backgroundColor = "#f8f9fa",
                        borderRadius = "5px")
  })

  # Improved nutrition chart with actual macronutrients
  output$nutrition_chart <- renderPlotly({
    req(nrow(meal_data()) > 0)
    
    # Calculate totals
    total_calories <- sum(meal_data()$calories)
    total_carbs <- sum(meal_data()$carbs)
    total_protein <- sum(meal_data()$protein)
    total_fats <- sum(meal_data()$fats)
    
    # Calculate percentages for macronutrients
    carbs_calories <- total_carbs * 4  # 4 calories per gram
    protein_calories <- total_protein * 4  # 4 calories per gram
    fat_calories <- total_fats * 9  # 9 calories per gram
    
    carbs_percent <- round((carbs_calories / total_calories) * 100)
    protein_percent <- round((protein_calories / total_calories) * 100)
    fat_percent <- round((fat_calories / total_calories) * 100)
    
    # Prepare data for the chart
    nutrient_data <- data.frame(
      Nutrient = c("Carbs", "Protein", "Fat"),
      Value = c(total_carbs, total_protein, total_fats),
      Percentage = c(carbs_percent, protein_percent, fat_percent),
      Color = c("#e74c3c", "#3498db", "#f39c12")
    )
    
    plot_ly(
      data = nutrient_data,
      x = ~Nutrient,
      y = ~Value,
      type = "bar",
      marker = list(color = ~Color),
      text = ~paste0(Nutrient, ": ", round(Value, 1), "g (", Percentage, "%)"),
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = paste0("Total Calories: ", round(total_calories), " kcal"),
          font = list(size = 16)
        ),
        xaxis = list(title = ""),
        yaxis = list(title = "Grams"),
        showlegend = FALSE,
        margin = list(l = 50, r = 50, t = 50, b = 50)
      )
  })

  # Calculate estimated glycemic load based on carbs and sugar content
  calculate_gl <- function(carbs, sugar) {
    # A simple estimate based on carbs and sugar content
    # Higher sugar content in relation to carbs means higher glycemic impact
    base_gl <- carbs * 0.7
    sugar_factor <- sugar/carbs * 10
    return(base_gl + sugar_factor)
  }

  # Updated glycemic gauge to use calculated GL
  output$glycemic_gauge <- renderPlotly({
    req(nrow(meal_data()) > 0)
    
    # Calculate total carbs and sugar
    total_carbs <- sum(meal_data()$carbs)
    total_sugar <- sum(meal_data()$free_sugar)
    
    # Calculate estimated glycemic load
    total_gl <- calculate_gl(total_carbs, total_sugar)
    
    # Define GL ranges
    low_gl <- 10
    medium_gl <- 20
    
    # Create gauge chart
    fig <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = total_gl,
      title = list(text = "Estimated Glycemic Load"),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(0, 30), tickwidth = 1),
        bar = list(color = "darkblue"),
        bgcolor = "white",
        borderwidth = 2,
        bordercolor = "gray",
        steps = list(
          list(range = c(0, low_gl), color = "#2ecc71"),
          list(range = c(low_gl, medium_gl), color = "#f39c12"),
          list(range = c(medium_gl, 30), color = "#e74c3c")
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = total_gl
        )
      )
    )
    
    fig
  })

  # Additional micronutrient chart
  output$micronutrient_chart <- renderPlotly({
    req(nrow(meal_data()) > 0)
    
    # Calculate totals of micronutrients
    total_fibre <- sum(meal_data()$fibre)
    total_sodium <- sum(meal_data()$sodium)
    total_calcium <- sum(meal_data()$calcium)
    total_iron <- sum(meal_data()$iron)
    total_vit_c <- sum(meal_data()$vitamin_c)
    total_folate <- sum(meal_data()$folate)
    
    # Daily recommended values (approximate)
    rec_fibre <- 25  # g
    rec_sodium <- 2300  # mg
    rec_calcium <- 1000  # mg
    rec_iron <- 18  # mg
    rec_vit_c <- 75  # mg
    rec_folate <- 400  # µg
    
    # Calculate percentages of daily values
    percent_fibre <- min(100, round((total_fibre / rec_fibre) * 100))
    percent_sodium <- min(100, round((total_sodium / rec_sodium) * 100))
    percent_calcium <- min(100, round((total_calcium / rec_calcium) * 100))
    percent_iron <- min(100, round((total_iron / rec_iron) * 100))
    percent_vit_c <- min(100, round((total_vit_c / rec_vit_c) * 100))
    percent_folate <- min(100, round((total_folate / rec_folate) * 100))
    
    # Create data frame for bar chart
    micro_data <- data.frame(
      Nutrient = c("Fiber", "Sodium", "Calcium", "Iron", "Vit C", "Folate"),
      Percent = c(percent_fibre, percent_sodium, percent_calcium, 
                  percent_iron, percent_vit_c, percent_folate),
      Value = c(paste0(round(total_fibre, 1), "g"), 
                paste0(round(total_sodium), "mg"),
                paste0(round(total_calcium), "mg"),
                paste0(round(total_iron, 1), "mg"),
                paste0(round(total_vit_c), "mg"),
                paste0(round(total_folate), "µg"))
    )
    
    plot_ly(
      data = micro_data,
      x = ~Nutrient,
      y = ~Percent,
      type = "bar",
      marker = list(color = "#3498db"),
      text = ~paste0(Nutrient, ": ", Value, " (", Percent, "% of daily value)"),
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = "Micronutrients (% of Daily Value)",
          font = list(size = 16)
        ),
        xaxis = list(title = ""),
        yaxis = list(title = "% of Daily Value", range = c(0, 100)),
        showlegend = FALSE,
        margin = list(l = 50, r = 50, t = 50, b = 50)
      )
  })

  # Enhanced nutrition recommendations with comprehensive analysis
  output$nutrition_recommendations <- renderUI({
    req(nrow(meal_data()) > 0)
    
    # Calculate totals
    total_calories <- sum(meal_data()$calories)
    total_carbs <- sum(meal_data()$carbs)
    total_protein <- sum(meal_data()$protein)
    total_fats <- sum(meal_data()$fats)
    total_fibre <- sum(meal_data()$fibre)
    total_sugar <- sum(meal_data()$free_sugar)
    total_sodium <- sum(meal_data()$sodium)
    
    # Calculate percentages of macronutrients
    carbs_calories <- total_carbs * 4
    protein_calories <- total_protein * 4
    fat_calories <- total_fats * 9
    
    carbs_percent <- round((carbs_calories / total_calories) * 100)
    protein_percent <- round((protein_calories / total_calories) * 100)
    fat_percent <- round((fat_calories / total_calories) * 100)
    
    # Calculate glycemic load
    total_gl <- calculate_gl(total_carbs, total_sugar)
    
    # Generate recommendations
    recommendations <- list()
    
    # Caloric assessment
    if(total_calories < 350) {
      recommendations <- c(recommendations, paste0("Your meal contains ", round(total_calories), " calories, which may be too light for a main meal. Consider adding more nutritious components."))
    } else if(total_calories > 700) {
      recommendations <- c(recommendations, paste0("Your meal contains ", round(total_calories), " calories, which is on the higher side for diabetes management. Consider reducing portion sizes."))
    } else {
      recommendations <- c(recommendations, paste0("Your meal contains ", round(total_calories), " calories, which is appropriate for a balanced meal."))
    }
    
    # Carb recommendations
    if(total_carbs > 60) {
      recommendations <- c(recommendations, paste0("Your meal contains ", round(total_carbs), "g of carbs, which exceeds the recommended amount for a single meal for diabetes management (45-60g). Consider reducing carbohydrate-rich items."))
    } else if(total_carbs < 30) {
      recommendations <- c(recommendations, paste0("Your meal contains only ", round(total_carbs), "g of carbs. While low-carb can be beneficial, ensure you're getting adequate nutrition from other sources."))
    } else {
      recommendations <- c(recommendations, paste0("Your meal contains ", round(total_carbs), "g of carbs, which is within the recommended range for diabetes management."))
    }
    
    # Sugar assessment
    if(total_sugar > 10) {
      recommendations <- c(recommendations, paste0("Your meal contains ", round(total_sugar), "g of free sugar, which is relatively high. Consider reducing added sugars for better blood glucose control."))
    }
    
    # Fiber assessment
    if(total_fibre < 5) {
      recommendations <- c(recommendations, paste0("Your meal is low in fiber (", round(total_fibre, 1), "g). Try adding more vegetables, whole grains, or legumes to slow sugar absorption."))
    } else {
      recommendations <- c(recommendations, paste0("Your meal contains ", round(total_fibre, 1), "g of fiber, which helps slow sugar absorption and improves blood glucose control."))
    }
    
    # Protein assessment
    if(protein_percent < 15) {
      recommendations <- c(recommendations, "Your meal is low in protein. Adding lean protein sources can help stabilize blood sugar levels.")
    } else if(protein_percent > 30) {
      recommendations <- c(recommendations, "Your meal is very high in protein. While protein is important, balance is key for long-term health.")
    } else {
      recommendations <- c(recommendations, "Your meal has a good protein content, which helps slow digestion and stabilize blood sugar.")
    }
    
    # Fat assessment
    if(fat_percent > 40) {
      recommendations <- c(recommendations, "Your meal is high in fat. Focus on healthier fat sources like nuts, seeds, and olive oil rather than saturated fats.")
    } else if(fat_percent < 15) {
      recommendations <- c(recommendations, "Your meal is low in fat. Including some healthy fats can help slow digestion and improve satisfaction.")
    }
    
    # Sodium assessment
    if(total_sodium > 600) {
      recommendations <- c(recommendations, paste0("Your meal contains ", round(total_sodium), "mg of sodium, which is high for a single meal. High sodium intake can affect blood pressure, which is important to manage with diabetes."))
    }
    
    # Glycemic load assessment
    if(total_gl < 10) {
      recommendations <- c(recommendations, "Your meal has a low glycemic load (< 10), which is excellent for blood sugar management.")
    } else if(total_gl < 20) {
      recommendations <- c(recommendations, "Your meal has a medium glycemic load (10-20). Consider including more protein and fiber to further slow digestion.")
    } else {
      recommendations <- c(recommendations, "Your meal has a high glycemic load (> 20), which may cause significant blood sugar spikes. Consider reducing portions of high-carb foods or adding more fiber and protein.")
    }
    
    # Create the HTML list with better formatting
    div(
      class = "nutrition-recommendations",
      tags$ul(
        lapply(recommendations, function(rec) {
          tags$li(rec, style = "margin-bottom: 10px;")
        }),
        style = "list-style-type: disc; padding-left: 20px;"
      ),
      tags$hr(),
      tags$p(strong("General Tips:"), "Spread carbohydrates throughout the day, pair carbs with protein, include fiber with each meal, and monitor post-meal blood glucose to understand your body's response.", 
            style = "font-style: italic; margin-top: 15px;")
    )
  })

}
shinyApp(ui = ui, server = server)

# nolint end
