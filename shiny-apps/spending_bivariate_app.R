library(shiny)
library(bslib)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(wacolors)
library(tidyr)

# Define UI ----
ui <- page_fillable(
  navset_card_pill(
    sidebar = sidebar(
      radioButtons(
        inputId = "radio",
        label = "Select a variable",
        choices = list(
          "follow_commitment" = "follow_commitment",
          "frugality" = "frugality",
          "worded_probability" = "worded_probability",
          "percentage_skill" = "percentage_skill",
          "goal_confidence" = "goal_confidence",
          "admire_luxury" = "admire_luxury",
          "self_worth" = "self_worth",
          "impress_people" = "impress_people",
          "psych" = "psych",
          "distress" = "distress",
          "impulsivity" = "impulsivity",
          "resist_temptation" = "resist_temptation",
          "long_term_goals" = "long_term_goals",
          "economic_mobility" = "economic_mobility",
          "age_group" = "age_group",
          "wellbeing" = "wellbeing"
        ),
        selected = "follow_commitment",
        inline = FALSE
      )
    ),
    nav_panel("Jitter Plot", plotOutput("jitter")), 
    nav_panel("Proportional Bar", plotOutput("proportional")),
    nav_panel("Tally", tableOutput("tally_table"))
  ), 
  id = "tab"
)



# Define server logic ----
server <- function(input, output, session) {
  finance_analysis <- readRDS("data/finance_analysis.rds")
  
  # reference objects (example: lists)
  quali_vars <- c("follow_commitment", "frugality", "worded_probability", 
                  "percentage_skill", "goal_confidence", "admire_luxury", 
                  "self_worth", "impress_people", "distress", "impulsivity", 
                  "resist_temptation", "long_term_goals", "economic_mobility", 
                  "spending_habit")
  
  quanti_vars <- c("psych", "wellbeing")
  demographic_vars <- ("age_group")
  
  
  # display jitter plots
  output$jitter <- renderPlot({
    req(input$radio)
    selected_var <- input$radio
    
    # qualitative jitter
    #if (selected_var %in% c(quali_vars, demographic_vars)) {
      ggplot(finance_analysis, aes(x = .data[[input$radio]], 
                                   y = spending_habit, 
                                   color = spending_habit)) + 
        geom_jitter() + 
        labs(
          title = paste("Relationship between spending_habit \n and", 
                        input$radio),
          x = input$radio,
          y = "Spending Habit Score") +
        theme_minimal() +
        scale_color_wa_d("stuart") +
        theme(
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(size = 22),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14))
       
      
      # quantitative jitter (flipped axes)
    # } else if (selected_var %in% quanti_vars) {
    #   ggplot(finance_analysis, aes(x = spending_habit, 
    #                                y = .data[[input$radio]], 
    #                                color = spending_habit)) +
    #     geom_jitter() +
    #     labs(
    #       title = paste("Relationship between spending_habit \n and", 
    #                     input$radio),
    #       x = "Spending Habit Score",
    #       y = input$radio) +
    #     theme_minimal() +
    #     scale_color_wa_d("stuart") +
    #     theme(
    #       axis.title.x = element_text(size = 14),
    #       axis.title.y = element_text(size = 14),
    #       axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    #       axis.text.y = element_text(size = 16),
    #       plot.title = element_text(size = 22),
    #       legend.title = element_text(size = 14),
    #       legend.text = element_text(size = 14))
    # }
    
  })
  
  # display proportional bar charts
  output$proportional <- renderPlot({
    req(input$radio)
    selected_var <- input$radio
    
    # qualitative proportional bar
   # if (selected_var %in% c(quali_vars, demographic_vars)) {
      ggplot(finance_analysis, aes(x = .data[[input$radio]], 
                                   fill = spending_habit)) + 
        geom_bar(position = "fill") + 
        labs(
          title = paste("Relationship between spending_habit \n and",
                        input$radio),
          x = input$radio,
          y = "Spending Habit Score") +
        theme_minimal() +
        scale_fill_wa_d("puget") +
        theme(
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(size = 22),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14))
      
      
      # quantitative proportional bar (flipped axes)
      # do NOT flip axes; fix later
    #} else if (selected_var %in% quanti_vars) {
     # ggplot(finance_analysis, aes(y = .data[[input$radio]], 
    #                                fill = spending_habit)) +
    #     geom_bar(position = "fill") +
    #     labs(
    #       title = paste("Relationship between spending_habit \n and", 
    #                     input$radio),
    #       x = "Spending Habit Score",
    #       y = input$radio) +
    #     theme_minimal() +
    #     scale_fill_wa_d("stuart") +
    #     theme(
    #       axis.title.x = element_text(size = 14),
    #       axis.title.y = element_text(size = 14),
    #       axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    #       axis.text.y = element_text(size = 16),
    #       plot.title = element_text(size = 22),
    #       legend.title = element_text(size = 14),
    #       legend.text = element_text(size = 14))
    # }
    
  })
  
  output$tally_table <- renderTable({
    req(input$radio)
    selected_var <- input$radio 
      
    formula_text <- paste("~ spending_habit +", selected_var)
    tbl <- xtabs(as.formula(formula_text), data = finance_analysis)
    
    df_tbl <- as.data.frame.matrix(tbl)
    
    
    df_tbl <- cbind(Spending_Habit = rownames(df_tbl), df_tbl)
    
    df_tbl 
    })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)