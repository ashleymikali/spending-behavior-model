library(shiny)
library(bslib)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(wacolors)

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
                       "spending_habit" = "spending_habit",
                       "age_group" = "age_group",
                       "wellbeing" = "wellbeing"
          ),
        selected = "follow_commitment",
        inline = FALSE
        )
      ),
    nav_panel("Data Dictionary", uiOutput("dictionary")), 
    nav_panel("Univariate Graph", plotOutput("distribution"))
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
  
  special_purple <- "#412d5e"
  
  # establish data dictionary
  data_dictionaries <- list(
    follow_commitment = "Prompt: 'I follow-through on my financial commitments 
    to others.' (1-5) <br>
    1: Not at all <br>
    2: Very little <br>
    3: Somewhat <br>
    4: Very well <br>
    5: Completely",
    
    frugality = "Prompt: 'If I can re-use an item I already have, there's no 
    sense in buying something new.' (1-6) <br>
    1: Strongly disagree <br>
    2: Disagree <br>
    3: Disagree slightly <br>
    4: Agree slightly <br>
    5: Agree <br>
    6: Strongly agree",
    
    worded_probability = "Prompt: 'Prefers words for expressions of 
    probabilities.' (1-6) <br>
    1: Always prefer words <br>
    2: 2 <br>
    3: 3 <br>
    4: 4 <br>
    5: 5 <br>
    6: Always prefer numbers",
    
    percentage_skill = "Prompt: 'How good are you at working with percentages?' 
    (1-6) <br>
    1: Not good at all <br>
    2: 2 <br>
    3: 3 <br>
    4: 4 <br>
    5: 5 <br>
    6: Extremely good",
    
    goal_confidence = "Prompt: 'Confidence in own ability to achieve financial 
    goals.' (1-4) <br>
    1: Not at all confident <br>
    2: Not very confident <br>
    3: Somewhat confident <br>
    4: Very confident",
    
    admire_luxury = "Prompt: 'I admire people who own expensive 
    homes/cars/clothes.' (1-5) <br>
    1: Strongly disagree <br>
    2: Disagree <br>
    3: Neither agree nor disagree <br>
    4: Agree <br>
    5: Strongly agree",
    
    self_worth = "Prompt: 'The things I own say a lot about how well I'm doing 
    in life.' (1-5) <br>
    1: Strongly disagree <br>
    2: Disagree <br>
    3: Neither agree nor disagree <br>
    4: Agree <br>
    5: Strongly agree",
    
    impress_people = "Prompt: 'I like to own things that impress people.' 
    (1-5) <br>
    1: Strongly disagree <br>
    2: Disagree <br>
    3: Neither agree nor disagree <br>
    4: Agree <br>
    5: Strongly agree",
    
    psych = "Prompt: 'Psychological connectedness.' <br>
    (Continuous variable)",
    
    distress = "Prompt: 'Lot of stress in respondent's life.' (1-5) <br>
    1: Strongly disagree <br>
    2: Disagree <br>
    3: Neither agree nor disagree <br>
    4: Agree <br>
    5: Strongly agree",
    
    impulsivity = "Prompt: 'I often act without thinking through all the 
    alternatives.' (1-4) <br>
    1: Not at all <br>
    2: Not very well <br>
    3: Very well <br>
    4: Completely well",
    
    resist_temptation = "Prompt: 'I am good at resisting temptation.' (1-4) <br>
    1: Not at all <br>
    2: Not very well <br>
    3: Very well <br>
    4: Completely well",
    
    long_term_goals = "Prompt: 'I am able to work dilligently toward long-term 
    goals.' (1-4) <br>
    1: Not at all <br>
    2: Not very well <br>
    3: Very well <br>
    4: Completely well",
    
    economic_mobility = "Prompt: 'Everyone has a fair chance at moving up the 
    economic ladder.' (1-7) <br>
    1: Strongly disagree <br>
    2: Disagree <br>
    3: Somewhat disagree <br>
    4: Neither agree nor disagree <br>
    5: Somewhat gree <br>
    6: Agree <br>
    7: Strongly agree",
    
    spending_habit = "Prompt: 'I know how to keep myself from spending too 
    much.' (1-5) <br>
    1: Not at all <br>
    2: Very little <br>
    3: Somewhat <br>
    4: Very well <br>
    5: Completely",
    
    age_group = "Respondent age, grouped into categories: <br>
    1: 18-24 <br>
    2: 25-34 <br>
    3: 35-44 <br>
    4: 45-54 <br>
    5: 55-61 <br>
    6: 62-69 <br>
    7: 70-74 <br>
    8: 75+",
    
    wellbeing = "Financial well-being scale score. <br>
    (Continuous variable)"
  )
  
  # display data dictionary
  output$dictionary <- renderUI({
    req(input$radio)
    selected_var <- input$radio
    desc <- HTML(data_dictionaries[[selected_var]])
    #HTML(paste("<h4>Variable Description:</h4>", "<p>", desc, "</p>"))
  })
  
  # display univariate graphs
  output$distribution <- renderPlot({
    req(input$radio)
    selected_var <- input$radio
    
    # qualitative variables
    if (selected_var %in% quali_vars) {
      ggplot(finance_analysis, aes(x = .data[[input$radio]])) +
        geom_bar(fill = special_purple, alpha = 0.9) +
        labs(
          title = paste("Distribution of scores for", input$radio),
          x = "Scores",
          y = "Number of Respondents") +
        theme_minimal() +
        theme(
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(size = 20))
      
    } else if (selected_var %in% quanti_vars) {
      # i am about to hard code the density calculation here, sorry
      # might fix that in the future
      # i'll include the calculation in the main code at least
      
      # financial wellbeing graph
      if (selected_var == "wellbeing") {
        wd <- density(finance_analysis$wellbeing, na.rm = TRUE)
        wmd <- approx(wd$x, wd$y, xout = 0.0001474624)$y
        wrd <- approx(wd$x, wd$y, xout = 0.0001545899)$y
        
        ggplot(finance_analysis, aes(x = .data[[input$radio]])) +
          geom_density(fill = special_purple, alpha = 0.9) +
          labs(
            title = paste("Distribution of scores for", input$radio),
            x = "Scores",
            y = "Density",
            caption = "* The red point indicates unrecorded responses. \n
            * The blue point indicates refusal to respond.") +
          theme_minimal() +
          theme(
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
            axis.text.y = element_text(size = 16),
            plot.title = element_text(size = 20),
            plot.caption = element_text(size = 16)) +
          geom_point(x = -2.8, y = wmd, color="red") +
          geom_point(x = -3.6, y = wrd, color="blue")
        
        # psychological connectedness graph
      } else if (selected_var == "psych") {
        pd <- density(finance_analysis$psych, na.rm = TRUE)
        prd <- approx(pd$x, pd$y, xout = 0.009324845)$y
        
        ggplot(finance_analysis, aes(x = .data[[input$radio]])) +
          geom_density(fill = special_purple, alpha = 0.9) +
          labs(
            title = paste("Distribution of scores for", input$radio),
            x = "Scores",
            y = "Density",
            caption = "* The blue point indicates refusal to respond.") +
          theme_minimal() +
          theme(
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
            axis.text.y = element_text(size = 16),
            plot.title = element_text(size = 20),
            plot.caption = element_text(size = 16))  +
          geom_point(x = -0.9, y = prd, color="blue")
      }
      
      # age group graph
    } else if (selected_var %in% demographic_vars) {
      ggplot(finance_analysis, aes(x = .data[[input$radio]])) +
        geom_bar(fill = special_purple, alpha = 0.9) +
        labs(
          title = paste("Distribution of responses for", input$radio),
          x = "Age Ranges",
          y = "Number of Respondents") +
        theme_minimal() +
        theme(
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(size = 22))
    }
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)