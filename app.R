library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bslib)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #logo {
        max-height: 40px;
        margin-top: -10px;
      }
      .navbar, .navbar-default, .navbar-static-top {
        background-color: black !important;
      }
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a,
      .navbar-light .navbar-brand,
      .navbar-light .navbar-nav .nav-link {
        color: orange !important;
      }
    "))
  ),
  navbarPage(
    title = span(img(src = "ASALogo.png", id = "logo"),
                 ""),
    id = "nav",
    header = tags$style(HTML("
      #nav {
        background-color: black !important;
      }
      #nav > li > a {
        color: black !important;
      }
    ")),  
  tabPanel("Anaesthesia Reimbursement Comparison"),
    sidebarLayout(
    sidebarPanel(
      width=3,
      numericInput("units", "Number of Units", value = 15, min = 1),
      numericInput("mbs_unit_value", "MBS Unit Value", value = 22.55),
      numericInput("ama_unit_value", "AMA Unit Value", value = 106),
      # Indexation Type Selection
      selectInput("indexation_type", "Indexation Type for Unit Values",
                  choices = c("Actual", "CPI Indexation", "Health CPI Indexation"),
                  selected = "Actual"),
      selectInput("known_gap_indexation_type", "Indexation Type for Known Gap",
                  choices = c("Actual", "CPI Indexation", "Health CPI Indexation"),
                  selected = "Actual"),
      checkboxGroupInput("health_funds", "Select Health Funds",
                         choices = c("Medibank Private", "AHSA", "St Luke's", "HCF", "Bupa Known Gap", "HCF No Gap", "nib", "HBF WA", "Bupa No Gap"),
                         selected = c("Medibank Private", "AHSA", "St Luke's", "HCF", "Bupa Known Gap", "HCF No Gap", "nib", "HBF WA", "Bupa No Gap"))
    ),
    mainPanel( 
      plotOutput("reimbursementPlot")
    )
  )
)
)

# Define server logic
server <- function(input, output) {
  
  output$reimbursementPlot <- renderPlot({

    # Constants    
    MBS_REIMBURSEMENT_PERCENT <- 0.75
    HEALTH_FUND_REQUIRED_PERCENT <- 0.25
    NO_GAP_MAX <- 0
    TOTAL_AMA_COST <- input$units * input$ama_unit_value
    
    # Health fund data
    initial_fund_data <- data.frame(
      health_fund = c("Medibank Private", "AHSA", "St Luke's", "HCF", "Bupa Known Gap", "HCF No Gap", "nib", "HBF WA", "Bupa No Gap"),
      fund_unit_value_actual = c(37.25, 37.56, 38.8, 37.2, 37.35, 37.20, 37.15, 41.45, 38.25),
      fund_unit_value_cpi = c(46.13, 47.26, 47.85, 48.71, 45.12, 47.76, 44.62, 53.21, 49.10), # CPI Indexed
      fund_unit_value_health_cpi = c(47.25, 48.42, 49.01, 49.89, 46.22, 48.91, 45.71, 54.50, 50.29), # Health CPI Indexed
      gap_type = c("Known Gap", "Known Gap", "Known Gap", "Known Gap", "Known Gap", "No Gap", "No Gap", "No Gap", "No Gap"),
      stringsAsFactors = FALSE
    )
    
    # Reactive MBS Unit Value
    mbs_unit_value_reactive <- reactive({
      index_type <- input$indexation_type
      if (index_type == "Actual") {
        input$mbs_unit_value  # Use the user's input directly
      } else if (index_type == "CPI Indexation") {
        # Apply CPI indexation to the MBS unit value
        input$mbs_unit_value * (mean(initial_fund_data$fund_unit_value_cpi, na.rm=TRUE) / mean(initial_fund_data$fund_unit_value_actual, na.rm=TRUE)) 
      } else if (index_type == "Health CPI Indexation") {
        # Apply Health CPI indexation
        input$mbs_unit_value * (mean(initial_fund_data$fund_unit_value_health_cpi, na.rm=TRUE) / mean(initial_fund_data$fund_unit_value_actual, na.rm=TRUE)) 
      }
    })
    
    current_mbs_unit_value <- mbs_unit_value_reactive()
    
    # Reactive Known Gap Maximum
    known_gap_max_reactive <- reactive({
      known_gap_index_type <- input$known_gap_indexation_type
      if (known_gap_index_type == "Actual") {
        500  # Actual Known Gap Maximum
      } else if (known_gap_index_type == "CPI Indexation") {
        848.26  # CPI Indexed Known Gap Maximum
      } else if (known_gap_index_type == "Health CPI Indexation") {
        880  # Health CPI Indexed Known Gap Maximum
      }
    })
    
    current_known_gap_max <- known_gap_max_reactive()
    
    # Reactive Data for Health Funds (updates based on indexation type)
    fund_data <- reactive({
      index_type <- input$indexation_type
      if (index_type == "Actual") {
        initial_fund_data %>% select(health_fund, fund_unit_value = fund_unit_value_actual, gap_type)
      } else if (index_type == "CPI Indexation") {
        initial_fund_data %>% select(health_fund, fund_unit_value = fund_unit_value_cpi, gap_type)
      } else if (index_type == "Health CPI Indexation") {
        initial_fund_data %>% select(health_fund, fund_unit_value = fund_unit_value_health_cpi, gap_type)
      }
    })
    
    
    # Filter selected funds
    all_funds <- fund_data()
    all_funds <- all_funds %>% filter(health_fund %in% input$health_funds)

    # Create two scenarios for each fund
    scenario_data <- all_funds %>%
      mutate(
        scenario = "Within Scheme Limits"
      ) %>%
      rbind(
        all_funds %>%
          mutate(
            scenario = "AMA Rate Charged" 
          ) 
      )

    # Calculate reimbursements for both scenarios
    calculate_reimbursements <- function(data) {
      data %>% mutate(
        total_mbs_value = current_mbs_unit_value * input$units,
        mbs_reimburse = current_mbs_unit_value * MBS_REIMBURSEMENT_PERCENT * input$units,
        fund_legislated_reimburse = current_mbs_unit_value * HEALTH_FUND_REQUIRED_PERCENT * input$units,
        
        # For Within Scheme Limits
        scheme_fund_reimburse = fund_unit_value * input$units,
        scheme_fund_above_legislated = pmax(0, scheme_fund_reimburse - fund_legislated_reimburse),
        scheme_gap_limit = ifelse(gap_type == "Known Gap", current_known_gap_max, NO_GAP_MAX),
        scheme_out_of_pocket = pmin(scheme_gap_limit,TOTAL_AMA_COST - mbs_reimburse - scheme_fund_reimburse),
        scheme_unreimbursed = pmax(0, TOTAL_AMA_COST - mbs_reimburse - scheme_fund_reimburse - scheme_out_of_pocket),
        
        # For AMA Rate Charged (punitive reduction)
        
        ama_fund_reimburse = fund_legislated_reimburse, # Only legislatively required
        ama_fund_above_legislated = 0, # No additional payment
        ama_out_of_pocket = TOTAL_AMA_COST - mbs_reimburse - ama_fund_reimburse,
        ama_unreimbursed = 0, # Everything is either paid by someone or out-of-pocket
        
        # Select values based on scenario
        fund_actual_reimburse = ifelse(scenario == "Within Scheme Limits", scheme_fund_reimburse, ama_fund_reimburse),
        fund_above_legislated = ifelse(scenario == "Within Scheme Limits", scheme_fund_above_legislated, ama_fund_above_legislated),
        out_of_pocket = ifelse(scenario == "Within Scheme Limits", scheme_out_of_pocket, ama_out_of_pocket), 
        unreimbursed = ifelse(scenario == "Within Scheme Limits", scheme_unreimbursed, ama_unreimbursed)
      )
    }
    
    result_data <- calculate_reimbursements(scenario_data)
    
    # Prepare data for plotting
    plot_data <- result_data %>% 
      select(health_fund, gap_type, scenario, mbs_reimburse, fund_legislated_reimburse, fund_above_legislated, out_of_pocket, unreimbursed) %>%
      pivot_longer(
        cols = c(mbs_reimburse, fund_legislated_reimburse, fund_above_legislated, out_of_pocket, unreimbursed),
        names_to = "reimbursement_type",
        values_to = "amount"
      )
    
    # Set factor levels for correct ordering (reversed for stacking bottom to top)
    plot_data$reimbursement_type <- factor(
      plot_data$reimbursement_type,
      levels = rev(c("mbs_reimburse", "fund_legislated_reimburse", "fund_above_legislated", "out_of_pocket", "unreimbursed"))
    )
    
    plot_data$gap_type <- factor( 
      plot_data$gap_type,
      levels = c("Known Gap", "No Gap")
    )
    
    plot_data$scenario <- factor( 
      plot_data$scenario,
      levels = c("Within Scheme Limits", "AMA Rate Charged")
    )    

    # Create descriptive labels
    labels <- c(
      mbs_reimburse = "MBS Reimbursement (75%)",
      fund_legislated_reimburse = "Health Fund Required (25%)",
      fund_above_legislated = "Health Fund Above Required",
      out_of_pocket = "Patient Out of Pocket",
      unreimbursed = "Unreimbursed Amount"
    )
    
    # Create a cleaner display name for x-axis
    plot_data <- plot_data %>%
      mutate(
        display_name = paste0(health_fund, "\n", scenario)
      )

    # Create the plot
    ggplot(plot_data, aes(x = display_name, y = amount, fill = reimbursement_type)) +
      geom_col(position = "stack") +
      geom_hline(yintercept = TOTAL_AMA_COST, linetype = "dashed", color = "black", linewidth = 1) +
      geom_text(
        aes(label = ifelse(amount >= 50, sprintf("$%.0f", amount), "")),
        position = position_stack(vjust = 0.5),
        color = "black", size = 3.5, fontface = "bold"
      ) +
      
      # Add AMA rate label
      annotate("text", x = 1.5, y = TOTAL_AMA_COST + 50,
               label = sprintf("AMA Rate: $%.0f", TOTAL_AMA_COST),
               color = "black", fontface = "bold", hjust = 0) +
      scale_fill_manual(
        values = c(
          "mbs_reimburse" = "#2ECC71",
          "fund_legislated_reimburse" = "#3498DB",
          "fund_above_legislated" = "#F39C12",
          "out_of_pocket" = "#E74C3C",
          "unreimbursed" = "white"
        ),
        labels = labels
      ) +
      facet_grid(. ~ gap_type, scales = "free_x", space = "free") +
      labs(
        title = "Comparison of Anaesthesia Reimbursement Scenarios",
        subtitle = paste0("Typical procedure (", input$units, " units) at AMA Rate ($", input$ama_unit_value, "/unit)"),
        x = "",
        y = "Amount ($)",
        fill = "Reimbursement Component"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "azure"),
        panel.grid.major.x = element_blank(),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size=12)
      ) +
      scale_y_continuous(
        labels = scales::dollar,
        breaks = seq(0, TOTAL_AMA_COST * 1.1, by = 200),
        limits = c(0, TOTAL_AMA_COST * 1.1)
      ) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  }, height = 800)
}

# Run the application
shinyApp(ui = ui, server = server) 