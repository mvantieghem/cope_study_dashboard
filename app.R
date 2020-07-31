# Shiny app for Exploratory data analysis of the COPE Study
# Author: Michelle VanTieghem
# Date: June 10, 2020

# load packages needed
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(shinydashboard)

# process data and generate tables and plots 
source("custom_functions.R")
source_rmd("analyses/report_pregnant_survey.Rmd") # fixed
source_rmd("analyses/report_birth_changes.Rmd") # fixed
source_rmd("analyses/report_postnatal_changes.Rmd") # foxed
source_rmd("analyses/report_baseline_distress_and_impact.Rmd") # fixed
source_rmd("analyses/report_financial_changes.Rmd")
source_rmd("analyses/report_covid_restrictions.Rmd")
source_rmd("analyses/report_cope_core_part1_disruptions.Rmd")
source_rmd("analyses/report_cope_core_part2_positive.Rmd")

# USER INTERFACE -----------------------------------------------------

# Define UI for app  --------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "About", icon = icon("th")),
    menuItem("Healthcare", tabName = "Healthcare", icon = icon("th")),
  # menuItem("Covid-19", tabName = "Covid-19", icon = icon("th")),
    menuItem("Impact", tabName = "Impact", icon = icon("th"))
  # menuItem("Well-being", tabName = "Well-being", icon = icon("th"))
  )
)
# Create body of dashboard ----------------

    # First tab content
About_tab <- tabItem(tabName = "About",
            h2("Coronovirus-19 and Perinatal Experiences (COPE) Study"),
            h3("About the Study"),
            p("The goal of this study is to assess experiences and feelings of pregnant and new mothers during the Covid-19 pandemic.
                            Women in the NYC metropolitan area were recruited through medical records to complete a longitudinal online survey study.
                            The data presented here represent the baseline assessment, which was collected between March 30 and June 15, 2020, 
                            during the peak of covid-19 cases in NYC."),
            h3("Study Materials"),
            HTML("<p>For this study, we used the Coronovirus Perinatal Experiences Impact Survey, a newly developed measure designed to learn more about the experiences of new and expectant mothers in the time of the COVID-19 pandemic.
                            This assessment tool arose as a collaborative effort of more than 100 expert scientists and clinicians that came together to build a tool that could be sensitive both to the events of women's lives, and their unique responses to those circumstances. 
                            For more information and access to survey materials, please visit the
                             <a href = 'https://osf.io/uqhcv/'>OSF Page</a>."),
            h3("About the Team"), 
            HTML("<p>This study is conducted by researchers in the <a href = 'https://www.babybees.org'>Baby Bees Lab</a> in the Department of Child and Adolescent Psychiatry at NYU Langone Health.
                            The lab, lead by Dr. Moriah Thomason, studies the role of prenatal exposures on fetal and infant brain and behavioral outcomes.
                               In response to the covid-19 pandemic, current research efforts are focused on understanding covid-19 related stressors on pregnant and expectant moms, 
                               and impacts on infant development. "),
            h3("COVGEN Research Alliance"), 
            HTML("<p> In March of 2020, investigators across multiple institutions began sharing tools and approaches in support of 
                          global research harmonization. The Baby Bees Lab, along with our collaborators, created the <a href = 'https://www.covgen.org/'> COVGEN Research Alliance</a> , which aims to support these research activities by highlighting COVGEN research around 
                          the world, and providing a platform for new collaborations to emerge."),
            h3("About the App"),
            HTML("<p>Author:  <a href = https://mvantieghem.github.io >Michelle VanTieghem</a>"),
            HTML("<p>Code on <a href =https://github.com/mvantieghem/cope_study_dashboard >Github</a>"))
    
    # Second tab content
Healthcare_tab <- tabItem(tabName = "Healthcare",
            h3("Perinatal Health Care during the Covid-19 pandemic."), 
            p("We asked new and expectant moms to share their experiences about how their health care changed due to the pandemic."),
            fluidRow(
              tabBox(
                title = "", 
                id = "tabset1", width = "450px", selected = "Summary", 
                tabPanel('Summary',  
                         fluidRow(
                           infoBoxOutput(outputId = "prenatal_fact", 
                                         width = 12),
                            infoBoxOutput(outputId = "delivery_fact", 
                                          width = 12),
                             infoBoxOutput(outputId = "support_partner_fact", 
                                width = 12),
                            infoBoxOutput(outputId = "postnatal_fact", 
                                          width = 12), fill = TRUE)
                            ),
                tabPanel('Prenatal Care', 
                         fluidRow(
                           box(title = "How has Covid-19 impacted prenatal care?",
                               width = 12, status = "primary", solidHeader =  T,
                             plotOutput (outputId = "prenatal_plot")),
                           # change this plot to means of concerns, and compare all 3
                           box(title = "Are women concerned about these changes?", 
                               width = 12, height = 550, status = "primary", solidHeader = T, 
                               plotOutput (outputId = "preg_concern_plots")))), 
               
                tabPanel("Delivery Plans", 
                         fluidRow(
                          # box(width = 12, status = "primary", solidHeader =  T,
                           #    plotOutput(outputId = 'birth_plot')),
                           box(title = "Birth during a pandemic: Changes to delivery plans over time", 
                               width = 12,status = "primary", solidHeader =  T,
                               checkboxGroupInput(inputId = 'change_choice', 
                                                  label = '', 
                                                  choices = list_birth_changes,
                                                  selected = "Any change to birth plan"),
                               plotOutput(outputId = 'birth_time_plot')))),
                tabPanel("Postnatal Care", 
                         fluidRow( 
                           box(title = "How has Covid-19 impacted postnatal care?", 
                               width = 12, status = "primary",solidHeader =  T,
                               plotOutput(outputId = 'postnatal_plot')),
                           # change this to their rating of support.
                            box(title = "How well are women being supported by their care providers?", 
                                width = 12, height = 400, status = "primary", solidHeader =  T,
                              plotOutput(outputId = 'support_provider_plot'))))
              )
            )
    )
    # THIRD TAB Content
covid_tab <- tabItem(tabName = "Covid-19",
            h3("Exposures to covid-19"),
            p("Pregnant women and new mothers shared information on their exposures to covid-19"),
            fluidRow(
              tabBox(
                title = "", 
                id = "tabset1", width = "450px", selected = "Summary", 
                tabPanel('Summary',  
                         fluidRow(
                           infoBoxOutput(outputId = "Women received a test for Covid-19", 
                                         width = 12),
                           infoBoxOutput(outputId = "Women tested positive for Covid-19", 
                                         width = 12),
                           infoBoxOutput(outputId = "Family members tested positive for Covid-19", 
                                         width = 12),
                           infoBoxOutput(outputId = "Women reported experiencing symptoms of Covid-19", 
                                         width = 12),fill = TRUE)
                ),
                tabPanel('Testing', 
                         fluidRow(
                           box(title = "Rates of Covid-19 Testing",  # self vs. family 
                               width = 12, status = "primary", solidHeader =  T,
                               plotOutput (outputId = "testing_plot")), 
                           box(title = "How has Covid-19 Testing changed over time?", # self vs. family
                               width = 12, status = "primary", solidHeader =  T,
                               plotOutput (outputId = "testing_change_plot")))), 
                tabPanel('Symptoms', 
                         fluidRow(
                           box(title = "Rates of Covid-19 Symptoms",  # self vs. family
                               width = 12, status = "primary", solidHeader =  T,
                               plotOutput(outputId = 'symptom_plot')),
                           box(title = "How has Covid-19 symptom rates changed over time?", # self vs. family
                               width = 12, status = "primary", solidHeader =  T,
                               plotOutput(outputId = 'symptom_change_plot')))),
                tabPanel('Concerns ',
                         fluidRow(
                           box(title = "How distressed are women about potential covid-19 illness?", 
                               width = 12, status = "primary", solidHeader =  T,
                               plotOutput(outputId = 'covid_distress_plot'))))
                          # can look at how this changes over time!
              )
            )
    )
    ## ANOTHER TAB CONTENT 
Impact_tab <- tabItem(tabName = "Impact", 
            h3("Impact on daily life during the Covid-19 pandemic"),
            p("Women shared their experiences and feelings about pandemic-related changes."),
            fluidRow(
              tabBox(
                title = "", 
                id = "tabset1", width = "450px", selected = "Summary", 
                tabPanel('Summary',  
                         fluidRow(
                           infoBoxOutput(outputId = "percent_job_future_fact", 
                                         width = 12),
                           infoBoxOutput(outputId = "percent_childcare_fact", 
                                         width = 12),
                           infoBoxOutput(outputId = "percent_miss_most_fact", 
                                         width = 12),
                           infoBoxOutput(outputId = "", 
                                         width = 12),fill = TRUE)),
                tabPanel('Financial & Employment Impacts', 
                         fluidRow(
                           box(title= "How has Covid-19 impacted women's employment and finances?",
                               width = 12, solidHeader =  T, status = "primary",
                                plotOutput (outputId = "financial_impact_plot")),
                           box(title = "How worried are women about these changes?", 
                               width = 12, height = 300, solidHeader =  T, status = "primary",
                               plotOutput (outputId = "job_distress_plot")))),
              
                tabPanel("Negative Daily Impacts", 
                         fluidRow(
                          
                           box(title = "What are the negative impacts of Covid-19 on women's lives?",
                               width = 12, solidHeader =  T, status = "primary",
                               plotOutput (outputId = "core1_neg_plot")),
                             box(title = "Which activities do women miss the most?",
                                 width = 12, solidHeader = T, status = "primary",
                                 plotOutput(outputId = 'miss_impact_plot')))), 
                tabPanel("Top concerns", 
                         fluidRow(
                           # instead of this time 1 plot, get the cope IU longitudinal disruptions
                           box(title = "Single greatest source of stress due to covid-19", 
                               width = 12, solidHeader =  T, status = "primary",
                               plotOutput (outputId = "stress_plot")), 
                           box(title = "What are women most concerned about?",
                               width = 12, solidHeader =  T, status = "primary",
                               plotOutput (outputId = "concern_plot"))))
                # could add stress plot, if it changes over time.
              )
            )
)

#   UNUSED PIECES
#box(title = "Has Covid-19 overall had a positive or negative impact?",
 #   width= 12, solidHeader =  T, status = "primary",
  #  plotOutput(outputId = "valence_plot")),

Wellbeing_tab <- tabItem(tabName  = "Well-being",
                         h3("Well-being, coping, and social support"),
                         p("We asked women how they are coping with stress during the pandemic"),
                fluidRow(
                  tabBox(
                    title = "", 
                    id = "tabset1", width = "450px", selected = "Summary", 
                    tabPanel('Summary',  
                             fluidRow(
                               infoBoxOutput(outputId = "", 
                                             width = 12),
                               infoBoxOutput(outputId = "", 
                                             width = 12),
                               infoBoxOutput(outputId = "", 
                                             width = 12),
                               infoBoxOutput(outputId = "", 
                                             width = 12),fill = TRUE)),
                  # tabPanel("Emotions and Feelings"),
                   # BSI and PTSD items?
                  tabPanel('Social Support', 
                         fluidRow(
                          
                            box(title = "How are women meeting their social support needs?", 
                                          width = 12, solidHeader = T, status = "primary", 
                                          plotOutput(outputId = "social_how_plot")),
                            box(title = "Who are women reciving social support from?",
                                          width = 12, solidHeader = T, status = "primary", 
                                          plotOutput(outputId = "social_who_plot")))),
              tabPanel("Positive Impacts", 
                       fluidRow(
                       box(title = "What are the positive effects of Covid-19 on women's daily lives?",
                           width = 12, solidHeader =  T, status = "primary",
                           plotOutput (outputId = "positive_change_plot")),
                       box(title = "What coping strategies are most helpful for women?",
                           width= 12, solidHeader =  T, status = "primary",
                           plotOutput(outputId = "coping_plot"))))
          
              )
         )
    )

  

# put tabs together  -------
body <-  dashboardBody(
  tabItems(About_tab, Healthcare_tab, Impact_tab)) # covid_tab, Wellbeing_tab))

# put the whole dashboard together
ui <-  dashboardPage(
  dashboardHeader(title = "NYU COPE Study"), 
  sidebar, body)

# SERVER FUNCTION ---------------------------------------------------------

server_function <- function(input, output) {
  ## OUTPUTS FOR INFOBOXES ----- 
  output$prenatal_fact <- renderInfoBox({
    infoBox(title = "",
            subtitle = "of pregnant women reported changes to prenatal care due to the Covid-19 pandemic",
            paste0("75%"), color = "navy", 
            icon = icon("female", class = "fas fa-female", lib = "font-awesome"))
  })
  output$delivery_fact <- renderInfoBox({
    infoBox(title = "",
            subtitle = "of women who delivered after March 15 reported changes to their birth plan",
            paste0(birth_change_fact$Percent, "%"), color = "navy",
            icon = icon("hospital", class = "fas fa-hospital", lib = "font-awesome"))
  })
  output$support_partner_fact <- renderInfoBox({
    infoBox(title = "",
            subtitle = "of new mothers reported that their support partner was not permitted at delivery",
            paste0(support_partner_fact$Percent, "%"), color = "navy",
            icon =  icon("heart", class = "fas fa-heart", lib = "font-awesome"))
  })
  output$postnatal_fact <- renderInfoBox({
    infoBox(title = "",
            subtitle = "of new mothers reported changes to postnatal care due to the Covid-19 pandemic",
            paste0(postnatal_change$Percent, "%"), color = "navy",
            icon =  icon("home", class = "fas fa-home", lib = "font-awesome"))
  })
  output$support_change_fact <- renderInfoBox({
    infoBox(title = "", 
            subtitle = "of new mothers reported that support from perinatal care providers has worsened due to the Covid-19 pandemic",
            paste0(worsened_care_change$Percent, "%"), color = "navy", 
            icon = icon("heart", class = "fas fa-heart", lib = "font-awesome"))
  })  
  output$percent_childcare_fact  <- renderInfoBox({
    infoBox(title = "", 
            paste0(percent_childcare, "%"), color = "navy",
            subtitle = "Of mothers (with at least one child) reported childcare challenges due to Covid-19",
            icon = icon("child", class = "fas fa-child", lib = "font-awesome"))
  })
  output$percent_job_future_fact <- renderInfoBox({
    infoBox(title = "",    
            paste0(fact_job_future$Percent, "%"), 
            subtitle = "Of women are concerned about future employment & financial impacts of Covid-19",
            icon = icon("dollar-sign", class = "fas fa-dollar-sign", lib = "font-awesome"), 
            color = "navy")
  })
  output$percent_miss_most_fact <- renderInfoBox({
    infoBox(title = "", 
            subtitle = "Of women reported that the activity they miss the most is in-person social contact", 
            paste0(miss_most$Percent, "%"), 
            icon = icon("user-friends", class = "fas fa-user-friends", lib = "font-awesome"), 
            color = "navy")
  })
  
  # OUTPUT FOR PLOTTING ----------------
  # load static plots for Perinatal healthcare panel
  output$prenatal_plot <- renderPlot({ prenatal_plot})
  output$preg_concern_plots <- renderPlot({ 
    plot_object <- grid.arrange(preg_concern_plot1, 
                                preg_concern_plot2,
                                preg_concern_plot3, 
                                nrow = 3)
    print(plot_object)
   }, height = 450)
  
  output$birth_plot <- renderPlot({birth_plot })

  output$birth_time_plot <- renderPlot({
    birth_time_table_long %>%
    filter(N_total > 10) %>%
    filter(Birth_Changes %in% input$change_choice) %>%
    ggplot(aes (x = child_birth_week, y = Percent)) + 
    geom_line() + custom_theme + xlab("Delivery Date") + 
    facet_wrap(~Birth_Changes, ncol = 2) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
      labs(caption = paste("Data aggregated from", birth_change_time_N, "women"))
  })

  output$postnatal_plot <- renderPlot({postnatal_plot })
  output$support_provider_plot <- renderPlot({support_provider_plot}, height = 300)
  
  # load more interactive plots for impact panel
   output$financial_impact_plot  <- renderPlot({financial_impact_plot}) 
   output$job_distress_plot  <- renderPlot({job_distress_plot}, height = 200)
  # output$restrictions_plot <- renderPlot({restrictions_plot})
   output$miss_impact_plot <- renderPlot({miss_impact_plot})
   output$valence_plot <- renderPlot({valence_plot})
   output$core1_neg_plot <- renderPlot({core1_neg_plot})
   
   output$concern_plot <- renderPlot({concern_plot})
   output$stress_plot <- renderPlot({stress_plot})
   output$positive_change_plot <- renderPlot({positive_change_plot})
  

}


# SHINY APP CALL --------------------------------------------------------------

shinyApp(ui = ui, server = server_function)
