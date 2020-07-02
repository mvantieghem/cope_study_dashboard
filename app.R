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
#source_rmd("scripts/1a_data_cleaning_baselineQ.Rmd")
#ource_rmd("scripts/1b_data_cleaning_new_momQ.Rmd")
source_rmd("analyses/report_pregnant_survey.Rmd")
source_rmd("analyses/report_birth_changes.Rmd")
source_rmd("analyses/report_postnatal_changes.Rmd")
source_rmd("analyses/report_concerns_stress.Rmd")
source_rmd("analyses/report_financial_changes.Rmd")
source_rmd("analyses/report_covid_restrictions.Rmd")

# USER INTERFACE -----------------------------------------------------

# Define UI for app  --------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "About", icon = icon("th")),
    menuItem("Healthcare", tabName = "Healthcare", icon = icon("th")),
  #  menuItem("Covid-19", tabName = "Covid-19", icon = icon("th")),
    menuItem("Impact", tabName = "Impact", icon = icon("th"))
    #menuItem("Stress", tabName = "Stress", icon = icon("th"))
    #menuItem("Coping", tabName= "Coping", icon = icon("th"))
  )
)
# Create body of dashboard ----------------
body <-  dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "About",
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
            HTML("<p>Code on <a href =https://github.com/mvantieghem/cope_study_dashboard >Github</a>")),
    
    # Second tab content
    tabItem(tabName = "Healthcare",
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
                           box(title = "What are pregnant women's top concerns?", 
                               width = 12, height = 550, status = "primary", solidHeader = T, 
                               plotOutput (outputId = "preg_concern_plots")))), 
                ##** ADD BOX FOR PROVIDER SUPPORT
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
                #*** ADD ROW WITH CHANGES OVER TIME BASED ON BIRTH***
                #* ADD LEVEL OF DISTRESS DUE TO THESE CHANGES****
                tabPanel("Postnatal Care", 
                         fluidRow( 
                           box(title = "How has Covid-19 impacted postnatal care?", 
                               width = 12, status = "primary",solidHeader =  T,
                               plotOutput(outputId = 'postnatal_plot')),
                            box(title = "Has support from perinatal care providers changed due to Covid-19?", 
                                width = 12, height = 400, status = "primary", solidHeader =  T,
                              plotOutput(outputId = 'support_provider_plot'))))
              )
            )
    ),
    # THIRD TAB Content ------------------------
    tabItem(tabName = "Covid-19",
            h3("Exposures to covid-19"),
            p("Pregnant women and new mothers shared information on their exposures to covid-19"),
            fluidRow(
              tabBox(
                title = "", 
                id = "tabset1", width = "450px", selected = "Summary", 
                tabPanel('Summary',  
                         fluidRow(
                           infoBoxOutput(outputId = "Pregnant Women tested positive", 
                                         width = 12),
                           infoBoxOutput(outputId = "New mothers tested positive", 
                                         width = 12),
                           infoBoxOutput(outputId = "Family member tested positive", 
                                         width = 12),
                           infoBoxOutput(outputId = "", 
                                         width = 12),fill = TRUE)
                ),
                tabPanel('Testing', 
                         fluidRow(
                           box(width = 12, status = "primary", solidHeader =  T,
                               plotOutput (outputId = "testing_plot")))), 
                tabPanel('Symptoms', 
                         fluidRow(
                           box(width = 12, status = "primary", solidHeader =  T,
                               plotOutput(outputId = 'symptom_plot')))),
                tabPanel('Exposures',
                         fluidRow(
                           box(width = 12, status = "primary", solidHeader =  T,
                               plotOutput(outputId = 'exposures_plot')))) 
              )
            )
    ),
    ## ANOTHER TAB CONTENT --------------------
    tabItem(tabName = "Impact", 
            h3("Impact on daily life during the Covid-19 pandemic"),
            p("We asked women how the covid-19 pandemic has impacted different facets of their daily lives"),
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
                                         width = 12),fill = TRUE)
                ),
                tabPanel('Impact on Employment', 
                         fluidRow(
                           box(title= "How has Covid-19 impacted women's employment and finances?",
                               width = 12, solidHeader =  T, status = "primary",
                                plotOutput (outputId = "financial_impact_plot")),
                       #  note that height is in pixels, and width is relative to actual width of page.
                         box(title = "How worried are women about these changes?", 
                             width = 12, height = 300, solidHeader = T, status = "primary",
                             plotOutput(outputId = "job_distress_plot")))),
                tabPanel('Restrictions to Activities', 
                         fluidRow(
                           #box(width = 12, status = "primary", solidHeader =  T,
                            #    plotOutput(outputId = 'restrictions_plot')),
                           box(title = "Which activities do women miss the most?",
                               width = 12, solidHeader = T, status = "primary",
                               plotOutput(outputId = 'miss_impact_plot')))) 
    ),
    tabItem(tabName = "Stress",
            h3("Concerns and worries during Covid-19"),
            p("Women shared their greatest sources of concern during this stressful time."),
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
                                         width = 12),fill = TRUE)
                ),
                
                tabPanel('Top Concerns', 
                         checkboxGroupInput(inputId = "concern_choice",
                                      label = "Concerns about reduced access to ...",
                                      choices = list_concerns, 
                                      selected = "food"),
                         plotOutput(outputId = 'concern_plot')),
                tabPanel('Biggest Stressors',
                         plotOutput(outputId = 'stress_plot')), 
                tabPanel('Changes',
                         plotOutput(outputId = 'change_plot'))
              )
            )
    ),
    tabItem(tabName = "Coping",
            h3("Coping during the time of the Covid-19 pandemic"),
            p("Women shared how they are coping with stress and receiving social support"),
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
                                         width = 12), fill = TRUE)
                ),
  
                tabPanel('Coping strategies', 
                         plotOutput(outputId = 'coping_plot')),
                tabPanel('Social support',
                         plotOutput(outputId = 'social_plot')), 
                tabPanel('Resources', 
                         plotOutput(outputId = 'resources_plot')), 
                tabPanel('Positive change',
                         plotOutput(outputId = 'positive_words'))
              )
            )
    )
  )
)

  # Put it all together -------
ui <-  dashboardPage(
  dashboardHeader(title = "NYU COPE Study"), 
  sidebar, 
  body)

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
            subtitle = "Of women reported childcare challenges due to Covid-19",
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
  
  # load static plots for Impact panel 
   output$financial_impact_plot  <- renderPlot({financial_impact_plot })
   output$job_distress_plot  <- renderPlot({job_distress_plot} , height = 250)
   output$restrictions_plot <- renderPlot({restrictions_plot})
   output$miss_impact_plot <- renderPlot({miss_impact_plot})

}


# SHINY APP CALL --------------------------------------------------------------

shinyApp(ui = ui, server = server_function)
