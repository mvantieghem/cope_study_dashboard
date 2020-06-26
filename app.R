# Shiny app for Exploratory data analysis of the COPE Study
# Author: Michelle VanTieghem
# Date: June 10, 2020

# load packages needed
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)

# process data and generate tables and plots 
source("scripts/custom_functions.R")
source_rmd("scripts/1a_data_cleaning_baselineQ.Rmd")
source_rmd("scripts/1b_data_cleaning_new_momQ.Rmd")
source_rmd("scripts/report_new_mom.Rmd")
source_rmd("scripts/report_pregnant_survey.Rmd")


# USER INTERFACE -----------------------------------------------------

# Define UI for app  ----
ui <-  fluidPage( titlePanel("COPE: Coronovirus-19 and Perinatal Experiences Study"), 
             theme = shinytheme("united"), # theme chosen for colors & font 
             #navbar page is a specific page layout we are using
      navbarPage(title = "",
                tabPanel('Study Overview',
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
                               and ... "),
                          h3("COVGEN Research Alliance"), 
                          HTML("<p> In March of 2020, investigators across multiple institutions began sharing tools and approaches in support of 
                          global research harmonization. The Baby Bees Lab, along with our collaborators, created the <a href = 'https://www.covgen.org/'> COVGEN Research Alliance</a> , which aims to support these research activities by highlighting COVGEN research around 
                          the world, and providing a platform for new collaborations to emerge.")),
                          
                 tabPanel('Perinatal Health Care', 
                           # main panel of display
                           mainPanel(
                             h3("Understanding the impact of COVID-19 pandemic on perinatal health care."), 
                             p("We asked new and expectant moms to share their experiences about how their health care changed due to the pandemic."),
                             p(""), # blank line, more space before tabs
                             #   allow the main panel to have tabs
                             tabsetPanel(type = 'tabs',
                                         # make a plot using the output from the server
                                         # identify it using output id 
                                         tabPanel('Prenatal Care', 
                                                  plotOutput (outputId = "prenatal_plot")), 
                                         tabPanel("Delivery Plans", 
                                                  plotOutput(outputId = 'birth_plot')),
                                         tabPanel("Postnatal Care", 
                                                  plotOutput(outputId = 'postnatal_plot')) 
                                 ),
                             h4(" Key findings from these data:"),
                             p(" - More than 75% of pregnant women reported significant changes to prenatal care, 
                               with most women reporting changes to virtual care and/or reduced prenatal visits."), 
                             p(" - 65% of women who gave birth during the Covid-19 pandemic (March 15 - June 1, 2020) 
                             reported significant changes to their delivery care plans, 
                                   and 40% reported that their preferred support person was not permitted at their delivery."),
                             p(" - 80% of new mothers reported changes to their postnatal care,
                                   with 70% reporting that family and friends were unable to visit.")
                            )
                 ),

                tabPanel('Impact',
                        mainPanel(
                           h3("How has the Covid-19 pandemic impacted pregnant and expectant moms?"), 
                           p("Women shared how Covid-19 has impacted their daily lives, 
                             and what aspects of those impacts are most concerning to them."),
                           p(""), # blank line, more space before tabs
                           #   allow the main panel to have tabs
                           tabsetPanel(type = 'tabs',
                                       # make a plot using the output from the server
                                       # identify it using output id 
                                       tabPanel('Financial',
                                               plotOutput (outputId = "financial_impact_plot"),
                                               h4("Key findings:"),
                                               p(" - More than 50% of women changed to remote work"), 
                                               p(" - X% of women reported job loss, and X% reported reduced pay."),
                                               p(" - 25% of women reported disruptions to jobs due to child care challenges.")),
                                       # associated concern levels - current and future 
                                       tabPanel("Health"),
                                       tabPanel("Restrictions"),
                                       tabPanel("Concerns"),
                                       tabPanel("Stressors")
                                       # make a summary plot that compares scores across all of the concern / distress categories 
                                               # how concerned they are about child's health
                                       # how concerned they are about prenatal care changes 
                                       # how concerned they are about postnatal care changes
                                       # how concerned they are about covid-19 exposures 
                                      # p("we asked women what the greatest source of stress is due to Covid-19"),
                                      # p("source_stress_covid, source_stress_covid2")
                           )
                        )
                ),
                  tabPanel('Mental Health',
                           h3("What factors are most important to predict maternal mental health?"), 
                           p("Interactive plot where X variables are factors \
                              (income, education, race, social support, \
                              job change, prenatal change, postnatal change, \
                               social support ratings, healthy coping behaviors) \
                               and y variables are bsi depression subscale")
                      )
)
)

# SERVER FUNCTION ---------------------------------------------------------

server_function <- function(input, output) {
  #load static plot created in report_new_mom.Rmd
  output$birth_plot <- renderPlot({
   birth_plot + theme(axis.text = element_text(size = 12))
  })
  
  #load static plot created in report_new_mom.Rmd
  output$postnatal_plot <- renderPlot({
     postnatal_plot + theme(axis.text = element_text(size = 12))
    })
  
  # load static plot that was created in report_pregnant_survey.Rmd
  output$prenatal_plot <- renderPlot({
   prenatal_plot +  theme(axis.text = element_text(size = 12))
    })
  
  #load static plot that was created in report_financial_changes.Rmd
  output$financial_impact_plot  <- renderPlot({
     financial_impact_plot + theme(axis.text = element_text(size = 12))
    
    })
  
}


# SHINY APP CALL --------------------------------------------------------------

shinyApp(ui = ui, server = server_function)
