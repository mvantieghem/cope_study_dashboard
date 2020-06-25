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
source("scripts/custom_functions.R")
source_rmd("scripts/1a_data_cleaning_baselineQ.Rmd")
source_rmd("scripts/1b_data_cleaning_new_momQ.Rmd")
source_rmd("reports/report_new_mom.Rmd")
source_rmd("reports/report_pregnant_survey.Rmd")


# USER INTERFACE -----------------------------------------------------

# Define UI for app  ----

             #theme = shinytheme("united"), # theme chosen for colors & font 
             #navbar page is a specific page layout we are using
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "About", icon = icon("th")),
    menuItem("Perinatal", tabName = "Perinatal", icon = icon("th"))
  )
)

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
                               and ... "),
            h3("COVGEN Research Alliance"), 
            HTML("<p> In March of 2020, investigators across multiple institutions began sharing tools and approaches in support of 
                          global research harmonization. The Baby Bees Lab, along with our collaborators, created the <a href = 'https://www.covgen.org/'> COVGEN Research Alliance</a> , which aims to support these research activities by highlighting COVGEN research around 
                          the world, and providing a platform for new collaborations to emerge.")),
    
    
    
    # Second tab content
    tabItem(tabName = "Perinatal",
            h3("Perinatal Health Care during the COVID-19 pandemic."), 
            p("We asked new and expectant moms to share their experiences about how their health care changed due to the pandemic."),
            fluidRow(
              tabBox(
                title = "", 
                id = "tabset1", width = "450px", selected = "Tab1", 
                tabPanel('Summary',  
                         fluidRow(
                           infoBoxOutput(outputId = "prenatal_fact", 
                                         width = 12),
                            infoBoxOutput(outputId = "delivery_fact", 
                                          width = 12),
                             infoBoxOutput(outputId = "support_partner_fact", 
                                width = 12),
                            infoBoxOutput(outputId = "postnatal_fact", 
                                          width = 12),fill = TRUE)
                            ),
                tabPanel('Prenatal Care', 
                         plotOutput (outputId = "prenatal_plot")), 
                tabPanel("Delivery Plans", 
                         plotOutput(outputId = 'birth_plot')),
                tabPanel("Postnatal Care", 
                         plotOutput(outputId = 'postnatal_plot')) 
              )
            )
    )
  )
)

# put it all together.
ui <-  dashboardPage(
  dashboardHeader(title = "NYU COPE Study"), 
  sidebar, 
  body)
# SERVER FUNCTION ---------------------------------------------------------

server_function <- function(input, output) {
  output$prenatal_fact <- renderInfoBox({
    infoBox(title = "changes to prenatal care",
            subtitle = "Most common change: changes to virtual care",
            paste0("75%"), icon = icon("female", class = "fas fa-female", lib = "font-awesome"))
  })
  output$delivery_fact <- renderInfoBox({
    infoBox(title = "Changes to birth & delivery plan",
            subtitle = "New mothers who delivered between March 15 - June 1, 2020",
            paste0("65%"), icon =  icon("female", class = "fas fa-female", lib = "font-awesome"))
  })
  output$support_partner_fact <- renderInfoBox({
    infoBox(title = "support partner was not permitted at delivery",
            paste0("40%"), icon =  icon("female", class = "fas fa-female", lib = "font-awesome"))
  })
  output$postnatal_fact <- renderInfoBox({
    infoBox(title = "changes to postnatal care",
            subtitle = "Most common change: family and friends aren't able to visit",
            paste0("85%"), icon =  icon("female", class = "fas fa-female", lib = "font-awesome"))
  })
  
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
