# Shiny app for Exploratory data analysis of the COPE Study
# Author: Michelle VanTieghem
# Date: June 10, 2020

# load packages needed
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)

# process data and generate tables we use to plot.
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
                  tabPanel('Impact & Stressors',
                           h3("What are the biggest stressors associated with the Covid-19 pandemic?"), 
                           p("New and expectant mothers shared how Covid-19 has impacted their daily lives, 
                             and what aspects of those impacts are most concerning to them."),
                           p(""), # blank line, more space before tabs
                           #   allow the main panel to have tabs
                           tabsetPanel(type = 'tabs',
                                       # make a plot using the output from the server
                                       # identify it using output id 
                                       tabPanel('Financial Impacts'), 
                                               # endorsed changes to jobs and childcare 
                                       # associated concern levels - current and future 
                                       tabPanel("Health Impacts"), 
                                               # how concerned they are about child's health
                                       # how concerned they are about prenatal care changes 
                                       # how concerned they are about postnatal care changes
                                       # how concerned they are about covid-19 exposures 
                                       tabPanel("Social Impacts")
                                                # social distruptions
                           ),
                           h4("Key findings:")
                           ),
                tabPanel('Social Support',
                         h3("How are women receiving social support during the COVID-19 pandemic?"), 
                         p("We asked women about how they have adapted to covid-19 to meet their social support needs."),
                         tabsetPanel(type = 'tabs',
                                     # make a plot using the output from the server
                                     # identify it using output id 
                                     tabPanel('Changes in Social Support'),  
                                     # do women report increases or decreases in social support
                                     tabPanel('Means of Social Support'),
                                     # how are they getting social support, and who are they finding most supportive?
                                     tabPanel('Disruption to social support')
                                     # how distressed are women about changes to social support? ideally not a lot.
                          ),
                          h4("Key findings:")
                ), 
                tabPanel('Coping and Resilience',
                           h3("How are women coping with the Covid-19 pandemic?"), 
                           p("We wanted to understand how women are coping with stressors associated with the pandemic,
                             and learn about how the circumstances of the pandemic may have influenced them in positive ways"),
                          tabsetPanel(type = 'tabs',
                                      # make a plot using the output from the server
                                      # identify it using output id 
                                      tabPanel('Coping Strategies'),  
                                      tabPanel('Positive Impacts'),
                                      tabPanel('Advice For Other Women')
                                      # social distruptions
                          ),
                          h4("Key findings:")
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

  output$birth_plot <- renderPlot({
    
    # create the plot using data table already created
    birth_plot <- birth_changes_table_long %>%
      filter(Delivery_date == "After March 15, 2020" & 
               Birth_Changes != "No changes") %>%
      mutate(Birth_Changes = reorder(Birth_Changes, Percent)) %>%
      ggplot(aes(x = Birth_Changes, y = Percent)) +
      geom_bar(stat =  "identity", position = position_dodge(), fill = "dark red") + 
      coord_flip()  + theme_bw() + xlab("") + ylim(0, 100)+
      labs( title = "Birth & Delivery during the COVID-19 outbreak", 
            subtitle = "Percent of women who reported changes to their birth plans due to covid-19", 
            caption = "Data aggregated from 147 women who delivered after March 15th, 2020")  + 
      ylab ("") +  theme(axis.text = element_text(size = 12))
    
    # Display the plot (whether it has facets or not)
    birth_plot
    
  })
  
  output$postnatal_plot <- renderPlot({
    
    # create the plot using data table already created
    postnatal_plot <- postnatal_changes_table_long %>%
      filter(Postnatal_Changes != "No change") %>%
      mutate(Postnatal_Changes = reorder(Postnatal_Changes, Percent)) %>%
      ggplot(aes(x = Postnatal_Changes, y = Percent)) +
      geom_bar(stat =  "identity", position = position_dodge(), fill = "dark red") + 
      coord_flip()  + theme_bw() + xlab("") + ylim(0, 100)+ 
      labs( title = "Postnatal care during the COVID-19 outbreak", 
            subtitle = "Percent of women who reported changes to postnatal care due to covid-19", 
            caption = "Data aggregated from 406 women with infants under 6 months of age") +
      ylab ("") +  theme(axis.text = element_text(size = 12))
    postnatal_plot
    
  })
  
  output$prenatal_plot <- renderPlot({
    # create the plot using data table already created
    prenatal_plot <- prenatal_changes_table_long %>%
      filter(prenatal_changes != "No changes") %>%
      mutate(prenatal_changes = reorder(prenatal_changes, Percent)) %>%
      ggplot( aes(x = prenatal_changes, y = Percent)) + 
      geom_bar(stat =  "identity", position = position_dodge(), fill = "dark red") + 
      coord_flip()  + theme_bw() + xlab("") + ylim(0, 100)  +
      labs( title = "Prenatal care during the COVID-19 outbreak", 
            subtitle = "Percent of women reporting reporting changes to prenatal care", 
            caption = "Data aggregated from 417 pregnant women")  +
      ylab ("") +  theme(axis.text = element_text(size = 12))
    
    prenatal_plot
  })
}


# SHINY APP CALL --------------------------------------------------------------

shinyApp(ui = ui, server = server_function)
