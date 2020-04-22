#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#pic <- readImage("C:/Users/Ryder Nguyen/Google Drive/Desktop/R Projects/personal_portfolio/fractal.jpg")
#setwd("C:/Users/Ryder Nguyen/Google Drive/Desktop/R Projects/personal_portfolio/")
library(flexdashboard)
library(shinyWidgets)
library(dplyr)
library(readtext)
library(shinyBS)
library(shinyjs)
#x <- readLines("C:/Users/Ryder Nguyen/Google Drive/Desktop/R Projects/personal_portfolio/files/MSMAbstract1.txt")
#class(x[1])
#x <- c()

library(plotly)
#library(R6)
#library(ggplot2)
#library(plotly)
#library(uuid)

#x <- readtext('C:/Users/Ryder Nguyen/Google Drive/Desktop/R Projects/personal_portfolio/files/Bio.txt')
#x$text

# Define UI for application that draws a histogram
ui <- shinyUI(
    fluidPage(
        
      #HTML FORMATS -----------------------------------------------------------------------------------------------------------------------------------
      tags$head(
            tags$style(HTML("
      
      h1{
        font-family: 'Arial', cursive;
        font-weight: 700;
        font-style: bold;
        line-height: 0;
        color: #2C4189;
      };
      
      h4{
        font-family: 'Arial', cursive;
        font-weight: 200;
        font-style: bold;
        line-height: 1;
        color: #2C4189;
      }"))),
        
        
        tags$head(tags$style("#MSMText1,#MSMText2,#MSMText3, #EarthText1, #EarthText2, #EarthText3
                                {color: black;
                                 font-family: 'Arial', cursive;
                                 font-style: bold;
                                 font-size: 17px;}")),
        
        tags$head(tags$style("#aboutme,#interest,#contact
                                {color: black;
                                 font-family: 'Arial';
                                  font-style: bold;
                                 font-size: 17px;}")),
        
        tags$head(tags$style("#contacttext
                                {color: black;
                                line-height: 2;
                                font-family: 'Arial';}")),
        
        setBackgroundImage(src = "blue.png"),
        
        tags$style(HTML("#homebutton {background-image: url('button.png');}")),
        
        tags$style(HTML("#first {background-image: url('block.png');}")),
        
        tags$style(HTML("#sidepanel {background-image: url('sidepanel1.png');
                        width: 100%; height: auto; padding-left: 1%;}")),
        
        tags$head(tags$style(
          type="text/css",
          "#headshot img {max-width: 100%; width: 100%; height: auto}"
        )),
        
        tags$head(tags$style(
          type="text/css",
          "#panel,#panel1, #panel2, # img {max-width: 100%; width: 100%; height: auto}"
        )),
        
        tags$head(tags$style(HTML(
        "#bodytxt {font-size:17px; height:300px;}"))),
      
        #UNIVERSAL FORMATS OF THE SELECTION INPUT
        tags$style(type='text/css', ".selectize-input {font-size: 15px; line-height: 15px; color: #2C4189; font-style: bold;} .selectize-dropdown {font-size: 15px; line-height: 15px; color: #2C4189; font-style: bold;}"),

        
    ################################################################################################################################################################################
    #START OF NAVIGATION BAR -------------------------------------------------------------------------------------------------------------------------------------------------------
    ################################################################################################################################################################################
    navbarPage(id="navbar",tags$h1("Portfolio"), position ="fixed-top", windowTitle = "Tien Nguyen_Portfolio",
               
               #####################################################################################################################################################################
               #Main Page ----------------------------------------------------------------------------------------------------------------------------------------------------------
               tabPanel(value = "Main",tags$h4("Main Page"),
                        tags$style(type="text/css", "body {padding-top: 100px;}"),
                        fluidRow(
                          column(id="myintro",width = 9,
                                 tags$div(
                                  tags$h1("About Me"),
                                  br(),
                                  textOutput("aboutme"),
                                  br(),
                                  tags$h1("Interests & Everything Beautiful"),
                                  br(),
                                  textOutput("interest"),
                                  br(),
                                  
                                  tags$div(id = "panelblock",
                                  tags$style(type="text/css", "#panelblock {background-image: url('block1.png');}"),
                                  tags$h1("Explore My Projects Below"),
                                  br(),
                                  
                                  fluidRow(
                                  column(width = 4,
                                         imageOutput("panel",click = "panel_click"),
                                         bsTooltip(id="panel", title = "Hello blaj hasdfa dafsdf fasdfads asdfadsfadf afsdfadfadf asdfasdfadfa asdfasdfadsfa afsddfsdfa afsdfafaf afsd!", placement = "right", trigger = "hover"),
                                         imageOutput("panel5",click = "panel5_click")),
                                  column(width = 4,
                                         imageOutput("panel1",click = "panel1_click"),
                                         imageOutput("panel4",click = "panel4_click")),
                                  column(width = 4,
                                         imageOutput("panel2",click = "panel2_click"),
                                         imageOutput("panel3",click = "panel_click3"))
                                  ))
                                  )),
                          
                          column(id = "floatcolumn", width = 3, offset = 9,
                                 tags$style(type="text/css", "#floatcolumn {position: fixed;}"),
                                   
                                 tags$img(id = "headshot", src='headshot.png', width = "100%", height = "auto"),
                                 tags$div(id="sidepanel",
                                          br(),
                                          tags$h1("Contact Info"),
                                          br(),
                                          
                                fluidRow(
                                  
                                  tags$style(type="text/css", "body #contacttext {padding-bottom: 10px;}"),
                                  
                                  #LINKEDIN LINKS --------------------------------------------------------
                                  column(id= "contacttext", width = 3,
                                                tags$a(href="https://www.linkedin.com/in/tien-ryder-nguyen-2251785a/",target = "_blank",
                                                tags$img(id = "linkedin", src='linkedin.png', width = "80%", height = "auto")),
                                                bsTooltip(id = "linkedin", title = "VISIT MY LINKEDIN", placement = "bottom", trigger = "hover")),
                                  
                                  #EMAIL LINKS --------------------------------------------------------
                                  column(id= "contacttext", width = 3,
                                                    tags$img(id = "email", src='email.png', width = "80%", height = "auto",style="cursor:pointer"),
                                                    useShinyjs(),
                                                    bsTooltip(id = "email", title = "LEAVE ME A MESSAGE", placement = "bottom", trigger = "hover")),
                                  
                                  #RESUME LINKS --------------------------------------------------------
                                  column(id= "contacttext", width = 3,
                                                  tags$img(id = "resume", src='resume.png', width = "80%", height = "auto",style="cursor:pointer"),
                                                  useShinyjs(),
                                                  bsTooltip(id = "resume", title = "VIEW MY RESUME", placement = "bottom", trigger = "hover")),
                                  
                                  #GITHUB LINKS --------------------------------------------------------
                                  column(id= "contacttext", width = 3,
                                                tags$a(href="https://github.com/rydernguyen?tab=repositories",target = "_blank",
                                                tags$img(id = "github", src='github.png', width = "80%", height = "auto")),
                                                bsTooltip(id = "github", title = "VIEW MY DEPOSITORY", placement = "bottom", trigger = "hover"),
                                        ))))
                        )),
               
               #####################################################################################################################################################################
               #Markov-Switching Multifractals------------------------------------------------------------------------------------------------------------------------------------
               tabPanel(value="MSM",tags$h4("Markov-Switching Multifractals"),
                        fluidRow(
                        column(id="first",width = 9,
                          headerPanel(tags$div(
                            tags$h1("Markov-Switching Multifractals Model"),
                            tags$h2("- Applicability & Advantages in Modeling Financial Time Series"))),
                          tags$div(
                            textOutput("MSMText1"),
                            br(),
                            textOutput("MSMText2"),
                            br(),
                            textOutput("MSMText3"),
                            br(),
                            htmlOutput("MSMLink")
                            )),
                        
                        column(width = 3,
                               tags$style(type="text/css", "{position: fixed;}"),
                               actionButton("viewMSM", "View Full Paper",icon("eye"),onclick ="window.open('https://drive.google.com/file/d/1MK6jnKnegdzeUZ9CthWJ9bIqme9Qs6Xc/view?usp=sharing', '_blank')"),
                               actionButton("homebutton", "Back to Main",icon("redo-alt")),
                               br(),
                               selectInput("MSMSelect","Navigate:",choices = c("Introduction","Abstract","Conclusion"),selected = "Introduction"))
                        )),
               
               #####################################################################################################################################################################
               #Earthquakes in Boston--------------------------------------------------------------------------------------------------------------------------------------------
              tabPanel(value="Earth",tags$h4("Impact of Earthquakes"),
                       fluidRow(
                         column(id="first", width = 9,
                                headerPanel(tags$div(
                                  tags$h1("Earthquake Risk in Back Bay, Boston"),
                                  tags$h2("- Estimation and Analysis of Losses"))),
                                tags$div(
                                  textOutput("EarthText1"),
                                  br(),
                                  textOutput("EarthText2"),
                                  br(),
                                  textOutput("EarthText3"),
                                  br(),
                                  htmlOutput("EarthLink")
                                  )),
                         
                         column(width = 3,
                                tags$style(type="text/css", "{position: fixed;}"),
                                actionButton("viewEarth", "View Full Paper",icon("eye"),onclick ="window.open('https://drive.google.com/file/d/1D9L9x1LfscHZ7SgwR4au-feIyyUvnpNt/view?usp=sharing', '_blank')"),
                                actionButton("homebutton1", "Back to Main",icon("redo-alt")),
                                br(),
                                selectInput("EarthSelect","Navigate:",choices = c("Introduction","Main Findings","Conclusion"),selected = "Introduction"))
                         )),
               
              #####################################################################################################################################################################
               #Monte Carlo Simulation-------------------------------------------------------------------------------------------------------------------------------------------
               tabPanel(value="Monte",tags$h4("Monte Carlo Simulation"),
                                 fluidRow(
                                   column(id="first", width = 9,
                                          headerPanel(tags$div(
                                            tags$h1("Monte Carlo Simulation"),
                                            tags$h2("- Internal Rate of Return for an Investment Fund"))),
                                          tags$div(
                                            tags$iframe(style = "width:1050px;height:550px",src="https://drive.google.com/file/d/1lOY45pN4oldj4BL7RlaJfpER5SQUb4Cj/preview"),
                                            br(),
                                            htmlOutput("MonteLink")
                                          )),
                                   column(width = 3,
                                          tags$style(type="text/css", "{position: fixed;}"),
                                          actionButton("viewMonte", "View Full Model",icon("eye"),onclick ="window.open('https://drive.google.com/file/d/1lOY45pN4oldj4BL7RlaJfpER5SQUb4Cj/view?usp=sharing', '_blank')"),
                                          actionButton("homebutton3", "Back to Main",icon("redo-alt")),
                                          br(),
                                          br(),
                                          tags$h4("Introduction"),
                                          textOutput("MonteIntro"))
                        )),
               
              ################################################################################################
               #BOX OF TREASURES #############################################################################
              ################################################################################################
               navbarMenu(tags$h4("Box of Treasures"), 
                        #CONTACT FORM ------------------------------------------------------------------------
                         tabPanel(value = "Contact", tags$h4("Contact"),
                                  fluidPage(
                                    column(width = 3,
                                           tags$h1("Contact Me"),
                                           br(),
                                           tags$iframe(style = "width:500px;height:620px",src="https://docs.google.com/forms/d/e/1FAIpQLSdce-nFquQ_L0rAYoZawJ7HCvPf_wm2O2jufzhtfviunL2dYg/viewform?embedded=true")
                                           ),
                                    
                                    column(width = 6, offset = 1,
                                           tags$h1("Resume"),
                                           br(),
                                           tags$iframe(width="950", height="620", src="https://drive.google.com/file/d/104TfnarYzEUllm_zyT6Np37Vi6FRoycP/preview")
                                    ),
                                    column(width = 2,
                                           br(),
                                           br(),
                                           downloadButton("downloadresume", "Download Resume"),
                                           actionButton("homebutton2", "Back to Main", icon("redo-alt"))
                                           ))),
                        
                        tabPanel(value = "Creatives", tags$h4("Creative Projects"))
               )
)))


###########################################################################################################################################################################
#############################################################################SERVER########################################################################################

server <- function(input, output, session) {

    #######################################################################################################################################   
    #LANDING PAGE
    output$aboutme <- renderText({x <- readLines('./files/Bio.txt')
    x[1]})

    output$interest <- renderText({x <- readLines('./files/Bio.txt')
    x[2]})
    
    output$contact <- renderText({x <- readLines('./files/Bio.txt')
    x[3]})
    
    
      #SET UP THE PANELS ###########################################################################
    output$panel <- renderImage({
      filename <- normalizePath(file.path('./www/', paste('panel', '.png', sep='')))
      list(src = filename)}, deleteFile = FALSE)
    
    observeEvent(input$panel_click, {
      updateNavbarPage(session,"navbar",
                        selected = "MSM")})
    
    output$panel1 <- renderImage({
      filename <- normalizePath(file.path('./www/', paste('panel1', '.png', sep='')))
      list(src = filename)}, deleteFile = FALSE)
    
    observeEvent(input$panel1_click, {
      updateNavbarPage(session,"navbar",
                       selected = "Earth")})
    
    output$panel2 <- renderImage({
      filename <- normalizePath(file.path('./www/', paste('panel2', '.png', sep='')))
      list(src = filename)}, deleteFile = FALSE)
    
    observeEvent(input$panel2_click, {
      updateNavbarPage(session,"navbar",
                       selected = "Monte")})
    
    output$panel3 <- renderImage({
      filename <- normalizePath(file.path('./www/', paste('panel3', '.png', sep='')))
      list(src = filename)}, deleteFile = FALSE)
    
    output$panel4 <- renderImage({
      filename <- normalizePath(file.path('./www/', paste('panel4', '.png', sep='')))
      list(src = filename)}, deleteFile = FALSE)
    
    output$panel5 <- renderImage({
      filename <- normalizePath(file.path('./www/', paste('panel5', '.png', sep='')))
      list(src = filename)}, deleteFile = FALSE)
    
      #OTHER NAVIGATION##################################################################################################################
    observeEvent(input$emailme_click, {updateNavbarPage(session,"navbar", selected = "Contact")})
    
    shinyjs::onclick("email",  updateNavbarPage(session,"navbar", selected = "Contact"))
    
    observeEvent(input$resume_click, {updateNavbarPage(session,"navbar", selected = "Contact")})
    
    shinyjs::onclick("resume",  updateNavbarPage(session,"navbar", selected = "Contact"))
    
    #######################################################################################################################################   
    #MARKOV-SWITCHING MULTIFRACTALS
    MSMtextfunc <- function(MSMSelect,i){
      if (MSMSelect == "Abstract") {x <- readLines('./files/MSMAbstract.txt')}
      else if (MSMSelect == "Conclusion") {x <- readLines('./files/MSMConclusion.txt')}
      else if (MSMSelect == "Introduction") {x <- readLines('./files/MSMIntroduction.txt')}

      if (length(x)>=i){return(x[i])}
    }
    
    output$MSMText1 <- renderText({MSMtextfunc(input$MSMSelect,1)})
    
    output$MSMText2 <- renderText({MSMtextfunc(input$MSMSelect,2)})
    
    output$MSMText3 <- renderText({MSMtextfunc(input$MSMSelect,3)})
    
    output$MSMLink <- renderUI(tags$a(href="https://drive.google.com/file/d/1MK6jnKnegdzeUZ9CthWJ9bIqme9Qs6Xc/view?usp=sharing",target = "_blank","Click here to view Full Paper!"))
    
    observeEvent(input$homebutton, {
      updateNavbarPage(session,"navbar",
                       selected = "Main")})
    
    #######################################################################################################################################
    #EARTHQUAKES IN BOSTON
    output$EarthLink <- renderUI(tags$a(href="https://drive.google.com/file/d/1D9L9x1LfscHZ7SgwR4au-feIyyUvnpNt/view?usp=sharing",target = "_blank","Click here to view Full Paper!"))
    
    output$backbay <- renderImage({
      filename <- normalizePath(file.path('./www/', paste('backbay', '.png', sep='')))
      list(src = filename,width = 375,
           height = 192.5)}, deleteFile = FALSE)
    
    
    Earthtextfunc <- function(EarthSelect,i){
      if (EarthSelect == "Main Findings") {x <- readLines('./files/EarthMainFindings.txt')}
      else if (EarthSelect == "Conclusion") {x <- readLines('./files/EarthConclusion.txt')}
      else if (EarthSelect == "Introduction") {x <- readLines('./files/EarthIntroduction.txt')}
      
      if (length(x)>=i){return(x[i])}
    }
    
    output$EarthText1 <- renderText({Earthtextfunc(input$EarthSelect,1)})
    
    output$EarthText2 <- renderText({Earthtextfunc(input$EarthSelect,2)})
    
    output$EarthText3 <- renderText({Earthtextfunc(input$EarthSelect,3)})
    
    
    observeEvent(input$homebutton1, {
      updateNavbarPage(session,"navbar",
                       selected = "Main")})
    
    
    
    #######################################################################################################################################
    #MONTE CARLO
    output$MonteIntro <- renderText("This project combined my skill in VBA and knowlegde in financial modeling. This was my first time working with a royalty-based capitalist. It was a project that incorporated what I learned in school into a real-life scenario.
                                    I used Monte Carlo simulations to look at different payback time windows and see how that would affect the Internal Rate of Return (IRR) in the long run.")
    
    output$MonteLink <- renderUI(tags$a(href="https://drive.google.com/file/d/1lOY45pN4oldj4BL7RlaJfpER5SQUb4Cj/view?usp=sharing",target = "_blank","Click here to view Full Model!"))
    
    observeEvent(input$homebutton3, {
      updateNavbarPage(session,"navbar",
                       selected = "Main")})
    
    
    #######################################################################################################################################
    #BOX OF TREASURES
    
    output$downloadresume <- downloadHandler(
      filename = "Tien Nguyen_Resume.pdf",
      content = function(file) {
        file.copy("Resume.pdf", file)
      })
    
    observeEvent(input$homebutton2, {
      updateNavbarPage(session,"navbar",
                       selected = "Main")})

    
}
# Run the application 
shinyApp(ui = ui, server = server)