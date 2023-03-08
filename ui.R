#treatfin

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(flexdashboard)


shinyUI(
  
  fluidPage(theme = shinytheme("flatly"),
    #shinyjs::useShinyjs(),
    # Application title
    titlePanel(
      windowTitle = "TREAT 2.0 Calculator",
      title = tags$head(tags$link(rel="icon",
                                  href="data:image/x-icon;base64,AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAgAAAAAAAAAAAAAAAEAAAAAAAAADy8vIAAAAAAOakKQDEiRsA////ANnZ2QAAAP8AzJEjAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAF3IiIiIiERE3IiIiIiIRETciIiIiIhERNyIiRCIiERE3IiJEIiIRETciREREIhERNyJEREQiERE3IiJEIiIRETciIkQiIhERNyIiIiIiERE3IiIiIiIRETciIiIiIhERNWZmAAARERETZmZ3d3ERERFmZhEREREREWZmERERERGADwAAAA8AAAAPAAAADwAAAA8AAAAPAAAADwAAAA8AAAAPAAAADwAAAA8AAAAPAAAAPwAAgB8AAMP/AADD/wAA",
                                  type="image/x-icon"))
    ),
    
    
    h1("TREAT 2.0", align="center"),
    h3("Lung Cancer Prediction in High-Risk Indeterminate Pulmonary Nodules", align="center"),
    p("Developed by MASLab at Vanderbilt University Medical Center", align="center"),
    
    
    wellPanel(fluidRow(
      
      column(3,),
      
      column(6,
             
             div(id = "mainApp",
             strong("Instructions: "),
             p("Input available patient information to genrate a probability of malignancy"),
             numericInput("age", "Age (Required)", value = NA, min = 0),
             splitLayout(
               numericInput("height", "Height (In)", value = NA, min = 0),
               numericInput("weight", "Weight (lb)", value = NA, min = 0)
             ),
             #numericInput("bmi_new", "BMI", value = NA, min = 0),
             numericInput("packs", "Pack Years (Required)", value = NA, min = 0),
             numericInput("fev1", "Predicted FEV%", value = NA, min = 0),
             numericInput("size", "Lesion Size (mm) (Required)", value = NA),
             
             radioGroupButtons("gender", "Sex (Required)", justified = TRUE, choices = c("Male", "Female"), selected = character(0)),
             radioGroupButtons("upperlobe", "Upper Lobe? (Required)", justified = TRUE, choices = c("Yes", "No"),selected = character(0)),
             radioGroupButtons("group", "Setting of Evaluation (Required)", justified = TRUE ,choices = c("Pulmonary Nodule Clinic", "Thoracic Surgery Clinic", "Other"), selected = character(0)),
            
             radioGroupButtons("spicul", "Spiculated Lesion Edge?", justified = TRUE, choices = c("Yes", "No", "Missing"), selected = "Missing"),
             radioGroupButtons("prev_cancer", "Previous Cancer?", justified = TRUE, choices = c("Yes", "No", "Missing"), selected = "Missing"),
             radioGroupButtons("anysympt", "Pre-Op Symptoms?", justified = TRUE, choices = c("Yes", "No", "Missing"), selected = "Missing"),
             radioGroupButtons("petavid", "FDG-PET Avid?", justified = TRUE, choices= c("Yes", "No", "Missing"), selected = "Missing"),
             radioGroupButtons("growthcat", "Lesion Growth", justified = TRUE, choices = c("Growth Observed", "No Lesion Growth","Insufficient Data"), selected = "Insufficient Data"),
             
             actionButton("submit", "Submit", class = "btn-primary"),
             actionButton("resetAll", "Reset"),
             h1("Probability of Malignancy: ", textOutput("result", inline = TRUE), align = "center"),
             gaugeOutput("gauge")
             )
             
      ),
      
      column(3,)

    ))


))
