#treatfin

library(shiny)
library(shinythemes)
library(shinyWidgets)


shinyUI(fluidPage(theme = shinytheme("flatly"),

    # Application title
    titlePanel(h1("TREAT 2.0", align="center")),
    p("Developed by MASLab at Vanderbilt University Medical Center", align="center"),
    h3("Lung Cancer Prediction in High-Risk Indeterminate Pulmonary Nodules", align="center"),
    h5("Instructions", align="center"),
    p("Input available patient information to genrate a probability of malignancy", align="center"),
    
    wellPanel(fluidRow(
      
      column(3,),
      
      column(6,
             numericInput("age", "Age (Required)", value = NA, min = 0),
             numericInput("bmi_new", "BMI", value = NA, min = 0),
             numericInput("packs", "Pack Years (Required)", value = NA, min = 0),
             numericInput("fev1", "Predicted FEV%", value = NA, min = 0),
             numericInput("size", "Lesion Size (mm) (Required)", value = NA),
             
             radioGroupButtons("gender", "Sex (Required)", justified = TRUE, choices = c("Male", "Female"), selected = character(0)),
             radioGroupButtons("upperlobe", "Upper Lobe? (Required)", justified = TRUE, choices = c("Yes", "No"),selected = character(0)),
             radioGroupButtons("group", "Setting of Evaluation", justified = TRUE ,choices = c("Pulmonary Nodule Clinic", "Thoracic Surgery Clinic"), selected = character(0)),
            
             radioGroupButtons("spicul", "Spiculated Lesion Edge?", justified = TRUE, choices = c("Yes", "No", "Missing"), selected = "Missing"),
             radioGroupButtons("prev_cancer", "Previous Cancer?", justified = TRUE, choices = c("Yes", "No", "Missing"), selected = "Missing"),
             radioGroupButtons("anysympt", "Pre-Op Symptoms?", justified = TRUE, choices = c("Yes", "No", "Missing"), selected = "Missing"),
             radioGroupButtons("petavid", "FDG-PET Avid?", justified = TRUE, choices= c("Yes", "No", "Missing"), selected = "Missing"),
             radioGroupButtons("growthcat", "Lesion Growth", justified = TRUE, choices = c("Growth Observed", "No Lesion Growth","Insufficient Data"), selected = "Insufficient Data"),
             
             actionButton("submit", "Submit", class = "btn-primary"),
             actionButton("resetAll", "Reset"),
             h1("Probability of Malignancy: ", textOutput("result", inline = TRUE), align = "center")
      ),
      
      column(3,)

    ))


))
