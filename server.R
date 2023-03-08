#treatfin

library(shiny)
library(rms)
library(dplyr)
library(shinyvalidate)
library(shinyjs)


TREAT2.0_data_dir = "./Data"
MP = get(load(file=file.path(TREAT2.0_data_dir, "MP_matrix.rds")))
PMKS_vars = get(load(file=file.path(TREAT2.0_data_dir, "PMKS_vars.rds")))


data_preprocess_VW_v3 = function(data3, y.name, vars) {
  
  ## Make spline terms and interactions
  packs_knots =   c("25%"=25, "50%"=40, "75%"=60) 
  age_knots =     c("25%"=59, "50%"=66, "75%"=72) 
  size_knots =    c("25%"=15, "50%"=22, "75%"=34) 
  bmi_new_knots = c("25%"=23.5, "50%"=26.5, "75%"=30.7) 
  fev1_knots =    c("25%"=63, "50%"=77, "75%"=89.5) 
  
  model_form_rcs = vars
  model_form_rcs = gsub("packs", paste("rcs(packs,c(",paste(packs_knots,collapse=","),")",")",sep=""), model_form_rcs)
  model_form_rcs = gsub("age", paste("rcs(age,c(",paste(age_knots,collapse=","),")",")",sep=""), model_form_rcs)
  model_form_rcs = gsub("size", paste("rcs(size,c(",paste(size_knots,collapse=","),")",")",sep=""), model_form_rcs)
  model_form_rcs = gsub("bmi_new", paste("rcs(bmi_new,c(",paste(bmi_new_knots,collapse=","),")",")",sep=""), model_form_rcs)
  model_form_rcs = gsub("fev1", paste("rcs(fev1,c(",paste(fev1_knots,collapse=","),")",")",sep=""), model_form_rcs)
  model_form_rcs = paste(model_form_rcs, collapse=" + ")
  
  ## Specify up to 4-way interaction terms (all 2-way, 3-way, and 4-way interactions:
  model_form_rcs = formula(paste(y.name, " ~ (", model_form_rcs, ")^4", sep=""))
  
  ## Make all spline, categorical dummy, and interaction terms:
  X = as.data.frame(t(model.matrix(object=model_form_rcs, data=data3)[,-1]))
  # View(t(X[,1:50]))
  
  
  ## Function to re-name the rcs terms found in the column names of the expanded model matrix
  names = colnames(X)
  for(i in 1:length(names)) {
    t = names[i]
    inter = strsplit(t, ":")[[1]]
    for(j in 1:length(inter)) {
      ti = inter[j]
      if(length(grep("rcs",ti,v=T))>0) {
        
        if(length(grep("\\)\\)",ti,v=T))==0) {
          ti1 = gsub("rcs\\((.*?)\\).*", "\\1", ti)
          ti2 = strsplit(ti1, ", ")[[1]]
          rn1 = gsub("\\(.*?\\)",".", ti)
        }
        
        if(length(grep("\\)\\)",ti,v=T))>0) {
          ti1 = gsub("rcs\\((.*?\\))\\).*", "\\1", ti)
          ti2 = strsplit(ti1, ", ")[[1]]
          rn1 = gsub("\\(.*?\\)\\)",".", ti)
        }
        
        rn2 = ifelse(length(grep("'",rn1,v=T))==0, paste(rn1,".0",sep=""), rn1 %>% gsub("'''''", ".6", .) %>% gsub("'''''", ".5", .) %>% gsub("''''", ".4", .) %>% gsub("'''", ".3", .) %>% gsub("''", ".2", .) %>% gsub("'", ".1", .))
        inter[j] = rn2
      }
    }
    
    names[i] = paste(inter, collapse=":")
    
  }
  
  # View(data.frame(rbind(t(colnames(X)), t(names)))) # check function
  
  data_return = data.frame(cbind(data3[,y.name], X))
  colnames(data_return) = c(y.name, names)
  ########################
  
  return(data_return)
  
}


shinyServer(function(input, output, session) {
  
  iv <- InputValidator$new()
  iv$add_rule("age", sv_required(message = "Age must be provided"))
  iv$add_rule("gender", sv_required(message = "Gender must be provided"))
  iv$add_rule("packs", sv_required(message = "Pack Years must be provided"))
  iv$add_rule("size", sv_required(message = "Lesion Size must be provided"))
  iv$add_rule("upperlobe", sv_required(message = "Upper Lobe status must be provided"))
  iv$add_rule("group", sv_required(message = "Setting of evaluation must be provided"))
  iv$add_rule("height", sv_optional())
  iv$add_rule("weight", sv_optional())
  iv$add_rule("height", ~ if(is.na(input$weight)) "Weight must be provided with height")
  iv$add_rule("weight", ~ if(is.na(input$height)) "Height must be provided with height")
  
  observeEvent(input$resetAll,{
    
    updateNumericInput(session, "age", value = NA)
    updateNumericInput(session, "height", value = NA)
    updateNumericInput(session, "weight", value = NA)
    updateNumericInput(session, "packs", value = NA)
    updateNumericInput(session, "fev1", value = NA)
    updateNumericInput(session, "size", value = NA)
    
    
    updateRadioGroupButtons(session, "spicul", choices = c("Yes", "No", "Missing"), selected = "Missing")
    updateRadioGroupButtons(session, "prev_cancer", choices = c("Yes", "No", "Missing"), selected = "Missing")
    updateRadioGroupButtons(session, "anysympt", choices = c("Yes", "No", "Missing"), selected = "Missing")
    updateRadioGroupButtons(session, "petavid", choices= c("Yes", "No", "Missing"), selected = "Missing")
    updateRadioGroupButtons(session, "growthcat", choices = c("Growth Observed", "No Lesion Growth","Insufficient Data"), selected = "Insufficient Data")
    
    
    updateRadioGroupButtons(session, "gender", choices = c("Male", "Female"), selected = character(0))
    updateRadioGroupButtons(session, "upperlobe", choices = c("Yes", "No"),selected = character(0))
    updateRadioGroupButtons(session, "group", choices = c("Pulmonary Nodule Clinic", "Thoracic Surgery Clinic", "Other"), selected = character(0))
    
    
    output$result <- renderText({
      
    })

  })
  
  observeEvent(input$submit,{
    
    if(iv$is_valid()){
      
      iv$enable
      inputdata <- eventReactive(input$submit,{
        data <- data.frame(
          age = input$age,
          height = input$height,
          weight = input$weight,
          #bmi_new = input$bmi_new,
          gender = input$gender,
          packs = input$packs,
          size = input$size,
          spicul = input$spicul,
          upperlobe = input$upperlobe,
          growthcat = input$growthcat,
          prev_cancer = input$prev_cancer,
          fev1 = input$fev1,
          anysympt = input$anysympt,
          petavid = input$petavid,
          group = input$group
        )
        data
      })
      
      output$result <- renderText({
        
        data = inputdata()
        
        pred.vars = c("Age" = "age", "BMI" = "bmi_new", "Gender" = "gender", "Pack Years" = "packs", "Lesion Size (mm)" = "size", "Spiculated Lesion Edge" = "spicul", "Upper Lobe?" = "upperlobe", "Lesion Growth" = "growthcat", "Previous Cancer?" = "prev_cancer", "Predicted FEV" = "fev1", "Pre-Op Symptoms?" = "anysympt", "FDG-PET Avid?" = "petavid", "Setting of Evaluation" = "group")
        pred.vars = sort(pred.vars)
        
        ## Growth variable - treat insufficient data as missing
        
        if(is.na(data$height)){
          data1.bmi_new = NA
          data$bmi_new = NA
        } else{
          data1.bmi_new = (data$weight / (data$height ^ 2)) * 703
          data$bmi_new = data1.bmi_new
        }
        
        if(data$growthcat=="Growth Observed"){
          data1.growthcat = 1
        } else if(data$growthcat=="No Lesion Growth"){
          data1.growthcat = 0
        } else{
          data1.growthcat = NA
        }
        data1.growthcat = as.numeric(data1.growthcat)
        data1.growthcat = factor(data1.growthcat, levels=c(0,1), 
                                 labels=c("No","Yes"))
        
        ## Format other categorical inputs
        if(data$gender=="Male"){
          data1.gender = 1
        } else if(data$gender=="Female"){
          data1.gender = 0
        } else{
          data1.gender = NA
        }
        data1.gender = factor(data1.gender, levels=c(0,1), 
                              labels=c("F","M"))
        

        if(data$spicul=="Yes"){
          data1.spicul = 1
        } else if(data$spicul=="No"){
          data1.spicul = 0
        } else{
          data1.spicul = NA
        }
        

        if(data$upperlobe == "Yes"){
          data1.upperlobe = 1
        } else if(data$upperlobe == "No"){
          data1.upperlobe = 0
        } else{
          data1.upperlobe = NA
        }
        

        if(data$prev_cancer == "Yes"){
          data1.prev_cancer = 1
        } else if(data$prev_cancer == "No"){
          data1.prev_cancer = 0
        } else{
          data1.prev_cancer = NA
        }
        

        if(data$anysympt == "Yes"){
          data1.anysympt = 1
        } else if(data$anysympt == "No"){
          data1.anysympt = 0
        } else{
          data1.anysympt = NA
        }
        

        if(data$petavid == "Yes"){
          data1.petavid = 1
        } else if(data$petavid == "No"){
          data1.petavid = 0
        } else{
          data1.petavid = NA
        }
        

        if(data$group == "Pulmonary Nodule Clinic"){
          data1.group = 1
        } else if(data$group=="Thoracic Surgery Clinic"){
          data1.group = 2
        } else if(data$group=="Surgical Resection"){
          data1.group = 3
        } else{
          data1.group = NA
        }
        data1.group = factor(data1.group, levels=c(1,2,3), 
                             labels=c("Pulm","Thoracic","Surgery"))
        
        ## Format numerical inputs if none given (still at 0 default value)
        
        data1.age = ifelse(data$age==0, NA, data$age)
        #data1.bmi_new = ifelse(data$bmi_new==0, NA, data$bmi_new)  
        data1.packs = ifelse(data$packs==0, NA, data$packs)  
        data1.size = ifelse(data$size==0, NA, data$size)  
        data1.fev1 = ifelse(data$fev1==0, NA, data$fev1)
        
        ## Combine into single data frame
        data1 = data.frame(
          "age" = data1.age,
          "bmi_new" = data1.bmi_new,
          "gender" = data1.gender,
          "packs" = data1.packs,
          "size" = data1.size,
          "spicul" = data1.spicul,
          "upperlobe" = data1.upperlobe,
          "growthcat" = data1.growthcat,
          "prev_cancer" = data1.prev_cancer,
          "fev1" = data1.fev1,
          "anysympt" = data1.anysympt,
          "petavid" = data1.petavid,
          "group" = data1.group)
        
        
        data1 = data1[pred.vars]  # re-order to match saved results
        NtMiss.Vars = pred.vars[which(!is.na(data1))]
        
        PMKS_vars_tmp_idx = which(lengths(PMKS_vars) == length(NtMiss.Vars))
        pattern = names(which(sapply( PMKS_vars_tmp_idx, function(j) length(setdiff(NtMiss.Vars, PMKS_vars[[j]])) ) == 0))
        which_fit = which(names(PMKS_vars) == pattern)
        
        fit = get(load(file.path(TREAT2.0_data_dir, paste("PMKS_treat2.0_fits_trimmed/PMKS_treat2.0_fits_",rownames(MP[which_fit,]),"_trimmed.rds",sep=""))))
        
        data = data[,NtMiss.Vars]
        
        data1.processed = data_preprocess_VW_v3(data=cbind("y"=1,data1),y.name="y",vars=NtMiss.Vars)
        data1.processed$y = NA
        
        expit = function(z){exp(z)/(1+exp(z))}
        lev = qt(1-0.05/2, fit$df.residual)  # qnorm(1-0.05/2)
        
        pred.val = predict(fit, newdata=data1.processed, type="link", se.fit=TRUE)
        
        prob = expit(pred.val$fit)
        prob.lowerbound = expit(pred.val$fit - lev*pred.val$se.fit)
        prob.upperbound = expit(pred.val$fit + lev*pred.val$se.fit)
        
        
        #draw table
        resultText = paste0(round(prob*100, 1), "%")
        resultText
        
      })
      
      
    } else {
      
      iv$enable()
      showNotification(
        "Incomplete data has been provided. Please correct the errors in the form and try again.",
        id = "submit_message", type = "error"
      )
      
    }
    
  })


  
})














