#treatfin

library(shiny)
library(rms)
library(dplyr)
library(shinyvalidate)
#library(shinyjs)
library(flexdashboard)


TREAT2.0_data_dir = "./Data"
MP = get(load(file=file.path(TREAT2.0_data_dir, "MP_matrix.rds")))
PMKS_vars = get(load(file=file.path(TREAT2.0_data_dir, "PMKS_vars.rds")))


Reorder_Interaction_Terms = function(names) {
  for(i in 1:length(names)) {
    if(length(grep(":", names[i], v=T))==1) {
      names[i] = paste(sort(strsplit(names[i], ":")[[1]]), collapse=":")
    }
  }
  return(names)
}


data_preprocess_VW_v3 = function(data3, y.name, vars) {
  ## Make spline terms and interactions
  packs_knots =   c("25%"=20, "50%"=40, "75%"=60)
  age_knots =     c("25%"=59, "50%"=66, "75%"=72)
  size_knots =    c("25%"=14, "50%"=20, "75%"=32)
  bmi_new_knots = c("25%"=23.479312895, "50%"=26.5, "75%"=30.712934115)
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
    
    output$gauge = renderGauge({

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
      
      fit = get(load(file.path(TREAT2.0_data_dir, paste("PMKS_treat2.0.sa.min_fits_trimmed/PMKS_treat2.0.sa.min_fits_fitcv5_",rownames(MP[which_fit,]),"_trimmed.rds",sep=""))))
      
      #data1 = data1[,NtMiss.Vars]
      data1.processed = data_preprocess_VW_v3(data=cbind("y"=1,data1),y.name="y",vars=NtMiss.Vars)
      data1.processed$y = NA
      
      expit = function(z){exp(z)/(1+exp(z))}
      lev = qt(1-0.05/2, fit$df.residual)  # qnorm(1-0.05/2)
      
      pred.val = predict(fit, newdata=data1.processed, type="link", se.fit=TRUE)
      prob = expit(pred.val$fit)
      if(pred.val$fit>=700) {prob = 1}
      
      
      data1.alt = dplyr::slice(data1, rep(1:n(), each=18))
      data1.alt[,"Var"] = c(rep("age",2), "anysympt", rep("bmi_new",2), rep("fev1",2), "gender", "group", "growthcat", rep("packs",2), "petavid", "prev_cancer", rep("size",2), "spicul", "upperlobe")
      
      
      age.incr = 5       # ! TO DO: final choice of value
      bmi_new.incr = 5   # ! TO DO: final choice of value
      fev1.incr = 10     # ! TO DO: final choice of value
      packs.incr = 10    # ! TO DO: final choice of value
      size.incr = 5      # ! TO DO: final choice of value
      
      
      # age
      idx = which(data1.alt[,"Var"]=="age")
      if(!is.na(data1$age)) {
        data1.alt[idx,"age"] = data1.alt[idx,"age"] + age.incr*c(1,-1)
      } else {
        data1.alt[idx,] = NA
      }
      
      # anysympt
      idx = which(data1.alt[,"Var"]=="anysympt")
      if(!is.na(data1$anysympt)) {
        if(data1$anysympt==1) {data1.alt[idx,"anysympt"]=0}
        if(data1$anysympt==0) {data1.alt[idx,"anysympt"]=1}
      } else {
        data1.alt[idx,] = NA
      }
      
      # bmi_new
      idx = which(data1.alt[,"Var"]=="bmi_new")
      if(!is.na(data1$bmi_new)) {
        data1.alt[idx,"bmi_new"] = data1.alt[idx,"bmi_new"] + bmi_new.incr*c(1,-1)
      } else {
        data1.alt[idx,] = NA
      }
      
      # fev1
      idx = which(data1.alt[,"Var"]=="fev1")
      if(!is.na(data1$fev1)) {
        data1.alt[idx,"fev1"] = data1.alt[idx,"fev1"] + fev1.incr*c(1,-1)
      } else {
        data1.alt[idx,] = NA
      }
      
      idx = which(data1.alt[,"Var"]=="gender")
      if(!is.na(data1$gender)) {
        if(data1$gender=="M") {data1.alt[idx,"gender"]="F"}
        if(data1$gender=="F") {data1.alt[idx,"gender"]="M"}
      } else {
        data1.alt[idx,] = NA
      }
      
      idx = which(data1.alt[,"Var"]=="group")
      if(!is.na(data1$group)) {
        if(data1$group=="Pulm") {data1.alt[idx,"group"]="Thoracic"}
        if(data1$group=="Thoracic") {data1.alt[idx,"group"]="Pulm"}
      } else {
        data1.alt[idx,] = NA
      }
      
      # growthcat
      idx = which(data1.alt[,"Var"]=="growthcat")
      if(!is.na(data1$growthcat)) {
        if(data1$growthcat=="Yes") {data1.alt[idx,"growthcat"]="No"}
        if(data1$growthcat=="No") {data1.alt[idx,"growthcat"]="Yes"}
      } else {
        data1.alt[idx,] = NA
      }
      
      # packs
      idx = which(data1.alt[,"Var"]=="packs")
      if(!is.na(data1$packs)) {
        data1.alt[idx,"packs"] = data1.alt[idx,"packs"] + packs.incr*c(1,-1)
      } else {
        data1.alt[idx,] = NA
      }
      
      # petavid
      idx = which(data1.alt[,"Var"]=="petavid")
      if(!is.na(data1$petavid)) {
        if(data1$petavid==1) {data1.alt[idx,"petavid"]=0}
        if(data1$petavid==0) {data1.alt[idx,"petavid"]=1}
      } else {
        data1.alt[idx,] = NA
      }
      
      # prev_cancer
      idx = which(data1.alt[,"Var"]=="prev_cancer")
      if(!is.na(data1$prev_cancer)) {
        if(data1$prev_cancer==1) {data1.alt[idx,"prev_cancer"]=0}
        if(data1$prev_cancer==0) {data1.alt[idx,"prev_cancer"]=1}
      } else {
        data1.alt[idx,] = NA
      }
      
      # size
      idx = which(data1.alt[,"Var"]=="size")
      if(!is.na(data1$size)) {
        data1.alt[idx,"size"] = data1.alt[idx,"size"] + size.incr*c(1,-1)
      } else {
        data1.alt[idx,] = NA
      }
      
      # spicul
      idx = which(data1.alt[,"Var"]=="spicul")
      if(!is.na(data1$spicul)) {
        if(data1$spicul==1) {data1.alt[idx,"spicul"]=0}
        if(data1$spicul==0) {data1.alt[idx,"spicul"]=1}
      } else {
        data1.alt[idx,] = NA
      }
      
      # upperlobe
      idx = which(data1.alt[,"Var"]=="upperlobe")
      if(!is.na(data1$upperlobe)) {
        if(data1$upperlobe==1) {data1.alt[idx,"upperlobe"]=0}
        if(data1$upperlobe==0) {data1.alt[idx,"upperlobe"]=1}
      } else {
        data1.alt[idx,] = NA
      }
      
      data1.alt = data1.alt[,NtMiss.Vars]
      data1.alt = na.omit(data1.alt)
      
      View(data1.alt)
      
      data1.alt.processed = data_preprocess_VW_v3(data=cbind("y"=1,data1.alt[1,]),y.name="y",vars=NtMiss.Vars)
      
      
      for (x in 2:nrow(data1.alt)){
        data1.alt.pro = data_preprocess_VW_v3(data=cbind("y"=1,data1.alt[x,]),y.name="y",vars=NtMiss.Vars)
        data1.alt.processed = rbind(data1.alt.processed, data1.alt.pro)
      }
      
      data1.alt.processed$y = NA

      prob.alt = sapply(1:nrow(data1.alt.processed), function(i) {
        logitp = predict(fit, newdata=data1.alt.processed[i,], type="link", se.fit=TRUE)$fit
        p = expit(logitp)
        if(logitp>=700) {p = 1}
        return(p)
      })
      
      diff.alt =  prob.alt - prob
      res.alt = data.frame(data1.alt, "prob.orig"=prob, "prob.new"=prob.alt, "diff"=diff.alt, "abs.diff"=abs(diff.alt), row.names=NULL)
      
      coef.cutoff = 7.773174
      data1.processed = na.omit(data1.processed)
      
      if(any(abs(fit$coefficients) > coef.cutoff)) {
        LGCOEF = TRUE
        which_lg = names(which(fit$coefficients > coef.cutoff))
        colnames(data1.processed) = Reorder_Interaction_Terms(colnames(data1.processed))
        x_lg = data1.processed[,which_lg]
        
        if(any(x_lg != 0)) {LGCOEF_USED = TRUE 
        } else {LGCOEF_USED = FALSE}
        
      } else {
        LGCOEF = FALSE; LGCOEF_USED = FALSE
      }
      
      # warn.cutoff = 0.5
      # block.cutoff = 0.8
      
      warn.cutoff = rep(NA, 13)
      names(warn.cutoff) = pred.vars
      
      
      warn.cutoff["age"] = 0.25           # ! TO DO: final choice of value
      warn.cutoff["anysympt"] = 0.3      # ! TO DO: final choice of value  
      warn.cutoff["bmi_new"] = 0.25       # ! TO DO: final choice of value
      warn.cutoff["fev1"] = 0.25          # ! TO DO: final choice of value
      warn.cutoff["gender"] = 0.25        # ! TO DO: final choice of value
      warn.cutoff["group"] = 0.6         # ! TO DO: final choice of value
      warn.cutoff["growthcat"] = 0.6     # ! TO DO: final choice of value
      warn.cutoff["packs"] = 0.25         # ! TO DO: final choice of value
      warn.cutoff["petavid"] = 0.3       # ! TO DO: final choice of value
      warn.cutoff["prev_cancer"] = 0.25   # ! TO DO: final choice of value
      warn.cutoff["size"] = 0.4          # ! TO DO: final choice of value
      warn.cutoff["spicul"] = 0.3        # ! TO DO: final choice of value
      warn.cutoff["upperlobe"] = 0.25     # ! TO DO: final choice of value
      
      block.cutoff = rep(NA, 13)
      names(block.cutoff) = pred.vars
      
      block.cutoff["age"] = 0.4           # ! TO DO: final choice of value
      block.cutoff["anysympt"] = 0.5      # ! TO DO: final choice of value
      block.cutoff["bmi_new"] = 0.4       # ! TO DO: final choice of value
      block.cutoff["fev1"] = 0.4          # ! TO DO: final choice of value
      block.cutoff["gender"] = 0.4        # ! TO DO: final choice of value
      block.cutoff["group"] = 0.75         # ! TO DO: final choice of value
      block.cutoff["growthcat"] = 0.75     # ! TO DO: final choice of value
      block.cutoff["packs"] = 0.4         # ! TO DO: final choice of value
      block.cutoff["petavid"] = 0.5       # ! TO DO: final choice of value
      block.cutoff["prev_cancer"] = 0.4   # ! TO DO: final choice of value
      block.cutoff["size"] = 0.7          # ! TO DO: final choice of value
      block.cutoff["spicul"] = 0.5        # ! TO DO: final choice of value
      block.cutoff["upperlobe"] = 0.4     # ! TO DO: final choice of value
      
      
      BLOCK.a = (LGCOEF_USED == TRUE)
      
      
      # WARN.a = (any(res.alt$abs.diff>=warn.cutoff) == TRUE)
      WARN.a = (any(res.alt$abs.diff >= warn.cutoff[res.alt$Var]) == TRUE)
      
      # BLOCK.b = (any(res.alt$abs.diff>=block.cutoff) == TRUE)
      BLOCK.b = (any(res.alt$abs.diff >= block.cutoff[res.alt$Var]) == TRUE)
      
      which.WARN.a = res.alt$Var[which(res.alt$abs.diff >= warn.cutoff[res.alt$Var])]
      which.BLOCK.b = res.alt$Var[which(res.alt$abs.diff >= block.cutoff[res.alt$Var])]
      
      
      BLOCK.1 = (prob>=0.999)    # ! TO DO: final choice of value
      BLOCK.0 = (prob<=0.001)    # ! TO DO: final choice of value
      
      if(BLOCK.1==TRUE){
        output$result <- renderText({
          #draw table
          resultText = paste0("\n", "\n","Warning: No probability of cancer estimated due to insufficient data in the training population and possible model instability")
          resultText
          
        })
        output$gauge = renderGauge({})
      } else if(BLOCK.0==TRUE){
        output$result <- renderText({
          #draw table
          resultText = paste0("\n", "\n","Warning: No probability of cancer estimated due to insufficient data in the training population and possible model instability")
          resultText
        })
        output$gauge = renderGauge({})
      } else if(BLOCK.a==TRUE){
        output$result <- renderText({
          #draw table
          resultText = paste0("\n", "\n","Warning: No probability of cancer estimated due to insufficient data in the training population and possible model instability")
          resultText
        })
        output$gauge = renderGauge({})
      } else if(BLOCK.b==TRUE){
        output$result <- renderText({
          #draw table
          resultText = paste0("\n", "\n","Warning: No probability of cancer estimated due to insufficient data in the training population and possible model instability")
          resultText
        })
        output$gauge = renderGauge({})
      } else if(WARN.a==TRUE){
        output$result <- renderText({
          #draw table
          resultText = paste0(round(prob*100, 1), "%", "\n", "Warning: Limited data in the training population may result in model instability or an inaccurate risk estimate. Defer to clinical judgement.")
          resultText
          
        })
        output$gauge = renderGauge({
          gauge(as.numeric(prob*100), 
                min = 0, 
                max = 100,
                symbol = "%",
                sectors = gaugeSectors(success = c(0, 10), 
                                       warning = c(10, 70),
                                       danger = c(70, 100)))
        })
      } else{
        output$result <- renderText({
          #draw table
          resultText = paste0(round(prob*100, 1), "%")
          resultText
          
        })
        output$gauge = renderGauge({
          gauge(as.numeric(prob*100), 
                min = 0, 
                max = 100,
                symbol = "%",
                sectors = gaugeSectors(success = c(0, 10), 
                                       warning = c(10, 70),
                                       danger = c(70, 100)))
        })
      }

      
      
    } else {
      
      iv$enable()
      showNotification(
        "Incomplete data has been provided. Please correct the errors in the form and try again.",
        id = "submit_message", type = "error"
      )
      
    }
    
  })


  
})














