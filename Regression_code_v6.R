# poi_reg_objects <- c()
# Poi_pVals <- c()
# Poi_dispersion <- c()
# quasipoi_pVals <- c()
# quasipoi_dispersion <- c()
# negBin_pVals <- c()
# negBin_dispersion <- c()

#Rank glm coefficients for poisson, qausipoisson and negative binomial
rank_glm_by_coeff <- function(glm_reg,dataframe,def_name,Step_name,negBin){
  # browser()
  coeffs <- coef(summary(glm_reg))
  exp_coeffs <- exp(coeffs[,1])
  eqp_def <- c(exp_coeffs[1]) #this is the reference variable
  for(i in (2:(length(coef(summary(glm_reg))[,1])))){
    eqp_def <- c(eqp_def,eqp_def[1]*exp_coeffs[i]) #appending average num of defects in the count case (anallogous zero truncated part)
  }
  eqp_rank <- rank(eqp_def)
  return(eqp_rank) #because the equipment giving least number of avg defects is ranked highest, i.e. rank 1
}

#Rank by zero-truncated coeffecients of logistic regression
rank_non_glm_by_coeff <- function(full_coeffs,dataframe,def_name,Step_name){
  eqp_def_count <- c(exp(full_coeffs[1,1])) #thisis the reference variable
  for(i in 2:length(full_coeffs[,1])){
    #The equation below Gives the change in the expected number of defects by a multiplicative
    #factor of exp(coeff) for an equipment over the reference equipment's expected number of defects
    eqp_def_count[(length(eqp_def_count)+1)] <- eqp_def_count[1]*exp(full_coeffs[i,1])
  }
  # browser()
  #------------------------------------------------------------------------------------#
  #--the below commented portion is performed directly in the switch case for ranking--#
  # unique_eqps <- as.character(sort(unique(dataframe[[Step_name]])))
  # def_considered <- as.list(rep(def_name,length(unique_eqps)))
  # eqp_rank <- cbind(def_considered,unique_eqps,rank(eqp_def_count))
  #------------------------------------------------------------------------------------#
  eqp_rank <- rank(eqp_def_count)
  return(eqp_rank) #because the equipment giving least number of avg defects is ranked highest, i.e. rank 1
}

#Rank hurdle model coefficients
rank_hurdle_by_coeff <- function(h_reg,dataframe,def_name,Step_name){
  # browser()
  n<-length(coef(summary(h_reg))$count[,1]) 
  # theta <- exp(coef(summary(h_reg))$count[n,1])
  coeffs_count <- coef(summary(h_reg))$count[1:(n-1),] #we exclude the last coefficient because its log theta
  coeffs_zero <- coef(summary(h_reg))$zero[1:(n-1),]
  exp_coeffs_count <- exp(coeffs_count[,1])
  exp_coeffs_zero <- exp(coeffs_zero[,1])
  eqp_zero_trunc_def_count <- c(exp_coeffs_count[1]) #this is the reference variable
  eqp_binary_def_probab <- c(exp_coeffs_zero[1]/1+exp_coeffs_zero[1])
  for(i in (2:(n-1))){
    # browser()
    eqp_zero_trunc_def_count <- c(eqp_zero_trunc_def_count,eqp_zero_trunc_def_count[1]*exp_coeffs_count[i]) #appending average num of defects in the count case (anallogous zero truncated part)
    eqp_binary_def_probab <- c(eqp_binary_def_probab,((exp_coeffs_zero[1]*exp_coeffs_zero[i])/1+(exp_coeffs_zero[1]*exp_coeffs_zero[i]))) #directly append probability from odds ratio
  }
  # browser()
  eqp_binary_def_probab_normalized <- eqp_binary_def_probab/sum(eqp_binary_def_probab)
  expected_def_count <- eqp_zero_trunc_def_count*eqp_binary_def_probab
  eqp_rank <- rank(expected_def_count)
  return(eqp_rank) #because the equipment giving least number of avg defects is ranked highest, i.e. rank 1
}

#1. Poisson Analysis
poi_regression <- function(dataframe,def,Step,get_coeffs,def_name,step_name,coeff_fileName) {
  preg <- glm( def ~  Step, data = dataframe, family=poisson(link=log))
  pvalue <- 1 - pchisq(summary(preg)$deviance,summary(preg)$df.residual)
  dispersion_stat <- preg$deviance/preg$df.residual
  aic_statistic <- AIC(preg)
  
  #Obtaining coefficients
  eqp_rank <- " "
  if(get_coeffs){
    eqp_rank <- rank_glm_by_coeff(preg,dataframe,def_name,step_name,FALSE)
  }
  else{
    eqp_rank <- "not asked yet"
  }
  
  return(c(pvalue,dispersion_stat,aic_statistic,eqp_rank))
}

#2. Quasiquasipoisson Analysis
quasipoi_regression <- function(dataframe,def,Step,get_coeffs,def_name,step_name,coeff_fileName) {
  qpreg <- glm( def ~  Step, data = dataframe, family=quasipoisson(link=log))
  # quasipoi_pVals[length(quasipoi_pVals)+1] <- 
  pvalue <- 1 - pchisq(summary(qpreg)$deviance,summary(qpreg)$df.residual)
  # quasipoi_dispersion[length(quasipoi_dispersion)+1] <- 
  dispersion_stat <- qpreg$deviance/qpreg$df.residual
  #browser()
  
  #Obtaining coefficients
  eqp_rank <- " "
  if(get_coeffs){
    eqp_rank <- rank_glm_by_coeff(qpreg,dataframe,def_name,step_name,FALSE)
  }
  else{
    eqp_rank <- "not asked yet"
  }
  
  
  #aic_statistic <- AIC(qpreg)
  #there is no log likelihood for quasi* family models, thus no AIC, which is why they are not calculated/printed in the glm() summary outputs.
  #See paper on link: https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf
  #So we return 1e7 for AIC so that while calculating the best model, this model does not reflect the minimum AIC value
  return(c(pvalue,dispersion_stat,1e7,eqp_rank))
}

#negBins Analysis
negBin_regression <- function(dataframe,def,Step,get_coeffs,def_name,step_name,coeff_fileName) {
  # browser()
  nbreg <- glm.nb( def ~  Step, data = dataframe )
  # negBin_pVals[length(negBin_pVals)+1]  <- 
  pvalue <- 1 - pchisq(summary(nbreg)$deviance,summary(nbreg)$df.residual)
  # negBin_dispersion[length(negBin_dispersion)+1] <- 
  dispersion_stat <- nbreg$deviance/nbreg$df.residual
  aic_statistic <- AIC(nbreg)
  
  #Obtaining coefficients
  eqp_rank <- " "
  if(get_coeffs){
    # browser()
    eqp_rank <- rank_glm_by_coeff(nbreg,dataframe,def_name,step_name,TRUE)
  }
  else{
    eqp_rank <- "not asked yet"
  }
  
  return(c(pvalue,dispersion_stat,aic_statistic,eqp_rank))
}

#4. Zero-truncated Poisson
# zero_trunc_poi <- function(dataframe,def_name,def,Step_name,Step) {
zero_trunc_poi <- function(dataframe,def_name,Step_name,get_coeffs,coeff_fileName) {
  # library(VGAM)
  # browser()
  data_pos_def <- dataframe[which(dataframe[[def_name]]>0),]
  #pos_def <- def[which(def>0)]
  #model_status <- "working"
  
  ztp_reg <- tryCatch(vglm(data_pos_def[[def_name]] ~  data_pos_def[[Step_name]], data = data_pos_def, family = pospoisson()), error = function(e){model_status <- "fail"})
  # ztp_reg <- tryCatch(vglm(data_pos_def[[def_name]] ~  data_pos_def[[Step_name]], data = data_pos_def, family = pospoisson()), error = function(e){return(vglm(data_pos_def[[def_name]] ~  data_pos_def[[Step_name]], data = data_pos_def, family = pospoisson(),crit="coef",trace=TRUE,maxit=2))})
  
  ## try(aic_stat <- AIC(ztp_reg))
  ##summary(ztp_reg)
  # if(ztp_reg == "fail"){
  #   aic_statistic <- 10e6 #if aic == 10e6 for a model then that model has failed
  # }
  # else{
  #   tryCatch(aic_statistic <- AIC(ztp_reg) , error = function(e){browser()})
  # }
  # return(c(aic_statistic))
  #browser()
  
  AIC_ztp = tryCatch(AIC(ztp_reg), error = function(e){return (1e7)})
  # AIC_ztnb = tryCatch(AIC(ztnb_reg), error = function(e){browser()})
  # browser()
  h_pb_reg <- " "
  eqp_rank <- " "
  coeffs <- " "
  exp_coeffs <- " "
  full_coeffs <- " "
  
  #if(typeof(ztnb_reg) == "character"){
  if(AIC_ztp == 1e7){
    #Part I - coefficients
    h_pb_reg  <- hurdle(dataframe[[def_name]] ~  dataframe[[Step_name]], data = dataframe, dist="poisson")
    coeffs <- coef(summary(h_pb_reg))$count
    #Part II - Binary model and AIC from hurdle
    unary_def_data <- data_pos_def
    unary_def_data[[def_name]] <- 1
    data_zero_def <- dataframe[which(dataframe[[def_name]]==0),]
    binary_def_data <- rbind(unary_def_data,data_zero_def)
    #browser()
    bernoulli_reg <- glm( binary_def_data[[def_name]] ~  binary_def_data[[Step_name]], data = binary_def_data, family=binomial(link=logit))
    AIC_binary = AIC(bernoulli_reg)
    AIC_hurdle = AIC(h_p_reg)
    AIC_ztp = (AIC_hurdle - AIC_binary)*length(dataframe[,1])/length(data_pos_def[,1])
    # browser()
  }
  # else{
  #   AIC_ztp = AIC(ztp_reg)
  # }
  
  #Obtaining coefficients
  if(get_coeffs){
    if(AIC_ztp == 1e7){
      # browser()
      n<-length(coef(summary(h_p_reg))$count[,1])
      coeffs <- coef(summary(h_p_reg))$count[1:(n-1),] #we exclude the last coefficient because its log theta
      exp_coeffs <- exp(coef(summary(h_p_reg))$count[1:(n-1),1])
    }
    else{
      # browser()
      coeffs <- coef(summary(ztp_reg))
      exp_coeffs <- exp(coef(ztp_reg))
    } 
    full_coeffs <- cbind(coeffs,exp_coeffs,sign(coeffs[,1]))
    coeff_data <- cbind(full_coeffs[,c(1.4,5,6)])
    #-------------------------------------------------------------------------------------------##
    ## The following commented part of code was used when the coefficients obtained form zero-truncated
    ## negative binomial regression had two intercepts, where the second intercept was the standard
    ## error of what though? Of the model?
    ##------------------------------------------
    # coeff_data <- rbind(full_coeffs[1,c(1,4,5,6)],full_coeffs[3:length(coeffs[,1]),c(1,4,5,6)])
    # base_incidence_rate <- coeff_data[1,3]*coeff_data[1,4]
    # incidence_rate <- c(base_incidence_rate)
    # incidence_delta_rate <- c(base_incidence_rate)
    # for(i in 2:length(coeff_data[,1])){
    #   incidence_delta_rate[(length(incidence_delta_rate)+1)] <- coeff_data[i,3]*coeff_data[i,4]
    #   incidence_rate[(length(incidence_rate)+1)] <- incidence_rate[1]+incidence_delta_rate[i]
    #   #browser()
    # }
    ##-------------------------------------------------------------------------------------------##
    eqp_rank <- rank_non_glm_by_coeff(coeff_data,dataframe,def_name,Step_name)
    #eqp_rank <- rank(incidence_rate)
  }
  else{
    eqp_rank <- "not asked yet"
  }
  
  if(AIC_ztp == 1e7){
    browser()
    return(c("fail","fail",1e7,"fail")) #The value 1e7 for AIC corresponds to a failure in obtaining it
  }
  else{
    return(c("NA in R", "NA in R", AIC_ztp,eqp_rank))
  }
}



#5. Zero-truncated Negative Binomial
# zero_trunc_negbin <- function(dataframe,def_name,def,Step) {
zero_trunc_negbin <- function(dataframe,def_name,Step_name,get_coeffs,coeff_fileName) {
  # library(VGAM)
  data_pos_def <- dataframe[which(dataframe[[def_name]]>0),]
  # pos_def <- def[which(def>0)]
  # model_status <- "working"
  #ztnb_reg <- vglm(data_pos_def[[def_name]] ~  data_pos_def[[Step_name]], data = data_pos_def, family = posnegbinomial())
  
  ztnb_reg <- tryCatch(vglm(data_pos_def[[def_name]] ~  data_pos_def[[Step_name]], data = data_pos_def, family = posnegbinomial()), error = function(e){return ("fail")})
  # The method below restricts the number of iterations to avoid 
  # ztnb_reg <- tryCatch(vglm(data_pos_def[[def_name]] ~  data_pos_def[[Step_name]], data = data_pos_def, family = posnegbinomial()), error = function(e){return (vglm(data_pos_def[[def_name]] ~  data_pos_def[[Step_name]], data = data_pos_def, family = posnegbinomial(),crit="coef",trace=TRUE,maxit=2))})
  
  ## try(aic_stat <- AIC(ztnb_reg))
  # if(ztnb_reg == "fail"){
  #   aic_statistic <- 10e6 #if aic == 10e6 for a model then that model has failed
  # }
  # else{
  #   tryCatch(aic_statistic <- AIC(ztnb_reg) , error = function(e){browser()})#if aic == 10e6 for a model then that model has failed
  # }
  # return(c(aic_statistic))
  # #summary(ztnb_reg)
  #dispersion is maybe given by: deviance.vlm(ztp_reg)/df.residual_vlm(ztp_reg), but this value is always 0
  
  #-----notes on AIC---#
  #if vglm did not converge for the negative binomial family then we can still obtain the
  #coefficients and the AIC using the "$count" part of the hurdle model and the AIC value using the
  #formula AIC_hurdle = (AIC_zero.trunc.count*(1 - N(>0))/N) + AIC_binary
  #Thus, AIC_zero.trunc.count = (AIC_hurdle - AIC_binary)*N/(1 - N(>0))
  #--------------------#
  
  AIC_ztnb = tryCatch(AIC(ztnb_reg), error = function(e){return (1e7)})
  # AIC_ztnb = tryCatch(AIC(ztnb_reg), error = function(e){browser()})
  # browser()
  h_nbb_reg <- " "
  eqp_rank <- " "
  coeffs <- " "
  exp_coeffs <- " "
  full_coeffs <- " "
  
  #if(typeof(ztnb_reg) == "character"){
  if(AIC_ztnb == 1e7){
    #Part I - coefficients
    h_nbb_reg  <- hurdle(dataframe[[def_name]] ~  dataframe[[Step_name]], data = dataframe, dist="negbin")
    coeffs <- coef(summary(h_nbb_reg))$count
    #Part II - Binary model and AIC from hurdle
    unary_def_data <- data_pos_def
    unary_def_data[[def_name]] <- 1
    data_zero_def <- dataframe[which(dataframe[[def_name]]==0),]
    binary_def_data <- rbind(unary_def_data,data_zero_def)
    #browser()
    bernoulli_reg <- glm( binary_def_data[[def_name]] ~  binary_def_data[[Step_name]], data = binary_def_data, family=binomial(link=logit))
    AIC_binary = AIC(bernoulli_reg)
    AIC_hurdle = AIC(h_nbb_reg)
    AIC_ztnb = (AIC_hurdle - AIC_binary)*length(dataframe[,1])/length(data_pos_def[,1])
    # browser()
  }
  # else{
  #   AIC_ztnb = AIC(ztnb_reg)
  # }
  
  #Obtaining coefficients
  if(get_coeffs){
    if(AIC_ztnb == 1e7){
      # browser()
      n<-length(coef(summary(h_nbb_reg))$count[,1])
      coeffs <- coef(summary(h_nbb_reg))$count[1:(n-1),] #we exclude the last coefficient because its log theta
      exp_coeffs <- exp(coef(summary(h_nbb_reg))$count[1:(n-1),1])
    }
    else{
      # browser()
      if(names(coef(ztnb_reg))[2]=="(Intercept):2"){
        coeffs <- rbind(coef(summary(ztnb_reg))[1,],coef(summary(ztnb_reg))[3:(length(coef(ztnb_reg))),])
        exp_coeffs <- c(exp(coef(ztnb_reg))[1],exp(coef(ztnb_reg))[3:(length(coef(ztnb_reg)))])
        # browser()
      }
      else{
        coeffs <- coef(summary(ztnb_reg))
        exp_coeffs <- exp(coef(ztnb_reg))
      }
    } 
    full_coeffs <- cbind(coeffs,exp_coeffs,sign(coeffs[,1]))
    coeff_data <- cbind(full_coeffs[,c(1,4,5,6)])
    #-------------------------------------------------------------------------------------------##
    ## The following commented part of code was used when the coefficients obtained form zero-truncated
    ## negative binomial regression had two intercepts, where the second intercept was the standard
    ## error of what though? Of the model?
    ##------------------------------------------
    # coeff_data <- rbind(full_coeffs[1,c(1,4,5,6)],full_coeffs[3:length(coeffs[,1]),c(1,4,5,6)])
    # base_incidence_rate <- coeff_data[1,3]*coeff_data[1,4]
    # incidence_rate <- c(base_incidence_rate)
    # incidence_delta_rate <- c(base_incidence_rate)
    # for(i in 2:length(coeff_data[,1])){
    #   incidence_delta_rate[(length(incidence_delta_rate)+1)] <- coeff_data[i,3]*coeff_data[i,4]
    #   incidence_rate[(length(incidence_rate)+1)] <- incidence_rate[1]+incidence_delta_rate[i]
    #   #browser()
    # }
    ##-------------------------------------------------------------------------------------------##
    # browser()
    eqp_rank <- rank_non_glm_by_coeff(coeff_data,dataframe,def_name,Step_name)
    #eqp_rank <- rank(incidence_rate)
  }
  else{
    eqp_rank <- "not asked yet"
  }
  
  if(AIC_ztnb == 1e7){
    print("zero truncated negative binomial has failed")
    browser()
    return(c("fail","fail",1e7,"fail")) #The value 1e7 for AIC corresponds to a failure in obtaining it
  }
  else{
    return(c("NA in R", "NA in R", AIC_ztnb,eqp_rank))
  }
}


#6. Hurdle with Poisson count and Bernoulli (binomial in code) hurdles
hurdle_poi_bern <- function(dataframe,def,Step,get_coeffs,def_name,step_name,coeff_fileName) {
  # library(pscl)
  h_pb_reg  = hurdle(def ~  Step, data = dataframe, dist="poisson")
  #summary(h_pb_reg)
  #aic_statistic <- AIC(h_pb_reg)
  # browser()
  #dispersion is maybe given by: deviance.vlm(ztp_reg)/df.residual_vlm(ztp_reg), but this value is always 0
  
  #Obtaining coefficients
  eqp_rank <- " "
  if(get_coeffs){
    eqp_rank <- rank_hurdle_by_coeff(h_pb_reg,dataframe,def_name,Step_name)
  }
  else{
    eqp_rank <- "not asked yet"
  }
  
  return(c("NA in R", "NA in R", AIC(h_pb_reg),eqp_rank))
}

#7. Hurdle with Negative Binomial count and Bernoulli (binomial in code) hurdles
hurdle_negbin_bern <- function(dataframe,def,Step,get_coeffs,def_name,step_name,coeff_fileName) {
  # library(pscl)
  h_nbb_reg  = hurdle(def ~  Step, data = dataframe, dist="negbin")
  #summary(h_nbb_reg)
  #aic_statistic <- AIC(h_nbb_reg)
  #dispersion is maybe given by: deviance.vlm(ztp_reg)/df.residual_vlm(ztp_reg), but this value is always 0
  
  #Obtaining coefficients
  eqp_rank <- " "
  if(get_coeffs){
    eqp_rank <- rank_hurdle_by_coeff(h_nbb_reg,dataframe,def_name,Step_name)
  }
  else{
    eqp_rank <- "not asked yet"
  }
  
  return(c("NA in R", "NA in R", AIC(h_nbb_reg),eqp_rank))
}


#scatter plots
#had reinstalled ggplot2 as the older version did not work with my R version
#---------------
#scatterplot, enter def as def1 or column name of def1
scatterplot_defects <- function(dataframe,def) {
  library(ggplot2)
  p1 <- ggplot(dataframe, aes(x = id, y = def))
  p1 + geom_point()
}


#3. Histograms, enter def_col as data$def1 etc.
histogram_defects <- function(def_col, breaks_def1=100){#, breaks_def2=80, breaks_def3=30, breaks_def4=30) {
  hist(def_col,col=rgb(0,0,0,0.5),ylim=c(0,800),xlim = c(0,70),main="Defect 1",xlab = "Number of defects",breaks = breaks_def1)
}


#Algorithm for count regression
count_reg_algo <- function(dataframe,defs_start_colNum,defs_end_colNum,steps_start_colNum,steps_end_colNum, output_filename, write_output,coeff_fileName="coefficient_file.xlsx") {
  #BELOW ARE DIRECTIONS ON HOW TO USE THIS FUNCTION
  #
  #sTEP 1:
  #First ensure that all defect columns are next to each other in the excel file, 
  #and that all the step columns are also next to each other, then:
  #
  #defs_start_colNum is the column number of the first defect column when counted from left
  #defs_end_colNum is the column number of the last defect column when counted from left
  #steps_start_colNum is the column number of the first step column when counted from left
  #steps_end_colNum is the column number of the last step column when counted from left
  #
  #
  #sTEP 2:
  #Then set the working directory to the folder in which your excel file with defect data is saved by coipying the
  #name in to quotes and changeing all the backward slashes to forward slashes as in the example command below
  # > setwd("C:/Users/Shreya/Dropbox/Shreya/Research/Samsung2/Samsung2_data/Data 4")
  #
  #Step 3:
  #Then import library xlsx and read the excel file with defect data using the command below (example file read in command):
  # > library(xlsx)
  # > data = read.xlsx("Data4_convert(rev2.0).xlsx",3,colClasses = c(rep('character',5),"Date",rep('character',11),rep('numeric',5)))
  #
  #Step 4:
  #If you have not imported the xlsx package yet then do so by uncommenting this line in code
  # > library(xlsx) 
  library(VGAM)
  library(pscl)
  defect_name <- c()
  step_name <- c()
  regression_technique <- c()
  pvalue <- c()
  deviance_dispersion <- c()
  aic_stat <- c()
  defs <- dataframe[,defs_start_colNum:defs_end_colNum]
  steps <- dataframe[,steps_start_colNum:steps_end_colNum]
  #min_aic <- c()
  best_model <- c()
  for (def in names(defs)) {
    for (step in names(steps)) {
      
      modelNum <- 0
      #local_aic <- c() #A list of AIC statistics local to the loop corresponding to a particular def-step combination
      
      #1. Poisson Regression
      modelNum <- modelNum+1
      #browser()
      reg_stats <- poi_regression(dataframe,dataframe[[def]],dataframe[[step]],FALSE,def,step,coeff_fileName)
      pval = reg_stats[1]
      disp = reg_stats[2]
      aic_value = reg_stats[3]
      defect_name[length(defect_name)+1] <- def
      step_name[length(step_name)+1] <- step
      regression_technique[length(regression_technique)+1] <- "poisson"
      pvalue[length(pvalue)+1] <- pval
      deviance_dispersion[length(deviance_dispersion)+1] <- disp
      aic_stat[length(aic_stat)+1] <- aic_value
      if(pval>0.05 && round(as.numeric(disp),2)<=1.05){
        cat(paste('P value > ',pval,' and dispersion ',disp,' is approx. = 1.05'))
        #print ('So we proceed with poisson regression')
        #min_aic[length(min_aic)+1] <- aic_value
        best_model[length(best_model)+1] <- "yes"
        # browser()
      }
      
      #2. Quasipoisson Regression
      else{
        modelNum <- modelNum+1
        cat(paste('P value <= ',pval,' and dispersion ',disp,' is > 1.05'))
        print ('Thus, Poisson regression was not apt. We resort to remedial measure 1')
        #min_aic[length(min_aic)+1] <- "-"
        best_model[length(best_model)+1] <- "no"
        # browser()
        reg_stats <- quasipoi_regression(dataframe,dataframe[[def]],dataframe[[step]],FALSE,def,step,coeff_fileName)
        defect_name[length(defect_name)+1] <- def
        step_name[length(step_name)+1] <- step
        regression_technique[length(regression_technique)+1] <- "quasipoisson"
        pval = reg_stats[1]
        disp = reg_stats[2]
        aic_value = reg_stats[3] 
        # Note: there is no log likelihood for quasi* family models, thus no AIC, which is why they are not calculated/printed in the glm() summary outputs. 
        #See paper on link: https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf
        pvalue[length(pvalue)+1] <- pval
        deviance_dispersion[length(deviance_dispersion)+1] <- disp
        aic_stat[length(aic_stat)+1] <- aic_value
        if(pval>0.05 && round(as.numeric(disp),2)<=1.25){
          cat(paste('P value > ',pval,' and dispersion ',disp,' is approx. = 1.05'))
          #print ('So we proceed with quasipoisson regression')
          #min_aic[length(min_aic)+1] <- aic_value
          best_model[length(best_model)+1] <- "yes"
        }
        
        #3. Negative Binomial Regression, #alpha = 1/theta
        #For def 3 we got under-dispersion with NB, so for now we will use disp <=1.25
        else{
          modelNum <- modelNum+1
          cat(paste('P value <= ',pval,' and dispersion ',disp,' is > 1.05'))
          print ('Thus, quasipoisson regression was not apt. We resort to remedial measure 2, negative binomial regression')
          #min_aic[length(min_aic)+1] <- "-"
          best_model[length(best_model)+1] <- "no"
          reg_stats <- negBin_regression(dataframe,dataframe[[def]],dataframe[[step]],FALSE,def,step,coeff_fileName)
          defect_name[length(defect_name)+1] <- def
          step_name[length(step_name)+1] <- step
          regression_technique[length(regression_technique)+1] <- "negative binomial"
          pval = reg_stats[1]
          disp = reg_stats[2]
          aic_value = reg_stats[3]
          pvalue[length(pvalue)+1] <- pval
          deviance_dispersion[length(deviance_dispersion)+1] <- disp
          aic_stat[length(aic_stat)+1] <- aic_value
          if(pval>0.05 && disp<=1.05){
            cat(paste('P value <= ',pval,' and dispersion ',disp,' is <= 1.05'))
            #print ('So we proceed with negative binomial regression')
            #min_aic[length(min_aic)+1] <- aic_value
            best_model[length(best_model)+1] <- "yes"
          } 
          else{
            cat(paste('P value <= ',pval,' and dispersion ',disp,' is > 1.05'))
            print ('Negative Binomial was not apt. We resort to remedial measure ')
            #min_aic[length(min_aic)+1] <- "-"
            best_model[length(best_model)+1] <- "no"
          }
          
          # #-----------------------------------------------------------------------------------------------# #
          # #Zero-truncated is not applicable in this analysis:
          # #
          # #4. Zero-truncated Poisson Regression
          # modelNum <- modelNum+1
          # #reg_stats <- zero_trunc_poi(dataframe,def,dataframe[[def]],step,dataframe[[step]])
          # reg_stats <- zero_trunc_poi(dataframe,def,step,FALSE,coeff_fileName)
          # #browser()
          # pval = reg_stats[1]
          # disp = reg_stats[2]
          # aic_value = reg_stats[3]
          # defect_name[length(defect_name)+1] <- def
          # step_name[length(step_name)+1] <- step
          # regression_technique[length(regression_technique)+1] <- "zero-truncated poisson"
          # pvalue[length(pvalue)+1] <- pval
          # deviance_dispersion[length(deviance_dispersion)+1] <- disp
          # aic_stat[length(aic_stat)+1] <- aic_value
          # #browser()
          # if(as.numeric(aic_value) != 1e7 & as.numeric(aic_value)==min(as.numeric(aic_stat[(length(aic_stat)-modelNum+1):length(aic_stat)]))){
          #   if(is.element("yes",best_model[(length(best_model)-modelNum+1):length(best_model)])){
          #   best_model[(length(best_model)-modelNum+2):length(best_model)] <- "no"
          #   }
          #   best_model[length(best_model)+1] <- "yes"
          #   print ('Zero-truncated poisson is a better fit')
          # }
          # else{
          #   best_model[length(best_model)+1] <- "no"
          #   print ('Zero-truncated poisson was not apt. We resort to remedial measures')
          # }
          # 
          # #5. Zero-truncated Negative Binomial Regression 
          # modelNum <- modelNum+1   
          # # reg_stats <- zero_trunc_negbin(dataframe,def,dataframe[[def]],dataframe[[step]])
          # reg_stats <- zero_trunc_negbin(dataframe,def,step,FALSE,coeff_fileName)
          # pval = reg_stats[1]
          # disp = reg_stats[2]
          # aic_value = reg_stats[3]
          # defect_name[length(defect_name)+1] <- def
          # step_name[length(step_name)+1] <- step
          # regression_technique[length(regression_technique)+1] <- "zero-truncated negative binomial"
          # pvalue[length(pvalue)+1] <- pval
          # deviance_dispersion[length(deviance_dispersion)+1] <- disp
          # aic_stat[length(aic_stat)+1] <- aic_value
          # if(as.numeric(aic_value) != 1e7 & as.numeric(aic_value)==min(as.numeric(aic_stat[(length(aic_stat)-modelNum+1):length(aic_stat)]))){
          #   #browser()
          #   if(is.element("yes",best_model[(length(best_model)-modelNum+1):length(best_model)])){
          #   best_model[(length(best_model)-modelNum+2):length(best_model)] <- "no"
          #   }
          #   best_model[length(best_model)+1] <- "yes"
          #   print ('Zero-truncated negative binomial is a better fit')
          # }
          # else{
          #   best_model[length(best_model)+1] <- "no"
          #   print ('Zero-truncated negative binomial was not apt. We resort to remedial measures')
          # }
          # # if (is.element(1e7,as.numeric(aic_value))){
          # #   browser()
          # # }
          # #-----------------------------------------------------------------------------------------------# #
          
          #6. Hurdle model with bernoulli hurdles and poisson counts
          modelNum <- modelNum+1
          reg_stats <- hurdle_poi_bern(dataframe,dataframe[[def]],dataframe[[step]],FALSE,def,step,coeff_fileName)
          pval = reg_stats[1]
          disp = reg_stats[2]
          aic_value = reg_stats[3]
          defect_name[length(defect_name)+1] <- def
          step_name[length(step_name)+1] <- step
          regression_technique[length(regression_technique)+1] <- "hurdle- binomial, poisson"
          pvalue[length(pvalue)+1] <- pval
          deviance_dispersion[length(deviance_dispersion)+1] <- disp
          aic_stat[length(aic_stat)+1] <- aic_value
          if(as.numeric(aic_value) != 1e7 & as.numeric(aic_value)==min(as.numeric(aic_stat[(length(aic_stat)-modelNum+1):length(aic_stat)]))){
            if(is.element("yes",best_model[(length(best_model)-modelNum+1):length(best_model)])){
              best_model[(length(best_model)-modelNum+2):length(best_model)] <- "no"
            }
            best_model[length(best_model)+1] <- "yes"
            print ('Hurdle model with bernoulli hurdles and poisson counts is a better fit')
          }
          else{
            best_model[length(best_model)+1] <- "no"
            print ('Hurdle model with bernoulli hurdles and poisson was not apt. We resort to remedial measures')
          }
          
          #7. Hurdle model with bernoulli hurdles and negative binomial counts
          modelNum <- modelNum+1
          reg_stats <- hurdle_negbin_bern(dataframe,dataframe[[def]],dataframe[[step]],FALSE,def,step,coeff_fileName)
          pval = reg_stats[1]
          disp = reg_stats[2]
          aic_value = reg_stats[3]
          defect_name[length(defect_name)+1] <- def
          step_name[length(step_name)+1] <- step
          regression_technique[length(regression_technique)+1] <- "hurdle- binomial, negative binomial"
          pvalue[length(pvalue)+1] <- pval
          deviance_dispersion[length(deviance_dispersion)+1] <- disp
          aic_stat[length(aic_stat)+1] <- aic_value
          if(as.numeric(aic_value) != 1e7 & as.numeric(aic_value)==min(as.numeric(aic_stat[(length(aic_stat)-modelNum+1):length(aic_stat)]))){
            # browser()
            if(is.element("yes",best_model[(length(best_model)-modelNum+1):length(best_model)])){
              best_model[(length(best_model)-modelNum+2):length(best_model)] <- "no"
            }
            best_model[length(best_model)+1] <- "yes"
            print ('Hurdle model with bernoulli hurdles and negative binomial counts is a better fit')
          }
          else{
            best_model[length(best_model)+1] <- "no"
            print ('Hurdle model with bernoulli hurdles and negative binomial was not apt. We resort to remedial measures')
          }
          #browser()
        }
        # browser()
        if(!(is.element("yes",best_model[(length(best_model)-modelNum+1):length(best_model)]))){
          # browser()
          aic_local <- as.numeric(aic_stat[(length(aic_stat)-modelNum+1):length(aic_stat)])
          best_model[which(aic_local == min(aic_local))] <- "yes"
        }
        #browser()
      }
    }
    
  }
  algo_stats = data.frame(defect_name=character(),
                          step_name=character(), 
                          regression_technique=character(),
                          pvalue=double(),
                          deviance_dispersion=double())
  algo_stats <- as.data.frame(cbind(defect_name, step_name, regression_technique, pvalue, deviance_dispersion,aic_stat,best_model))
  # algo_stats$defect_name = defect_name
  # algo_stats$step_name = step_name 
  # algo_stats$regression_technique = regression_technique
  # algo_stats$pvalue = pvalue
  # algo_stats$deviance_dispersion = deviance_dispersion
  #browser()
  if(write_output){
    write.xlsx(algo_stats,file=output_filename,sheetName = "1",append = TRUE, row.names = FALSE,col.names = TRUE,showNA = TRUE)
  }
  # return(c(defect_name, step_name, regression_technique, pvalue, deviance_dispersion))
  return(c(names(defs),names(steps),best_model))
}


rank_from_coefs <- function(defs_steps_bestModel,numdefs,numsteps,
                            dataframe,do_binary_ranking,do_count_ranking,write_binary_rank_output,
                            binary_rank_output_file_name,write_count_output,count_output_filename,num_count_reg_models){
  defs <- defs_steps_bestModel[1:numdefs]
  steps <- defs_steps_bestModel[(1+numdefs):(numsteps+numdefs)]
  best_model <- defs_steps_bestModel[(1+numdefs+numsteps):length(defs_steps_bestModel)]
  num_combos <- length(defs)*length(steps)
  data_zero_Defects <- " "
  eqp_rank_table <- " "
  complete_set_of_count_ranks <- " "
  complete_set_of_binary_ranks <- " "
  i<-0
  
  for(def_name in defs){
    
    # if(do_binary_ranking){
    #   data_zero_Defects <- dataframe[which(dataframe[[def_name]]>0),]
    # }
    
    for(Step_name in steps){
      if(do_count_ranking){
        i<-i+1
        eqp_rank <- " "
        model_set <- best_model[((i-1)*num_count_reg_models+1):((i-1)*num_count_reg_models+num_count_reg_models)]
        tryCatch(best_model_index <- which(model_set=="yes"),
                 error = function(e){
                   print("best model index allocation failed")
                   browser()
                   }
                 )
        # print(i)
        # print((i-1)*7+1)
        # print((i-1)*7+7)
        # print(model_set)
        # browser()
        #We will switch by index so no need to write cases as: 1={print('poi)}
        tryCatch(
          switch(as.integer(best_model_index),
                 { cat(paste(i,' ',def_name,' ',Step_name,' poi'))
                   reg_stats <- poi_regression(dataframe,dataframe[[def_name]],dataframe[[Step_name]],TRUE,def_name,Step_name,coeff_fileName) 
                   print(reg_stats[4:length(reg_stats)])
                   eqp_rank <- reg_stats[4:length(reg_stats)]
                 },
                 { cat(paste(i,' ',def_name,' ',Step_name,' qpoi'))
                   reg_stats <- quasipoi_regression(dataframe,dataframe[[def_name]],dataframe[[Step_name]],TRUE,def_name,Step_name,coeff_fileName)
                   print(reg_stats[4:length(reg_stats)])
                   eqp_rank <- reg_stats[4:length(reg_stats)]
                 },
                 { #browser()
                   cat(paste(i,' ',def_name,' ',Step_name,' negbin'))
                   reg_stats <- negBin_regression(dataframe,dataframe[[def_name]],dataframe[[Step_name]],TRUE,def_name,Step_name,coeff_fileName) 
                   print(reg_stats[4:length(reg_stats)])
                   eqp_rank <- reg_stats[4:length(reg_stats)]
                   
                 },
                 ##---------We are not using zero-truncated models anymore-----------##
                 # { cat(paste(i,' ',def_name,' ',Step_name,' ztp'))
                 #   reg_stats <- zero_trunc_poi(dataframe,def_name,Step_name,TRUE,coeff_fileName) 
                 #   print(reg_stats[4:length(reg_stats)])
                 #   eqp_rank <- reg_stats[4:length(reg_stats)]
                 #   # unique_eqps <- as.character(sort(unique(dataframe[[Step_name]])))
                 #   # def_considered <- as.list(rep(def_name,length(unique_eqps)))
                 #   # step_considered <- as.list(rep(Step_name,length(unique_eqps)))
                 #   # eqp_rank_table <- cbind(def_considered,step_considered,unique_eqps,eqp_rank)
                 #   # print(eqp_rank_table)
                 # },
                 # { cat(paste(i,' ',def_name,' ',Step_name,' ztnb'))
                 #   # browser()
                 #   reg_stats <- zero_trunc_negbin(dataframe,def_name,Step_name,TRUE,coeff_fileName) 
                 #   # browser()
                 #   print(reg_stats[4:length(reg_stats)])
                 #   eqp_rank <- reg_stats[4:length(reg_stats)]
                 # },
                 ##-----------------------------------------------------------------##
                 { cat(paste(i,' ',def_name,' ',Step_name,' h_p_bern'))
                   reg_stats <- hurdle_poi_bern(dataframe,dataframe[[def_name]],dataframe[[Step_name]],TRUE,def_name,Step_name,coeff_fileName)  
                   print(reg_stats[4:length(reg_stats)])
                   eqp_rank <- reg_stats[4:length(reg_stats)]
                   # browser()
                 },
                 { #browser()
                   cat(paste(i,' ',def_name,' ',Step_name,' h_nb_bern'))
                   reg_stats <- hurdle_negbin_bern(dataframe,dataframe[[def_name]],dataframe[[Step_name]],TRUE,def_name,Step_name,coeff_fileName) 
                   print(reg_stats[4:length(reg_stats)])
                   eqp_rank <- reg_stats[4:length(reg_stats)]
                   # browser()
                 },
                 { print('error')
                 }
          ),
          error = function(e){
            print("switch function has failed")
            browser()
            }
        )
        unique_eqps <- as.character(sort(unique(dataframe[[Step_name]])))
        def_considered <- as.list(rep(def_name,length(unique_eqps)))
        step_considered <- as.list(rep(Step_name,length(unique_eqps)))
        eqp_rank_table <- cbind(def_considered,step_considered,unique_eqps,eqp_rank)
        print(eqp_rank_table)
        # if(length(complete_set_of_ranks[,1])==0)
        complete_set_of_count_ranks <- rbind(complete_set_of_count_ranks,eqp_rank_table)
      }
      
      if(do_binary_ranking){
        print("attempt to perform binary ranking")
        eqp_zero_probab <- c()
        for(eqp in as.character(sort(unique(dataframe[[Step_name]])))){
          def_step_data_slice <- dataframe[which(dataframe[[Step_name]]==eqp),]
          num_count_defs <- length(def_step_data_slice[,1])
          # def_step_slice_zero_defs <- data_zero_Defects[which(def_data_zero_Defects[[Step_name]]==eqp),]
          num_zero_defs <- length(subset(def_step_data_slice,def_step_data_slice[[def_name]]==0)[,1])
          eqp_zero_probab[(length(eqp_zero_probab)+1)]<-(num_zero_defs/num_count_defs)
        }
        # browser()
        eqp_rank <- rank(-eqp_zero_probab) #higher probability of zero will have a higher rank
        unique_eqps <- as.character(sort(unique(dataframe[[Step_name]])))
        # dummy_row_names <- as.list(rep("row_name",length(unique_eqps)))
        def_considered <- as.list(rep(def_name,length(unique_eqps)))
        step_considered <- as.list(rep(Step_name,length(unique_eqps)))
        eqp_rank_table <- cbind(def_considered,step_considered,unique_eqps,eqp_rank)#cbind(dummy_row_names,def_considered,step_considered,unique_eqps,eqp_rank)
        print(eqp_rank_table)
        complete_set_of_binary_ranks <- rbind(complete_set_of_binary_ranks,eqp_rank_table)
      }
    }
  }
  
  #Writing the rank outputs into excel files
  if(do_count_ranking){
    print(complete_set_of_count_ranks)
    if(write_count_output){
      # browser()
      count_rank_dataframe <- as.data.frame(complete_set_of_count_ranks[2:(length(complete_set_of_count_ranks[,2])),]) #because first row is empty
      colnames(count_rank_dataframe)[4] <- "rank"
      # browser()
      count_rank_dataframe$rank <- as.numeric(count_rank_dataframe$rank)
      write.xlsx(count_rank_dataframe,file=count_output_filename,sheetName = "1",append = TRUE, row.names = FALSE,col.names = TRUE,showNA = TRUE)
    }
  }
  if(do_binary_ranking){
    # browser()
    print("attempt to write binary ranks")
    print(complete_set_of_binary_ranks)
    # row.names(complete_set_of_binary_ranks) <- seq(1,length(complete_set_of_binary_ranks[,2]))
    # binary_rank_dataframe <- as.data.frame(complete_set_of_binary_ranks)
    binary_rank_dataframe <- as.data.frame(complete_set_of_binary_ranks[2:(length(complete_set_of_binary_ranks[,2])),]) #because first row is empty
    colnames(binary_rank_dataframe)[4] <- "rank"
    # browser()
    # binary_rank_dataframe$rank <- as.numeric(binary_rank_dataframe$rank)
    if(write_binary_rank_output){
      write.xlsx(binary_rank_dataframe,file=binary_rank_output_file_name,sheetName = "1",append = TRUE, row.names = FALSE,col.names = TRUE,showNA = TRUE)
    }
  }
}



rank_routes_from_file <- function(defs_start_colNum,defs_end_colNum,steps_start_colNum,steps_end_colNum,eqp_rank_file,numdefs,numsteps,defect_weights,dataframe,write_output_file,output_file_name){
  print("Attempting to use the equipment rank file to rank and write the output into an excel file")
  eqp_ranks <- read.xlsx(eqp_rank_file,1)
  defects <- colnames(dataframe[,defs_start_colNum:defs_end_colNum])
  steps <- colnames(dataframe[,steps_start_colNum:steps_end_colNum])
  # def_col <- "def_considered"
  # steps_col <-  "step_considered"
  # eqp_col <- "unique_eqps"
  # rank_col <- "rank"
  for(defect in defects){
    def_df <- eqp_ranks[which(eqp_ranks$def_considered == defect),]
    sum_over_steps <- c()
    sum_over_steps <- paste("dataframe$",defect,"_",steps[1],sep="",collapse="")
    for(step in steps){
      # browser()
      if((length(steps)>1) & (step %in% steps[(-1)])){
        sum_over_steps <- paste(sum_over_steps," + dataframe$",defect,"_",step,sep="",collapse="")
        # browser()
      }
      route_at_step_rank <- c()
      # browser()
      def_step_data <- na.omit(def_df[which(eqp_ranks$step_considered==step),])
      for(eqp in dataframe[[step]]){
        route_at_step_rank <- c(route_at_step_rank,na.omit(def_step_data[which(def_step_data$unique_eqps==eqp), ])$rank)
        # browser()
      }
      # browser()
      tryCatch(dataframe[[paste(defect,step,sep="_")]] <- route_at_step_rank,error = function(e){browser()})
    }
    # browser()
    # sum_over_steps <- paste(sum_over_steps,")")
    route_rank_for_defects <- eval(parse(text = sum_over_steps))
    dataframe[[paste(defect,"_route_rank",sep="")]] <- route_rank_for_defects
    # route_rank_for_defects <- c()
    # for(i in (1:length(datframe[,1]))){
    #   rank <- 0
    #   for(step in steps){
    #     rank = rank + dataframe[[paste(defect,step,sep="")]][i]
    #   }
    #   
    # }
  }
  route_rank <- 0
  for(i in (1:length(defects))){
    route_rank <- route_rank + dataframe[[paste("def",i,"_route_rank",sep="")]]*defect_weights[1]
    # browser()
    # route_rank_for_defects <- c()
    # for(step in steps){
    #
    # }
    # for(i in (1:length(defect_weights))){
    #   # eval(parse(paste("defect",i,"_weight")))
    #   route_rank_for_defects <- c(route_rank_for_defects,eval(parse(text = paste("def",i,"_route_rank",sep=""))))
    # }
  }
  # browser()
  dataframe$weighted_route_rank <- route_rank
  
  ##----------------------------------------------------------------------------------------------------##
  ##---The code below did not work because the computer ran out of memory due to too many combination---##
  ##------So we adopted individually ranking routes within the data set as seen in the code above-------##
  ##----------------------------------------------------------------------------------------------------##
  # # parsed_grid_cmd <- ""
  # # eval_grid_cmd <- ""
  # # routes<-paste("expand.grid(unique(dataframe[[",steps[1],"]])",sep="") #this statement does not treat step as atomic vector requiring [[.]] but requires the usual $.
  # routes<-paste("expand.grid(unique(dataframe$",steps[1],")",sep="")
  # if(length(steps)>1){
  #   for(step in steps[(-1)]){
  #     # browser()
  #     routes<-paste(routes,", unique(dataframe$",step,")",sep="")
  #   }
  # }
  # routes<-paste(routes,")",sep="")
  # # browser()
  # parsed_grid_cmd<-parse(text=routes)    47 47 43
  # eval_grid_cmd <- eval(parsed_grid_cmd) 57 44 65
  # # for(def in defs){                    104 91 108
  # #   for(step in steps){
  # #     
  # #   }
  # # }
  # # return(k)
  ##----------------------------------------------------------------------------------------------------##
  # browser()
  if(write_output_file){
    write.xlsx(dataframe,file=output_file_name,sheetName = "1",append = TRUE, row.names = FALSE,col.names = TRUE,showNA = TRUE)
    
  }
}


rank_routes <- function(numdefs,numsteps,defect_weights,defs_start_colNum,defs_end_colNum,steps_start_colNum,steps_end_colNum,dataframe,output_file_name_for_binary_eqp_ranks,output_file_name_for_count_eqp_ranks,do_binary_ranking,do_count_ranking,make_new_output_file_for_binary_route_ranks,output_file_name_for_binary_route_ranks,make_new_output_file_for_count_route_ranks,output_file_name_for_count_route_ranks){
  print("Attempting to rank routes")
  # browser()
  if(do_binary_ranking & do_count_ranking){
    print("Attempting to use both count and binary ranks and generate two separate set of ranks for the routes")
    rank_routes_from_file(defs_start_colNum,defs_end_colNum,steps_start_colNum,steps_end_colNum,output_file_name_for_binary_eqp_ranks,numdefs,numsteps,defect_weights,dataframe,make_new_output_file_for_binary_route_ranks,output_file_name_for_binary_route_ranks)
    rank_routes_from_file(defs_start_colNum,defs_end_colNum,steps_start_colNum,steps_end_colNum,output_file_name_for_count_eqp_ranks,numdefs,numsteps,defect_weights,dataframe,make_new_output_file_for_count_route_ranks,output_file_name_for_count_route_ranks)
    # browser()
  }
  else if(do_binary_ranking){
    print("Attempting to use only binary ranks and generate a set of ranks for the routes")
    # col_names <- colnames(eqp_ranks)
    # def_col <- "def_considered"
    # steps_col <-  "step_considered"
    # eqp_col <- "unique_eqps"
    # rank_col <- "rank"
    rank_routes_from_file(defs_start_colNum,defs_end_colNum,steps_start_colNum,steps_end_colNum,output_file_name_for_binary_eqp_ranks,numdefs,numsteps,defect_weights,dataframe,make_new_output_file_for_binary_route_ranks,output_file_name_for_binary_route_ranks)
  }
  else if(do_count_ranking){
    # browser()
    print("Attempting to use only count ranks and generate a set of ranks for the routes")
    rank_routes_from_file(defs_start_colNum,defs_end_colNum,steps_start_colNum,steps_end_colNum,output_file_name_for_count_eqp_ranks,numdefs,numsteps,defect_weights,dataframe,make_new_output_file_for_count_route_ranks,output_file_name_for_count_route_ranks)
  }
  else{
    cat("Make at least one of the ranking methods, count or binary, TRUE. The corresponding variable names are \"do_count_ranking\" and \"do_binary_ranking\"")
  }
}


#commands
library(rJava)
library(xlsx)
setwd("C:/Users/Shreya/Dropbox/Shreya/Research/Samsung2/Samsung2_data/Data 4")
dataframe = read.xlsx("Data4_convert(rev2.0).xlsx",3,colClasses = c(rep('character',5),"Date",rep('character',11),rep('numeric',5)))
colnames(dataframe)[18:21] <- c("def1","def2","def3","def4")
colnames(dataframe)[3:6] <- c("LW","Inspection_Dates","Corrected_Dates","Date")
# dataframe[1,] #This is to check and change column names, adn to rearrange columns if needed. Ensure that all defect columns
#are nect to each other in the excel file, and all the step columns are next to each other
# count_reg_algo(d4,18,19,7,8)

#Perform best fitting count regression on all the defect-step slices of the defect data
numdefs<-4
numsteps<-11
defs_start_colNum = 18
defs_end_colNum = 21
steps_start_colNum = 7
steps_end_colNum = 17
make_new_output_file_for_models = FALSE
output_file_name_for_models <- "Count_Reg_Algo_Stats14.xlsx"
defs_steps_bestModel <- count_reg_algo(dataframe,defs_start_colNum,defs_end_colNum,steps_start_colNum,steps_end_colNum,output_file_name_for_models,make_new_output_file_for_models,"coefficient_file.xlsx")

#Ranking equipments within steps
num_count_reg_models = 5
do_binary_ranking = FALSE
make_new_output_file_for_binary_ranks = TRUE
output_file_name_for_binary_eqp_ranks <- "DefStep_Ranks_via_bern_reg_4.xlsx"
do_count_ranking = TRUE
make_new_output_file_for_count_ranks = FALSE
output_file_name_for_count_eqp_ranks <- "DefStep_Ranks_via_count_reg_Algos6.xlsx"
rank_from_coefs(defs_steps_bestModel,numdefs,numsteps,dataframe,do_binary_ranking,do_count_ranking,make_new_output_file_for_binary_ranks,output_file_name_for_binary_eqp_ranks,make_new_output_file_for_count_ranks,output_file_name_for_count_eqp_ranks,num_count_reg_models)

#Rank routes
#Step 1. Assign all defect weights here and create new weights if necessary using the same format
#The defect weight here should correspond to the order in which they appear in the dataframe
#e.g, "Defect2(<12)" appears 2nd in the dataframe, so the variable name "defect2_weight" corresponds to "Defect2(<12)"
defect1_weight = 1
defect2_weight = 1
defect3_weight = 1
defect4_weight = 1
defect_weights <- c()
for(i in 1:numdefs){
  # eval(parse(paste("defect",i,"_weight")))
  defect_weights <- c(defect_weights,eval(parse(text = paste("defect",i,"_weight",sep="",collapse=""))))
}
# defect_weights <- c(defect1_weight,defect2_weight,defect3_weight,defect3_weight)
make_new_output_file_for_binary_route_ranks = FALSE
output_file_name_for_binary_route_ranks = "Route_Ranks_binary_1.xlsx"
make_new_output_file_for_count_route_ranks = TRUE
output_file_name_for_count_route_ranks = "Route_Ranks_count_1.xlsx"
rank_routes(numdefs,numsteps,defect_weights,defs_start_colNum,defs_end_colNum,steps_start_colNum,steps_end_colNum,dataframe,output_file_name_for_binary_eqp_ranks,output_file_name_for_count_eqp_ranks,do_binary_ranking,do_count_ranking,make_new_output_file_for_binary_route_ranks,output_file_name_for_binary_route_ranks,make_new_output_file_for_count_route_ranks,output_file_name_for_count_route_ranks)

# output_file_name_for_count_eqp_ranks <- "DefStep_Ranks_via_count_reg_Algos6.xlsx"
# rank_from_coefs(numdefs,numsteps,dataframe,do_binary_ranking,do_count_ranking,make_new_output_file_for_binary_ranks,output_file_name_for_binary_eqp_ranks,make_new_output_file_for_count_ranks,output_file_name_for_count_eqp_ranks,num_count_reg_models)

