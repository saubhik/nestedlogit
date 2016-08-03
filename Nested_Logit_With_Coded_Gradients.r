####################################################################################################################################################
## FULL NESTED LOGIT WITH CODED GRADIENT ##                                                                                                       ##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 88                               88                               88                                  ## 
##                                       ## 88                               88                               ""                                  ##
##          July 21, 2016                ## 88                               88                                                                   ##
##                                       ## 88,dPPYba,  88       88  ,adPPYb,88 8b      db      d8  ,adPPYba, 88  ,adPPYba,  ,adPPYba, 8b,dPPYba, ##  
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 88P'    "8a 88       88 a8"    `Y88 `8b    d88b    d8' a8P_____88 88  I8[    "" a8P_____88 88P'   "Y8 ##
##                                       ## 88       d8 88       88 8b       88  `8b  d8'`8b  d8'  8PP""""""" 88   `"Y8ba,  8PP""""""" 88         ##
##          Team Conjoint                ## 88b,   ,a8" "8a,   ,a88 "8a,   ,d88   `8bd8'  `8bd8'   "8b,   ,aa 88  aa    ]8I "8b,   ,aa 88         ##
##                                       ## 8Y"Ybbd8"'   `"YbbdP'Y8  `"8bbdP"Y8     YP      YP      `"Ybbd8"' 88  `"YbbdP"'  `"Ybbd8"' 88         ##
##                                       ##                                                                                                       ##                          
####################################################################################################################################################

rm(list=ls())
library(data.table)
library(maxLik)
library(dummies)
setwd("E:/AB InBev/UK/script.outputs")

uk <- read.csv("uk_data.csv", stringsAsFactors = FALSE)
uk <- subset(uk, uk$channel.x == "convenience")

### Nests #########################################################################################################
nest1 <- c(62, 31, 64, 30, 42, 63, 51, 40, 26, 19)
nest2 <- c(12, 11, 17, 83, 74, 73, 84, 81, 82)
nest3 <- c(56, 34, 54, 89, 78, 87, 75, 76)
nest4 <- c(29, 20, 27, 23, 38, 16, 10, 21, 32, 28, 13, 33, 22, 18, 58)
nest5 <- c(36, 35, 71, 47, 15, 14, 72)
nest6 <- c(85, 77, 86, 91, 88, 90, 79, 80)
nest7 <- c(3, 2, 1, 45, 50, 37, 39, 61, 60, 52)
nest8 <- c(7, 6, 5, 4, 9, 8)
nest9 <- c(57, 43, 69, 66, 49, 24, 25)
nest10 <- c(48, 44, 55, 68, 53, 41, 70, 59, 46, 67, 65)
nests <- 11
uk$nest <- ifelse(uk$sku %in% nest1, 1,
                  ifelse(uk$sku %in% nest2, 2,
                         ifelse(uk$sku %in% nest3, 3,
                                ifelse(uk$sku %in% nest4, 4,
                                       ifelse(uk$sku %in% nest5, 5,
                                              ifelse(uk$sku %in% nest6, 6,
                                                     ifelse(uk$sku %in% nest7, 7,
                                                            ifelse(uk$sku %in% nest8, 8,
                                                                   ifelse(uk$sku %in% nest9, 9,
                                                                          ifelse(uk$sku %in% nest10, 10, 11))))))))))
###################################################################################################################

uk <- dummy.data.frame(uk, names = c("sku", "brand", "gender", "marital_status", "education"))

# Interaction Dummies #############################################################################################
uk$`gender1_brandSTELLA4%` <- uk$gender1 * uk$`brandSTELLA 4%`
uk$`gender1_brandBUDLIGHT` <- uk$gender1 * uk$`brandBUD LIGHT`
uk$`gender1_brandBECKSVIER` <- uk$gender1 * uk$`brandBECKS VIER`
uk$`gender1_brandBUDWEISER` <- uk$gender1 * uk$brandBUDWEISER
uk$`gender1_brandCARLSBERG` <- uk$gender1 * uk$brandCARLSBERG
uk$`gender1_brandFOSTER'S` <- uk$gender1 * uk$`brandFOSTER'S`
uk$`gender1_brandSTELLAARTOIS` <- uk$gender1 * uk$`brandSTELLA ARTOIS`
uk$`gender1_brandCARLING` <- uk$gender1 * uk$brandCARLING
uk$`gender1_brandSANMIGUEL` <- uk$gender1 * uk$`brandSAN MIGUEL`
uk$`gender1_brandCARLSBERGEXPORT` <- uk$gender1 * uk$`brandCARLSBERG EXPORT`
uk$`gender1_brandHEINEKEN` <- uk$gender1 * uk$brandHEINEKEN
uk$`gender1_brandPERONINASTROAZZURRO` <- uk$gender1 * uk$`brandPERONI NASTRO AZZURRO`
uk$`gender1_brandPRIVATELABEL` <- uk$gender1 * uk$`brandPRIVATE LABEL`

uk$`age_brandSTELLA4%` <- log(uk$age) * uk$`brandSTELLA 4%`
uk$`age_brandBUDLIGHT` <- log(uk$age) * uk$`brandBUD LIGHT`
uk$`age_brandBECKSVIER` <- log(uk$age) * uk$`brandBECKS VIER`
uk$`age_brandBUDWEISER` <- log(uk$age) * uk$brandBUDWEISER
uk$`age_brandCARLSBERG` <- log(uk$age) * uk$brandCARLSBERG
uk$`age_brandFOSTER'S` <- log(uk$age) * uk$`brandFOSTER'S`
uk$`age_brandSTELLAARTOIS` <- log(uk$age) * uk$`brandSTELLA ARTOIS`
uk$`age_brandCARLING` <- log(uk$age) * uk$brandCARLING
uk$`age_brandSANMIGUEL` <- log(uk$age) * uk$`brandSAN MIGUEL`
uk$`age_brandCARLSBERGEXPORT` <- log(uk$age) * uk$`brandCARLSBERG EXPORT`
uk$`age_brandHEINEKEN` <- log(uk$age) * uk$brandHEINEKEN
uk$`age_brandPERONINASTROAZZURRO` <- log(uk$age) * uk$`brandPERONI NASTRO AZZURRO`
uk$`age_brandPRIVATELABEL` <- log(uk$age) * uk$`brandPRIVATE LABEL`

uk$`marital_status1_brandSTELLA4%` <- uk$marital_status1 * uk$`brandSTELLA 4%`
uk$`marital_status1_brandBUDLIGHT` <- uk$marital_status1 * uk$`brandBUD LIGHT`
uk$`marital_status1_brandBECKSVIER` <- uk$marital_status1 * uk$`brandBECKS VIER`
uk$`marital_status1_brandBUDWEISER` <- uk$marital_status1 * uk$brandBUDWEISER
uk$`marital_status1_brandCARLSBERG` <- uk$marital_status1 * uk$brandCARLSBERG
uk$`marital_status1_brandFOSTER'S` <- uk$marital_status1 * uk$`brandFOSTER'S`
uk$`marital_status1_brandSTELLAARTOIS` <- uk$marital_status1 * uk$`brandSTELLA ARTOIS`
uk$`marital_status1_brandCARLING` <- uk$marital_status1 * uk$brandCARLING
uk$`marital_status1_brandSANMIGUEL` <- uk$marital_status1 * uk$`brandSAN MIGUEL`
uk$`marital_status1_brandCARLSBERGEXPORT` <- uk$marital_status1 * uk$`brandCARLSBERG EXPORT`
uk$`marital_status1_brandHEINEKEN` <- uk$marital_status1 * uk$brandHEINEKEN
uk$`marital_status1_brandPERONINASTROAZZURRO` <- uk$marital_status1 * uk$`brandPERONI NASTRO AZZURRO`
uk$`marital_status1_brandPRIVATELABEL` <- uk$marital_status1 * uk$`brandPRIVATE LABEL`

uk$`marital_status2_brandSTELLA4%` <- uk$marital_status2 * uk$`brandSTELLA 4%`
uk$`marital_status2_brandBUDLIGHT` <- uk$marital_status2 * uk$`brandBUD LIGHT`
uk$`marital_status2_brandBECKSVIER` <- uk$marital_status2 * uk$`brandBECKS VIER`
uk$`marital_status2_brandBUDWEISER` <- uk$marital_status2 * uk$brandBUDWEISER
uk$`marital_status2_brandCARLSBERG` <- uk$marital_status2 * uk$brandCARLSBERG
uk$`marital_status2_brandFOSTER'S` <- uk$marital_status2 * uk$`brandFOSTER'S`
uk$`marital_status2_brandSTELLAARTOIS` <- uk$marital_status2 * uk$`brandSTELLA ARTOIS`
uk$`marital_status2_brandCARLING` <- uk$marital_status2 * uk$brandCARLING
uk$`marital_status2_brandSANMIGUEL` <- uk$marital_status2 * uk$`brandSAN MIGUEL`
uk$`marital_status2_brandCARLSBERGEXPORT` <- uk$marital_status2 * uk$`brandCARLSBERG EXPORT`
uk$`marital_status2_brandHEINEKEN` <- uk$marital_status2 * uk$brandHEINEKEN
uk$`marital_status2_brandPERONINASTROAZZURRO` <- uk$marital_status2 * uk$`brandPERONI NASTRO AZZURRO`
uk$`marital_status2_brandPRIVATELABEL` <- uk$marital_status2 * uk$`brandPRIVATE LABEL`

uk$`marital_status3_brandSTELLA4%` <- uk$marital_status3 * uk$`brandSTELLA 4%`
uk$`marital_status3_brandBUDLIGHT` <- uk$marital_status3 * uk$`brandBUD LIGHT`
uk$`marital_status3_brandBECKSVIER` <- uk$marital_status3 * uk$`brandBECKS VIER`
uk$`marital_status3_brandBUDWEISER` <- uk$marital_status3 * uk$brandBUDWEISER
uk$`marital_status3_brandCARLSBERG` <- uk$marital_status3 * uk$brandCARLSBERG
uk$`marital_status3_brandFOSTER'S` <- uk$marital_status3 * uk$`brandFOSTER'S`
uk$`marital_status3_brandSTELLAARTOIS` <- uk$marital_status3 * uk$`brandSTELLA ARTOIS`
uk$`marital_status3_brandCARLING` <- uk$marital_status3 * uk$brandCARLING
uk$`marital_status3_brandSANMIGUEL` <- uk$marital_status3 * uk$`brandSAN MIGUEL`
uk$`marital_status3_brandCARLSBERGEXPORT` <- uk$marital_status3 * uk$`brandCARLSBERG EXPORT`
uk$`marital_status3_brandHEINEKEN` <- uk$marital_status3 * uk$brandHEINEKEN
uk$`marital_status3_brandPERONINASTROAZZURRO` <- uk$marital_status3 * uk$`brandPERONI NASTRO AZZURRO`
uk$`marital_status3_brandPRIVATELABEL` <- uk$marital_status3 * uk$`brandPRIVATE LABEL`

uk$`education1_brandSTELLA4%` <- uk$education1 * uk$`brandSTELLA 4%`
uk$`education1_brandBUDLIGHT` <- uk$education1 * uk$`brandBUD LIGHT`
uk$`education1_brandBECKSVIER` <- uk$education1 * uk$`brandBECKS VIER`
uk$`education1_brandBUDWEISER` <- uk$education1 * uk$brandBUDWEISER
uk$`education1_brandCARLSBERG` <- uk$education1 * uk$brandCARLSBERG
uk$`education1_brandFOSTER'S` <- uk$education1 * uk$`brandFOSTER'S`
uk$`education1_brandSTELLAARTOIS` <- uk$education1 * uk$`brandSTELLA ARTOIS`
uk$`education1_brandCARLING` <- uk$education1 * uk$brandCARLING
uk$`education1_brandSANMIGUEL` <- uk$education1 * uk$`brandSAN MIGUEL`
uk$`education1_brandCARLSBERGEXPORT` <- uk$education1 * uk$`brandCARLSBERG EXPORT`
uk$`education1_brandHEINEKEN` <- uk$education1 * uk$brandHEINEKEN
uk$`education1_brandPERONINASTROAZZURRO` <- uk$education1 * uk$`brandPERONI NASTRO AZZURRO`
uk$`education1_brandPRIVATELABEL` <- uk$education1 * uk$`brandPRIVATE LABEL`

uk$`education2_brandSTELLA4%` <- uk$education2 * uk$`brandSTELLA 4%`
uk$`education2_brandBUDLIGHT` <- uk$education2 * uk$`brandBUD LIGHT`
uk$`education2_brandBECKSVIER` <- uk$education2 * uk$`brandBECKS VIER`
uk$`education2_brandBUDWEISER` <- uk$education2 * uk$brandBUDWEISER
uk$`education2_brandCARLSBERG` <- uk$education2 * uk$brandCARLSBERG
uk$`education2_brandFOSTER'S` <- uk$education2 * uk$`brandFOSTER'S`
uk$`education2_brandSTELLAARTOIS` <- uk$education2 * uk$`brandSTELLA ARTOIS`
uk$`education2_brandCARLING` <- uk$education2 * uk$brandCARLING
uk$`education2_brandSANMIGUEL` <- uk$education2 * uk$`brandSAN MIGUEL`
uk$`education2_brandCARLSBERGEXPORT` <- uk$education2 * uk$`brandCARLSBERG EXPORT`
uk$`education2_brandHEINEKEN` <- uk$education2 * uk$brandHEINEKEN
uk$`education2_brandPERONINASTROAZZURRO` <- uk$education2 * uk$`brandPERONI NASTRO AZZURRO`
uk$`education2_brandPRIVATELABEL` <- uk$education2 * uk$`brandPRIVATE LABEL`

uk$`education3_brandSTELLA4%` <- uk$education3 * uk$`brandSTELLA 4%`
uk$`education3_brandBUDLIGHT` <- uk$education3 * uk$`brandBUD LIGHT`
uk$`education3_brandBECKSVIER` <- uk$education3 * uk$`brandBECKS VIER`
uk$`education3_brandBUDWEISER` <- uk$education3 * uk$brandBUDWEISER
uk$`education3_brandCARLSBERG` <- uk$education3 * uk$brandCARLSBERG
uk$`education3_brandFOSTER'S` <- uk$education3 * uk$`brandFOSTER'S`
uk$`education3_brandSTELLAARTOIS` <- uk$education3 * uk$`brandSTELLA ARTOIS`
uk$`education3_brandCARLING` <- uk$education3 * uk$brandCARLING
uk$`education3_brandSANMIGUEL` <- uk$education3 * uk$`brandSAN MIGUEL`
uk$`education3_brandCARLSBERGEXPORT` <- uk$education3 * uk$`brandCARLSBERG EXPORT`
uk$`education3_brandHEINEKEN` <- uk$education3 * uk$brandHEINEKEN
uk$`education3_brandPERONINASTROAZZURRO` <- uk$education3 * uk$`brandPERONI NASTRO AZZURRO`
uk$`education3_brandPRIVATELABEL` <- uk$education3 * uk$`brandPRIVATE LABEL`
###################################################################################################################

# Creating Variable Matrix ####
print(any(is.na(uk))) #Check if there are any NAs in the data frame. If no NAs, will print FALSE.
# var_matrix <- uk[,c(101, 6:96, 157:260)] #Check these
uk <- uk[order(uk$RespID, uk$task),]
uk$id <- c(1:nrow(uk))
print(names(uk))
var_matrix <- uk[,c(101)] #include more variables here. Right now, only price_perch included.
dt <- data.table(uk)
dt <- dt[,list(id, RespID, task, weight, nest)]
###############################

# llf - loglikelihood function ####
llf <- function(par) {
  lambda <- par[2:(1+nests)]
  beta <- par[1]
  
  dt$lambda <- lambda[dt$nest]
  
  dt$utility <- (as.matrix(var_matrix) %*% beta)/dt$lambda
  dt$exp_utility <- exp(dt$utility)
  dt <- dt[, sum_exp_utility_nest := sum(exp_utility), by=list(RespID,task,nest)]
  dt$prob_within_nest <- dt$exp_utility / dt$sum_exp_utility_nest
  
  dt$log_sum <- log(dt$sum_exp_utility_nest)
  dt$exp_log_sum <- exp(dt$log_sum * dt$lambda)
  dt$dum <- 1
  dt <- dt[, sum_dum := sum(dum), by=list(RespID,task,nest)]
  dt <- dt[, sum_exp_log_sum := sum(exp_log_sum / sum_dum), by=list(RespID, task)]
  dt$prob_of_nest <- dt$exp_log_sum / dt$sum_exp_log_sum
  
  dt$prob <- dt$prob_within_nest * dt$prob_of_nest
  dt$weighted_log_prob <- dt$weight * log(dt$prob)
  
  loglikelihood <- (dt$weighted_log_prob)
  return(loglikelihood)
}

# llg - loglikelihood gradient ####
llg <- function(par) {
  lambdas <- par[2:(1+nests)]
  beta <- par[1]
  
  dt$lambda <- lambdas[dt$nest]
  
  dt$utility <- (as.matrix(var_matrix) %*% beta)
  dt$exp_utility <- exp(dt$utility/dt$lambda)
  dt <- dt[, sum_exp_utility_nest := sum(exp_utility), by=list(RespID,task,nest)]
  dt$prob_within_nest <- dt$exp_utility / dt$sum_exp_utility_nest
  
  dt$log_sum <- log(dt$sum_exp_utility_nest)
  dt$exp_log_sum <- exp(dt$log_sum * dt$lambda)
  dt$dum <- 1
  dt <- dt[, sum_dum := sum(dum), by=list(RespID,task,nest)]
  dt <- dt[, sum_exp_log_sum := sum(exp_log_sum / sum_dum), by=list(RespID, task)]
  dt$prob_of_nest <- dt$exp_log_sum / dt$sum_exp_log_sum
  
  dt$prob <- dt$prob_within_nest * dt$prob_of_nest
  
  # Beta Gradients ################################################################################################
  temp1 <- data.frame(var_matrix * dt$prob_within_nest)
  temp1 <- cbind(dt$id, dt$RespID, dt$task, dt$nest, temp1)
  names(temp1)[1:4] <- c("id", "RespID", "task", "nest")
  temp1 <- data.table(temp1)
  tp <- temp1[, lapply(.SD, sum), by=list(RespID, task, nest), .SDcols=c(5:ncol(temp1))]
  temp1 <- merge(temp1[,list(id, RespID, task, nest)], tp, by=c("RespID","task","nest"))
  temp1 <- data.frame(temp1)
  temp1 <- temp1[,c(4:ncol(temp1))]
  temp1 <- temp1[order(temp1$id),]
  temp1$id <- NULL
  
  temp2 <- data.frame(var_matrix * dt$prob)
  temp2 <- cbind(dt$id, dt$RespID, dt$task, temp2)
  names(temp2)[1:3] <- c("id", "RespID", "task")
  temp2 <- data.table(temp2)
  tp <- temp2[, lapply(.SD, sum), by=list(RespID, task), .SDcols=c(4:ncol(temp2))]
  temp2 <- merge(temp2[,list(id, RespID, task)], tp, by=c("RespID","task"))
  temp2 <- data.frame(temp2)
  temp2 <- temp2[,c(3:ncol(temp2))]
  temp2 <- temp2[order(temp2$id),]
  temp2$id <- NULL
  
  temp <- (var_matrix/dt$lambda) + ((1-(1/dt$lambda))*temp1) - temp2
  temp <- dt$weight * temp
  grad_beta <- temp
  
  rm(temp1, temp2, tp, temp) 
  #################################################################################################################
  
  # Lambda Gradients ##############################################################################################
  dt <- dt[, sum_weighted_prob_nest := sum(prob_within_nest*utility), by=list(RespID,task,nest)]
  dt$common <- dt$prob_of_nest*((1/dt$lambda)*dt$sum_weighted_prob_nest - dt$log_sum)
  
  # Copy and paste these lines. Replace @ by appropriate number (for each nest).
  # dt$common@ <- ifelse(dt$nest == @, dt$common, 0)
  # dt <- dt[, common@ := sum(common@/sum_dum), by=list(RespID, task)]
  # dt$grad_lambda_@ <- ifelse(dt$nest == @, dt$common@ - (1/(dt$lambda*dt$lambda))*(dt$utility - dt$sum_weighted_prob_nest) + dt$log_sum - (1/dt$lambda)*dt$sum_weighted_prob_nest, dt$common@)
  # dt$grad_lambda_@ <- dt$weight * dt$grad_lambda_@
  ###############################################################
  
  dt$common1 <- ifelse(dt$nest == 1, dt$common, 0)
  dt <- dt[, common1 := sum(common1/sum_dum), by=list(RespID, task)]
  dt$grad_lambda_1 <- ifelse(dt$nest == 1, dt$common1 - (1/(dt$lambda*dt$lambda))*(dt$utility - dt$sum_weighted_prob_nest) + dt$log_sum - (1/dt$lambda)*dt$sum_weighted_prob_nest, dt$common1)
  dt$grad_lambda_1 <- dt$weight * dt$grad_lambda_1
  
  dt$common2 <- ifelse(dt$nest == 2, dt$common, 0)
  dt <- dt[, common2 := sum(common2/sum_dum), by=list(RespID, task)]
  dt$grad_lambda_2 <- ifelse(dt$nest == 2, dt$common2 - (1/(dt$lambda*dt$lambda))*(dt$utility - dt$sum_weighted_prob_nest) + dt$log_sum - (1/dt$lambda)*dt$sum_weighted_prob_nest, dt$common2)
  dt$grad_lambda_2 <- dt$weight * dt$grad_lambda_2
  
  dt$common3 <- ifelse(dt$nest == 3, dt$common, 0)
  dt <- dt[, common3 := sum(common3/sum_dum), by=list(RespID, task)]
  dt$grad_lambda_3 <- ifelse(dt$nest == 3, dt$common3 - (1/(dt$lambda*dt$lambda))*(dt$utility - dt$sum_weighted_prob_nest) + dt$log_sum - (1/dt$lambda)*dt$sum_weighted_prob_nest, dt$common3)
  dt$grad_lambda_3 <- dt$weight * dt$grad_lambda_3
  
  dt$common4 <- ifelse(dt$nest == 4, dt$common, 0)
  dt <- dt[, common4 := sum(common4/sum_dum), by=list(RespID, task)]
  dt$grad_lambda_4 <- ifelse(dt$nest == 4, dt$common4 - (1/(dt$lambda*dt$lambda))*(dt$utility - dt$sum_weighted_prob_nest) + dt$log_sum - (1/dt$lambda)*dt$sum_weighted_prob_nest, dt$common4)
  dt$grad_lambda_4 <- dt$weight * dt$grad_lambda_4
  
  dt$common5 <- ifelse(dt$nest == 5, dt$common, 0)
  dt <- dt[, common5 := sum(common5/sum_dum), by=list(RespID, task)]
  dt$grad_lambda_5 <- ifelse(dt$nest == 5, dt$common5 - (1/(dt$lambda*dt$lambda))*(dt$utility - dt$sum_weighted_prob_nest) + dt$log_sum - (1/dt$lambda)*dt$sum_weighted_prob_nest, dt$common5)
  dt$grad_lambda_5 <- dt$weight * dt$grad_lambda_5
  
  dt$common6 <- ifelse(dt$nest == 6, dt$common, 0)
  dt <- dt[, common6 := sum(common6/sum_dum), by=list(RespID, task)]
  dt$grad_lambda_6 <- ifelse(dt$nest == 6, dt$common6 - (1/(dt$lambda*dt$lambda))*(dt$utility - dt$sum_weighted_prob_nest) + dt$log_sum - (1/dt$lambda)*dt$sum_weighted_prob_nest, dt$common6)
  dt$grad_lambda_6 <- dt$weight * dt$grad_lambda_6
  
  dt$common7 <- ifelse(dt$nest == 7, dt$common, 0)
  dt <- dt[, common7 := sum(common7/sum_dum), by=list(RespID, task)]
  dt$grad_lambda_7 <- ifelse(dt$nest == 7, dt$common7 - (1/(dt$lambda*dt$lambda))*(dt$utility - dt$sum_weighted_prob_nest) + dt$log_sum - (1/dt$lambda)*dt$sum_weighted_prob_nest, dt$common7)
  dt$grad_lambda_7 <- dt$weight * dt$grad_lambda_7
  
  dt$common8 <- ifelse(dt$nest == 8, dt$common, 0)
  dt <- dt[, common8 := sum(common8/sum_dum), by=list(RespID, task)]
  dt$grad_lambda_8 <- ifelse(dt$nest == 8, dt$common8 - (1/(dt$lambda*dt$lambda))*(dt$utility - dt$sum_weighted_prob_nest) + dt$log_sum - (1/dt$lambda)*dt$sum_weighted_prob_nest, dt$common8)
  dt$grad_lambda_8 <- dt$weight * dt$grad_lambda_8
  
  dt$common9 <- ifelse(dt$nest == 9, dt$common, 0)
  dt <- dt[, common9 := sum(common9/sum_dum), by=list(RespID, task)]
  dt$grad_lambda_9 <- ifelse(dt$nest == 9, dt$common9 - (1/(dt$lambda*dt$lambda))*(dt$utility - dt$sum_weighted_prob_nest) + dt$log_sum - (1/dt$lambda)*dt$sum_weighted_prob_nest, dt$common9)
  dt$grad_lambda_9 <- dt$weight * dt$grad_lambda_9
  
  dt$common10 <- ifelse(dt$nest == 10, dt$common, 0)
  dt <- dt[, common10 := sum(common10/sum_dum), by=list(RespID, task)]
  dt$grad_lambda_10 <- ifelse(dt$nest == 10, dt$common10 - (1/(dt$lambda*dt$lambda))*(dt$utility - dt$sum_weighted_prob_nest) + dt$log_sum - (1/dt$lambda)*dt$sum_weighted_prob_nest, dt$common10)
  dt$grad_lambda_10 <- dt$weight * dt$grad_lambda_10
  
  dt$common11 <- ifelse(dt$nest == 11, dt$common, 0)
  dt <- dt[, common11 := sum(common11/sum_dum), by=list(RespID, task)]
  dt$grad_lambda_11 <- ifelse(dt$nest == 11, dt$common11 - (1/(dt$lambda*dt$lambda))*(dt$utility - dt$sum_weighted_prob_nest) + dt$log_sum - (1/dt$lambda)*dt$sum_weighted_prob_nest, dt$common11)
  dt$grad_lambda_11 <- dt$weight * dt$grad_lambda_11
  
  grad_lambda <- cbind(dt$grad_lambda_1,
                       dt$grad_lambda_2,
                       dt$grad_lambda_3,
                       dt$grad_lambda_4,
                       dt$grad_lambda_5,
                       dt$grad_lambda_6,
                       dt$grad_lambda_7,
                       dt$grad_lambda_8,
                       dt$grad_lambda_9,
                       dt$grad_lambda_10,
                       dt$grad_lambda_11)
  #################################################################################################################

  grad <- cbind(grad_beta, grad_lambda)
  return(grad)
}

# Verify llg & maxLik ####
params <- c(-0.4, sample(c(1:9)/10,11,replace = T))
numericalApprox <- numDeriv::jacobian(llf, params, method = "Richardson")
print(max(abs(numericalApprox - llg(params)))) #should be in ~e-10
ml <- maxLik(llf, start = params, method = "bfgs", print.level = 3, grad = llg, finalHessian = FALSE)
print(summary(ml))
