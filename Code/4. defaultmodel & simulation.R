# import packages
library('data.table')
library('readr')
library('survival')
library("ggplot2")
library("pROC")
library(ggfortify)
library(dplyr)
library(pammtools)

# load the training dataset with time varying variables
setwd("/Users/yifei/Documents/Yifei_Stanford/Winter_2/MS&E 246/Project/Data")
df <- read_csv("time_varing_training_dataset.csv")
df <- subset(df, select = -c(BorrState, ...1, GrossApproval, ApprovalDate, LoanStatus, ChargeOffDate, GrossChargeOffAmount, end_date,quarter_index,ApprovalYear, EndYear, IndustryGDP,indicator_IndustryGDP,sub_NaicsCode,indicator_NaicsCode, indicator_PersonalIncome,indicator_UnemploymentRate))
df <- na.omit(df)
# Take log when necessary
meanTIM <- mean(df$TermInMonths)
sdTIM <- sd(df$TermInMonths)
names(df)[names(df) == 'death'] <- 'default'
names(df)[names(df) == 'S&P500'] <- 'log_SP'
names(df)[names(df) == 'GSP'] <- 'log_GSP'
names(df)[names(df) == 'HPI'] <- 'log_HPI'
names(df)[names(df) == 'PersonalIncome'] <- 'log_PersonalIncome'

df$log_SP <- log(df$log_SP)
df$log_GSP <- log(as.numeric(df$log_GSP))
df$log_PersonalIncome <- log(df$log_PersonalIncome)
df$log_HPI= as.numeric(df$log_HPI)
df<-df[df$log_HPI > 80,]
summary(df$log_HPI)
df$log_HPI= log(as.numeric(df$log_HPI))
# specify factor variables
df$ProjectState = as.factor(df$ProjectState)
df$BusinessType = as.factor(df$BusinessType)
df$sub_zipcode = as.factor(df$sub_zipcode)
df$loan_purpose= as.factor(df$loan_purpose)


df$indicator_Leverage= as.factor(df$indicator_Leverage)
df$indicator_GSP= as.factor(df$indicator_GSP)

df$check <- df$time_end-df$time_start
df$time_end[df$check==0] <- df$time_end[df$check==0]+1
df$default<- as.factor(df$default)

# Data Normalization for df
df$TermInMonths <- (df$TermInMonths-mean(df$TermInMonths))/(0.00000001+sd(df$TermInMonths))
#df$log_amount <- (df$log_amount-mean(df$log_amount))/(0.00000001+sd(df$log_amount))
df$log_SP <- (df$log_SP-mean(df$log_SP))/(0.00000001+sd(df$log_SP))
df$VIX <- (df$VIX-mean(df$VIX))/(0.00000001+sd(df$VIX))
df$TED <- (df$TED-mean(df$TED))/(0.00000001+sd(df$TED))
df$PRIME <- (df$PRIME-mean(df$PRIME))/(0.00000001+sd(df$PRIME))
df$Leverage <- (df$Leverage-mean(df$Leverage))/(0.00000001+sd(df$Leverage))
#df$log_GSP <- (df$log_GSP-mean(df$log_GSP))/(0.00000001+sd(df$log_GSP))


#split data set
df_id <- unique(df$id)
sample_size <- floor(0.1*length(df_id))
sampled_id <- sample(df_id, sample_size)
df_out <- df[df$id %in% sampled_id,]
df <- df[!(df$id %in% sampled_id),]


# load the training dataset WITHOUT time varying variables
setwd("/Users/yifei/Documents/Yifei_Stanford/Winter_2/MS&E 246/Project/Data")
dfori <- read_csv("time_independent_training_dataset.csv")
dfori <- subset(dfori, select = -c(BorrState, ...1, GrossApproval, ApprovalDate, LoanStatus, ChargeOffDate, GrossChargeOffAmount, end_date,ApprovalYear, EndYear, IndustryGDP,indicator_IndustryGDP,sub_NaicsCode,indicator_NaicsCode, indicator_PersonalIncome,indicator_UnemploymentRate))

dfori$GSP <- log(dfori$GSP)
dfori$PersonalIncome <- log(dfori$PersonalIncome)
names(dfori)[names(dfori) == 'death'] <- 'default'
names(dfori)[names(dfori) == 'S&P500'] <- 'log_SP'
names(dfori)[names(dfori) == 'GSP'] <- 'log_GSP'
names(dfori)[names(dfori) == 'PersonalIncome'] <- 'log_PersonalIncome'
dfori$log_SP <- log(dfori$log_SP)
dfori$ProjectState = as.factor(dfori$ProjectState)
dfori$BusinessType = as.factor(dfori$BusinessType)
dfori$sub_zipcode = as.factor(dfori$sub_zipcode)
dfori$loan_purpose= as.factor(dfori$loan_purpose)
dfori$is_Same_Borr_CDC= as.factor(dfori$is_Same_Borr_CDC)
dfori$is_Same_Borr_Project= as.factor(dfori$is_Same_Borr_Project)
#dfori$indicator_UnemploymentRate= as.factor(dfori$indicator_UnemploymentRate)
dfori$indicator_Leverage= as.factor(dfori$indicator_Leverage)
#dfori$indicator_PersonalIncome= as.factor(dfori$indicator_PersonalIncome)
dfori$indicator_GSP= as.factor(dfori$indicator_GSP)
#dfori$HPI= log(dfori$HPI)
#dforinew <-dfori[dfori$check>=0,]
#dfori$default<- as.factor(dfori$default)

# Data Normalization for dfori
dfori$TermInMonths <- (dfori$TermInMonths-mean(dfori$TermInMonths))/(0.00000001+sd(dfori$TermInMonths))
dfori$log_amount <- (dfori$log_amount-mean(dfori$log_amount))/(0.00000001+sd(dfori$log_amount))
dfori$log_SP <- (dfori$log_SP-mean(dfori$log_SP))/(0.00000001+sd(dfori$log_SP))
dfori$VIX <- (dfori$VIX-mean(dfori$VIX))/(0.00000001+sd(dfori$VIX))
dfori$TED <- (dfori$TED-mean(dfori$TED))/(0.00000001+sd(dfori$TED))
dfori$PRIME <- (dfori$PRIME-mean(dfori$PRIME))/(0.00000001+sd(dfori$PRIME))
dfori$Leverage <- (dfori$Leverage-mean(dfori$Leverage))/(0.00000001+sd(dfori$Leverage))
dfori$log_GSP <- (dfori$log_GSP-mean(dfori$log_GSP))/(0.00000001+sd(dfori$log_GSP))
dfori$log_PersonalIncome <- (dfori$log_PersonalIncome-mean(dfori$log_PersonalIncome))/(0.00000001+sd(dfori$log_PersonalIncome))
dfori$UnemploymentRate <- (dfori$UnemploymentRate-mean(dfori$UnemploymentRate))/(0.00000001+sd(dfori$UnemploymentRate))

dfori$censor <- !(dfori$status ==0)
fit_null <-coxph(Surv(time, censor)~1,data= dfori)
dfori$status <- as.factor(dfori$status)
#cox.zph(fit_null)
residual <- residuals(fit_null,  type= 'martingale')
plot(residual)
#plot(dfori$TED, residual, xlab= "TED", ylab='Residual', col = dfori$status == 0)
ggplot(dfori,aes(x=end_day,y=residual,group=censor))+geom_point(aes(color=censor))+ scale_color_discrete(name="censored",labels=c("True","False"))
ggplot(dfori,aes(x=TED,y=residual,group=censor))+geom_point(aes(color=censor))+ scale_color_discrete(name="censored",labels=c("True","False"))
ggplot(dfori,aes(x=TermInMonths,y=residual,group=censor))+geom_point(aes(color=censor))+ scale_color_discrete(name="censored",labels=c("True","False"))
ggplot(dfori,aes(x=log_amount,y=residual,group=censor))+geom_point(aes(color=censor))+ scale_color_discrete(name="censored",labels=c("True","False"))
ggplot(dfori,aes(x=log_SP,y=residual,group=censor))+geom_point(aes(color=censor))+ scale_color_discrete(name="censored",labels=c("True","False"))
ggplot(dfori,aes(x=VIX,y=residual,group=censor))+geom_point(aes(color=censor))+ scale_color_discrete(name="censored",labels=c("True","False"))
ggplot(dfori,aes(x=PRIME,y=residual,group=censor))+geom_point(aes(color=censor))+ scale_color_discrete(name="censored",labels=c("True","False"))
ggplot(dfori,aes(x=Leverage,y=residual,group=censor))+geom_point(aes(color=censor))+ scale_color_discrete(name="censored",labels=c("True","False"))

ggplot(dfori,aes(x=log_GSP,y=residual,group=censor))+geom_point(aes(color=censor))+ scale_color_discrete(name="censored",labels=c("True","False"))
ggplot(dfori,aes(x=log_PersonalIncome,y=residual,group=censor))+geom_point(aes(color=censor))+ scale_color_discrete(name="censored",labels=c("True","False"))
ggplot(dfori,aes(x=UnemploymentRate,y=residual,group=censor))+geom_point(aes(color=censor))+ scale_color_discrete(name="censored",labels=c("True","False"))
#ggplot(dfori,aes(x=GSP,y=residual,group=censor))+geom_point(aes(color=censor))
summary(dfori$TermInMonths[dfori$censor==0])
summary(dfori$log_SP[dfori$censor==0])
summary(dfori$VIX[dfori$censor==0])
summary(dfori$Leverage[dfori$censor==0])

# preliminary model fit
fit_ori <- coxph(Surv(time, status==2)~log_amount+ TED+TermInMonths+log_amount+log_SP+VIX+PRIME+Leverage+BusinessType+is_Same_Borr_CDC+is_Same_Borr_Project+loan_purpose,data= dfori, ties = "efron", id = id)
print(fit_ori)

# test proportional hazard hypothesis
cox.zph(fit_ori)
survOri <- survfit(fit_ori)
plot(survOri,ylim = c(0.88,1.0))
autoplot(survOri, surv.colour = 'orange', censor.colour = 'black')

# fit the default model
fit <- coxph(Surv(time_start, time_end, default==2)~ pspline(TermInMonths)+UnemploymentRate+log_HPI+is_Same_Borr_CDC+is_Same_Borr_Project+BusinessType+log_amount+ridge(log_GSP)+cluster(id), data = df, id=id, x = TRUE)
print(fit)

df_K <- na.omit(df)
df_K$TermInMonths <- ((0.00000001+sdTIM)*df_K$TermInMonths)+meanTIM
df_K$status <- as.factor(df_K$status)
dfbetas <- residuals(fit,  type= 'dfbetas')


# Influences analyses
ggplot(df_K,aes(x=UnemploymentRate,y=dfbetas[,13],group=status))+geom_point(aes(color=status))
ggplot(df_K,aes(x=log_HPI,y=dfbetas[,14],group=status))+geom_point(aes(color=status))
ggplot(df_K,aes(x=log_amount,y=dfbetas[,19],group=status))+geom_point(aes(color=status))
ggplot(df_K,aes(x=log_GSP,y=dfbetas[,20],group=status))+geom_point(aes(color=status))

# compute and plot base hazard
baseH <- basehaz(fit)
plot(baseH)
ggplot(baseH, aes(x = time, y = hazard)) +
  geom_stephazard() +
  ylab(expression(hat(Lambda)(t))) + xlab("t") +
  ggtitle("Nelson-Aalen estimate of the cumulative hazard")
# compute and plot the cumulative hazard
surv <- survfit(fit)
autoplot(surv, surv.colour = 'orange', censor.colour = 'black')

# in_sample_fit
sf_in <- survfit(fit, newdata = df,id = id)
res_in <- broom::tidy(sf_in)
res_in <- cbind(res_in, sf_in$cumhaz)
id_in <- unique(res_in$strata)
prob_in <- c()
strata_in <- c()
def_in <- c()
curr_s <- res_in$strata[1]
for (i in 2:dim(res_in)[1]){
  now <- res_in$strata[i]
  if (now != curr_s){
    prob_in<- append(prob_in, res_in$estimate[i-1])
    strata_in <- append(strata_in, curr_s)
  }
  curr_s <- now
}

def_in <- as.numeric(dfori$status)
def_in <- def_in[as.numeric(strata_in)+1]
def_in <- def_in == 3
roc_in <- roc(def_in, prob_in)

# plot in-sample ROC
plot.roc(def_in, prob_in)
print(auc(roc_in))

# out_sample_fit
sf_out <- survfit(fit, newdata = df_out,id = id)
res_out <- broom::tidy(sf_out)
res_out <- cbind(res_out, sf_out$cumhaz)
id_out <- unique(res_out$strata)

prob_out <- c()
strata_out <- c()
def_out <- c()
curr_s <- res_out$strata[1]
for (i in 2:dim(res_out)[1]){
  now <- res_out$strata[i]
  if (now != curr_s){
    prob_out<- append(prob_out, res_out$estimate[i-1])
    strata_out <- append(strata_out, curr_s)
  }
  curr_s <- now
}

def_out <- as.numeric(dfori$status)
def_out <- def_out[as.numeric(strata_out)+1]
def_out <- def_out == 3
roc_out <- roc(def_out, prob_out)

# plot out-of-sample ROC
plot.roc(def_out, prob_out,xlim = c(1,0), ylim = c(0,1))
print(auc(roc_out))


# simulations
library(readr)

setwd("/Users/yifei/Documents/Yifei_Stanford/Winter_2/MS&E 246/simulation")
df0 <- read.csv("time_varing_portfolio_500_loans.csv")
df0 <- df0[!duplicated(df0["id"]),]
df_fixed <- subset(df0,select = c(id,GrossApproval,TermInMonths,ProjectState,BusinessType,
                                  is_Same_Borr_CDC,is_Same_Borr_Project,
                                  loan_purpose,sub_zipcode,sub_NaicsCode))
StateName <- df_fixed$ProjectState
Zipcode <- df_fixed$sub_zipcode
Naicscode <- df_fixed$sub_NaicsCode
Time <- 90*(1:40)

fixed_var <- read.csv("extra_data/FixedVariables.csv")
lev <- read.csv("extra_data/Leverage.csv")

gsp <- read.csv("extra_data/GSP_Statelevel.csv")
inc <- read.csv("extra_data/PersonalIncome_StateLevel.csv")
une <- read.csv("extra_data/UnemploymentRate_StateLevel.csv")
ind <- read.csv("extra_data/IndustryGDP_97-14.csv")
hpi <- read.csv("extra_data/HPI.csv")

sample_from_data_diff <- function(x){
  x <- x[!is.na(x)]; n <- length(x);  y <- c()
  y[1] <- x[sample.int(n,1)]
  for (i in 2:40){j <- sample.int(n-1,1); y[i] <- abs(y[i-1] + (x[j+1] - x[j]))}
  return(y)
}

sample_from_data_random <- function(x){
  x <- x[!is.na(x)]; n <- length(x);  y <- c()
  for (i in 1:40){y[i] <- x[sample.int(n,1)]}
  return(y)
}

sample_from_data_fix <- function(x,ratio){
  y <- c(); y[1] <- x[61]
  for (i in 2:40){
    if(x[60 + i] - x[60 + i - 1] > 0){
      U <- 1 + ratio + ratio*0.20*(runif(1)-0.5)
    }
    else{
      U <- 1 - ratio + ratio*0.20*(runif(1)-0.5)
    }
    
    y[i] <- abs((x[60 + i] - x[60 + i - 1])*U + y[i-1])
  }
  return(y)
}


sample_fixed <- function(fixed_var,lev,ratio){
  df_varying <- data.frame(days = Time)
  df_varying["SP500"] <- sample_from_data_fix(fixed_var$S.P500,ratio)
  df_varying["VIX"] <- sample_from_data_fix(fixed_var$VIX,-ratio)
  df_varying["TED"] <- sample_from_data_fix(fixed_var$TED,ratio)
  df_varying["PRIME"] <- sample_from_data_fix(fixed_var$PRIME,ratio)
  df_varying["Leverage"] <- sample_from_data_fix(lev$Leverage,-ratio)
  return(df_varying)
}

sample_others <- function(Naics,Zip,State,gsp,inc,une,ind,hpi,ratio){
  df_others <- data.frame(days = Time)
  #df_others <- data.frame()
  df_others["GSP"] <- sample_from_data_fix(gsp[State][,1],ratio)
  df_others["PersonalIncome"] <- sample_from_data_fix(inc[State][,1],-ratio)
  df_others["UnemploymentRate"] <- sample_from_data_fix(une[State][,1],-ratio)
  df_others["IndustryGDP"] <- sample_from_data_fix(ind[Naics][,1],ratio)
  if (! Zip %in% names(hpi)){df_others["HPI"] <- sample_from_data_fix(hpi["X010"][,1],ratio)}
  else{df_others["HPI"] <- sample_from_data_fix(hpi[Zip][,1],ratio)}
  return(df_others)
}

preprocessing_predict <- function(j,df_fixed,df_varying,df_others,term_mean,term_var){
  df_k_j <- data.frame(); df_k_j <- rbind(df_k_j,df_varying)
  df_k_j["time_start"] = df_k_j$days - 90; df_k_j["time_end"] = df_k_j$days
  df_k_j <- subset(df_k_j, select = -c(days))
  df_k_j <- cbind(df_k_j, subset(df_others, select = -c(days)))
  df_k_j["log_PersonalIncome"] <- log(df_k_j["PersonalIncome"])
  df_k_j["log_GSP"] <- log(df_k_j["GSP"])
  df_k_j["log_HPI"] <- log(df_k_j["HPI"])
  df_k_j = merge(df_k_j,df_fixed[j,])
  if (runif(1)>0.90){df_k_j["GrossApproval"] = df_k_j["GrossApproval"]*10}
  df_k_j["log_amount"] <- log(df_k_j["GrossApproval"])
  df_k_j["TermInMonths"] <- (df_k_j["TermInMonths"]-term_mean)/(0.00000001+term_var)
  return(df_k_j)
}


# Here please compute the correct term_mean and term_var
term_mean = mean(df_fixed$TermInMonths)
term_var = sd(df_fixed$TermInMonths)
term_mean = meanTIM
term_var = sdTIM
#

ratio = runif(1)*0.5 - 0.25

df_varying <- sample_fixed(fixed_var,lev,ratio)
j = 1
Naics <- sprintf("X%02d",df_fixed$sub_NaicsCode[j])
Zip <- sprintf("X%03d",df_fixed$sub_zipcode[j])
State <- df_fixed$ProjectState[j]
df_others <- sample_others(Naics,Zip,State,gsp,inc,une,ind,hpi,ratio)

K <- 2000
N <- 500
max_ratio <- 0.25
min_ratio <- -0.6
df_pass_5yr <- data.frame()
df_pass_10yr <- data.frame()
for (k in 1:K){
  ratio <- runif(1)*(max_ratio-min_ratio) + min_ratio
  cat(k, "\n")
  df_varying <- sample_fixed(fixed_var,lev,ratio)
  df_k <- data.frame()
  for (j in 1:N){
    Naics <- sprintf("X%02d",df_fixed$sub_NaicsCode[j])
    Zip <- sprintf("X%03d",df_fixed$sub_zipcode[j])
    State <- df_fixed$ProjectState[j]
    df_others <- sample_others(Naics,Zip,State,gsp,inc,une,ind,hpi,ratio)
    
    # combine df_fixed[j] df_varying, df_others together; add time_start, time_end
    df_k_j <- preprocessing_predict(j,df_fixed,df_varying,df_others,term_mean,term_var)
    df_k <- rbind(df_k, df_k_j)
    
  }
  
  
  # compute default_time T_j, return default_status for simulation k, in quarter; 
  
  #if (runif(1) > 0.97){default_status <- TRUE} else{default_status <-FALSE}
  #T_j = max(1,min((40*rweibull(1,2))%/%1.4,40))
  df_k$default <- 0
  df_predict <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df_predict) <- c("default5", "default_time", "index")
  sfc <- survfit(fit_sim, newdata = df_k, id = id, ctype=2)
  resultsc <- broom::tidy(sfc)
  ids <- unique(resultsc$strata)
  resultsc <- as.data.frame(cbind(resultsc$estimate,resultsc$strata, sfc$cumhaz))
  colnames(resultsc) <- c("estimate", "strata", "haz")
  index = 1
  for (i in ids){
    U <- runif(1); check <- resultsc$estimate[resultsc$strata==i]
    y5 <- check[length(check)/2]; y10 <- check[length(check)]
    if(y10<U){
      # if default at year 10
      y5d <- FALSE
      if(y5<U){
        y5d <- TRUE
      }
      for (c in 1:length(check)){
        if (check[c] < U){
          default_time <- c * 13 
          break
        }
      }
      df_predict <- rbind(df_predict, c(index, y5d, default_time))
    }
    index <- index+1
  }
  df_others_c <- subset(df_k,select = c(GSP,PersonalIncome,UnemploymentRate,IndustryGDP,HPI))
  for (i in 1:dim(df_predict)[1]){
    j <- df_predict[i,1]; T_j <- max(1, min(df_predict[i,3]%/%90,40))
    new_data = cbind(data.frame("simulation"=k), df_fixed[j,], 
                     df_varying[T_j,], df_others_c[40*j+T_j,])
    df_pass_10yr = rbind(df_pass_10yr,new_data)
    if (df_predict[i,2]){
      df_pass_5yr = rbind(df_pass_5yr,new_data)
    }
  }
  for (j in 1:N){
    if ((!j %in% df_predict[,1])&(runif(1) > 0.96)){
      T_j <- max(12,min((40*rweibull(1,2))%/%1.4,40))
      new_data = cbind(data.frame("simulation"=k), df_fixed[j,], 
                       df_varying[T_j,], df_others_c[40*j+T_j,])
      df_pass_10yr = rbind(df_pass_10yr,new_data)
      if (T_j <= 20){
        df_pass_5yr = rbind(df_pass_5yr,new_data)
      }
    }
  }
  #print(default_status)
  
}

colnames(df_pass_5yr)[which(names(df_pass_5yr) == "id")] <- "loan"
colnames(df_pass_5yr)[which(names(df_pass_5yr) == "days")] <- "age"
colnames(df_pass_10yr)[which(names(df_pass_10yr) == "id")] <- "loan"
colnames(df_pass_10yr)[which(names(df_pass_10yr) == "days")] <- "age"
write.csv(df_pass_5yr,"Predict500_5yr.csv")
write.csv(df_pass_10yr,"Predict500_10yr.csv")
