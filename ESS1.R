library(dplyr)
library(lavaan)
library(tidyr)
library(ggplot2)
library(mmgsem)


#library(devtools)
#library(usethis)
#library(gitcreds)
#gitcreds_set()
#devtools::install_github("AndresFPA/mmgsem")

###################################################################################
####################### Data Management ###########################################
###################################################################################

##Read the data in:
ESS1<-read.csv("./ESS1.csv")

##Select the 19 countries in Davidov et el. (2008)'s paper
ESS1<-ESS1 %>%
  filter(cntry == "AT" | cntry == "BE" | cntry == "CZ" | cntry == "DK" | 
           cntry == "FI" | cntry == "FR" | cntry == "DE" | cntry == "GB" | 
           cntry == "GR" | cntry == "HU" | cntry == "IE" | cntry == "NL" | 
           cntry == "NO" | cntry == "PL" | cntry == "PT" | cntry == "SI" | 
           cntry == "ES" | cntry == "SE" | cntry == "CH" )

##Select the items needed for the analysis
ESS1<-ESS1 %>%
  select(idno, cntry, ##grouping variable
         trstep, trstlgl, trstplc, trstprl, trstplt, ##political trust
         imdfetn, eimpcnt, imrcntr, impcntr, ##Reject items
         imbgeco, imueclt, imwbcnt, ##Perceived Ethnic Threat items
         qfimedu, qfimwsk, ##Conditions for Immigrants items
         ipeqopt, ipudrst, impenv, iphlppl, iplylfr, ##Self-Transcendence Value
         ipmodst, imptrad, ipfrule, ipbhprp, impsafe, ipstrgv) ##Conservation Value

##Handle missing values for Reject items:
#
#7,8,9 should be NA and store in new columns called R1, R2, R3, R4
ESS1[,c("R1", "R2", "R3", "R4")]<-lapply(ESS1[,c("imdfetn", "eimpcnt", "imrcntr", "impcntr")],
                                                             function(x) ifelse(x %in% c(7,8,9),NA,x))

##Handle missing values & Reverse coded for Perceived Ethnic Threat Item
#
##77,88,99 should be NA and store in new columns called T1, T2, T3
ESS1[,c("T1", "T2", "T3")]<-lapply(ESS1[,c("imbgeco", "imueclt", "imwbcnt")],
                                   function(x) ifelse(x %in% c(77,88,99),NA,x))
#
##Reverse coding:
ESS1[,c("T1","T2","T3")]<-lapply(ESS1[,c("T1","T2","T3")],
                                function(x) -x+10)

##Handle missing values for Conditions for Immigrants Item
#
##77,88,99 should be NA and store in new columns called Condition1 and Condition2
ESS1[,c("Condition1", "Condition2")]<-lapply(ESS1[,c("qfimedu", "qfimwsk")],
                                             function(x) ifelse(x %in% c(77,88,99),NA,x))

##Handle missing values & Reverse coded for Self-Transcendence Value items
#
##7,8,9 should be NA and store in new columns S1, S2, S3, S4, S5
ESS1[,c("S1", "S2", "S3", "S4", "S5")]<-lapply(ESS1[,c("ipeqopt", "ipudrst", "impenv", "iphlppl", "iplylfr")],
                                               function(x) ifelse(x %in% c(7,8,9),NA,x))
#
##Reverse the scale
ESS1[,c("S1", "S2", "S3", "S4", "S5")]<-lapply(ESS1[,c("S1", "S2", "S3", "S4", "S5")],
                                               function(x) -x+7)

##Handle missing values & Reverse coded for Conservation Value items
ESS1[,c("C1", "C2", "C3", "C4", "C5", "C6")]<-lapply(ESS1[,c("ipmodst", "imptrad", "ipfrule", "ipbhprp", "impsafe", "ipstrgv")],
                                                     function(x) ifelse(x %in% c(7,8,9),NA,x))
#
##Reverse the Scale
ESS1[,c("C1", "C2", "C3", "C4", "C5", "C6")]<-lapply(ESS1[,c("C1", "C2", "C3", "C4", "C5", "C6")],
                                                     function(x) -x+7)


##Handle missing values for political trust items
#
##define 77,88,99 as NA
ESS1[,c("P1","P2","P3","P4", "P5")]<-lapply(ESS1[,c("trstep","trstlgl","trstplc","trstprl", "trstplt")], 
                                            function(x) ifelse(x %in% c(77,88,99), NA, x))

###################################################################################
############## Covariance matrix invariance testing ###############################
###################################################################################

ESS1 %>% filter(cntry == "AT") %>%
  select(S1, S2, S3, S4, S5, C1, C2, C3, C4, C5, C6, 
         R1, R2, R3, R4, T1, T2, T3) %>%
  cov(use = "pairwise.complete.obs", method = "pearson")

ESS1 %>% filter(cntry == "BE") %>%
  select(S1, S2, S3, S4, S5, C1, C2, C3, C4, C5, C6, 
         R1, R2, R3, R4, T1, T2, T3) %>%
  cov(use = "pairwise.complete.obs", method = "pearson")


###################################################################################
####################### Measurement Model #########################################
###################################################################################

######-------------------------------------------------------------------------------
##Measurement Block 1: Human Values (Self-Transcendence & Conservation)

#Configural Invariance Model 1: 
HV.Config.M1<-'
SelfTran=~S1+S2+S3+S4+S5
Conser=~C1+C2+C3+C4+C5+C6
'

HV.Config.fit1<-cfa(model = HV.Config.M1,
                 data = ESS1,
                 group = "cntry",
                 estimator="MLR",
                 missing="FIML",
                 std.lv=T)

sink("./Sink Output/ESS1/HV_Config_fit1.txt")
summary(HV.Config.fit1, fit.measures=T, standardized=T)
sink()

##inspect the modification indices for the configural model 1
sink("./Sink Output/ESS1/HV_Config_fit1_mi.txt")
options(max.print = 999999)
modindices(HV.Config.fit1, minimum.value = 10, sort. = T)
sink()
##SelfTran =~  C3 crossloading appear in multiple groups modification indices


#Configural Invariance Model 2 (add cross loading of SelfTran=~C3): 
HV.Config.M2<-'
SelfTran=~S1+S2+S3+S4+S5+C3
Conser=~C1+C2+C3+C4+C5+C6
'

HV.Config.fit2<-cfa(model = HV.Config.M2,
                 data = ESS1,
                 group = "cntry",
                 estimator="MLR",
                 missing="FIML",
                 std.lv=T)

sink("./Sink Output/ESS1/HV_Config_fit2.txt")
summary(HV.Config.fit2, fit.measures=T, standardized=T)
sink()

##inspect the modification indices for the configural model 2
sink("./Sink Output/ESS1/HV_Config_fit2_mi.txt")
options(max.print = 999999)
modindices(HV.Config.fit2, minimum.value = 10, sort. = T)
sink()
##C5~~C6 appear in multiple group modification indices


#Configural Invariance Model 3 (add error term correlation of C5~~C6): 
HV.Config.M3<-'
SelfTran=~S1+S2+S3+S4+S5+C3
Conser=~C1+C2+C3+C4+C5+C6

##Error terms correlation
C5~~C6
'

HV.Config.fit3<-cfa(model = HV.Config.M3,
                 data = ESS1,
                 group = "cntry",
                 estimator="MLR",
                 missing="FIML",
                 std.lv=T)

sink("./Sink Output/ESS1/HV_Config_fit3.txt")
summary(HV.Config.fit3, fit.measures=T, standardized=T)
sink()

##inspect the modification indices for the configural model 3
sink("./Sink Output/ESS1/HV_Config_fit3_mi.txt")
options(max.print = 999999)
modindices(HV.Config.fit3, minimum.value = 10, sort. = T)
sink()
##SelfTran =~  C4 appears in multiple group's (high) modification indices


#Configural Invariance Model 4 (add cross loading SelfTran =~  C4): 
HV.Config.M4<-'
SelfTran=~S1+S2+S3+S4+S5+C3+C4
Conser=~C1+C2+C3+C4+C5+C6

##Error terms correlation
C5~~C6
'

HV.Config.fit4<-cfa(model = HV.Config.M4,
                 data = ESS1,
                 group = "cntry",
                 estimator="MLR",
                 missing="FIML",
                 std.lv=T)

sink("./Sink Output/ESS1/HV_Config_fit4.txt")
summary(HV.Config.fit4, fit.measures=T, standardized=T)
sink()

##inspect the modification indices for the configural model 4
sink("./Sink Output/ESS1/HV_Config_fit4_mi.txt")
options(max.print = 999999)
modindices(HV.Config.fit4, minimum.value = 10, sort. = T)
sink()
##error term correlation S1~~S2 taps on universalism 

#Configural Invariance Model 5 (add error term correlation S1~~S2): 
HV.Config.M5<-'
SelfTran=~S1+S2+S3+S4+S5+C3+C4
Conser=~C1+C2+C3+C4+C5+C6

##Error terms correlation
C5~~C6
S1~~S2
'

HV.Config.fit5<-cfa(model = HV.Config.M5,
                 data = ESS1,
                 group = "cntry",
                 estimator="MLR",
                 missing="FIML",
                 std.lv=T)

sink("./Sink Output/ESS1/HV_Config_fit5.txt")
summary(HV.Config.fit5, fit.measures=T, standardized=T)
sink()

##Go for Metric invariance from here

##First a full metric:
HV.FullMetric.M1<-'
SelfTran=~S1+S2+S3+S4+S5+C3+C4
Conser=~C1+C2+C3+C4+C5+C6

##Error terms correlation
C5~~C6
S1~~S2
'

HV.FullMetric.Fit1<-cfa(model = HV.FullMetric.M1,
                          data = ESS1,
                          group = "cntry",
                          estimator="MLR",
                          missing="FIML",
                          group.equal="loadings",
                          std.lv=T)

sink("./Sink Output/ESS1/HV_FullMetric_fit1.txt")
summary(HV.FullMetric.Fit1, fit.measures=T, standardized=T)
sink()

##Check the modification Indices:
sink("./Sink Output/ESS1/HV_FullMetric_fit1_mi.txt")
options(max.print = 999999)
lavTestScore(HV.FullMetric.Fit1, epc = T)
sink()

##Partial Metric
HV.PartialMetric.M2<-'
SelfTran=~S1+S2+S3+S4+S5+C3+C4
Conser=~C1+C2+C3+C4+C5+C6

##Error terms correlation
C5~~C6
S1~~S2
'

HV.PartialMetric.Fit2<-cfa(model = HV.PartialMetric.M2,
                    data = ESS1,
                    group = "cntry",
                    estimator="MLR",
                    missing="FIML",
                    group.equal="loadings",
                    group.partial=c("SelfTran=~C3"),
                    std.lv=T)

sink("./Sink Output/ESS1/HV_PartialMetric_fit2.txt")
summary(HV.PartialMetric.Fit2, fit.measures=T, standardized=T)
sink()

##For the moment, we select the full metric model:
#
##We need to use the marker variable approach to rerun the full metric model, which will be used as the input for MMGSEM
HV.FullMetric.FinalModel<-'
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6

##Error terms correlation
C5~~C6
S1~~S2
'

HV.FullMetric.Final<-cfa(model = HV.FullMetric.FinalModel,
                        data = ESS1,
                        group = "cntry",
                        estimator="MLR",
                        missing="FIML",
                        group.equal="loadings")

sink("./Sink Output/ESS1/HV_FullMetric_Final.txt")
summary(HV.FullMetric.Final, fit.measures=T, standardized=T)
sink()




######-------------------------------------------------------------------------------
##Measurement Block 2: Anti-immigrant Attitude
##Option A: Reject

##Configural Invariance Model 1:
Reject.Config.M1<-'
Reject=~R1+R2+R3+R4
'

Reject.Config.Fit1<-cfa(model = Reject.Config.M1,
                        data = ESS1,
                        group = "cntry",
                        estimator="MLR",
                        missing="FIML",
                        std.lv=T)

sink("./Sink Output/ESS1/Reject_Config_fit1.txt")
summary(Reject.Config.Fit1, fit.measures=T, standardized=T)
sink()

##inspect the modification indices for the configural model 1
sink("./Sink Output/ESS1/Reject_Config_fit1_mi.txt")
options(max.print = 999999)
modindices(Reject.Config.Fit1, minimum.value = 10, sort. = T)
sink()



##Configural Invariance Model 2:
Reject.Config.M2<-'
Reject=~R1+R2+R3+R4

##add error term correlation
R2~~R4
'

Reject.Config.Fit2<-cfa(model = Reject.Config.M2,
                        data = ESS1,
                        group = "cntry",
                        estimator="MLR",
                        missing="FIML",
                        std.lv=T)

sink("./Sink Output/ESS1/Reject_Config_fit2.txt")
summary(Reject.Config.Fit2, fit.measures=T, standardized=T)
sink()


##From here, we impose metric invariance

##Full metric invariance model 1
Reject.Metric.M1<-'
Reject=~R1+R2+R3+R4

##add error term correlation
R2~~R4
'

Reject.Metric.Fit1<-cfa(model = Reject.Metric.M1,
                        data = ESS1,
                        group = "cntry",
                        estimator="MLR",
                        missing="FIML",
                        group.equal="loadings",
                        std.lv=T)

sink("./Sink Output/ESS1/Reject_Metric_fit1.txt")
summary(Reject.Metric.Fit1, fit.measures=T, standardized=T)
sink()

##We will use the Full Metric Invariance model 1 as the final solution
#
##For MMGSEM, we need to rerun this model with the marker variable approach
Reject.FullMetric.FinalModel<-'
Reject=~R1+R2+R3+R4

##add error term correlation
R2~~R4
'

Reject.FullMetric.Final<-cfa(model = Reject.FullMetric.FinalModel,
                        data = ESS1,
                        group = "cntry",
                        estimator="MLR",
                        missing="FIML",
                        group.equal="loadings")

sink("./Sink Output/ESS1/Reject_FullMetric_Final.txt")
summary(Reject.FullMetric.Final, fit.measures=T, standardized=T)
sink()



######-------------------------------------------------------------------------------
##Measurement Block 2: Anti-immigrant Attitude
##Option B: Perceived Ethnic Threat

##Configural Invariance: perfect fit due to df=0

##Impose full metric invariance Model 1:
Threat.Metric.M1<-'
Threat=~T1+T2+T3
'

Threat.Metric.Fit1<-cfa(model = Threat.Metric.M1,
                        data = ESS1,
                        group = "cntry",
                        estimator="MLR",
                        missing="FIML",
                        group.equal="loadings",
                        std.lv=T)

sink("./Sink Output/ESS1/Threat_Metric_fit1.txt")
summary(Threat.Metric.Fit1, fit.measures=T, standardized=T)
sink()

##inspect the modification indices for the metric model 1
sink("./Sink Output/ESS1/Threat_Metric_fit1_mi.txt")
options(max.print = 999999)
lavTestScore(Threat.Metric.Fit1,epc = T)
sink()

##Partial metric invariance Model 2:
Threat.Metric.M2<-'
Threat=~T1+T2+T3
'

Threat.Metric.Fit2<-cfa(model = Threat.Metric.M2,
                        data = ESS1,
                        group = "cntry",
                        estimator="MLR",
                        missing="FIML",
                        group.equal="loadings",
                        group.partial=c("Threat=~T3"),
                        std.lv=T)

sink("./Sink Output/ESS1/Threat_Metric_fit2.txt")
summary(Threat.Metric.Fit2, fit.measures=T, standardized=T)
sink()

##We select the Partial Metric Invariance Model 2 as the final model
##Now we need to rerun the model with the marker variable approach for MMGSEM
Threat.PartialMetric.FinalModel<-'
Threat=~T1+T2+T3
'

Threat.PartialMetric.Final<-cfa(model = Threat.PartialMetric.FinalModel,
                        data = ESS1,
                        group = "cntry",
                        estimator="MLR",
                        missing="FIML",
                        group.equal="loadings",
                        group.partial=c("Threat=~T3"))

sink("./Sink Output/ESS1/Threat_PartialMetric_Final.txt")
summary(Threat.PartialMetric.Final, fit.measures=T, standardized=T)
sink()





###################################################################################
####################### Regular MGSEM ############################################
###################################################################################

#####---------------------------------------------------------------------------------
##Select the HV.Metric.fit2.Alter as the final measurement model for Human Values
##Select the Reject.Metric.Fit1 as the final measurement model for Anti-immigrant Attitudes

##Group 1-3: ##AT#BE#CH
##Group 4-6: ##CZ#DE#DK
##Group 7-9: ##ES#FI#FR
##Group 10-12: ##GB#GR#HU
##Group 13-15: ##IE#NL#NO
##Group 16-18: ##PL#PT#SE
##Group 19: ##SI

HV.Reject.FreeSEM.M1<-'
##Measurement model for human values:
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6

##Error terms correlation
C5~~C6
S1~~S2

##Measurement model for reject:
Reject=~R1+R2+R3+R4

##add error term correlation
R2~~R4

##Structural Part:
Reject~c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19)*SelfTran+ 
          c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19)*Conser
'

HV.Reject.FreeSEM.Fit1<-cfa(model = HV.Reject.FreeSEM.M1,
                            data = ESS1,
                            group = "cntry",
                            estimator="MLR",
                            missing="FIML",
                            group.equal="loadings")

sink("./Sink Output/ESS1/HV_Reject_FreeSEM_fit1.txt")
summary(HV.Reject.FreeSEM.Fit1, fit.measures=T, standardized=T)
sink()

##Extract all the parameters
param<-parameterestimates(HV.Reject.FreeSEM.Fit1, standardized = T)
#
##Extract the coefficients Reject~SelfTran
SelfTran_beta<-param %>%
  filter(lhs == "Reject" & op == "~" & rhs == "SelfTran")
#
##Extract the coefficients Reject ~Conser
Conser_beta<-param %>%
  filter(lhs == "Reject" & op == "~" & rhs == "Conser")
#
##bind the coefficients together and sort by group
HV_beta<-rbind(SelfTran_beta, Conser_beta) %>%
  arrange(group)
#
#Give the country name to each group:
HV_beta<-HV_beta %>%
  mutate(country = case_when(
    group == 1 ~ "AT",
    group == 2 ~ "BE",
    group == 3 ~ "CH",
    group == 4 ~ "CZ",
    group == 5 ~ "DE",
    group == 6 ~ "DK",
    group == 7 ~ "ES",
    group == 8 ~ "FI",
    group == 9 ~ "FR",
    group == 10 ~ "GB",
    group == 11 ~ "GR",
    group == 12 ~ "HU",
    group == 13 ~ "IE",
    group == 14 ~ "NL",
    group == 15 ~ "NO",
    group == 16 ~ "PL",
    group == 17 ~ "PT",
    group == 18 ~ "SE",
    group == 19 ~ "SI"
  )) %>%
  select(rhs, group, est, country)
#
##Make the data wider
HV_beta<-HV_beta %>%
  pivot_wider(names_from = "rhs", values_from = "est")
#
##Plot the coefficients:
ggplot(HV_beta, aes(x=SelfTran, y=Conser))+
  geom_point()+
  geom_text(aes(label = country),vjust=-0.5, hjust=0.5)+
  labs(title = "Regression Coefficients of Human Values on Reject")+
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  theme_minimal()


#####---------------------------------------------------------------------------------
##Select the HV.FullMetric.Final as the final measurement model for Human Values
##Select the Threat.PartialMetric.Final as the final measurement model for Anti-immigrant Attitudes

HV.Threat.FreeSEM.M1<-'
##Measurement model for human values
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6

##Error terms correlation
C5~~C6
S1~~S2

##Measurement model for Threat
Threat=~T1+T2+T3

##Structural Model
Threat~c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19)*SelfTran+ 
          c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19)*Conser
'

HV.Threat.FreeSEM.Fit1<-cfa(model = HV.Threat.FreeSEM.M1,
                            data = ESS1,
                            group = "cntry",
                            estimator="MLR",
                            missing="FIML",
                            group.equal="loadings",
                            group.partial=c("Threat=~T3"))

sink("./Sink Output/ESS1/HV_Threat_FreeSEM_fit1.txt")
summary(HV.Threat.FreeSEM.Fit1, fit.measures=T, standardized=T)
sink()

##Extract all the parameters
param<-parameterestimates(HV.Threat.FreeSEM.Fit1, standardized = T)
#
##Extract the coefficients Reject~SelfTran
SelfTran_beta2<-param %>%
  filter(lhs == "Threat" & op == "~" & rhs == "SelfTran")
#
##Extract the coefficients Reject ~Conser
Conser_beta2<-param %>%
  filter(lhs == "Threat" & op == "~" & rhs == "Conser")
#
##bind the coefficients together and sort by group
HV_beta2<-rbind(SelfTran_beta2, Conser_beta2) %>%
  arrange(group)
#
#Give the country name to each group:
HV_beta2<-HV_beta2 %>%
  mutate(country = case_when(
    group == 1 ~ "AT",
    group == 2 ~ "BE",
    group == 3 ~ "CH",
    group == 4 ~ "CZ",
    group == 5 ~ "DE",
    group == 6 ~ "DK",
    group == 7 ~ "ES",
    group == 8 ~ "FI",
    group == 9 ~ "FR",
    group == 10 ~ "GB",
    group == 11 ~ "GR",
    group == 12 ~ "HU",
    group == 13 ~ "IE",
    group == 14 ~ "NL",
    group == 15 ~ "NO",
    group == 16 ~ "PL",
    group == 17 ~ "PT",
    group == 18 ~ "SE",
    group == 19 ~ "SI"
  )) %>%
  select(rhs, group, est, country)
#
##Make the data wider
HV_beta2<-HV_beta2 %>%
  pivot_wider(names_from = "rhs", values_from = "est")
#
##Plot the coefficients:
ggplot(HV_beta2, aes(x=SelfTran, y=Conser))+
  geom_point()+
  geom_text(aes(label = country),vjust=-0.5, hjust=0.5)+
  labs(title = "Regression Coefficients of Human Values")+
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  theme_minimal()



###################################################################################
####################### MMGSEM ####################################################
###################################################################################


###-------------------------------------------------------------------------------------
##For human values and reject

##Specify the structural model:
Str_model<-'
Reject ~ SelfTran + Conser
'

##Need to first omit all the missing values???
ESS1_selected<-ESS1 %>%
  select(cntry, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5, C6, R1, R2, R3, R4) %>%
  na.omit()

##Model selection:
HV.Reject.ModelSelection<-ModelSelection(
  dat=ESS1_selected,
  S1 = list(HV.FullMetric.FinalModel,Reject.FullMetric.FinalModel),
  S2 = Str_model,
  group="cntry",
  clusters=c(1,6),
  seed = 100,
  userStart = NULL,
  s1_fit = list(HV.FullMetric.Final,Reject.FullMetric.Final),
  max_it = 10000L,
  nstarts = 50L,
  printing = FALSE,
  partition = "hard",
  endogenous_cov = TRUE,
  endo_group_specific = TRUE,
  sam_method = "local",
  meanstr = FALSE,
  rescaling = F
)

View(HV.Reject.ModelSelection$Overview)

##CHull
ggplot(HV.Reject.ModelSelection$Overview, aes(x=nrpar,y=LL))+
  geom_point()+geom_line()+labs(title = "CHull - Human Values on Reject")+
  xlab("number of free parameters")+ylab("Loglikelihood")+
  theme_minimal()

##BIC observed
ggplot(HV.Reject.ModelSelection$Overview, aes(x=Clusters,y=BIC_G))+
  geom_point()+geom_line()+labs(title = "BIC Observed - Human Values on Reject")+
  theme_minimal()

##BIC factor
ggplot(HV.Reject.ModelSelection$Overview, aes(x=Clusters,y=BIC_G_fac))+
  geom_point()+geom_line()+labs(title = "BIC factor - Human Values on Reject")+
  theme_minimal()


####Try with multiple number of clusters from 1 to 6:
#
##1 cluster
HV.Reject.1clus<-MMGSEM(dat = ESS1_selected, 
                        S1=list(HV.FullMetric.FinalModel,Reject.FullMetric.FinalModel),
                        S2=Str_model, group = "cntry",
                        nclus = 1, seed = 100, printing = F, nstarts = 50,
                        partition = "hard",
                        s1_fit = list(HV.FullMetric.Final,Reject.FullMetric.Final),
                        endogenous_cov = T)
#
##2 clusters
HV.Reject.2clus<-MMGSEM(dat = ESS1_selected, 
                        S1=list(HV.FullMetric.FinalModel,Reject.FullMetric.FinalModel),
                        S2=Str_model, group = "cntry",
                        nclus = 2, seed = 100, printing = F, nstarts = 50,
                        partition = "hard",
                        s1_fit = list(HV.FullMetric.Final,Reject.FullMetric.Final),
                        endogenous_cov = T)
#
##3 clusters
HV.Reject.3clus<-MMGSEM(dat = ESS1_selected, 
                        S1=list(HV.FullMetric.FinalModel,Reject.FullMetric.FinalModel),
                        S2=Str_model, group = "cntry",
                        nclus = 3, seed = 100, printing = F, nstarts = 50,
                        partition = "hard",
                        s1_fit = list(HV.FullMetric.Final,Reject.FullMetric.Final),
                        endogenous_cov = T)
#
##4 clusters
HV.Reject.4clus<-MMGSEM(dat = ESS1_selected, 
                        S1=list(HV.FullMetric.FinalModel,Reject.FullMetric.FinalModel),
                        S2=Str_model, group = "cntry",
                        nclus = 4, seed = 100, printing = F, nstarts = 50,
                        partition = "hard",
                        s1_fit = list(HV.FullMetric.Final,Reject.FullMetric.Final),
                        endogenous_cov = T)
#
##5 clusters
HV.Reject.5clus<-MMGSEM(dat = ESS1_selected, 
                        S1=list(HV.FullMetric.FinalModel,Reject.FullMetric.FinalModel),
                        S2=Str_model, group = "cntry",
                        nclus = 5, seed = 100, printing = F, nstarts = 50,
                        partition = "hard",
                        s1_fit = list(HV.FullMetric.Final,Reject.FullMetric.Final),
                        endogenous_cov = T)
#
##6 clusters
HV.Reject.6clus<-MMGSEM(dat = ESS1_selected, 
                        S1=list(HV.FullMetric.FinalModel,Reject.FullMetric.FinalModel),
                        S2=Str_model, group = "cntry",
                        nclus = 6, seed = 100, printing = F, nstarts = 50,
                        partition = "hard",
                        s1_fit = list(HV.FullMetric.Final,Reject.FullMetric.Final),
                        endogenous_cov = T)


##Extract the BIC_G for observed and factors:
BIC_O<-c(HV.Reject.1clus$model_sel$BIC$observed$BIC_G, HV.Reject.2clus$model_sel$BIC$observed$BIC_G, 
         HV.Reject.3clus$model_sel$BIC$observed$BIC_G, HV.Reject.4clus$model_sel$BIC$observed$BIC_G, 
         HV.Reject.5clus$model_sel$BIC$observed$BIC_G, HV.Reject.6clus$model_sel$BIC$observed$BIC_G)
BIC_F<-c(HV.Reject.1clus$model_sel$BIC$Factors$BIC_G, HV.Reject.2clus$model_sel$BIC$Factors$BIC_G,
         HV.Reject.3clus$model_sel$BIC$Factors$BIC_G, HV.Reject.4clus$model_sel$BIC$Factors$BIC_G,
         HV.Reject.5clus$model_sel$BIC$Factors$BIC_G, HV.Reject.6clus$model_sel$BIC$Factors$BIC_G)

clus<-c(1:6)
#
##put in a df
BIC_df<-data.frame(cluster=clus,
                   BIC_O=BIC_O,
                   BIC_F=BIC_F)

ggplot(BIC_df, aes(x=cluster, y=BIC_O))+
  geom_point()+geom_line()+
  labs(title = "Model Selection - BIC_O")+xlab("number of clusters")+ylab("BIC_O")+
  theme_minimal()

ggplot(BIC_df, aes(x=cluster, y=BIC_F))+
  geom_point()+geom_line()+
  labs(title = "Model Selection - BIC_F")+xlab("number of clusters")+ylab("BIC_F")+
  theme_minimal()

###Based on BIC_G observed, we will select 3 clusters:
HV.Reject.3clus$param$beta_ks
HV.Reject.3clus$param$lambda

clustering<-t(apply(HV.Reject.3clus$posteriors,1,function(x) as.numeric(x==max(x))))
clustering[,2]<-ifelse(clustering[,2]==1,2,0)
clustering[,3]<-ifelse(clustering[,3]==1,3,0)
ClusMembership<-apply(clustering,1,function(x) sum(x))
ClusterRes<-data.frame(group=c(1:19),
                       ClusMembership=ClusMembership)

##Group 1-3: ##AT#BE#CH
##Group 4-6: ##CZ#DE#DK
##Group 7-9: ##ES#FI#FR
##Group 10-12: ##GB#GR#HU
##Group 13-15: ##IE#NL#NO
##Group 16-18: ##PL#PT#SE
##Group 19: ##SI

##Merge with the HV_beta table where we store the estimates from the regular MGSEM
HV_beta<-merge(HV_beta, ClusterRes,
               by.x = "group",
               by.y = "group")


Convex_hull<-HV_beta %>%
  group_by(ClusMembership) %>%
  slice(chull(SelfTran, Conser))

ggplot(HV_beta, aes(x=SelfTran, y=Conser, color = factor(ClusMembership)))+
  geom_point()+
  geom_text(aes(label = country),vjust=-0.5, hjust=0.5)+
  labs(title = "Regression Coefficients of Human Values in 3 clusters")+
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  theme_minimal()+
  geom_polygon(data = Convex_hull, aes(fill = factor(ClusMembership), color=factor(ClusMembership), group = ClusMembership),
               alpha=0.1)




###-------------------------------------------------------------------------------------
##For human values and threat
##Specify the structural model:
Str_model<-'
Threat ~ SelfTran + Conser
'

##Need to first omit all the missing values???
ESS1_selected<-ESS1 %>%
  select(cntry, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5, C6, T1,T2,T3) %>%
  na.omit()

##Model Selection
HV.Threat.ModelSelection<-ModelSelection(
  dat=ESS1_selected,
  S1 = list(HV.FullMetric.FinalModel, Threat.PartialMetric.FinalModel),
  S2 = Str_model,
  group = "cntry",
  clusters=c(1,6),
  seed = 100,
  userStart = NULL,
  s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final),
  max_it = 10000L,
  nstarts = 50L,
  printing = FALSE,
  partition = "hard",
  endogenous_cov = TRUE,
  endo_group_specific = TRUE,
  sam_method = "local",
  meanstr = FALSE,
  rescaling = F)

HV.Threat.ModelSelection$Overview

##CHull
ggplot(HV.Threat.ModelSelection$Overview, aes(x=nrpar, y=LL))+
  geom_point()+geom_line()+labs(title = "CHull - Human Values on Threat")+
  theme_minimal()

##BIC observed
ggplot(HV.Threat.ModelSelection$Overview, aes(x=Clusters, y=BIC_G))+
  geom_point()+geom_line()+labs(title = "BIC observed - Human Values on Threat")+
  theme_minimal()

##BIC factor
ggplot(HV.Threat.ModelSelection$Overview, aes(x=Clusters, y=BIC_G_fac))+
  geom_point()+geom_line()+labs(title = "BIC factor - Human Values on Threat")+
  theme_minimal()

####Try with multiple number of clusters from 1 to 6:
#
##1 cluster
HV.Threat.1clus<-MMGSEM(dat = ESS1_selected, 
                        S1=list(HV.FullMetric.FinalModel, Threat.PartialMetric.FinalModel),
                        S2=Str_model, group = "cntry",
                        nclus = 1, seed = 100, printing = F, nstarts = 50,
                        partition = "hard",
                        s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final),
                        endogenous_cov = T)
#
##2 cluster
HV.Threat.2clus<-MMGSEM(dat = ESS1_selected, 
                        S1=list(HV.FullMetric.FinalModel, Threat.PartialMetric.FinalModel),
                        S2=Str_model, group = "cntry",
                        nclus = 2, seed = 100, printing = F, nstarts = 50,
                        partition = "hard",
                        s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final),
                        endogenous_cov = T)
#
##3 cluster
HV.Threat.3clus<-MMGSEM(dat = ESS1_selected, 
                        S1=list(HV.FullMetric.FinalModel, Threat.PartialMetric.FinalModel),
                        S2=Str_model, group = "cntry",
                        nclus = 3, seed = 100, printing = F, nstarts = 50,
                        partition = "hard",
                        s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final),
                        endogenous_cov = T)
#
##4 clusters
HV.Threat.4clus<-MMGSEM(dat = ESS1_selected, 
                        S1=list(HV.FullMetric.FinalModel, Threat.PartialMetric.FinalModel),
                        S2=Str_model, group = "cntry",
                        nclus = 4, seed = 100, printing = F, nstarts = 50,
                        partition = "hard",
                        s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final),
                        endogenous_cov = T)
#
##5 cluster
HV.Threat.5clus<-MMGSEM(dat = ESS1_selected, 
                        S1=list(HV.FullMetric.FinalModel, Threat.PartialMetric.FinalModel),
                        S2=Str_model, group = "cntry",
                        nclus = 5, seed = 100, printing = F, nstarts = 50,
                        partition = "hard",
                        s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final),
                        endogenous_cov = T)
#
##6 cluster
HV.Threat.6clus<-MMGSEM(dat = ESS1_selected, 
                        S1=list(HV.FullMetric.FinalModel, Threat.PartialMetric.FinalModel),
                        S2=Str_model, group = "cntry",
                        nclus = 6, seed = 100, printing = F, nstarts = 50,
                        partition = "hard",
                        s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final),
                        endogenous_cov = T)




##Extract the BIC_G for observed and factors:
BIC_O<-c(HV.Threat.1clus$model_sel$BIC$observed$BIC_G, HV.Threat.2clus$model_sel$BIC$observed$BIC_G, 
         HV.Threat.3clus$model_sel$BIC$observed$BIC_G, HV.Threat.4clus$model_sel$BIC$observed$BIC_G, 
         HV.Threat.5clus$model_sel$BIC$observed$BIC_G, HV.Threat.6clus$model_sel$BIC$observed$BIC_G)
BIC_F<-c(HV.Threat.1clus$model_sel$BIC$Factors$BIC_G, HV.Threat.2clus$model_sel$BIC$Factors$BIC_G,
         HV.Threat.3clus$model_sel$BIC$Factors$BIC_G, HV.Threat.4clus$model_sel$BIC$Factors$BIC_G,
         HV.Threat.5clus$model_sel$BIC$Factors$BIC_G, HV.Threat.6clus$model_sel$BIC$Factors$BIC_G)

clus<-c(1:6)
#
##put in a df
BIC_df<-data.frame(cluster=clus,
                   BIC_O=BIC_O,
                   BIC_F=BIC_F)

ggplot(BIC_df, aes(x=cluster, y=BIC_O))+
  geom_point()+geom_line()+
  labs(title = "Model Selection - BIC_O")+xlab("number of clusters")+ylab("BIC_O")+
  theme_minimal()

ggplot(BIC_df, aes(x=cluster, y=BIC_F))+
  geom_point()+geom_line()+
  labs(title = "Model Selection - BIC_F")+xlab("number of clusters")+ylab("BIC_F")+
  theme_minimal()


###Try 3 clusters:
HV.Threat.3clus$param$beta_ks
HV.Threat.3clus$param$lambda


clustering<-t(apply(HV.Threat.3clus$posteriors,1,function(x) as.numeric(x==max(x))))
clustering[,2]<-ifelse(clustering[,2]==1,2,0)
clustering[,3]<-ifelse(clustering[,3]==1,3,0)
ClusMembership<-apply(clustering,1,function(x) sum(x))
ClusterRes<-data.frame(group=c(1:19),
                       ClusMembership=ClusMembership)


##Merge with the HV_beta table where we store the estimates from the regular MGSEM
HV_beta2_3clus<-merge(HV_beta2, ClusterRes,
               by.x = "group",
               by.y = "group")


Convex_hull<-HV_beta2_3clus %>%
  group_by(ClusMembership) %>%
  slice(chull(SelfTran, Conser))

ggplot(HV_beta2_3clus, aes(x=SelfTran, y=Conser, color = factor(ClusMembership)))+
  geom_point()+
  geom_text(aes(label = country),vjust=-0.5, hjust=0.5)+
  labs(title = "Regression Coefficients of Human Values in 3 clusters")+
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  theme_minimal()+
  geom_polygon(data = Convex_hull, aes(fill = factor(ClusMembership), color=factor(ClusMembership), group = ClusMembership),
               alpha=0.1)


###Try 4 clusters:
HV.Threat.4clus$param$beta_ks
HV.Threat.4clus$param$lambda


clustering<-t(apply(HV.Threat.4clus$posteriors,1,function(x) as.numeric(x==max(x))))
clustering[,2]<-ifelse(clustering[,2]==1,2,0)
clustering[,3]<-ifelse(clustering[,3]==1,3,0)
clustering[,4]<-ifelse(clustering[,4]==1,4,0)
ClusMembership<-apply(clustering,1,function(x) sum(x))
ClusterRes<-data.frame(group=c(1:19),
                       ClusMembership=ClusMembership)


##Merge with the HV_beta table where we store the estimates from the regular MGSEM
HV_beta2_4clus<-merge(HV_beta2, ClusterRes,
                by.x = "group",
                by.y = "group")


Convex_hull<-HV_beta2_4clus %>%
  group_by(ClusMembership) %>%
  slice(chull(SelfTran, Conser))

ggplot(HV_beta2_4clus, aes(x=SelfTran, y=Conser, color = factor(ClusMembership)))+
  geom_point()+
  geom_text(aes(label = country),vjust=-0.5, hjust=0.5)+
  labs(title = "Regression Coefficients of Human Values in 4 clusters")+
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  theme_minimal()+
  geom_polygon(data = Convex_hull, aes(fill = factor(ClusMembership), color=factor(ClusMembership), group = ClusMembership),
               alpha=0.1)




####################################################################################
##################################### Validation ###################################
####################################################################################


####-------------------------------------------------------------------------------------------
##Select the HV.Metric.fit2.Alter as the final measurement model for Human Values
##Select the Reject.Metric.Fit1 as the final measurement model for Anti-immigrant Attitudes

##Group 1-3: ##AT#BE#CH
##Group 4-6: ##CZ#DE#DK
##Group 7-9: ##ES#FI#FR
##Group 10-12: ##GB#GR#HU
##Group 13-15: ##IE#NL#NO
##Group 16-18: ##PL#PT#SE
##Group 19: ##SI

HV.Reject.Constrain.M1<-'
##Measurement model for human values:
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6

##Error terms correlation
C5~~C6
S1~~S2

##Measurement model for reject:
Reject=~R1+R2+R3+R4

##add error term correlation
R2~~R4

##Structural Part:
Reject~c(a2,a1,a2,a1,a2,a2,a2,a2,a1,a1,a1,a3,a2,a2,a2,a1,a3,a2,a1)*SelfTran+ 
          c(b2,b1,b2,b1,b2,b2,b2,b2,b1,b1,b1,b3,b2,b2,b2,b1,b3,b2,b1)*Conser
'

HV.Reject.Constrain.Fit1<-cfa(model = HV.Reject.Constrain.M1,
                              data = ESS1,
                              group = "cntry",
                              estimator="MLR",
                              missing="FIML",
                              group.equal="loadings")

lavTestLRT(HV.Reject.FreeSEM.Fit1, HV.Reject.Constrain.Fit1)

sink("./Sink Output/ESS1/HV_Reject_Constrain_fit1.txt")
summary(HV.Reject.Constrain.Fit1, fit.measures=T, standardized=T)
sink()


####---------------------------------------------------------------------------------------
##Select the HV.Metric.fit2.Alter as the final measurement model for Human Values
##Select the Reject.Metric.Fit1 as the final measurement model for Anti-immigrant Attitudes

##Group 1-3: ##AT#BE#CH
##Group 4-6: ##CZ#DE#DK
##Group 7-9: ##ES#FI#FR
##Group 10-12: ##GB#GR#HU
##Group 13-15: ##IE#NL#NO
##Group 16-18: ##PL#PT#SE
##Group 19: ##SI

HV.Threat.Constrain.M1<-'
##Measurement model for human values
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6

##Error terms correlation
C5~~C6
S1~~S2

##Measurement model for Threat
Threat=~T1+T2+T3

##Structural Model
Threat~c(a3,a4,a4,a1,a4,a4,a4,a4,a1,a3,a3,a3,a2,a4,a4,a2,a4,a4,a3)*SelfTran+ 
        c(b3,b4,b4,b1,b4,b4,b4,b4,b1,b3,b3,b3,b2,b4,b4,b2,b4,b4,b3)*Conser
'

HV.Threat.Constrain.Fit1<-cfa(model = HV.Threat.Constrain.M1,
                            data = ESS1,
                            group = "cntry",
                            estimator="MLR",
                            missing="FIML",
                            group.equal="loadings",
                            group.partial=c("Threat=~T3"))

lavTestLRT(HV.Threat.Constrain.Fit1, HV.Threat.FreeSEM.Fit1)

sink("./Sink Output/ESS1/HV_Threat_Constrain_4clus.txt")
summary(HV.Threat.Constrain.Fit1, fit.measures=T, standardized=T)
sink()

##Validation for 3 cluster solution
HV.Threat.Constrain.3Clus<-'
##Measurement model for human values
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6

##Error terms correlation
C5~~C6
S1~~S2

##Measurement model for Threat
Threat=~T1+T2+T3

##Structural Model
Threat~c(a3,a4,a4,a1,a4,a4,a4,a4,a1,a3,a3,a3,a4,a4,a4,a4,a3,a4,a3)*SelfTran+ 
        c(b3,b4,b4,b1,b4,b4,b4,b4,b1,b3,b3,b3,b4,b4,b4,b4,b3,b4,b3)*Conser
'

HV.Threat.Constrain.3clus<-cfa(model = HV.Threat.Constrain.3Clus,
                              data = ESS1,
                              group = "cntry",
                              estimator="MLR",
                              missing="FIML",
                              group.equal="loadings",
                              group.partial=c("Threat=~T3"))
lavTestLRT(HV.Threat.Constrain.3clus, HV.Threat.FreeSEM.Fit1)

sink("./Sink Output/ESS1/HV_Threat_Constrain_3clus.txt")
summary(HV.Threat.Constrain.3clus, fit.measures=T, standardized=T)
sink()




####################################################################################
################## Andres Theory Testing - follow-up comparison ####################
####################################################################################

###---------------------------------------------------------------------------------
##Human values and Reject

###---------------------------------------------------------------------------------
##Comparison 1: HV Model without cross-loadings but with error term correlation

#Human Value Full Metric Invariance Comparison Model 1: 
HV.FullMetric.Comp.M1<-'
SelfTran=~S1+S2+S3+S4+S5
Conser=~C1+C2+C3+C4+C5+C6

##Error term correlation:
S1~~S2
C5~~C6
'

HV.FullMetric.Comp.fit1<-cfa(model = HV.FullMetric.Comp.M1,
                    data = ESS1,
                    group = "cntry",
                    estimator="MLR",
                    missing="FIML",
                    group.equal=c("loadings"),
                    std.lv=T)

sink("./Sink Output/ESS1/Comp1_HV_FullMetric_fit.txt")
summary(HV.FullMetric.Comp.fit1, fit.measures=T, standardized=T)
sink()
#
##marker variable approach
HV.FullMetric.Comp1Model<-'
SelfTran=~S4+S1+S2+S3+S5
Conser=~C2+C1+C3+C4+C5+C6

##Error term correlation:
S1~~S2
C5~~C6
'

HV.FullMetric.Comp1<-cfa(model = HV.FullMetric.Comp1Model,
                             data = ESS1,
                             group = "cntry",
                             estimator="MLR",
                             missing="FIML",
                             group.equal=c("loadings"))



##Reject Full metric invariance comparison model 1 with error term
##which means that this is the same as the original model
##For this reason, we can go ahead to do the marker variable approach directly
Reject.FullMetric.Comp1Model<-'
Reject=~R1+R2+R3+R4

R2~~R4
'

Reject.FullMetric.Comp1<-cfa(model = Reject.FullMetric.Comp1Model,
                        data = ESS1,
                        group = "cntry",
                        estimator="MLR",
                        missing="FIML",
                        group.equal="loadings")


###------------------------------------------------------------------------------------------
##MMGSEM
##Specify the structural model:
Str_model<-'
Reject ~ SelfTran + Conser
'

##Need to first omit all the missing values???
ESS1_selected<-ESS1 %>%
  select(cntry, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5, C6, R1, R2, R3, R4) %>%
  na.omit()

##Model Selection
Comp1_ModelSelection<-ModelSelection(
  dat=ESS1_selected,
  S1 = list(HV.FullMetric.Comp1Model,Reject.FullMetric.Comp1Model),
  S2 = Str_model,
  group = "cntry",
  clusters = c(1,6),
  seed = 100,
  userStart = NULL,
  s1_fit = list(HV.FullMetric.Comp1, Reject.FullMetric.Comp1),
  max_it = 10000L,
  nstarts = 50L,
  printing = FALSE,
  partition = "hard",
  endogenous_cov = TRUE,
  endo_group_specific = TRUE,
  sam_method = "local",
  meanstr = FALSE,
  rescaling = F)

Comp1_ModelSelection$Overview

##CHull visual
ggplot(data = Comp1_ModelSelection$Overview, aes(x=nrpar, y=LL))+
  geom_point()+geom_line()+
  labs(title = "CHull")+xlab("number of free parameters")+ylab("Loglikelihood")+
  theme_minimal()

##BIC observed
ggplot(data = Comp1_ModelSelection$Overview, aes(x=Clusters, y=BIC_G))+
  geom_point()+geom_line() + 
  labs(title = "BIC_G (observed)")+
  theme_minimal()

##BIC factor
ggplot(data = Comp1_ModelSelection$Overview, aes(x=Clusters, y=BIC_G_fac))+
  geom_point()+
  geom_line() + labs(title = "BIC_G(factor)")+
  theme_minimal()


##Continue with 3 clusters for comparison in any case
#
#3 cluster
Comp1.HV.Reject.3clus<-MMGSEM(dat = ESS1_selected, 
                               S1=list(HV.FullMetric.Comp1Model,Reject.FullMetric.Comp1Model),
                               S2=Str_model, group = "cntry",
                               nclus = 3, seed = 100, printing = F, nstarts = 50,
                               partition = "hard",
                               s1_fit = list(HV.FullMetric.Comp1, Reject.FullMetric.Comp1),
                               endogenous_cov = T)

clustering<-t(apply(Comp1.HV.Reject.3clus$posteriors,1,function(x) as.numeric(x==max(x))))
clustering[,2]<-ifelse(clustering[,2]==1,2,0)
clustering[,3]<-ifelse(clustering[,3]==1,3,0)
ClusMembership<-apply(clustering,1,function(x) sum(x))
ClusterRes<-data.frame(group=c(1:19),
                       ClusMembership=ClusMembership)

###Redo the regular MGSEM without cross-loadings but with error terms:
##Marker variable approach
HV.Reject.FreeSEM.Comp1Model<-'
##Measurement model for human values:
SelfTran=~S4+S1+S2+S3+S5
Conser=~C2+C1+C3+C4+C5+C6

##Error term correlation:
S1~~S2
C5~~C6

##Measurement model for reject:
Reject=~R1+R2+R3+R4

##error term correlation:
R2~~R4

##Structural Part:
Reject~c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19)*SelfTran+ 
          c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19)*Conser
'

HV.Reject.FreeSEM.Comp1<-cfa(model = HV.Reject.FreeSEM.Comp1Model,
                            data = ESS1,
                            group = "cntry",
                            estimator="MLR",
                            missing="FIML",
                            group.equal="loadings")

sink("./Sink Output/ESS1/Comp1_HV_Reject_FreeSEM.txt")
summary(HV.Reject.FreeSEM.Comp1, fit.measures=T, standardized=T)
sink()

##Extract all the parameters
param<-parameterestimates(HV.Reject.FreeSEM.Comp1, standardized = T)
#
##Extract the coefficients Reject~SelfTran
SelfTran_beta<-param %>%
  filter(lhs == "Reject" & op == "~" & rhs == "SelfTran")
#
##Extract the coefficients Reject ~Conser
Conser_beta<-param %>%
  filter(lhs == "Reject" & op == "~" & rhs == "Conser")
#
##bind the coefficients together and sort by group
HV_beta<-rbind(SelfTran_beta, Conser_beta) %>%
  arrange(group)
#
#Give the country name to each group:
HV_beta<-HV_beta %>%
  mutate(country = case_when(
    group == 1 ~ "AT",
    group == 2 ~ "BE",
    group == 3 ~ "CH",
    group == 4 ~ "CZ",
    group == 5 ~ "DE",
    group == 6 ~ "DK",
    group == 7 ~ "ES",
    group == 8 ~ "FI",
    group == 9 ~ "FR",
    group == 10 ~ "GB",
    group == 11 ~ "GR",
    group == 12 ~ "HU",
    group == 13 ~ "IE",
    group == 14 ~ "NL",
    group == 15 ~ "NO",
    group == 16 ~ "PL",
    group == 17 ~ "PT",
    group == 18 ~ "SE",
    group == 19 ~ "SI"
  )) %>%
  select(rhs, group, est, country)
#
##Make the data wider
HV_beta<-HV_beta %>%
  pivot_wider(names_from = "rhs", values_from = "est")


HV_beta2<-merge(HV_beta, ClusterRes,
                by.x = "group",
                by.y = "group")


##Map it out the one with the clusters
Convex_hull<-HV_beta2 %>%
  group_by(ClusMembership) %>%
  slice(chull(SelfTran, Conser))

ggplot(HV_beta2, aes(x=SelfTran, y=Conser, color = factor(ClusMembership)))+
  geom_point()+
  geom_text(aes(label = country),vjust=-0.5, hjust=0.5)+
  labs(title = "Regression Coefficients of Human Values in 3 clusters")+
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  theme_minimal()+
  geom_polygon(data = Convex_hull, aes(fill = factor(ClusMembership), color=factor(ClusMembership), group = ClusMembership),
               alpha=0.1)



###---------------------------------------------------------------------------------
##Comparison 2: HV Model with cross-loadings but without error term correlation

#Human Value Full Metric Invariance Comparison Model 1: 
HV.FullMetric.Comp.M2<-'
SelfTran=~S1+S2+S3+S4+S5+C3+C4
Conser=~C1+C2+C3+C4+C5+C6
'

HV.FullMetric.Comp.fit2<-cfa(model = HV.FullMetric.Comp.M2,
                             data = ESS1,
                             group = "cntry",
                             estimator="MLR",
                             missing="FIML",
                             group.equal=c("loadings"),
                             std.lv=T)

sink("./Sink Output/ESS1/Comp2_HV_FullMetric_fit.txt")
summary(HV.FullMetric.Comp.fit2, fit.measures=T, standardized=T)
sink()
#
##marker variable approach
HV.FullMetric.Comp2Model<-'
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6
'

HV.FullMetric.Comp2<-cfa(model = HV.FullMetric.Comp2Model,
                         data = ESS1,
                         group = "cntry",
                         estimator="MLR",
                         missing="FIML",
                         group.equal=c("loadings"))



##Reject Full metric invariance comparison model 1 without error term
##marker approach directly
Reject.FullMetric.Comp2Model<-'
Reject=~R1+R2+R3+R4
'

Reject.FullMetric.Comp2<-cfa(model = Reject.FullMetric.Comp2Model,
                             data = ESS1,
                             group = "cntry",
                             estimator="MLR",
                             missing="FIML",
                             group.equal="loadings")

sink("./Sink Output/ESS1/Comp2_Reject_FullMetric_fit.txt")
summary(Reject.FullMetric.Comp2, fit.measures=T, standardized=T)
sink()

###------------------------------------------------------------------------------------------
##MMGSEM
##Specify the structural model:
Str_model<-'
Reject ~ SelfTran + Conser
'

##Need to first omit all the missing values???
ESS1_selected<-ESS1 %>%
  select(cntry, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5, C6, R1, R2, R3, R4) %>%
  na.omit()

##Model Selection
Comp2_ModelSelection<-ModelSelection(
  dat=ESS1_selected,
  S1 = list(HV.FullMetric.Comp2Model,Reject.FullMetric.Comp2Model),
  S2 = Str_model,
  group = "cntry",
  clusters = c(1,6),
  seed = 100,
  userStart = NULL,
  s1_fit = list(HV.FullMetric.Comp2, Reject.FullMetric.Comp2),
  max_it = 10000L,
  nstarts = 50L,
  printing = FALSE,
  partition = "hard",
  endogenous_cov = TRUE,
  endo_group_specific = TRUE,
  sam_method = "local",
  meanstr = FALSE,
  rescaling = F)

Comp2_ModelSelection$Overview

##CHull visual
ggplot(data = Comp2_ModelSelection$Overview, aes(x=nrpar, y=LL))+
  geom_point()+geom_line()+
  labs(title = "CHull")+xlab("number of free parameters")+ylab("Loglikelihood")+
  theme_minimal()

##BIC observed
ggplot(data = Comp2_ModelSelection$Overview, aes(x=Clusters, y=BIC_G))+
  geom_point()+geom_line() + 
  labs(title = "BIC_G (observed)")+
  theme_minimal()

##BIC factor
ggplot(data = Comp2_ModelSelection$Overview, aes(x=Clusters, y=BIC_G_fac))+
  geom_point()+
  geom_line() + labs(title = "BIC_G(factor)")+
  theme_minimal()

##Continue with 3 clusters for comparison in any case
#
#3 cluster
Comp2.HV.Reject.3clus<-MMGSEM(dat = ESS1_selected, 
                              S1=list(HV.FullMetric.Comp2Model,Reject.FullMetric.Comp2Model),
                              S2=Str_model, group = "cntry",
                              nclus = 3, seed = 100, printing = F, nstarts = 50,
                              partition = "hard",
                              s1_fit = list(HV.FullMetric.Comp2, Reject.FullMetric.Comp2),
                              endogenous_cov = T)

clustering<-t(apply(Comp2.HV.Reject.3clus$posteriors,1,function(x) as.numeric(x==max(x))))
clustering[,2]<-ifelse(clustering[,2]==1,2,0)
clustering[,3]<-ifelse(clustering[,3]==1,3,0)
ClusMembership<-apply(clustering,1,function(x) sum(x))
ClusterRes<-data.frame(group=c(1:19),
                       ClusMembership=ClusMembership)

###Redo the regular MGSEM without cross-loadings but with error terms:
##Marker variable approach
HV.Reject.FreeSEM.Comp2Model<-'
##Measurement model for human values:
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6

##Measurement model for reject:
Reject=~R1+R2+R3+R4

##Structural Part:
Reject~c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19)*SelfTran+ 
          c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19)*Conser
'

HV.Reject.FreeSEM.Comp2<-cfa(model = HV.Reject.FreeSEM.Comp2Model,
                             data = ESS1,
                             group = "cntry",
                             estimator="MLR",
                             missing="FIML",
                             group.equal="loadings")

sink("./Sink Output/ESS1/Comp2_HV_Reject_FreeSEM.txt")
summary(HV.Reject.FreeSEM.Comp2, fit.measures=T, standardized=T)
sink()

##Extract all the parameters
param<-parameterestimates(HV.Reject.FreeSEM.Comp2, standardized = T)
#
##Extract the coefficients Reject~SelfTran
SelfTran_beta<-param %>%
  filter(lhs == "Reject" & op == "~" & rhs == "SelfTran")
#
##Extract the coefficients Reject ~Conser
Conser_beta<-param %>%
  filter(lhs == "Reject" & op == "~" & rhs == "Conser")
#
##bind the coefficients together and sort by group
HV_beta<-rbind(SelfTran_beta, Conser_beta) %>%
  arrange(group)
#
#Give the country name to each group:
HV_beta<-HV_beta %>%
  mutate(country = case_when(
    group == 1 ~ "AT",
    group == 2 ~ "BE",
    group == 3 ~ "CH",
    group == 4 ~ "CZ",
    group == 5 ~ "DE",
    group == 6 ~ "DK",
    group == 7 ~ "ES",
    group == 8 ~ "FI",
    group == 9 ~ "FR",
    group == 10 ~ "GB",
    group == 11 ~ "GR",
    group == 12 ~ "HU",
    group == 13 ~ "IE",
    group == 14 ~ "NL",
    group == 15 ~ "NO",
    group == 16 ~ "PL",
    group == 17 ~ "PT",
    group == 18 ~ "SE",
    group == 19 ~ "SI"
  )) %>%
  select(rhs, group, est, country)
#
##Make the data wider
HV_beta<-HV_beta %>%
  pivot_wider(names_from = "rhs", values_from = "est")


HV_beta2<-merge(HV_beta, ClusterRes,
                by.x = "group",
                by.y = "group")


##Map it out the one with the clusters
Convex_hull<-HV_beta2 %>%
  group_by(ClusMembership) %>%
  slice(chull(SelfTran, Conser))

ggplot(HV_beta2, aes(x=SelfTran, y=Conser, color = factor(ClusMembership)))+
  geom_point()+
  geom_text(aes(label = country),vjust=-0.5, hjust=0.5)+
  labs(title = "Regression Coefficients of Human Values in 3 clusters")+
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  theme_minimal()+
  geom_polygon(data = Convex_hull, aes(fill = factor(ClusMembership), color=factor(ClusMembership), group = ClusMembership),
               alpha=0.1)




###---------------------------------------------------------------------------------
##Comparison 3: Model without cross-loadings and without error term correlation

#Human Value Full Metric Invariance Comparison Model 1: 
HV.FullMetric.Comp.M3<-'
SelfTran=~S1+S2+S3+S4+S5
Conser=~C1+C2+C3+C4+C5+C6
'

HV.FullMetric.Comp.fit3<-cfa(model = HV.FullMetric.Comp.M3,
                             data = ESS1,
                             group = "cntry",
                             estimator="MLR",
                             missing="FIML",
                             group.equal=c("loadings"),
                             std.lv=T)

sink("./Sink Output/ESS1/Comp3_HV_FullMetric_fit.txt")
summary(HV.FullMetric.Comp.fit3, fit.measures=T, standardized=T)
sink()
#
##marker variable approach
HV.FullMetric.Comp3Model<-'
SelfTran=~S4+S1+S2+S3+S5
Conser=~C2+C1+C3+C4+C5+C6
'

HV.FullMetric.Comp3<-cfa(model = HV.FullMetric.Comp3Model,
                         data = ESS1,
                         group = "cntry",
                         estimator="MLR",
                         missing="FIML",
                         group.equal=c("loadings"))



##Reject Full metric invariance comparison model 1 without error term
##marker approach directly
Reject.FullMetric.Comp3Model<-'
Reject=~R1+R2+R3+R4
'

Reject.FullMetric.Comp3<-cfa(model = Reject.FullMetric.Comp3Model,
                             data = ESS1,
                             group = "cntry",
                             estimator="MLR",
                             missing="FIML",
                             group.equal="loadings")

sink("./Sink Output/ESS1/Comp3_Reject_FullMetric_fit.txt")
summary(Reject.FullMetric.Comp3, fit.measures=T, standardized=T)
sink()

###------------------------------------------------------------------------------------------
##MMGSEM
##Specify the structural model:
Str_model<-'
Reject ~ SelfTran + Conser
'

##Need to first omit all the missing values???
ESS1_selected<-ESS1 %>%
  select(cntry, S1, S2, S3, S4, S5, C1, C2, C3, C4, C5, C6, R1, R2, R3, R4) %>%
  na.omit()

##Model Selection
Comp3_ModelSelection<-ModelSelection(
  dat=ESS1_selected,
  S1 = list(HV.FullMetric.Comp3Model,Reject.FullMetric.Comp3Model),
  S2 = Str_model,
  group = "cntry",
  clusters = c(1,6),
  seed = 100,
  userStart = NULL,
  s1_fit = list(HV.FullMetric.Comp3, Reject.FullMetric.Comp3),
  max_it = 10000L,
  nstarts = 50L,
  printing = FALSE,
  partition = "hard",
  endogenous_cov = TRUE,
  endo_group_specific = TRUE,
  sam_method = "local",
  meanstr = FALSE,
  rescaling = F)

Comp3_ModelSelection$Overview

##CHull visual
ggplot(data = Comp3_ModelSelection$Overview, aes(x=nrpar, y=LL))+
  geom_point()+geom_line()+
  labs(title = "CHull")+xlab("number of free parameters")+ylab("Loglikelihood")+
  theme_minimal()

##BIC observed
ggplot(data = Comp3_ModelSelection$Overview, aes(x=Clusters, y=BIC_G))+
  geom_point()+geom_line() + 
  labs(title = "BIC_G (observed)")+
  theme_minimal()

##BIC factor
ggplot(data = Comp3_ModelSelection$Overview, aes(x=Clusters, y=BIC_G_fac))+
  geom_point()+
  geom_line() + labs(title = "BIC_G(factor)")+
  theme_minimal()

##Continue with 3 clusters for comparison in any case
#
#3 cluster
Comp3.HV.Reject.3clus<-MMGSEM(dat = ESS1_selected, 
                              S1=list(HV.FullMetric.Comp3Model,Reject.FullMetric.Comp3Model),
                              S2=Str_model, group = "cntry",
                              nclus = 3, seed = 100, printing = F, nstarts = 50,
                              partition = "hard",
                              s1_fit = list(HV.FullMetric.Comp3, Reject.FullMetric.Comp3),
                              endogenous_cov = T)

clustering<-t(apply(Comp3.HV.Reject.3clus$posteriors,1,function(x) as.numeric(x==max(x))))
clustering[,2]<-ifelse(clustering[,2]==1,2,0)
clustering[,3]<-ifelse(clustering[,3]==1,3,0)
ClusMembership<-apply(clustering,1,function(x) sum(x))
ClusterRes<-data.frame(group=c(1:19),
                       ClusMembership=ClusMembership)


###Redo the regular MGSEM without cross-loadings but with error terms:
##Marker variable approach
HV.Reject.FreeSEM.Comp3Model<-'
##Measurement model for human values:
SelfTran=~S4+S1+S2+S3+S5
Conser=~C2+C1+C3+C4+C5+C6

##Measurement model for reject:
Reject=~R1+R2+R3+R4

##Structural Part:
Reject~c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19)*SelfTran+ 
          c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19)*Conser
'

HV.Reject.FreeSEM.Comp3<-cfa(model = HV.Reject.FreeSEM.Comp3Model,
                             data = ESS1,
                             group = "cntry",
                             estimator="MLR",
                             missing="FIML",
                             group.equal="loadings")

sink("./Sink Output/ESS1/Comp3_HV_Reject_FreeSEM.txt")
summary(HV.Reject.FreeSEM.Comp3, fit.measures=T, standardized=T)
sink()

##Extract all the parameters
param<-parameterestimates(HV.Reject.FreeSEM.Comp3, standardized = T)
#
##Extract the coefficients Reject~SelfTran
SelfTran_beta<-param %>%
  filter(lhs == "Reject" & op == "~" & rhs == "SelfTran")
#
##Extract the coefficients Reject ~Conser
Conser_beta<-param %>%
  filter(lhs == "Reject" & op == "~" & rhs == "Conser")
#
##bind the coefficients together and sort by group
HV_beta<-rbind(SelfTran_beta, Conser_beta) %>%
  arrange(group)
#
#Give the country name to each group:
HV_beta<-HV_beta %>%
  mutate(country = case_when(
    group == 1 ~ "AT",
    group == 2 ~ "BE",
    group == 3 ~ "CH",
    group == 4 ~ "CZ",
    group == 5 ~ "DE",
    group == 6 ~ "DK",
    group == 7 ~ "ES",
    group == 8 ~ "FI",
    group == 9 ~ "FR",
    group == 10 ~ "GB",
    group == 11 ~ "GR",
    group == 12 ~ "HU",
    group == 13 ~ "IE",
    group == 14 ~ "NL",
    group == 15 ~ "NO",
    group == 16 ~ "PL",
    group == 17 ~ "PT",
    group == 18 ~ "SE",
    group == 19 ~ "SI"
  )) %>%
  select(rhs, group, est, country)
#
##Make the data wider
HV_beta<-HV_beta %>%
  pivot_wider(names_from = "rhs", values_from = "est")


HV_beta2<-merge(HV_beta, ClusterRes,
                by.x = "group",
                by.y = "group")


##Map it out the one with the clusters
Convex_hull<-HV_beta2 %>%
  group_by(ClusMembership) %>%
  slice(chull(SelfTran, Conser))

ggplot(HV_beta2, aes(x=SelfTran, y=Conser, color = factor(ClusMembership)))+
  geom_point()+
  geom_text(aes(label = country),vjust=-0.5, hjust=0.5)+
  labs(title = "Regression Coefficients of Human Values in 3 clusters")+
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  theme_minimal()+
  geom_polygon(data = Convex_hull, aes(fill = factor(ClusMembership), color=factor(ClusMembership), group = ClusMembership),
               alpha=0.1)




#########################################################################################
####################  Treat as Categorical Variable #####################################
#########################################################################################


######-------------------------------------------------------------------------------
##Measurement Block 2: Anti-immigrant Attitude - Reject

##Question for Andres:
##1. Why the df is 38 for Reject.Config.M1.Cat?

##The polychoric correlation among the items:
lavCor(ESS1[,c("R1","R2","R3","R4")], ordered = c("R1","R2","R3","R4"))

##Configural Invariance Model 1:
Reject.Config.M1.Cat<-'
Reject=~R1+R2+R3+R4
'

Reject.Config.Fit1.Cat<-cfa(model = Reject.Config.M1.Cat,
                        data = ESS1,
                        group = "cntry",
                        ordered = c("R1","R2","R3","R4"),
                        estimator="WLSMV",
                        std.lv=T)

##https://lavaan.ugent.be/tutorial/cat.html
##FIML is not supported --> resort to the default listwise deletion

sink("./Sink Output/ESS1/RejectCat_Config_fit1.txt")
summary(Reject.Config.Fit1.Cat, fit.measures=T, standardized=T)
sink()

##inspect the modification indices:
sink("./Sink Output/ESS1/RejectCat_Config_fit1_mi.txt")
options(max.print = 99999)
modindices(Reject.Config.Fit1.Cat, minimum.value = 10, sort. = T)
sink()

##Configural Invariance Model 2:
Reject.Config.M2.Cat<-'
Reject=~R1+R2+R3+R4

R2~~R4
'

Reject.Config.Fit2.Cat<-cfa(model = Reject.Config.M2.Cat,
                            data = ESS1,
                            group = "cntry",
                            ordered = c("R1","R2","R3","R4"),
                            estimator="WLSMV",
                            std.lv=T)

sink("./Sink Output/ESS1/RejectCat_Config_fit2.txt")
summary(Reject.Config.Fit2.Cat, fit.measures=T, standardized=T)
sink()

##Before imposing metric invariance model, first constrain the threshold to be equal:
Reject.Thres.M1.Cat<-'
Reject=~R1+R2+R3+R4

R2~~R4
'

Reject.Thres.Fit1.Cat<-cfa(model = Reject.Thres.M1.Cat,
                                data = ESS1,
                                group = "cntry",
                                ordered = c("R1","R2","R3","R4"),
                                estimator="WLSMV",
                                group.equal=c("thresholds"),
                                std.lv=T)

sink("./Sink Output/ESS1/RejectCat_Thres_fit1.txt")
summary(Reject.Thres.Fit1.Cat, fit.measures=T, standardized=T)
sink()

##check the modification indices:
sink("./Sink Output/ESS1/RejectCat_Thres_fit1_mi.txt")
options(max.print = 99999)
lavTestScore(Reject.Thres.Fit1.Cat, epc = T)
sink()

##Free the threshold of item R3: R3|t1, R3|t3
Reject.Thres.M2.Cat<-'
Reject=~R1+R2+R3+R4

R2~~R4
'

Reject.Thres.Fit2.Cat<-cfa(model = Reject.Thres.M2.Cat,
                           data = ESS1,
                           group = "cntry",
                           ordered = c("R1","R2","R3","R4"),
                           estimator="WLSMV",
                           group.equal=c("thresholds"),
                           group.partial=c("R3|t1","R3|t3"),
                           std.lv=T)

sink("./Sink Output/ESS1/RejectCat_Thres_fit2.txt")
summary(Reject.Thres.Fit2.Cat, fit.measures=T, standardized=T)
sink()

##Free the additional threshold of item R3: R4|t1 (Have tried also with R4|t2 and R2|t2)
Reject.Thres.M3.Cat<-'
Reject=~R1+R2+R3+R4

R2~~R4
'

Reject.Thres.Fit3.Cat<-cfa(model = Reject.Thres.M3.Cat,
                           data = ESS1,
                           group = "cntry",
                           ordered = c("R1","R2","R3","R4"),
                           estimator="WLSMV",
                           group.equal=c("thresholds"),
                           group.partial=c("R3|t1","R3|t3",
                                           "R4|t1"),
                           std.lv=T)

sink("./Sink Output/ESS1/RejectCat_Thres_fit3.txt")
summary(Reject.Thres.Fit3.Cat, fit.measures=T, standardized=T)
sink()

##Impose metric invariance on the threshold model 2 in any case:
Reject.Metric.M1.Cat<-'
Reject=~R1+R2+R3+R4

R2~~R4
'

Reject.Metric.Fit1.Cat<-cfa(model = Reject.Metric.M1.Cat,
                            data = ESS1,
                            group = "cntry",
                            ordered = c("R1","R2","R3","R4"),
                            estimator="WLSMV",
                            group.equal=c("loadings","thresholds"),
                            group.partial=c("R3|t1","R3|t3",
                                            "R4|t1"),
                            std.lv=T)

sink("./Sink Output/ESS1/RejectCat_Metric_fit1.txt")
summary(Reject.Metric.Fit1.Cat, fit.measures=T, standardized=T)
sink()

##check the modification indices:
sink("./Sink Output/ESS1/RejectCat_Metric_fit1_mi.txt")
options(max.print = 99999)
lavTestScore(Reject.Metric.Fit1.Cat, epc = T)
sink()

##metric invariance freeing Reject=~R3:
Reject.Metric.M2.Cat<-'
Reject=~R1+R2+R3+R4

R2~~R4
'

Reject.Metric.Fit2.Cat<-cfa(model = Reject.Metric.M2.Cat,
                            data = ESS1,
                            group = "cntry",
                            ordered = c("R1","R2","R3","R4"),
                            estimator="WLSMV",
                            group.equal=c("loadings","thresholds"),
                            group.partial=c("R3|t1","R3|t3",
                                            "R4|t1",
                                            "Reject=~R3"),
                            std.lv=T)

sink("./Sink Output/ESS1/RejectCat_Metric_fit2.txt")
summary(Reject.Metric.Fit2.Cat, fit.measures=T, standardized=T)
sink()




#######################################################################################
########################## Error to show Kim ##########################################
############## Threat and Reject and HV together in the model #########################
#################### New Measurement Model  ###########################################
#######################################################################################

###-----------------------------------------------------------------------------------------
##Human Values: Keep the Self-Transcendence Value and Conservation value as it is:
##Final Model with the marker variable approach:
#
##We need to use the marker variable approach to rerun the full metric model, which will be used as the input for MMGSEM
HV.FullMetric.FinalModel<-'
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6

##Error terms correlation
C5~~C6
S1~~S2
'

HV.FullMetric.Final<-cfa(model = HV.FullMetric.FinalModel,
                         data = ESS1,
                         group = "cntry",
                         estimator="MLR",
                         missing="FIML",
                         group.equal="loadings")





###-----------------------------------------------------------------------------------------
##Anti-immigrant Attitudes: Testing Threat and Reject in the same block


##Start with configural invariance
Threat.Reject.Config.M1<-'
Threat=~T1+T2+T3
Reject=~R1+R2+R3+R4
Threat~~Reject
'

Threat.Reject.Config.Fit1<-cfa(model = Threat.Reject.Config.M1,
                               data = ESS1,
                               group = "cntry",
                               estimator="MLR",
                               missing="FIML",
                               std.lv=T)


sink("./Sink Output/ESS1/ThreatReject_Config_fit1.txt")
summary(Threat.Reject.Config.Fit1, fit.measures=T, standardized=T)
sink()

##Impose full metric invariance
Threat.Reject.FullMetric.M2<-'
Threat=~T1+T2+T3
Reject=~R1+R2+R3+R4
Threat~~Reject
'

Threat.Reject.FullMetric.Fit2<-cfa(model = Threat.Reject.FullMetric.M2,
                               data = ESS1,
                               group = "cntry",
                               estimator="MLR",
                               missing="FIML",
                               group.equal=c("loadings"),
                               std.lv=T)

sink("./Sink Output/ESS1/ThreatReject_FullMetric_fit2.txt")
summary(Threat.Reject.FullMetric.Fit2, fit.measures=T, standardized=T)
sink()


##check the modification indices to see if there is anything that could improve the model substantially
sink("./Sink Output/ESS1/ThreatReject_FullMetric_fit2_mi.txt")
options(max.print = 999999)
lavTestScore(Threat.Reject.FullMetric.Fit2, epc = T)
sink()

##Test partial metric invariance with Threat=~T3 free
Threat.Reject.Metric.M3<-'
Threat=~T1+T2+T3
Reject=~R1+R2+R3+R4
Threat~~Reject
'

Threat.Reject.Metric.Fit3<-cfa(model = Threat.Reject.Metric.M3,
                                   data = ESS1,
                                   group = "cntry",
                                   estimator="MLR",
                                   missing="FIML",
                                   group.equal=c("loadings"),
                               group.partial=c("Threat=~T3"),
                                   std.lv=T)

sink("./Sink Output/ESS1/ThreatReject_Metric_fit3.txt")
summary(Threat.Reject.Metric.Fit3, fit.measures=T, standardized=T)
sink()


##Freeing Threat=~T3 is not necessary, so we will proceed with the full metric invariance model
##Now, we need to change the full metric invariance model with marker variable approach:
Threat.Reject.FullMetric.FinalModel<-'
Threat=~T1+T2+T3
Reject=~R1+R2+R3+R4
Threat~~Reject
'

Threat.Reject.FullMetric.Final<-cfa(model = Threat.Reject.FullMetric.FinalModel,
                                   data = ESS1,
                                   group = "cntry",
                                   estimator="MLR",
                                   missing="FIML",
                                   group.equal=c("loadings"))

sink("./Sink Output/ESS1/ThreatReject_FullMetricMarker_final.txt")
summary(Threat.Reject.FullMetric.Final, fit.measures=T, standardized=T)
sink()



#######################################################################################
############## Threat and Reject and HV together in the model #########################
#################### Regular MGSEM  ###################################################
#######################################################################################

ThreatReject.HV.FreeSEM.M1<-'
##MM for Human Values
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6
SelfTran~~Conser

C5~~C6
S1~~S2

##MM for Reject and Threat
Threat=~T1+T2+T3
Reject=~R1+R2+R3+R4
Threat~~Reject

##Structural model:
Reject~SelfTran+Conser
Threat~SelfTran+Conser
'

ThreatReject.HV.FreeSEM.fit1<-cfa(model = ThreatReject.HV.FreeSEM.M1,
                                  data = ESS1, 
                                  group = "cntry",
                                  estimator="MLR",
                                  missing="FIML",
                                  group.equal=c("loadings"))

sink("./Sink Output/ESS1/ThreatReject_HV_FreeSEM_fit1.txt")
summary(ThreatReject.HV.FreeSEM.fit1, fit.measures=T, standardized=T)
sink()

##Extract the parameters
param<-parameterestimates(ThreatReject.HV.FreeSEM.fit1, standardized = T)
#
##Filter only the coefficient and select the needed columns
RegreCoef<-param %>%
  filter((lhs=="Reject" & op == "~" & rhs=="SelfTran") | (lhs=="Reject" & op == "~" & rhs=="Conser") |
           (lhs=="Threat" & op == "~" & rhs=="SelfTran") | (lhs=="Threat" & op == "~" & rhs=="Conser") ) %>%
  select(lhs, rhs, group, est) %>%
  mutate(country = case_when(
    group == 1 ~ "AT",
    group == 2 ~ "BE",
    group == 3 ~ "CH",
    group == 4 ~ "CZ",
    group == 5 ~ "DE",
    group == 6 ~ "DK",
    group == 7 ~ "ES",
    group == 8 ~ "FI",
    group == 9 ~ "FR",
    group == 10 ~ "GB",
    group == 11 ~ "GR",
    group == 12 ~ "HU",
    group == 13 ~ "IE",
    group == 14 ~ "NL",
    group == 15 ~ "NO",
    group == 16 ~ "PL",
    group == 17 ~ "PT",
    group == 18 ~ "SE",
    group == 19 ~ "SI"))
#
#rename the columns:
colnames(RegreCoef)<-c("Endog","Exogen","group","coef","country")
#
##Make the data into the wide format:
RegreCoef<-RegreCoef %>% 
  pivot_wider(names_from = "Exogen", values_from = "coef")

##plot it:
ggplot(RegreCoef, aes(x=SelfTran, y=Conser))+
  geom_point()+
  geom_text(aes(label = country), hjust=0.5, vjust=-0.5)+
  facet_wrap(~factor(Endog), scales = "free")+
  theme_bw()+
  xlab("Self-Transcendence")+ylab("Conservation")+
  labs(title = "Human Values on Reject & Threat - MGSEM")



#######################################################################################
########################## Error to show Kim ##########################################
############## Threat and Reject and HV together in the model #########################
######################## MMGSEM  ######################################################
#######################################################################################

##Specify the structural model
Str_model<-'
Reject~SelfTran+Conser
Threat~SelfTran+Conser
'

##listwise deletion for the MMGSEM
ESS1_selected<-na.omit(ESS1)

##Model Selection
ThreatReject.HV.ModelSelection<-ModelSelection(dat=ESS1_selected, 
                                               S1 = list(HV.FullMetric.FinalModel, Threat.Reject.FullMetric.FinalModel),
                                               S2 = Str_model,
                                               group = "cntry",
                                               clusters=c(1,8),
                                               seed = 100,
                                               userStart = NULL,
                                               s1_fit = list(HV.FullMetric.Final, Threat.Reject.FullMetric.Final),
                                               max_it = 10000L,
                                               nstarts = 50L,
                                               printing = FALSE,
                                               partition = "hard",
                                               endogenous_cov = TRUE,
                                               endo_group_specific = TRUE,
                                               sam_method = "local",
                                               meanstr = FALSE,
                                               rescaling = F)



##MMGSEM one by one
#
##1 cluster
ThreatReject.HV.1clus<-MMGSEM(dat = ESS1_selected,
                              S1 = list(HV.FullMetric.FinalModel, Threat.Reject.FullMetric.FinalModel),
                              S2 = Str_model,
                              group = "cntry",
                              nclus = 1,
                              seed = 100,
                              userStart = NULL,
                              s1_fit = list(HV.FullMetric.Final, Threat.Reject.FullMetric.Final),
                              max_it = 10000L,
                              nstarts = 50L,
                              printing = F,
                              partition = "hard",
                              endogenous_cov = T,
                              endo_group_specific = T,
                              sam_method = "local",
                              meanstr = F,
                              rescaling = F)
#2 cluster
ThreatReject.HV.2clus<-MMGSEM(dat = ESS1_selected,
                              S1 = list(HV.FullMetric.FinalModel, Threat.Reject.FullMetric.FinalModel),
                              S2 = Str_model,
                              group = "cntry",
                              nclus = 2,
                              seed = 100,
                              userStart = NULL,
                              s1_fit = list(HV.FullMetric.Final, Threat.Reject.FullMetric.Final),
                              max_it = 10000L,
                              nstarts = 50L,
                              printing = F,
                              partition = "hard",
                              endogenous_cov = T,
                              endo_group_specific = T,
                              sam_method = "local",
                              meanstr = F,
                              rescaling = F)







#######################################################################################
############## Threat and Reject and HV together in the same block ####################
#################### New Measurement Model  ###########################################
#######################################################################################

##Configural invariance
FullMM.config.model<-'
##MM for Human Values
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6
SelfTran~~Conser

##Error terms correlation
C5~~C6
S1~~S2

##MM for Anti-immigrant attitudes
Threat=~T1+T2+T3
Reject=~R1+R2+R3+R4
Threat~~Reject
'

FullMM.Config.fit<-cfa(model = FullMM.config.model,
                       data = ESS1,
                       group = "cntry",
                       estimator="MLR",
                       missing="FIML",
                       std.lv=T)

sink("./Sink Output/ESS1/FullMM_Config.txt")
summary(FullMM.Config.fit, fit.measures=T, standardized=T)
sink()


##Impose Full Metric invariance
FullMM.FullMetric.model<-'
##MM for Human Values
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6
SelfTran~~Conser

##Error terms correlation
C5~~C6
S1~~S2

##MM for Anti-immigrant attitudes
Threat=~T1+T2+T3
Reject=~R1+R2+R3+R4
Threat~~Reject
'

FullMM.FullMetric.fit<-cfa(model = FullMM.FullMetric.model,
                       data = ESS1,
                       group = "cntry",
                       estimator="MLR",
                       missing="FIML",
                       group.equal=c("loadings"),
                       std.lv=T)

sink("./Sink Output/ESS1/FullMM_FullMetric.txt")
summary(FullMM.FullMetric.fit, fit.measures=T, standardized=T)
sink()

##proceed with the full metric invariance model with the marker variable approach:
FullMM.FullMetric.FinalModel<-'
##MM for Human Values
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6
SelfTran~~Conser

##Error terms correlation
C5~~C6
S1~~S2

##MM for Anti-immigrant attitudes
Threat=~T1+T2+T3
Reject=~R1+R2+R3+R4
Threat~~Reject
'

FullMM.FullMetric.final<-cfa(model = FullMM.FullMetric.FinalModel,
                           data = ESS1,
                           group = "cntry",
                           estimator="MLR",
                           missing="FIML",
                           group.equal=c("loadings"))

sink("./Sink Output/ESS1/FullMM_FullMetric_Markerfinal.txt")
summary(FullMM.FullMetric.final, fit.measures=T, standardized=T)
sink()



ESS1_selected<-na.omit(ESS1)

Str_model<-'
Reject~SelfTran+Conser+Threat
Threat~SelfTran+Conser
'


trial <- MMGSEM(dat = ESS1_selected,
                S1 = FullMM.config.model,
                S2 = Str_model,
                group = "cntry",
                nclus = 2,
                seed = 100,
                userStart = NULL,
                s1_fit = FullMM.Config.fit,
                max_it = 10000L,
                nstarts = 50L,
                printing = F,
                partition = "hard",
                endogenous_cov = T,
                endo_group_specific = T,
                sam_method = "local",
                meanstr = F,
                rescaling = F)




#######################################################################################
#################### Threat and Reject and HV 3 separate blocks #######################
#################### mediation ########################################################
#################### Copied Measurement Model  ########################################
#######################################################################################

##Human Value:
HV.FullMetric.FinalModel<-'
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6

##Error terms correlation
C5~~C6
S1~~S2
'

HV.FullMetric.Final<-cfa(model = HV.FullMetric.FinalModel,
                         data = ESS1,
                         group = "cntry",
                         estimator="MLR",
                         missing="FIML",
                         group.equal="loadings")


##Threat:
Threat.PartialMetric.FinalModel<-'
Threat=~T1+T2+T3
'

Threat.PartialMetric.Final<-cfa(model = Threat.PartialMetric.FinalModel,
                                data = ESS1,
                                group = "cntry",
                                estimator="MLR",
                                missing="FIML",
                                group.equal="loadings",
                                group.partial=c("Threat=~T3"))


##Reject:
Reject.FullMetric.FinalModel<-'
Reject=~R1+R2+R3+R4

##add error term correlation
R2~~R4
'

Reject.FullMetric.Final<-cfa(model = Reject.FullMetric.FinalModel,
                             data = ESS1,
                             group = "cntry",
                             estimator="MLR",
                             missing="FIML",
                             group.equal="loadings")


#######################################################################################
#################### Threat and Reject and HV 3 separate blocks #######################
#################### mediation ########################################################
#################### MMGSEM  ##########################################################
#######################################################################################

Mediation_Str_model<-'
Threat~SelfTran+Conser
Reject~SelfTran+Conser+Threat
'

ESS1_selected<-na.omit(ESS1)

##Model selection
Mediation.ModelSelection<-ModelSelection(dat=ESS1_selected, 
                                               S1 = list(HV.FullMetric.FinalModel,Threat.PartialMetric.FinalModel,Reject.FullMetric.FinalModel),
                                               S2 = Mediation_Str_model,
                                               group = "cntry",
                                               clusters=c(1,8),
                                               seed = 100,
                                               userStart = NULL,
                                               s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final,Reject.FullMetric.Final),
                                               max_it = 10000L,
                                               nstarts = 50L,
                                               printing = FALSE,
                                               partition = "hard",
                                               endogenous_cov = TRUE,
                                               endo_group_specific = TRUE,
                                               sam_method = "local",
                                               meanstr = FALSE,
                                               rescaling = F)
#
##Plot the CHull
ggplot(Mediation.ModelSelection$Overview, aes(x=nrpar, y=LL))+
  geom_point()+
  geom_line()+
  xlab("number of parameters")+ylab("LogLikelihood")+labs(title = "CHull")+
  theme_minimal()
#
##Plot the BIC_G observed
ggplot(Mediation.ModelSelection$Overview, aes(x=Clusters, y=BIC_G))+
  geom_point()+
  geom_line()+
  xlab("number of clusters")+ylab("BIC_G")+labs(title = "BIC_G Observed")+
  theme_minimal()
#
##Plot the BIC_G factor
ggplot(Mediation.ModelSelection$Overview, aes(x=Clusters, y=BIC_G_fac))+
  geom_point()+
  geom_line()+
  xlab("number of clusters")+ylab("BIC_G")+labs(title = "BIC_G factor")+
  theme_minimal()
#
##suggest 2 or 5 clusters


##Running MMGSEM one by one
#
#1 cluster:
Mediation_1clus<-MMGSEM(dat = ESS1_selected,
                        S1 = list(HV.FullMetric.FinalModel,Threat.PartialMetric.FinalModel,Reject.FullMetric.FinalModel),
                        S2 = Mediation_Str_model,
                        group = "cntry",
                        nclus = 1,
                        seed=100,
                        userStart = NULL,
                        s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final,Reject.FullMetric.Final),
                        max_it = 10000L,
                        nstarts = 50L,
                        printing = F,
                        partition = "hard",
                        endogenous_cov = T,
                        endo_group_specific = T,
                        sam_method = "local",
                        meanstr = F,
                        rescaling = F)
#
##2 cluster:
Mediation_2clus<-MMGSEM(dat = ESS1_selected,
                        S1 = list(HV.FullMetric.FinalModel,Threat.PartialMetric.FinalModel,Reject.FullMetric.FinalModel),
                        S2 = Mediation_Str_model,
                        group = "cntry",
                        nclus = 2,
                        seed=100,
                        userStart = NULL,
                        s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final,Reject.FullMetric.Final),
                        max_it = 10000L,
                        nstarts = 50L,
                        printing = F,
                        partition = "hard",
                        endogenous_cov = T,
                        endo_group_specific = T,
                        sam_method = "local",
                        meanstr = F,
                        rescaling = F)
#
##3 cluster:
Mediation_3clus<-MMGSEM(dat = ESS1_selected,
                        S1 = list(HV.FullMetric.FinalModel,Threat.PartialMetric.FinalModel,Reject.FullMetric.FinalModel),
                        S2 = Mediation_Str_model,
                        group = "cntry",
                        nclus = 3,
                        seed=100,
                        userStart = NULL,
                        s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final,Reject.FullMetric.Final),
                        max_it = 10000L,
                        nstarts = 50L,
                        printing = F,
                        partition = "hard",
                        endogenous_cov = T,
                        endo_group_specific = T,
                        sam_method = "local",
                        meanstr = F,
                        rescaling = F)
#
##4 cluster:
Mediation_4clus<-MMGSEM(dat = ESS1_selected,
                        S1 = list(HV.FullMetric.FinalModel,Threat.PartialMetric.FinalModel,Reject.FullMetric.FinalModel),
                        S2 = Mediation_Str_model,
                        group = "cntry",
                        nclus = 4,
                        seed=100,
                        userStart = NULL,
                        s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final,Reject.FullMetric.Final),
                        max_it = 10000L,
                        nstarts = 50L,
                        printing = F,
                        partition = "hard",
                        endogenous_cov = T,
                        endo_group_specific = T,
                        sam_method = "local",
                        meanstr = F,
                        rescaling = F)
#
##5 cluster:
Mediation_5clus<-MMGSEM(dat = ESS1_selected,
                        S1 = list(HV.FullMetric.FinalModel,Threat.PartialMetric.FinalModel,Reject.FullMetric.FinalModel),
                        S2 = Mediation_Str_model,
                        group = "cntry",
                        nclus = 5,
                        seed=100,
                        userStart = NULL,
                        s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final,Reject.FullMetric.Final),
                        max_it = 10000L,
                        nstarts = 50L,
                        printing = F,
                        partition = "hard",
                        endogenous_cov = T,
                        endo_group_specific = T,
                        sam_method = "local",
                        meanstr = F,
                        rescaling = F)
#
##6 cluster:
Mediation_6clus<-MMGSEM(dat = ESS1_selected,
                        S1 = list(HV.FullMetric.FinalModel,Threat.PartialMetric.FinalModel,Reject.FullMetric.FinalModel),
                        S2 = Mediation_Str_model,
                        group = "cntry",
                        nclus = 6,
                        seed=100,
                        userStart = NULL,
                        s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final,Reject.FullMetric.Final),
                        max_it = 10000L,
                        nstarts = 50L,
                        printing = F,
                        partition = "hard",
                        endogenous_cov = T,
                        endo_group_specific = T,
                        sam_method = "local",
                        meanstr = F,
                        rescaling = F)
#
##7 cluster:
Mediation_7clus<-MMGSEM(dat = ESS1_selected,
                        S1 = list(HV.FullMetric.FinalModel,Threat.PartialMetric.FinalModel,Reject.FullMetric.FinalModel),
                        S2 = Mediation_Str_model,
                        group = "cntry",
                        nclus = 7,
                        seed=100,
                        userStart = NULL,
                        s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final,Reject.FullMetric.Final),
                        max_it = 10000L,
                        nstarts = 50L,
                        printing = F,
                        partition = "hard",
                        endogenous_cov = T,
                        endo_group_specific = T,
                        sam_method = "local",
                        meanstr = F,
                        rescaling = F)
#
##8 cluster:
Mediation_8clus<-MMGSEM(dat = ESS1_selected,
                        S1 = list(HV.FullMetric.FinalModel,Threat.PartialMetric.FinalModel,Reject.FullMetric.FinalModel),
                        S2 = Mediation_Str_model,
                        group = "cntry",
                        nclus = 8,
                        seed=100,
                        userStart = NULL,
                        s1_fit = list(HV.FullMetric.Final,Threat.PartialMetric.Final,Reject.FullMetric.Final),
                        max_it = 10000L,
                        nstarts = 50L,
                        printing = F,
                        partition = "hard",
                        endogenous_cov = T,
                        endo_group_specific = T,
                        sam_method = "local",
                        meanstr = F,
                        rescaling = F)


##Manual model selection
BIC_G_selection<-data.frame(cluster=1:8,
                            BIC_G_observed=c(Mediation_1clus$model_sel$BIC$observed$BIC_G, Mediation_2clus$model_sel$BIC$observed$BIC_G,
                                                Mediation_3clus$model_sel$BIC$observed$BIC_G, Mediation_4clus$model_sel$BIC$observed$BIC_G,
                                                Mediation_5clus$model_sel$BIC$observed$BIC_G, Mediation_6clus$model_sel$BIC$observed$BIC_G,
                                                Mediation_7clus$model_sel$BIC$observed$BIC_G, Mediation_8clus$model_sel$BIC$observed$BIC_G),
                            BIC_G_factor=c(Mediation_1clus$model_sel$BIC$Factors$BIC_G, Mediation_2clus$model_sel$BIC$Factors$BIC_G,
                                           Mediation_3clus$model_sel$BIC$Factors$BIC_G, Mediation_4clus$model_sel$BIC$Factors$BIC_G,
                                           Mediation_5clus$model_sel$BIC$Factors$BIC_G, Mediation_6clus$model_sel$BIC$Factors$BIC_G,
                                           Mediation_7clus$model_sel$BIC$Factors$BIC_G, Mediation_8clus$model_sel$BIC$Factors$BIC_G))

##plot the BIC_G observed
ggplot(BIC_G_selection, aes(x=cluster, y=BIC_G_observed))+
  geom_point()+
  geom_line()+
  xlab("number of cluster")+labs(title = "BIC_G observed")+
  theme_minimal()
#
##plot the BIC_G factor
ggplot(BIC_G_selection, aes(x=cluster, y=BIC_G_factor))+
  geom_point()+
  geom_line()+
  xlab("number of cluster")+labs(title = "BIC_G factor")+
  theme_minimal()


#####--------------------------------------------------------------------------
##Extract the 2-cluster solution
clustering_2clus<-t(apply(Mediation_2clus$posteriors,1,function(x) as.numeric(x==max(x))))
clustering_2clus[,2]<-ifelse(clustering_2clus[,2]==1,2,0)
ClusMembership_2clus<-apply(clustering_2clus,1,function(x) sum(x))
ClusterRes_2clus<-data.frame(group=c(1:19),
                       ClusMembership=ClusMembership_2clus)

Mediation_2clus$param$beta_ks

##The visualization can be found in the next section


#####--------------------------------------------------------------------------
##Extract the 5-cluster solution
clustering_5clus<-t(apply(Mediation_5clus$posteriors,1,function(x) as.numeric(x==max(x))))
clustering_5clus[,2]<-ifelse(clustering_5clus[,2]==1,2,0)
clustering_5clus[,3]<-ifelse(clustering_5clus[,3]==1,3,0)
clustering_5clus[,4]<-ifelse(clustering_5clus[,4]==1,4,0)
clustering_5clus[,5]<-ifelse(clustering_5clus[,5]==1,5,0)
ClusMembership_5clus<-apply(clustering_5clus,1,function(x) sum(x))
ClusterRes_5clus<-data.frame(group=c(1:19),
                             ClusMembership=ClusMembership_5clus)

Mediation_5clus$param$beta_ks

##The visualization can be found in the next section



#######################################################################################
#################### Threat and Reject and HV 3 separate blocks #######################
#################### mediation ########################################################
#################### Regular MGSEM  ###################################################
#################### Visualization  ###################################################
#######################################################################################

Mediation.FreeSEM.model<-'
##MM for HV:
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6

##Error terms correlation
C5~~C6
S1~~S2

##MM for Threat:
Threat=~T1+T2+T3

##MM for Reject:
Reject=~R1+R2+R3+R4

##add error term correlation
R2~~R4

##Structural model:
Threat~SelfTran+Conser
Reject~SelfTran+Conser+Threat
'

Mediation.FreeSEM.fit<-cfa(model = Mediation.FreeSEM.model,
                           data = ESS1,
                           group = "cntry",
                           estimator="MLR",
                           missing="FIML",
                           group.equal="loadings",
                           group.partial=c("Threat=~T3"))

sink("./Sink Output/ESS1/Mediation_FreeSEM.txt")
summary(Mediation.FreeSEM.fit, fit.measures=T, standardized=T)
sink()

##extract the parameter estimates 
param<-parameterestimates(Mediation.FreeSEM.fit, standardized = T)

##filter out the regression parameters for the direct effect
RegCoef_HVtoThreatReject<-param %>%
  filter((lhs=="Threat" & op == "~" & rhs == "SelfTran") |
           (lhs=="Threat" & op == "~" & rhs == "Conser") | 
           (lhs=="Reject" & op == "~" & rhs == "SelfTran") | 
           (lhs=="Reject" & op == "~" & rhs == "Conser")) %>%
  select(lhs, rhs, group, est) %>%
  mutate(country = case_when(
    group == 1 ~ "AT",
    group == 2 ~ "BE",
    group == 3 ~ "CH",
    group == 4 ~ "CZ",
    group == 5 ~ "DE",
    group == 6 ~ "DK",
    group == 7 ~ "ES",
    group == 8 ~ "FI",
    group == 9 ~ "FR",
    group == 10 ~ "GB",
    group == 11 ~ "GR",
    group == 12 ~ "HU",
    group == 13 ~ "IE",
    group == 14 ~ "NL",
    group == 15 ~ "NO",
    group == 16 ~ "PL",
    group == 17 ~ "PT",
    group == 18 ~ "SE",
    group == 19 ~ "SI"
  )) %>%
  pivot_wider(names_from = "rhs", values_from = "est")
#
##filter out the regression parameters from human values to Threat in order to calculate indirect effects
RegCoef_HVtoThreat<-param %>%
  filter((lhs=="Threat" & op == "~" & rhs == "SelfTran") |
           (lhs=="Threat" & op == "~" & rhs == "Conser")) %>%
  select(lhs, rhs, group, est)
#
##filter out the regression parameter from threat to reject
RegCoef_ThreatToReject<-param %>%
  filter(lhs=="Reject" & op == "~" & rhs == "Threat") %>%
  select(group, est)
colnames(RegCoef_ThreatToReject)[2]<-c("ThreatToReject_est")
#
RegCoef_IndirectEffect<-merge(RegCoef_HVtoThreat, RegCoef_ThreatToReject,
                              by.x = "group",
                              by.y = "group")
#
##Make df for the indirect effect of self-transcendence value to reject through threat
RegCoef_IndirectEffect_SelfTran<-RegCoef_IndirectEffect %>%
  filter(rhs == "SelfTran") %>%
  mutate(SelfTran_Indirect=est*ThreatToReject_est) %>%
  select(group, SelfTran_Indirect)
#
##Make df for the indirect effect of conservation value to reject through threat
RegCoef_IndirectEffect_Conser<-RegCoef_IndirectEffect %>%
  filter(rhs == "Conser") %>%
  mutate(Conser_Indirect=est*ThreatToReject_est) %>%
  select(group, Conser_Indirect)
#
RegCoef_IndirectEffect<-merge(RegCoef_IndirectEffect_SelfTran, RegCoef_IndirectEffect_Conser,
                              by.x = "group",
                              by.y = "group") %>%
  mutate(country = case_when(
    group == 1 ~ "AT",
    group == 2 ~ "BE",
    group == 3 ~ "CH",
    group == 4 ~ "CZ",
    group == 5 ~ "DE",
    group == 6 ~ "DK",
    group == 7 ~ "ES",
    group == 8 ~ "FI",
    group == 9 ~ "FR",
    group == 10 ~ "GB",
    group == 11 ~ "GR",
    group == 12 ~ "HU",
    group == 13 ~ "IE",
    group == 14 ~ "NL",
    group == 15 ~ "NO",
    group == 16 ~ "PL",
    group == 17 ~ "PT",
    group == 18 ~ "SE",
    group == 19 ~ "SI"
  )) 
  


##plot the direct effect from human values to threat and reject
ggplot(RegCoef_HVtoThreatReject, aes(x=SelfTran, y=Conser))+
  geom_point()+
  geom_text(aes(label = country), hjust=0.5, vjust=-0.5, size = 4) +
  facet_wrap(~lhs, scales = "free")+
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  labs(title = "Direct effects of Human Values on Reject")+
  theme_bw()

##plot the indirect effect from human values to reject VIA threat
ggplot(RegCoef_IndirectEffect, aes(x=SelfTran_Indirect,y=Conser_Indirect))+
  geom_point()+
  geom_text(aes(label=country), hjust=0.5, vjust=-0.5) +
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  labs(title = "Indirect effects of Human values on Reject via threat")+
  theme_minimal()

###--------------------------------------------------------------------------------------
##2 clusters: map the cluster on the plot above for direct effect
RegCoef_HVtoThreatReject_2clus<-merge(RegCoef_HVtoThreatReject, ClusterRes_2clus,
                                      by.x = "group",
                                      by.y = "group")

Convex_hull_2clus_direct<-RegCoef_HVtoThreatReject_2clus %>%
  group_by(ClusMembership, lhs) %>%
  slice(chull(SelfTran, Conser))

ggplot(RegCoef_HVtoThreatReject_2clus, aes(x=SelfTran, y=Conser, color = factor(ClusMembership)))+
  geom_point()+
  geom_text(aes(label = country),vjust=-0.5, hjust=0.5)+
  facet_wrap(~lhs, scales = "free")+
  labs(title = "Direct effects of Human Values on Reject in 2 clusters")+
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  theme_bw()+
  geom_polygon(data = Convex_hull_2clus_direct, aes(fill = factor(ClusMembership), color=factor(ClusMembership), group = interaction(ClusMembership, lhs)),
               alpha=0.1)

##map the cluster on the plot above for the indirect effect
RegCoef_IndirectEffect_2clus<-merge(RegCoef_IndirectEffect, ClusterRes_2clus,
                                      by.x = "group",
                                      by.y = "group")
#
Convex_hull_2clus_indirect<-RegCoef_IndirectEffect_2clus %>%
  group_by(ClusMembership) %>%
  slice(chull(SelfTran_Indirect, Conser_Indirect))
#
ggplot(RegCoef_IndirectEffect_2clus, aes(x=SelfTran_Indirect, y=Conser_Indirect, color = factor(ClusMembership)))+
  geom_point()+
  geom_text(aes(label = country),vjust=-0.5, hjust=0.5)+
  labs(title = "Indirect effects of Human Values on Reject through threat in 2 clusters")+
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  theme_bw()+
  geom_polygon(data = Convex_hull_2clus_indirect, aes(fill = factor(ClusMembership), color=factor(ClusMembership), group = factor(ClusMembership)),
               alpha=0.1)


###--------------------------------------------------------------------------------------
##5 clusters: map the cluster on the plot above for direct effect
RegCoef_HVtoThreatReject_5clus<-merge(RegCoef_HVtoThreatReject, ClusterRes_5clus,
                                      by.x = "group",
                                      by.y = "group")

Convex_hull_5clus_direct<-RegCoef_HVtoThreatReject_5clus %>%
  group_by(ClusMembership, lhs) %>%
  slice(chull(SelfTran, Conser))

ggplot(RegCoef_HVtoThreatReject_5clus, aes(x=SelfTran, y=Conser, color = factor(ClusMembership)))+
  geom_point()+
  geom_text(aes(label = country),vjust=-0.5, hjust=0.5)+
  facet_wrap(~lhs, scales = "free")+
  labs(title = "Direct effects of Human Values on Reject in 5 clusters")+
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  theme_bw()+
  geom_polygon(data = Convex_hull_5clus_direct, aes(fill = factor(ClusMembership), color=factor(ClusMembership), group = interaction(ClusMembership, lhs)),
               alpha=0.1)

##map the cluster on the plot above for the indirect effect
RegCoef_IndirectEffect_5clus<-merge(RegCoef_IndirectEffect, ClusterRes_5clus,
                                    by.x = "group",
                                    by.y = "group")
#
Convex_hull_5clus_indirect<-RegCoef_IndirectEffect_5clus %>%
  group_by(ClusMembership) %>%
  slice(chull(SelfTran_Indirect, Conser_Indirect))
#
ggplot(RegCoef_IndirectEffect_5clus, aes(x=SelfTran_Indirect, y=Conser_Indirect, color = factor(ClusMembership)))+
  geom_point()+
  geom_text(aes(label = country),vjust=-0.5, hjust=0.5)+
  labs(title = "Indirect effects of Human Values on Reject through threat in 5 clusters")+
  xlab("Self-Transcendence Value")+ylab("Conservation Value")+
  theme_bw()+
  geom_polygon(data = Convex_hull_5clus_indirect, aes(fill = factor(ClusMembership), color=factor(ClusMembership), group = factor(ClusMembership)),
               alpha=0.1)






#######################################################################################
#################### Threat and Reject and HV 3 separate blocks #######################
#################### mediation ########################################################
####################### Validation  ###################################################
#######################################################################################

##Group 1-3: ##AT#BE#CH
##Group 4-6: ##CZ#DE#DK
##Group 7-9: ##ES#FI#FR
##Group 10-12: ##GB#GR#HU
##Group 13-15: ##IE#NL#NO
##Group 16-18: ##PL#PT#SE
##Group 19: ##SI

Mediation.Constrain.model.2clus<-'
##MM for HV:
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6

##Error terms correlation
C5~~C6
S1~~S2

##MM for Threat:
Threat=~T1+T2+T3

##MM for Reject:
Reject=~R1+R2+R3+R4

##add error term correlation
R2~~R4

##Structural model:
Threat~c(a1,a2,a1,a1,a2,a1,a2,a2,a1,a2,a1,a1,a1,a2,a2,a1,a2,a2,a2)*SelfTran+
        c(b1,b2,b1,b1,b2,b1,b2,b2,b1,b2,b1,b1,b1,b2,b2,b1,b2,b2,b2)*Conser
Reject~c(c1,c2,c1,c1,c2,c1,c2,c2,c1,c2,c1,c1,c1,c2,c2,c1,c2,c2,c2)*SelfTran+
          c(d1,d2,d1,d1,d2,d1,d2,d2,d1,d2,d1,d1,d1,d2,d2,d1,d2,d2,d2)*Conser+
          c(e1,e2,e1,e1,e2,e1,e2,e2,e1,e2,e1,e1,e1,e2,e2,e1,e2,e2,e2)*Threat
'

Mediation.Constrain.fit.2clus<-cfa(model = Mediation.Constrain.model.2clus,
                           data = ESS1,
                           group = "cntry",
                           estimator="MLR",
                           missing="FIML",
                           group.equal="loadings",
                           group.partial=c("Threat=~T3"))


sink("./Sink Output/ESS1/Mediation_Constrain_2clus.txt")
summary(Mediation.Constrain.fit.2clus, fit.measures=T, standardized=T)
sink()

lavTestLRT(Mediation.Constrain.fit.2clus, Mediation.FreeSEM.fit)


##Group 1-3: ##AT#BE#CH
##Group 4-6: ##CZ#DE#DK
##Group 7-9: ##ES#FI#FR
##Group 10-12: ##GB#GR#HU
##Group 13-15: ##IE#NL#NO
##Group 16-18: ##PL#PT#SE
##Group 19: ##SI

Mediation.Constrain.model.5clus<-'
##MM for HV:
SelfTran=~S4+S1+S2+S3+S5+C3+C4
Conser=~C2+C1+C3+C4+C5+C6

##Error terms correlation
C5~~C6
S1~~S2

##MM for Threat:
Threat=~T1+T2+T3

##MM for Reject:
Reject=~R1+R2+R3+R4

##add error term correlation
R2~~R4

##Structural model:
Threat~c(a3,a4,a3,a2,a1,a3,a4,a4,a2,a1,a3,a3,a3,a1,a1,a5,a1,a1,a1)*SelfTran+
        c(b3,b4,b3,b2,b1,b3,b4,b4,b2,b1,b3,b3,b3,b1,b1,b5,b1,b1,b1)*Conser
Reject~c(c3,c4,c3,c2,c1,c3,c4,c4,c2,c1,c3,c3,c3,c1,c1,c5,c1,c1,c1)*SelfTran+
          c(d3,d4,d3,d2,d1,d3,d4,d4,d2,d1,d3,d3,d3,d1,d1,d5,d1,d1,d1)*Conser+
          c(e3,e4,e3,e2,e1,e3,e4,e4,e2,e1,e3,e3,e3,e1,e1,e5,e1,e1,e1)*Threat
'

Mediation.Constrain.fit.5clus<-cfa(model = Mediation.Constrain.model.5clus,
                                   data = ESS1,
                                   group = "cntry",
                                   estimator="MLR",
                                   missing="FIML",
                                   group.equal="loadings",
                                   group.partial=c("Threat=~T3"))

sink("./Sink Output/ESS1/Mediation_Constrain_5clus.txt")
summary(Mediation.Constrain.fit.5clus, fit.measures=T, standardized=T)
sink()

lavTestLRT(Mediation.Constrain.fit.5clus, Mediation.FreeSEM.fit)
