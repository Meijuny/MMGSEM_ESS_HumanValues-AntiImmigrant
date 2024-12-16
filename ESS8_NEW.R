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
ESS8<-read.csv("./ESS8.csv")

##Select the variables:
ESS8<-ESS8 %>%
  select(idno, cntry, ##grouping variable,
        ipeqopt, ipudrst, impenv, iphlppl, iplylfr, ##self-transcendence
        ipmodst, imptrad, ipfrule, ipbhprp, impsafe, ipstrgv, ##conservation
        ipcrtiv, impfree, impdiff, ipadvnt, ipgdtim, impfun, ##Opennes to change
        ipshabt, ipsuces, imprich, iprspot, ##self-enhancement
        clmchng, ccnthum, ccgdbd, ##climate change skepticism
        wrclmch, ##worries about climate change
        cflsenr, ccrdprs, ##personal efficacy regarding climate change
        inctxff, sbsrnen, banhhap, ##opposition for climate change policy
        lrscale, gndr, agea, eduyrs ##demographic
        )
##Data cleaning:
##rename the variable cntry to country
ESS8<-ESS8 %>% rename(country=cntry)

##Self-transcendence value:
#
##define 7,8,9 as missing value
ESS8[,c("ST1","ST2","ST3","ST4","ST5")]<-lapply(ESS8[,c("ipeqopt", "ipudrst", "impenv",  "iphlppl", "iplylfr")],
                                                                 function(x) ifelse(x %in% c(7,8,9), NA, x))
#
##reverse the scale
ESS8[,c("ST1","ST2","ST3","ST4","ST5")]<-lapply(ESS8[,c("ST1","ST2","ST3","ST4","ST5")],
                                                                function(x) -x+7)
##Conservation value:
#
##define 7,8,9 as missing value
ESS8[,c("C1","C2","C3","C4","C5","C6")]<-lapply(ESS8[,c("ipmodst", "imptrad", "ipfrule", "ipbhprp", "impsafe","ipstrgv")],
                                                                function(x) ifelse(x %in% c(7,8,9), NA, x))
#
##reverse the scale
ESS8[,c("C1","C2","C3","C4","C5","C6")]<-lapply(ESS8[,c("C1","C2","C3","C4","C5","C6")],
                                                                function(x) -x+7)
##Openness to change:
#
##define 7,8,9 as missing value
ESS8[,c("O1","O2","O3","O4","O5","O6")]<-lapply(ESS8[,c("ipcrtiv", "impfree", "impdiff", "ipadvnt", "ipgdtim", "impfun")],
                                                                function(x) ifelse(x %in% c(7,8,9), NA, x))
#
##reverse the scale
ESS8[,c("O1","O2","O3","O4","O5","O6")]<-lapply(ESS8[,c("O1","O2","O3","O4","O5","O6")],
                                                                function(x) -x+7)
##Self-enhancement:
#
##define 7,8,9 as missing value
ESS8[,c("SE1","SE2","SE3","SE4")]<-lapply(ESS8[,c("ipshabt", "ipsuces", "imprich", "iprspot")],
                                                          function(x) ifelse(x %in% c(7,8,9), NA, x))
#
##reverse the scale:
ESS8[,c("SE1","SE2","SE3","SE4")]<-lapply(ESS8[,c("SE1","SE2","SE3","SE4")],
                                                      function(x) -x+7)
##climate change skepticism (higher value indicates higher level of skepticism)
ESS8<-ESS8 %>%
mutate(TrendSke=ifelse(clmchng %in% c(7,8,9), NA, clmchng),
                       AttriSke=ifelse(ccnthum %in% c(55,66,77,88,99), NA, ccnthum),
                       ImpactSke=ifelse(ccgdbd %in% c(66,77,88,99), NA, ccgdbd)) %>%
  mutate(AttriSke=-(AttriSke)+6)
##Climate change concern
ESS8<-ESS8 %>%
  mutate(ClimateConcern=ifelse(wrclmch %in% c(6,7,8,9), NA, wrclmch))

##Personal Efficacy regarding climate change
ESS8<-ESS8 %>%
  mutate(PE1=ifelse(cflsenr %in% c(77,88,99), NA, cflsenr),
        PE2=ifelse(ccrdprs %in% c(66,77,88,99), NA, ccrdprs))
##opposition to climate change policy
ESS8[,c("oppo1","oppo2","oppo3")]<-lapply(ESS8[,c("inctxff","sbsrnen","banhhap")],
                                           function(x) ifelse(x %in% c(7,8,9), NA, x))
##Political placement & demographics:
ESS8<-ESS8 %>%
  mutate(PoliScale=ifelse(lrscale %in% c(77,88,99), NA, lrscale),
                       male=case_when(
                         gndr == 1 ~ 1,
                         gndr == 2 ~ 0,
                         gndr == 9 ~ NA
                         ),
                       age=ifelse(agea==999, NA, agea),
                       eduyrs=ifelse(eduyrs>30, NA, eduyrs))


#####################################################################################
################## 4 Separate Measurement Model for 4 HV ############################
#####################################################################################

#openness to change
Open.Config.M1<-'
OpenChange=~O1+O2+O3+O4+O5+O6
'

Open.Config.fit1<-cfa(model = Open.Config.M1,
                      data = ESS8,
                      group = "country",
                      estimator="MLR",
                      missing="FIML",
                      std.lv=T)
sink("./Sink Output/ESS8/Openness_Config_fit1.txt")
summary(Open.Config.fit1, fit.measures=T, standardized=T)
sink()

##Self-Enhancement
SelfEnhan.Config.M1<-'
SelfEnhan=~SE1+SE2+SE3+SE4
'

SelfEnhan.Config.Fit1<-cfa(model = SelfEnhan.Config.M1,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/ESS8/SelfEnhan_Config_fit1.txt")
summary(SelfEnhan.Config.Fit1, fit.measures=T, standardized=T)
sink()

##Self-Transcendence
SelfTran.Config.M1<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5
'

SelfTran.Config.Fit1<-cfa(model = SelfTran.Config.M1,
                          data = ESS8,
                          group = "country",
                          estimator="MLR",
                          missing="FIML",
                          std.lv=T)

sink("./Sink Output/ESS8/SelfTran_Config_fit1.txt")
summary(SelfTran.Config.Fit1, fit.measures=T, standardized=T)
sink()

##Conservation
Conser.Config.M1<-'
Conser=~C1+C2+C3+C4+C5+C6
'

Conser.Config.Fit1<-cfa(model = Conser.Config.M1,
                        data = ESS8,
                        group = "country",
                        estimator="MLR",
                        missing="FIML",
                        std.lv=T)

sink("./Sink Output/ESS8/Conser_Config_fit1.txt")
summary(Conser.Config.Fit1, fit.measures=T, standardized=T)
sink()


#####################################################################################
################## HV measurement model without Openness ############################
#####################################################################################


NoOpen.HV.Metric.M2<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+C3+C4
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit2<-cfa(model = NoOpen.HV.Metric.M2,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           group.equal="loadings",
                           group.partial=c("SelEnhan=~SE3"),
                           std.lv=T)

sink("./Sink Output/ESS8/TEST_NoOpen_HV_Metric_fit2.txt")
summary(NoOpen.HV.Metric.Fit2, fit.measures=T, standardized=T)
sink()