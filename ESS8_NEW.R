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
################## HV full measurement model with 4 values ##########################
#####################################################################################

##Configural Model 1:
HV.Config.M1<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5
Conser=~C1+C2+C3+C4+C5+C6
OpenChange=~O1+O2+O3+O4+O5+O6
SelfEnhan=~SE1+SE2+SE3+SE4
'

HV.Config.Fit1<-cfa(model = HV.Config.M1,
                    data = ESS8,
                    group = "country",
                    estimator="MLR",
                    missing="FIML",
                    std.lv=T)

MI.Config.M1<-modindices(HV.Config.Fit1, minimum.value = 10, sort. = T)
MI.Config.M1<-MI.Config.M1 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))

ParameterCount<-MI.Config.M1 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

sink("./Sink Output/ESS8/HV_Config_fit1.txt")
summary(HV.Config.Fit1, fit.measures=T, standardized=T)
sink()

##Configural Model 2: Add cross loading Conser=~O4
HV.Config.M2<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5
Conser=~C1+C2+C3+C4+C5+C6+O4
OpenChange=~O1+O2+O3+O4+O5+O6
SelfEnhan=~SE1+SE2+SE3+SE4
'

HV.Config.Fit2<-cfa(model = HV.Config.M2,
                    data = ESS8,
                    group = "country",
                    estimator="MLR",
                    missing="FIML",
                    std.lv=T)

MI.Config.M2<-modindices(HV.Config.Fit2, minimum.value = 10, sort. = T)
MI.Config.M2<-MI.Config.M2 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-MI.Config.M2 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

sink("./Sink Output/ESS8/HV_Config_fit2.txt")
summary(HV.Config.Fit2, fit.measures=T, standardized=T)
sink()


##Configural Model 3: Add cross loading SelfTran=~SE3
HV.Config.M3<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3
Conser=~C1+C2+C3+C4+C5+C6+O4
OpenChange=~O1+O2+O3+O4+O5+O6
SelfEnhan=~SE1+SE2+SE3+SE4
'

HV.Config.Fit3<-cfa(model = HV.Config.M3,
                    data = ESS8,
                    group = "country",
                    estimator="MLR",
                    missing="FIML",
                    std.lv=T)

MI.Config.M3<-modindices(HV.Config.Fit3, minimum.value = 10, sort. = T)
MI.Config.M3<-MI.Config.M3 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-MI.Config.M3 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

sink("./Sink Output/ESS8/HV_Config_fit3.txt")
summary(HV.Config.Fit3, fit.measures=T, standardized=T)
sink()

##Configural Model 4: Add error term correlation O5~~O6
HV.Config.M4<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3
Conser=~C1+C2+C3+C4+C5+C6+O4
OpenChange=~O1+O2+O3+O4+O5+O6
SelfEnhan=~SE1+SE2+SE3+SE4

##error term correlation
O5~~O6
'

HV.Config.Fit4<-cfa(model = HV.Config.M4,
                    data = ESS8,
                    group = "country",
                    estimator="MLR",
                    missing="FIML",
                    std.lv=T)

MI.Config.M4<-modindices(HV.Config.Fit4, minimum.value = 10, sort. = T)
MI.Config.M4<-MI.Config.M4 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-MI.Config.M4 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

sink("./Sink Output/ESS8/HV_Config_fit4.txt")
summary(HV.Config.Fit4, fit.measures=T, standardized=T)
sink()

##Configural Model 5: Add cross loading SelfEnhan=~C1
HV.Config.M5<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3
Conser=~C1+C2+C3+C4+C5+C6+O4
OpenChange=~O1+O2+O3+O4+O5+O6
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##error term correlation
O5~~O6
'

HV.Config.Fit5<-cfa(model = HV.Config.M5,
                    data = ESS8,
                    group = "country",
                    estimator="MLR",
                    missing="FIML",
                    std.lv=T)

MI.Config.M5<-modindices(HV.Config.Fit5, minimum.value = 10, sort. = T)
MI.Config.M5<-MI.Config.M5 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-MI.Config.M5 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

sink("./Sink Output/ESS8/HV_Config_fit5.txt")
summary(HV.Config.Fit5, fit.measures=T, standardized=T)
sink()

##Configural Model 6: Add cross-loading: Conser=~SE4
HV.Config.M6<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3
Conser=~C1+C2+C3+C4+C5+C6+O4+SE4
OpenChange=~O1+O2+O3+O4+O5+O6
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##error term correlation
O5~~O6
'

HV.Config.Fit6<-cfa(model = HV.Config.M6,
                    data = ESS8,
                    group = "country",
                    estimator="MLR",
                    missing="FIML",
                    std.lv=T)
MI.Config.M6<-modindices(HV.Config.Fit6, minimum.value = 10, sort. = T)
MI.Config.M6<-MI.Config.M6 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-MI.Config.M6 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

sink("./Sink Output/ESS8/HV_Config_fit6.txt")
summary(HV.Config.Fit6, fit.measures=T, standardized=T)
sink()

##Configural Model 7: SelfTran=~O2
HV.Config.M7<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+O2
Conser=~C1+C2+C3+C4+C5+C6+O4+SE4
OpenChange=~O1+O2+O3+O4+O5+O6
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##error term correlation
O5~~O6
'

HV.Config.Fit7<-cfa(model = HV.Config.M7,
                    data = ESS8,
                    group = "country",
                    estimator="MLR",
                    missing="FIML",
                    std.lv=T)

MI.Config.M7<-modindices(HV.Config.Fit7, minimum.value = 10, sort. = T)
MI.Config.M7<-MI.Config.M7 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-MI.Config.M7 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

sink("./Sink Output/ESS8/HV_Config_fit7.txt")
summary(HV.Config.Fit7, fit.measures=T, standardized=T)
sink()

##Configural Model 8: Add error term correlation C3~~C4
HV.Config.M8<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+O2
Conser=~C1+C2+C3+C4+C5+C6+O4+SE4
OpenChange=~O1+O2+O3+O4+O5+O6
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##error term correlation
O5~~O6
C3~~C4
'

HV.Config.Fit8<-cfa(model = HV.Config.M8,
                    data = ESS8,
                    group = "country",
                    estimator="MLR",
                    missing="FIML",
                    std.lv=T)

MI.Config.M8<-modindices(HV.Config.Fit8, minimum.value = 10, sort. = T)
MI.Config.M8<-MI.Config.M8 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-MI.Config.M8 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

sink("./Sink Output/ESS8/HV_Config_fit8.txt")
summary(HV.Config.Fit8, fit.measures=T, standardized=T)
sink()

##Configural Model 9: Add cross-loading SelfTran=~C1
HV.Config.M9<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+O2+C1
Conser=~C1+C2+C3+C4+C5+C6+O4+SE4
OpenChange=~O1+O2+O3+O4+O5+O6
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##error term correlation
O5~~O6
C3~~C4
'

HV.Config.Fit9<-cfa(model = HV.Config.M9,
                    data = ESS8,
                    group = "country",
                    estimator="MLR",
                    missing="FIML",
                    std.lv=T)

MI.Config.M9<-modindices(HV.Config.Fit9, minimum.value = 10, sort. = T)
MI.Config.M9<-MI.Config.M9 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-MI.Config.M9 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

sink("./Sink Output/ESS8/HV_Config_fit9.txt")
summary(HV.Config.Fit9, fit.measures=T, standardized=T)
sink()

##Configural Model 10: 
##first attempt: add error term correlation C5~~C6 
##--> leads to correlation between SelfTran and Conser larger than 1 in group 16
##second attempt: add cross-loading SelfTran=~O1
HV.Config.M10<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+O2+C1+O1
Conser=~C1+C2+C3+C4+C5+C6+O4+SE4
OpenChange=~O1+O2+O3+O4+O5+O6
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##error term correlation
O5~~O6
C3~~C4
'

HV.Config.Fit10<-cfa(model = HV.Config.M10,
                    data = ESS8,
                    group = "country",
                    estimator="MLR",
                    missing="FIML",
                    std.lv=T)

MI.Config.M10<-modindices(HV.Config.Fit10, minimum.value = 10, sort. = T)
MI.Config.M10<-MI.Config.M10 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-MI.Config.M10 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

sink("./Sink Output/ESS8/HV_Config_fit10.txt")
summary(HV.Config.Fit10, fit.measures=T, standardized=T)
sink()

#####################################################################################
################## HV measurement model without Openness ############################
#####################################################################################

##Configural model 1
NoOpen.HV.Config.M1<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5
Conser=~C1+C2+C3+C4+C5+C6
SelfEnhan=~SE1+SE2+SE3+SE4
'

NoOpen.HV.Config.Fit1<-cfa(model = NoOpen.HV.Config.M1,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/ESS8/NoOpen_HV_Config_fit1.txt")
summary(NoOpen.HV.Config.Fit1, fit.measures=T, standardized=T)
sink()

NoOpen.MI.Config.M1<-modindices(NoOpen.HV.Config.Fit1, minimum.value = 10, sort. = T)
NoOpen.MI.Config.M1<-NoOpen.MI.Config.M1 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-NoOpen.MI.Config.M1 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

##Configural Model 2: add cross-loading: Conser=~SE4
NoOpen.HV.Config.M2<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4
'

NoOpen.HV.Config.Fit2<-cfa(model = NoOpen.HV.Config.M2,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/ESS8/NoOpen_HV_Config_fit2.txt")
summary(NoOpen.HV.Config.Fit2, fit.measures=T, standardized=T)
sink()

NoOpen.MI.Config.M2<-modindices(NoOpen.HV.Config.Fit2, minimum.value = 10, sort. = T)
NoOpen.MI.Config.M2<-NoOpen.MI.Config.M2 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-NoOpen.MI.Config.M2 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))


##Configural Model 3: add cross-loading: SelfTran=~SE3
NoOpen.HV.Config.M3<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4
'

NoOpen.HV.Config.Fit3<-cfa(model = NoOpen.HV.Config.M3,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/ESS8/NoOpen_HV_Config_fit3.txt")
summary(NoOpen.HV.Config.Fit3, fit.measures=T, standardized=T)
sink()

NoOpen.MI.Config.M3<-modindices(NoOpen.HV.Config.Fit3, minimum.value = 10, sort. = T)
NoOpen.MI.Config.M3<-NoOpen.MI.Config.M3 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-NoOpen.MI.Config.M3 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

##Configural Model 4: add cross-loading: SelfEnhan=~C1
NoOpen.HV.Config.M4<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1
'

NoOpen.HV.Config.Fit4<-cfa(model = NoOpen.HV.Config.M4,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/ESS8/NoOpen_HV_Config_fit4.txt")
summary(NoOpen.HV.Config.Fit4, fit.measures=T, standardized=T)
sink()

NoOpen.MI.Config.M4<-modindices(NoOpen.HV.Config.Fit4, minimum.value = 10, sort. = T)
NoOpen.MI.Config.M4<-NoOpen.MI.Config.M4 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-NoOpen.MI.Config.M4 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))


##Configural Model 5: add error term correlation: C5~~C6
NoOpen.HV.Config.M5<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Config.Fit5<-cfa(model = NoOpen.HV.Config.M5,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/ESS8/NoOpen_HV_Config_fit5.txt")
summary(NoOpen.HV.Config.Fit5, fit.measures=T, standardized=T)
sink()

NoOpen.MI.Config.M5<-modindices(NoOpen.HV.Config.Fit5, minimum.value = 10, sort. = T)
NoOpen.MI.Config.M5<-NoOpen.MI.Config.M5 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-NoOpen.MI.Config.M5 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

##Configural Model 6: Allow cross-loading SelfTran=~C3
NoOpen.HV.Config.M6<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+C3
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Config.Fit6<-cfa(model = NoOpen.HV.Config.M6,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/ESS8/NoOpen_HV_Config_fit6.txt")
summary(NoOpen.HV.Config.Fit6, fit.measures=T, standardized=T)
sink()

NoOpen.MI.Config.M6<-modindices(NoOpen.HV.Config.Fit6, minimum.value = 10, sort. = T)
NoOpen.MI.Config.M6<-NoOpen.MI.Config.M6 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-NoOpen.MI.Config.M6 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

##Configural Model 7: Allow cross-loading SelfTran=~C4
NoOpen.HV.Config.M7<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+C3+C4
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Config.Fit7<-cfa(model = NoOpen.HV.Config.M7,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           std.lv=T)

sink("./Sink Output/ESS8/NoOpen_HV_Config_fit7.txt")
summary(NoOpen.HV.Config.Fit7, fit.measures=T, standardized=T)
sink()

NoOpen.MI.Config.M7<-modindices(NoOpen.HV.Config.Fit7, minimum.value = 10, sort. = T)
NoOpen.MI.Config.M7<-NoOpen.MI.Config.M7 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))
ParameterCount<-NoOpen.MI.Config.M7 %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

##(full) Metric Model 1
NoOpen.HV.Metric.M1<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+C3+C4
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit1<-cfa(model = NoOpen.HV.Metric.M1,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           group.equal="loadings",
                           std.lv=T)

sink("./Sink Output/ESS8/NoOpen_HV_Metric_fit1.txt")
summary(NoOpen.HV.Metric.Fit1, fit.measures=T, standardized=T)
sink()

##request modification indices:
NoOpen.MI.Metric.M1<-lavTestScore(NoOpen.HV.Metric.Fit1, epc = T)

Chi2Diff.MI.M1<-NoOpen.MI.Metric.M1$uni
EPC.MI.M1<-NoOpen.MI.Metric.M1$epc

Chi2Diff.MI.M1<-Chi2Diff.MI.M1 %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M1<-EPC.MI.M1 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))

EPC.Chi2Diff.M1<-merge(EPC.MI.M1, Chi2Diff.MI.M1,
                       by.x = "plabel",
                       by.y = "plabel")

EPC.Chi2Diff.Summary<-EPC.Chi2Diff.M1 %>%
  filter(X2 >= 10) %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

##(Partial) Metric Model 2: let SelfEnhan=~SE3 to be freely estimated
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
                           group.partial=c("SelfEnhan=~SE3"),
                           std.lv=T)

sink("./Sink Output/ESS8/NoOpen_HV_Metric_fit2.txt")
summary(NoOpen.HV.Metric.Fit2, fit.measures=T, standardized=T)
sink()

##request modification indices:
NoOpen.MI.Metric.M2<-lavTestScore(NoOpen.HV.Metric.Fit2, epc = T)

Chi2Diff.MI.M2<-NoOpen.MI.Metric.M2$uni
Chi2Diff.MI.M2<-Chi2Diff.MI.M2 %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M2<-NoOpen.MI.Metric.M2$epc
EPC.MI.M2<-EPC.MI.M2 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))

EPC.Chi2Diff.M2<-merge(EPC.MI.M2, Chi2Diff.MI.M2,
                       by.x = "plabel",
                       by.y = "plabel")

EPC.Chi2Diff.Summary<-EPC.Chi2Diff.M2 %>%
  filter(X2 >= 10) %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

##(Partial) Metric Model 3: let SelfEnhan=~C1 to be freely estimated
NoOpen.HV.Metric.M3<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+C3+C4
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit3<-cfa(model = NoOpen.HV.Metric.M3,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           group.equal="loadings",
                           group.partial=c("SelfEnhan=~SE3",
                                           "SelfEnhan=~C1"),
                           std.lv=T)

sink("./Sink Output/ESS8/NoOpen_HV_Metric_fit3.txt")
summary(NoOpen.HV.Metric.Fit3, fit.measures=T, standardized=T)
sink()

##request modification indices:
NoOpen.MI.Metric.M3<-lavTestScore(NoOpen.HV.Metric.Fit3, epc = T)

Chi2Diff.MI.M3<-NoOpen.MI.Metric.M3$uni
Chi2Diff.MI.M3<-Chi2Diff.MI.M3 %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M3<-NoOpen.MI.Metric.M3$epc
EPC.MI.M3<-EPC.MI.M3 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))

EPC.Chi2Diff.M3<-merge(EPC.MI.M3, Chi2Diff.MI.M3,
                       by.x = "plabel",
                       by.y = "plabel")

EPC.Chi2Diff.Summary<-EPC.Chi2Diff.M3 %>%
  filter(X2 >= 10) %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))


##(Partial) Metric Model 4: let SelfTran=~SE3 to be freely estimated
NoOpen.HV.Metric.M4<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+C3+C4
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit4<-cfa(model = NoOpen.HV.Metric.M4,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           group.equal="loadings",
                           group.partial=c("SelfEnhan=~SE3",
                                           "SelfEnhan=~C1",
                                           "SelfTran=~SE3"),
                           std.lv=T)

sink("./Sink Output/ESS8/NoOpen_HV_Metric_fit4.txt")
summary(NoOpen.HV.Metric.Fit4, fit.measures=T, standardized=T)
sink()

##request modification indices:
NoOpen.MI.Metric.M4<-lavTestScore(NoOpen.HV.Metric.Fit4, epc = T)

Chi2Diff.MI.M4<-NoOpen.MI.Metric.M4$uni
Chi2Diff.MI.M4<-Chi2Diff.MI.M4 %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M4<-NoOpen.MI.Metric.M4$epc
EPC.MI.M4<-EPC.MI.M4 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))

EPC.Chi2Diff.M4<-merge(EPC.MI.M4, Chi2Diff.MI.M4,
                       by.x = "plabel",
                       by.y = "plabel")

EPC.Chi2Diff.Summary<-EPC.Chi2Diff.M4 %>%
  filter(X2 >= 10) %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

##(Partial) Metric Model 5: let Conser=~C1 to be freely estimated
NoOpen.HV.Metric.M5<-'
SelfTran=~ST1+ST2+ST3+ST4+ST5+SE3+C3+C4
Conser=~C1+C2+C3+C4+C5+C6+SE4
SelfEnhan=~SE1+SE2+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit5<-cfa(model = NoOpen.HV.Metric.M5,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           group.equal="loadings",
                           group.partial=c("SelfEnhan=~SE3",
                                           "SelfEnhan=~C1",
                                           "SelfTran=~SE3",
                                           "Conser=~C1"),
                           std.lv=T)

sink("./Sink Output/ESS8/NoOpen_HV_Metric_fit5.txt")
summary(NoOpen.HV.Metric.Fit5, fit.measures=T, standardized=T)
sink()
