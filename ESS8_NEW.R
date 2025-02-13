library(dplyr)
library(lavaan)
library(tidyr)
library(ggplot2)
library(plotly)
library(forcats)
library(htmltools)
library(maps)
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

##climate change belief (higher value indicates higher level of awareness and belief in climate change)
ESS8<-ESS8 %>%
  mutate(TrendBelief=ifelse(clmchng %in% c(7,8,9), NA, clmchng),
       AttriBelief=ifelse(ccnthum %in% c(55,66,77,88,99), NA, ccnthum),
       ImpactBelief=ifelse(ccgdbd %in% c(66,77,88,99), NA, ccgdbd)) %>%
  mutate(TrendBelief=-(TrendBelief)+5,
         ImpactBelief=-(ImpactBelief)+10) %>%
  mutate(ImpactBelief=ImpactBelief/2)

##Climate change concern
ESS8<-ESS8 %>%
  mutate(ClimateConcern=ifelse(wrclmch %in% c(6,7,8,9), NA, wrclmch))

##Personal Efficacy regarding climate change
ESS8<-ESS8 %>%
  mutate(PE1=ifelse(cflsenr %in% c(77,88,99), NA, cflsenr),
        PE2=ifelse(ccrdprs %in% c(66,77,88,99), NA, ccrdprs))

##opposition to climate change policy
ESS8[,c("support1","support2","support3")]<-lapply(ESS8[,c("inctxff","sbsrnen","banhhap")],
                                           function(x) ifelse(x %in% c(7,8,9), NA, x))
ESS8[,c("support1","support2","support3")]<-lapply(ESS8[,c("support1","support2","support3")],
                                                   function(x) -x+6)

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
################## DUMP IT! HV full measurement model with 4 values #################
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

#####################################################################################
############### Climate Change Belief - Measurement Model ###########################
#####################################################################################

##Configural invariance Model 1: perfect fit since the model is just identified
CCBelief.Config.M1<-'
CCBelief=~TrendBelief+AttriBelief+ImpactBelief
'

CCBelief.Config.Fit1<-cfa(model = CCBelief.Config.M1,
                          data = ESS8,
                          group = "country",
                          estimator="MLR",
                          missing="FIML",
                          std.lv=T)

sink("./Sink Output/ESS8/CCBelief_Config_fit1.txt")
summary(CCBelief.Config.Fit1, fit.measures=T, standardized=T)
sink()

##(Full) Metric Model 1: 
CCBelief.Metric.M1<-'
CCBelief=~TrendBelief+AttriBelief+ImpactBelief
'

CCBelief.Metric.Fit1<-cfa(model = CCBelief.Metric.M1,
                          data = ESS8,
                          group = "country",
                          estimator="MLR",
                          missing="FIML",
                          group.equal="loadings",
                          std.lv=T)

sink("./Sink Output/ESS8/CCBelief_Metric_fit1.txt")
summary(CCBelief.Metric.Fit1, fit.measures=T, standardized=T)
sink()

#####################################################################################
############# Climate Change Policy Support - Measurement Model #####################
#####################################################################################


###---------------------------------------------------------------------------------------
##Configural Model with standardized factor variance approach:

##Configural Invariance Model 1: should be perfect fit since the model is just identified
##but cannot converge
CCPolSupport.Config.M1<-'
CCPolicySupport=~support1+support2+support3
'

CCPolSupport.Config.Fit1<-cfa(model = CCPolSupport.Config.M1,
                              data = ESS8,
                              group = "country",
                              estimator="MLR",
                              missing="FIML",
                              std.lv=T)

sink("./Sink Output/ESS8/CCPolicySupport_Config_fit1.txt")
summary(CCPolSupport.Config.Fit1, fit.measures=T, standardized=T)
sink()

##Configural Invariance Model 1 with wide bounded estimation:
CCPolSupport.Config.M1<-'
CCPolicySupport=~support1+support2+support3
'

CCPolSupport.Config.Fit1.WideBound<-cfa(model = CCPolSupport.Config.M1,
                              data = ESS8,
                              group = "country",
                              estimator="MLR",
                              missing="FIML",
                              std.lv=T,
                              bounds="wide")

sink("./Sink Output/ESS8/CCPolicySupport_Config_fit1_WideBound.txt")
summary(CCPolSupport.Config.Fit1.WideBound, fit.measures=T, standardized=T)
sink()

##Configural Invariance Model 1 with standard bounded estimation:
CCPolSupport.Config.M1<-'
CCPolicySupport=~support1+support2+support3
'

CCPolSupport.Config.Fit1.StandardBound<-cfa(model = CCPolSupport.Config.M1,
                                        data = ESS8,
                                        group = "country",
                                        estimator="MLR",
                                        missing="FIML",
                                        std.lv=T,
                                        bounds="standard")

sink("./Sink Output/ESS8/CCPolicySupport_Config_fit1_StandardBound.txt")
summary(CCPolSupport.Config.Fit1.StandardBound, fit.measures=T, standardized=T)
sink()

###---------------------------------------------------------------------------------------
##Full Metric Model with standardized factor variance approach:

##(full) metric Invariance Model 1:
CCPolSupport.Metric.M1<-'
CCPolicySupport=~support1+support2+support3
'

CCPolSupport.Metric.Fit1<-cfa(model = CCPolSupport.Metric.M1,
                              data = ESS8,
                              group = "country",
                              estimator="MLR",
                              missing="FIML",
                              group.equal="loadings",
                              std.lv=T)

sink("./Sink Output/ESS8/CCPolicySupport_Metric_fit1.txt")
summary(CCPolSupport.Metric.Fit1, fit.measures=T, standardized=T)
sink()


##print the modification indices
#sink("./Sink Output/ESS8/CCPolicySupport_FullMetric_MI.txt")
#options(max.print = 99999)
#lavTestScore(CCPolSupport.Metric.Fit1, epc = T)
#sink()

##request modification indices:
CCPolSupport.MI.Metric.M1<-lavTestScore(CCPolSupport.Metric.Fit1, epc = T)

Chi2Diff.MI.M1<-CCPolSupport.MI.Metric.M1$uni
Chi2Diff.MI.M1<-Chi2Diff.MI.M1 %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M1<-CCPolSupport.MI.Metric.M1$epc
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


##(full) metric Invariance Model 1 with wide bounded estimation:
CCPolSupport.Metric.M1.wide<-'
##constrain the loadings to be equal across group
CCPolicySupport=~L1*NA*support1+
                  L2*support2+
                  L3*support3

##Constrain the first group to have the variance as 1:
CCPolicySupport~~c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*CCPolicySupport
'

CCPolSupport.Metric.Fit1.WideBound<-cfa(model = CCPolSupport.Metric.M1.wide,
                                        data = ESS8,
                                        group = "country",
                                        estimator="MLR",
                                        missing="FIML",
                                        group.equal="loadings",
                                        bounds="wide")

sink("./Sink Output/ESS8/CCPolicySupport_Metric_fit1_WideBound.txt")
summary(CCPolSupport.Metric.Fit1.WideBound, fit.measures=T, standardized=T)
sink()


##print the modification indices
#sink("./Sink Output/ESS8/CCPolicySupport_FullMetric_WideBound_MI.txt")
#options(max.print = 99999)
#lavTestScore(CCPolSupport.Metric.Fit1.WideBound, epc = T)
#sink()

##request modification indices:
CCPolSupport.MI.Metric.M1.wide<-lavTestScore(CCPolSupport.Metric.Fit1.WideBound, epc = T)

Chi2Diff.MI.M1.wide<-CCPolSupport.MI.Metric.M1.wide$uni
Chi2Diff.MI.M1.wide<-Chi2Diff.MI.M1.wide %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M1.wide<-CCPolSupport.MI.Metric.M1.wide$epc
EPC.MI.M1.wide<-EPC.MI.M1.wide %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))

EPC.Chi2Diff.M1.wide<-merge(EPC.MI.M1.wide, Chi2Diff.MI.M1.wide,
                       by.x = "plabel",
                       by.y = "plabel")

EPC.Chi2Diff.wide.Summary<-EPC.Chi2Diff.M1.wide %>%
  filter(X2 >= 10) %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))


##(full) metric Invariance Model 1 with standard bounded estimation:
CCPolSupport.Metric.M1.standard<-'
##constrain the loadings to be equal across group
CCPolicySupport=~L1*NA*support1+
                  L2*support2+
                  L3*support3

##Constrain the first group to have the variance as 1:
CCPolicySupport~~c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*CCPolicySupport
'

CCPolSupport.Metric.Fit1.StandBound<-cfa(model = CCPolSupport.Metric.M1.standard,
                                        data = ESS8,
                                        group = "country",
                                        estimator="MLR",
                                        missing="FIML",
                                        group.equal="loadings",
                                        bounds="standard")

sink("./Sink Output/ESS8/CCPolicySupport_Metric_fit1_StandBound.txt")
summary(CCPolSupport.Metric.Fit1.StandBound, fit.measures=T, standardized=T)
sink()


##request modification indices:
CCPolSupport.MI.Metric.M1.Stand<-lavTestScore(CCPolSupport.Metric.Fit1.StandBound, epc = T)

Chi2Diff.MI.M1.Stand<-CCPolSupport.MI.Metric.M1.Stand$uni
Chi2Diff.MI.M1.Stand<-Chi2Diff.MI.M1.Stand %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M1.Stand<-CCPolSupport.MI.Metric.M1.Stand$epc
EPC.MI.M1.Stand<-EPC.MI.M1.Stand %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))

EPC.Chi2Diff.M1.Stand<-merge(EPC.MI.M1.Stand, Chi2Diff.MI.M1.Stand,
                            by.x = "plabel",
                            by.y = "plabel")

EPC.Chi2Diff.Stand.Summary<-EPC.Chi2Diff.M1.Stand %>%
  filter(X2 >= 10) %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))


###---------------------------------------------------------------------------------------
##Full Metric Model with Marker Variable approach (support2 as marker):

##no bound
CCPolSupport.Metric.M1.Marker<-'
CCPolicySupport=~support2+support1+support3
'

CCPolSupport.Metric.Fit1.Marker<-cfa(model = CCPolSupport.Metric.M1.Marker,
                                     data = ESS8,
                                     group = "country",
                                     estimator="MLR",
                                     missing="FIML",
                                     group.equal="loadings")

sink("./Sink Output/ESS8/CCPolicySupport_Metric_fit1_Marker.txt")
summary(CCPolSupport.Metric.Fit1.Marker, fit.measures=T, standardized=T)
sink()

##request mi
CCPolSupport.MI.Metric.M1<-lavTestScore(CCPolSupport.Metric.Fit1.Marker, epc = T)

Chi2Diff.MI.M1<-CCPolSupport.MI.Metric.M1$uni
Chi2Diff.MI.M1<-Chi2Diff.MI.M1 %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M1<-CCPolSupport.MI.Metric.M1$epc
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

##Support2 as marker: wide bound
CCPolSupport.Metric.M1.Marker<-'
CCPolicySupport=~support2+support1+support3
'

CCPolSupport.Metric.Fit1.WideBound.Marker<-cfa(model = CCPolSupport.Metric.M1.Marker,
                                        data = ESS8,
                                        group = "country",
                                        estimator="MLR",
                                        missing="FIML",
                                        group.equal="loadings",
                                        bounds="wide")

sink("./Sink Output/ESS8/CCPolicySupport_Metric_fit1_Marker_WideBound.txt")
summary(CCPolSupport.Metric.Fit1.WideBound.Marker, fit.measures=T, standardized=T)
sink()

##request modification indices:
CCPolSupport.MI.Metric.M1.wide<-lavTestScore(CCPolSupport.Metric.Fit1.WideBound.Marker, epc = T)

Chi2Diff.MI.M1.wide<-CCPolSupport.MI.Metric.M1.wide$uni
Chi2Diff.MI.M1.wide<-Chi2Diff.MI.M1.wide %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M1.wide<-CCPolSupport.MI.Metric.M1.wide$epc
EPC.MI.M1.wide<-EPC.MI.M1.wide %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))

EPC.Chi2Diff.M1.wide<-merge(EPC.MI.M1.wide, Chi2Diff.MI.M1.wide,
                            by.x = "plabel",
                            by.y = "plabel")

EPC.Chi2Diff.wide.Summary<-EPC.Chi2Diff.M1.wide %>%
  filter(X2 >= 10) %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

##Support2 as marker: standard bound
CCPolSupport.Metric.M1.Marker<-'
CCPolicySupport=~support2+support1+support3
'

CCPolSupport.Metric.Fit1.StandBound.Marker<-cfa(model = CCPolSupport.Metric.M1.Marker,
                                               data = ESS8,
                                               group = "country",
                                               estimator="MLR",
                                               missing="FIML",
                                               group.equal="loadings",
                                               bounds="standard")

sink("./Sink Output/ESS8/CCPolicySupport_Metric_fit1_Marker_StandardBound.txt")
summary(CCPolSupport.Metric.Fit1.StandBound.Marker, fit.measures=T, standardized=T)
sink()

##request mi:
CCPolSupport.MI.Metric.M1.Stand<-lavTestScore(CCPolSupport.Metric.Fit1.StandBound.Marker, epc = T)

Chi2Diff.MI.M1.Stand<-CCPolSupport.MI.Metric.M1.Stand$uni
Chi2Diff.MI.M1.Stand<-Chi2Diff.MI.M1.Stand %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M1.Stand<-CCPolSupport.MI.Metric.M1.Stand$epc
EPC.MI.M1.Stand<-EPC.MI.M1.Stand %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))

EPC.Chi2Diff.M1.Stand<-merge(EPC.MI.M1.Stand, Chi2Diff.MI.M1.Stand,
                             by.x = "plabel",
                             by.y = "plabel")

EPC.Chi2Diff.Stand.Summary<-EPC.Chi2Diff.M1.Stand %>%
  filter(X2 >= 10) %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))



###---------------------------------------------------------------------------------------
##Partial Metric Model with standardized factor variance approach:

##no bound:
CCPolSupport.Metric.M2<-'
##constrain the loadings to be equal across group
CCPolicySupport=~L1*NA*support2+
                  L2*support1+
                  c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*support3

##Constrain the first group to have the variance as 1:
CCPolicySupport~~c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*CCPolicySupport
'

CCPolSupport.Metric.Fit2<-cfa(model = CCPolSupport.Metric.M2,
                                        data = ESS8,
                                        group = "country",
                                        estimator="MLR",
                                        missing="FIML")

sink("./Sink Output/ESS8/CCPolicySupport_Metric_fit2.txt")
summary(CCPolSupport.Metric.Fit2, fit.measures=T, standardized=T)
sink()


##Partial with standardized factor variance approach:
##wide bound
CCPolSupport.Metric.M2.wide<-'
##constrain the loadings to be equal across group
CCPolicySupport=~L1*NA*support2+
                  L2*support1+
                  c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*support3

##Constrain the first group to have the variance as 1:
CCPolicySupport~~c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*CCPolicySupport
'

CCPolSupport.Metric.Fit2.WideBound<-cfa(model = CCPolSupport.Metric.M2.wide,
                                        data = ESS8,
                                        group = "country",
                                        estimator="MLR",
                                        missing="FIML",
                                        bounds="wide")

sink("./Sink Output/ESS8/CCPolicySupport_Metric_fit2_WideBound.txt")
summary(CCPolSupport.Metric.Fit2.WideBound, fit.measures=T, standardized=T)
sink()

##Partial with standardized factor variance approach:
##wide bound
CCPolSupport.Metric.M2.Stand<-'
##constrain the loadings to be equal across group
CCPolicySupport=~L1*NA*support2+
                  L2*support1+
                  c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*support3

##Constrain the first group to have the variance as 1:
CCPolicySupport~~c(1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*CCPolicySupport
'

CCPolSupport.Metric.Fit2.StandBound<-cfa(model = CCPolSupport.Metric.M2.Stand,
                                        data = ESS8,
                                        group = "country",
                                        estimator="MLR",
                                        missing="FIML",
                                        bounds="standard")

sink("./Sink Output/ESS8/CCPolicySupport_Metric_fit2_StandBound.txt")
summary(CCPolSupport.Metric.Fit2.StandBound, fit.measures=T, standardized=T)
sink()


###---------------------------------------------------------------------------------------
##Partial Metric Model with Marker Variable approach (support2 as marker):

##(partial) metric Invariance Model 2: free the loading CCPolicySupport=~Support3
##no bound
##cannot converge
CCPolSupport.Metric.M2.Marker<-'
##constrain the loadings to be equal across group
CCPolicySupport=~L1*support2+
                  L2*support1+
                  c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*support3
'

CCPolSupport.Metric.Fit2.Marker<-cfa(model = CCPolSupport.Metric.M2.Marker,
                                     data = ESS8,
                                     group = "country",
                                     estimator="MLR",
                                     missing="FIML")

sink("./Sink Output/ESS8/CCPolicySupport_Metric_fit2_marker.txt")
summary(CCPolSupport.Metric.Fit2.Marker, fit.measures=T, standardized=T)
sink()

##(partial) metric Invariance Model 2: free the loading CCPolicySupport=~Support3
##wide bound
CCPolSupport.Metric.M2.Marker<-'
##constrain the loadings to be equal across group
CCPolicySupport=~L1*support2+
                  L2*support1+
                  c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*support3
'

CCPolSupport.Metric.Fit2.Marker.WideBound<-cfa(model = CCPolSupport.Metric.M2.Marker,
                                     data = ESS8,
                                     group = "country",
                                     estimator="MLR",
                                     missing="FIML",
                                     bounds="wide")

sink("./Sink Output/ESS8/CCPolicySupport_Metric_fit2_marker_wideBound.txt")
summary(CCPolSupport.Metric.Fit2.Marker.WideBound, fit.measures=T, standardized=T)
sink()


##(partial) metric Invariance Model 2: free the loading CCPolicySupport=~Support3
##standard bound
CCPolSupport.Metric.M2.Marker<-'
##constrain the loadings to be equal across group
CCPolicySupport=~L1*support2+
                  L2*support1+
                  c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)*support3
'

CCPolSupport.Metric.Fit2.Marker.StandBound<-cfa(model = CCPolSupport.Metric.M2.Marker,
                                               data = ESS8,
                                               group = "country",
                                               estimator="MLR",
                                               missing="FIML",
                                               bounds="standard")

sink("./Sink Output/ESS8/CCPolicySupport_Metric_fit2_marker_StandBound.txt")
summary(CCPolSupport.Metric.Fit2.Marker.StandBound, fit.measures=T, standardized=T)
sink()


#####################################################################################
############# Climate Change Policy Support - MM (removing Hungary) #################
#####################################################################################

ESS8_noHU<-ESS8 %>%
  filter(country != "HU")

###---------------------------------------------------------------------------------------
##Configural Invariance Model 1: should be perfect fit since the model is just identified
##can converge now
CCPolSupport.Config.M1<-'
CCPolicySupport=~support1+support2+support3
'

CCPolSupport.Config.Fit1<-cfa(model = CCPolSupport.Config.M1,
                              data = ESS8_noHU,
                              group = "country",
                              estimator="MLR",
                              missing="FIML",
                              std.lv=T)

sink("./Sink Output/ESS8/NoHU_CCPolSup_Config_fit1.txt")
summary(CCPolSupport.Config.Fit1, fit.measures=T, standardized=T)
sink()


##(full) metric Invariance Model 1:
CCPolSupport.Metric.M1<-'
CCPolicySupport=~support1+support2+support3
'

CCPolSupport.Metric.Fit1<-cfa(model = CCPolSupport.Metric.M1,
                              data = ESS8_noHU,
                              group = "country",
                              estimator="MLR",
                              missing="FIML",
                              group.equal="loadings",
                              std.lv=T)

sink("./Sink Output/ESS8/NoHU_CCPolSup_Metric_fit1.txt")
summary(CCPolSupport.Metric.Fit1, fit.measures=T, standardized=T)
sink()


##request modification indices:
CCPolSupport.MI.Metric.M1<-lavTestScore(CCPolSupport.Metric.Fit1, epc = T)

Chi2Diff.MI.M1<-CCPolSupport.MI.Metric.M1$uni
Chi2Diff.MI.M1<-Chi2Diff.MI.M1 %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M1<-CCPolSupport.MI.Metric.M1$epc
EPC.MI.M1<-EPC.MI.M1 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))

EPC.Chi2Diff.M1<-merge(EPC.MI.M1, Chi2Diff.MI.M1,
                       by.x = "plabel",
                       by.y = "plabel")

EPC.Chi2Diff.Summary<-EPC.Chi2Diff.M1 %>%
  filter(X2 >= 15) %>%
  group_by(parameter) %>%
  summarise(count=n()) %>%
  arrange(desc(count))



##(partial) metric Invariance Model 1:
##test to free support3:
##(full) metric Invariance Model 1:
CCPolSupport.Metric.M2<-'
CCPolicySupport=~support1+support2+support3
'

CCPolSupport.Metric.Fit2.FreeSup3<-cfa(model = CCPolSupport.Metric.M2,
                              data = ESS8_noHU,
                              group = "country",
                              estimator="MLR",
                              missing="FIML",
                              group.equal="loadings",
                              group.partial=c("CCPolicySupport=~support3"),
                              std.lv=T)

sink("./Sink Output/ESS8/NoHU_CCPolSup_Metric_fit2_freeSup3.txt")
summary(CCPolSupport.Metric.Fit2.FreeSup3, fit.measures=T, standardized=T)
sink()


##test to free support2:
##(full) metric Invariance Model 1:
CCPolSupport.Metric.M2<-'
CCPolicySupport=~support1+support2+support3
'

CCPolSupport.Metric.Fit2.FreeSup2<-cfa(model = CCPolSupport.Metric.M2,
                                       data = ESS8_noHU,
                                       group = "country",
                                       estimator="MLR",
                                       missing="FIML",
                                       group.equal="loadings",
                                       group.partial=c("CCPolicySupport=~support2"),
                                       std.lv=T)

sink("./Sink Output/ESS8/NoHU_CCPolSup_Metric_fit2_freeSup2.txt")
summary(CCPolSupport.Metric.Fit2.FreeSup2, fit.measures=T, standardized=T)
sink()
##does not improve the fit


##test to free support1:
##(full) metric Invariance Model 1:
CCPolSupport.Metric.M2<-'
CCPolicySupport=~support1+support2+support3
'

CCPolSupport.Metric.Fit2.FreeSup1<-cfa(model = CCPolSupport.Metric.M2,
                                       data = ESS8_noHU,
                                       group = "country",
                                       estimator="MLR",
                                       missing="FIML",
                                       group.equal="loadings",
                                       group.partial=c("CCPolicySupport=~support1"),
                                       std.lv=T)

sink("./Sink Output/ESS8/NoHU_CCPolSup_Metric_fit2_freeSup1.txt")
summary(CCPolSupport.Metric.Fit2.FreeSup1, fit.measures=T, standardized=T)
sink()
##does not improve the fit so well as freeing support3


#####################################################################################
################### Personal Efficacy - Measurement Model ###########################
#####################################################################################

##Configural model is under-identified --> directly go for full metric invariance model

##(full) metric model 1:
PE.Metric.M1<-'
PEfficacy=~PE1+PE2
'

PE.Metric.Fit1<-cfa(model = PE.Metric.M1,
                    data = ESS8,
                    group = "country",
                    estimator="MLR",
                    missing="FIML",
                    group.equal="loadings",
                    std.lv=T)

sink("./Sink Output/ESS8/PersonalEfficacy_Metric_fit1.txt")
summary(PE.Metric.Fit1, fit.measures=T, standardized=T)
sink()

fitMeasures(PE.Metric.Fit1)
#Error in if (!is.finite(X2) || !is.finite(df) || !is.finite(X2.null) ||  : 
#             missing value where TRUE/FALSE needed
#             In addition: Warning message:
#               lavaan->lav_fit_cfi_lavobject():  
#               computation of robust CFI resulted in NA values.

param<-parameterestimates(PE.Metric.Fit1)
std_param<-standardizedSolution(PE.Metric.Fit1)

##Configural model with climate change belief and climate change efficacy together
PE.CCBelief.Config.M1<-'
CCBelief=~TrendBelief+AttriBelief+ImpactBelief
PEfficacy=~PE1+PE2
'

PE.CCBelief.Config.Fit1<-cfa(model = PE.CCBelief.Config.M1,
                             data = ESS8,
                             group = "country",
                             estimator="MLR",
                             missing="FIML",
                             std.lv=T)

sink("./Sink Output/ESS8/PE_CCBelief_Config_fit1.txt")
summary(PE.CCBelief.Config.Fit1, fit.measures=T, standardized=T)
sink()

##Full Metric model with climate change belief and climate change efficacy together:
PE.CCBelief.Metric.M1<-'
CCBelief=~TrendBelief+AttriBelief+ImpactBelief
PEfficacy=~PE1+PE2
'

PE.CCBelief.Metric.Fit1<-cfa(model = PE.CCBelief.Metric.M1,
                             data = ESS8,
                             group = "country",
                             estimator="MLR",
                             missing="FIML",
                             group.equal="loadings",
                             std.lv=T)

sink("./Sink Output/ESS8/PE_CCBelief_Metric_fit1.txt")
summary(PE.CCBelief.Metric.Fit1, fit.measures=T, standardized=T)
sink()

##request modification indices:
PE.CCBelief.MI.Metric.M1<-lavTestScore(PE.CCBelief.Metric.Fit1, epc = T)

Chi2Diff.MI.M1<-PE.CCBelief.MI.Metric.M1$uni
Chi2Diff.MI.M1<-Chi2Diff.MI.M1 %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M1<-PE.CCBelief.MI.Metric.M1$epc
EPC.MI.M1<-EPC.MI.M1 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))

EPC.Chi2Diff.M1<-merge(EPC.MI.M1, Chi2Diff.MI.M1,
                       by.x = "plabel",
                       by.y = "plabel")


##(partial) Metric model with climate change belief and climate change efficacy together:
PE.CCBelief.Metric.M2<-'
CCBelief=~TrendBelief+AttriBelief+ImpactBelief
PEfficacy=~PE1+PE2
'

PE.CCBelief.Metric.Fit2<-cfa(model = PE.CCBelief.Metric.M2,
                             data = ESS8,
                             group = "country",
                             estimator="MLR",
                             missing="FIML",
                             group.equal="loadings",
                             group.partial=c("CCBelief=~ImpactBelief"),
                             std.lv=T)

sink("./Sink Output/ESS8/PE_CCBelief_Metric_fit2.txt")
summary(PE.CCBelief.Metric.Fit2, fit.measures=T, standardized=T)
sink()




##Configural model with climate change belief and climate change efficacy and climate change worries together
PE.CCBelief.Concern.Config.M1<-'
CCBelief=~TrendBelief+AttriBelief+ImpactBelief
PEfficacy=~PE1+PE2
CCWorry=~ClimateConcern
'

PE.CCBelief.Concern.Config.Fit1<-cfa(model = PE.CCBelief.Concern.Config.M1,
                             data = ESS8,
                             group = "country",
                             estimator="MLR",
                             missing="FIML",
                             std.lv=T)

sink("./Sink Output/ESS8/PE_CCBelief_Concern_Config_fit1.txt")
summary(PE.CCBelief.Concern.Config.Fit1, fit.measures=T, standardized=T)
sink()


##(full) metric model with climate change belief and climate change efficacy and climate change worries together
PE.CCBelief.Concern.Metric.M1<-'
CCBelief=~TrendBelief+AttriBelief+ImpactBelief
PEfficacy=~PE1+PE2
CCWorry=~ClimateConcern
'

PE.CCBelief.Concern.Metric.Fit1<-cfa(model = PE.CCBelief.Concern.Metric.M1,
                                     data = ESS8,
                                     group = "country",
                                     estimator="MLR",
                                     missing="FIML",
                                     group.equal="loadings",
                                     std.lv=T)

sink("./Sink Output/ESS8/PE_CCBelief_Concern_Metric_fit1.txt")
summary(PE.CCBelief.Concern.Metric.Fit1, fit.measures=T, standardized=T)
sink()

##request modification indices:
PE.CCBelief.Concern.MI.Metric.M1<-lavTestScore(PE.CCBelief.Concern.Metric.Fit1, epc = T)

Chi2Diff.MI.M1<-PE.CCBelief.Concern.MI.Metric.M1$uni
Chi2Diff.MI.M1<-Chi2Diff.MI.M1 %>%
  select(rhs, X2) %>%
  rename(plabel=rhs) %>%
  mutate(X2=round(X2, digits = 3))

EPC.MI.M1<-PE.CCBelief.Concern.MI.Metric.M1$epc
EPC.MI.M1<-EPC.MI.M1 %>%
  mutate(parameter=paste(lhs, op, rhs, sep = ""))

EPC.Chi2Diff.M1<-merge(EPC.MI.M1, Chi2Diff.MI.M1,
                       by.x = "plabel",
                       by.y = "plabel")


##(Partial) metric model with climate change belief and climate change efficacy and climate change worries together
##allow CCBelief=~ImpactBelief to be freely estiamted:
PE.CCBelief.Concern.Metric.M2<-'
CCBelief=~TrendBelief+AttriBelief+ImpactBelief
PEfficacy=~PE1+PE2
CCWorry=~ClimateConcern
'

PE.CCBelief.Concern.Metric.Fit2<-cfa(model = PE.CCBelief.Concern.Metric.M2,
                                     data = ESS8,
                                     group = "country",
                                     estimator="MLR",
                                     missing="FIML",
                                     group.equal="loadings",
                                     group.partial=c("CCBelief=~ImpactBelief"),
                                     std.lv=T)

sink("./Sink Output/ESS8/PE_CCBelief_Concern_Metric_fit2.txt")
summary(PE.CCBelief.Concern.Metric.Fit2, fit.measures=T, standardized=T)
sink()



#####################################################################################
################## Regular MGSEM with plot options ##################################
#####################################################################################

##Specify the model:
BasicModel.HV.CCBelief<-'
##human values
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

##Climate Change Belief
CCBelief=~ImpactBelief+TrendBelief+AttriBelief

##Structural Model:
CCBelief~SelfTran+Conser+SelfEnhan
'

##run regular MGSEM:
RegSEM.BasicModel.HV.CCBelief<-cfa(model = BasicModel.HV.CCBelief,
                                   data = ESS8,
                                   group = "country",
                                   estimator="MLR",
                                   missing="FIML",
                                   group.equal="loadings",
                                   group.partial=c("SelfEnhan=~SE3",
                                                   "SelfEnhan=~C1",
                                                   "SelfTran=~SE3",
                                                   "Conser=~C1"))

sink("./Sink Output/ESS8/BasicModel_FreeSEM.txt")
summary(RegSEM.BasicModel.HV.CCBelief, fit.measures=T, standardized=T)
sink()

##extract the parameter estimate
param<-parameterEstimates(RegSEM.BasicModel.HV.CCBelief)

##--------------------------------------------------------------------------------------
##Faceted Dot Plot
reg_param<-param %>%
  filter(op == "~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))

countries<-data.frame(group=c(1:23),
                      country=lavInspect(RegSEM.BasicModel.HV.CCBelief, "group.label"))

reg_param<-merge(reg_param, countries,
                 by.x = "group", by.y = "group")

ggplot(reg_param, aes(x=est, y=country, color=Human.Values))+
  geom_point(size=3)+
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  labs(title = "Simultaneous MGSEM - Human Values on Climate Change Belief")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()


##--------------------------------------------------------------------------------------
##3D scatterplot
reg_param<-param %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

countries<-data.frame(group=c(1:23),
                      country=lavInspect(RegSEM.BasicModel.HV.CCBelief, "group.label"))

reg_param<-merge(reg_param, countries,
                 by.x = "group", by.y = "group")

plot_ly(reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country,
        type = "scatter3d", mode="markers+text") %>%
  layout(title="Simultaneous MGSEM - Human Values on Climate Change Belief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))



#####################################################################################
############################# Basic Model: MMGSEM  ##################################
#####################################################################################

##First, take all the necessary measurement models and change to marker variable approach:
#
##Human Values without Openness to Change
#
NoOpen.HV.Metric.M2.Marker<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit2.Marker<-cfa(model = NoOpen.HV.Metric.M2.Marker,
                           data = ESS8,
                           group = "country",
                           estimator="MLR",
                           missing="FIML",
                           group.equal="loadings",
                           group.partial=c("SelfEnhan=~SE3"))

#sink("./Sink Output/ESS8/NoOpen_HV_Metric_fit2_Marker.txt")
#summary(NoOpen.HV.Metric.Fit2.Marker, fit.measures=T, standardized=T)
#sink()

#
##Climate Change Belief
#
CCBelief.Metric.M1.Marker<-'
CCBelief=~ImpactBelief+TrendBelief+AttriBelief
'

CCBelief.Metric.Fit1.Marker<-cfa(model = CCBelief.Metric.M1.Marker,
                          data = ESS8,
                          group = "country",
                          estimator="MLR",
                          missing="FIML",
                          group.equal="loadings")

#sink("./Sink Output/ESS8/CCBelief_Metric_fit1_marker.txt")
#summary(CCBelief.Metric.Fit1.Marker, fit.measures=T, standardized=T)
#sink()

##listwise deletion:
#ESS8_lw<-na.omit(ESS8)

##Structural model
Str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan
'

##Model selection 
BasicModel.Selection<-ModelSelection(dat=ESS8,
                                     S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker),
                                     S2 = Str_model,
                                     group = "country",
                                     clusters=c(1,8),
                                     seed = 100,
                                     userStart = NULL,
                                     s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker),
                                     max_it = 10000L,
                                     nstarts = 100L,
                                     printing = FALSE,
                                     partition = "hard",
                                     endogenous_cov = TRUE,
                                     endo_group_specific = TRUE,
                                     sam_method = "local",
                                     meanstr = FALSE,
                                     rescaling = F,
                                     missing="FIML")
#
View(BasicModel.Selection$Overview)

##plot for CHull observed
ggplot(BasicModel.Selection$Overview, aes(x=nrpar, y=LL)) +
  geom_point()+
  geom_line()+
  labs(title = "CHUll observed")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()
#
##plot for CHull factor
ggplot(BasicModel.Selection$Overview, aes(x=nrpar_fac, y=LL_fac)) +
  geom_point()+
  geom_line()+
  labs(title = "CHUll factor")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()
#
##plot for BIC_G observed
ggplot(BasicModel.Selection$Overview, aes(x=Clusters, y=BIC_G))+
  geom_point()+geom_line()+
  labs(title = "BIC_G Observed")+xlab("Number of Clusters")+ylab("BIC_G")+
  theme_minimal()
#
##plot for BIC_G factor
ggplot(BasicModel.Selection$Overview, aes(x=Clusters, y=BIC_G_fac))+
  geom_point()+geom_line()+
  labs(title = "BIC_G Factor")+xlab("Number of Clusters")+ylab("BIC_G")+
  theme_minimal()


##MMGSEM - 2 cluster - 50 starts
BasicModel.2clus.50S<-MMGSEM(dat=ESS8_lw,
                         S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker),
                         S2 = Str_model,
                         group = "country",
                         nclus=2,
                         seed = 100,
                         userStart = NULL,
                         s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker),
                         max_it = 10000L,
                         nstarts = 50L,
                         printing = FALSE,
                         partition = "hard",
                         endogenous_cov = TRUE,
                         endo_group_specific = TRUE,
                         sam_method = "local",
                         meanstr = FALSE,
                         rescaling = F)

##MMGSEM - 2 cluster - 150 starts
BasicModel.2clus.150S<-MMGSEM(dat=ESS8_lw,
                         S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker),
                         S2 = Str_model,
                         group = "country",
                         nclus=2,
                         seed = 100,
                         userStart = NULL,
                         s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker),
                         max_it = 10000L,
                         nstarts = 150L,
                         printing = FALSE,
                         partition = "hard",
                         endogenous_cov = TRUE,
                         endo_group_specific = TRUE,
                         sam_method = "local",
                         meanstr = FALSE,
                         rescaling = F)

##test 3 clusters
##150 starts - FIML
BasicModel.3clus.150S.FIML<-MMGSEM(dat=ESS8,
                                   S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker),
                                   S2 = Str_model,
                                   group = "country",
                                   nclus=3,
                                   seed = 100,
                                   userStart = NULL,
                                   s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker),
                                   max_it = 10000L,
                                   nstarts = 150L,
                                   printing = FALSE,
                                   partition = "hard",
                                   endogenous_cov = TRUE,
                                   endo_group_specific = TRUE,
                                   sam_method = "local",
                                   meanstr = FALSE,
                                   rescaling = F,
                                   missing="FIML")

#
##MMGSEM - 4 cluster - 50 starts
BasicModel.4clus.50S<-MMGSEM(dat=ESS8_lw,
                             S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker),
                             S2 = Str_model,
                             group = "country",
                             nclus=4,
                             seed = 100,
                             userStart = NULL,
                             s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker),
                             max_it = 10000L,
                             nstarts = 50L,
                             printing = FALSE,
                             partition = "hard",
                             endogenous_cov = TRUE,
                             endo_group_specific = TRUE,
                             sam_method = "local",
                             meanstr = FALSE,
                             rescaling = F)

##MMGSEM - 4 cluster - 150 starts
BasicModel.4clus.150S<-MMGSEM(dat=ESS8_lw,
                              S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker),
                              S2 = Str_model,
                              group = "country",
                              nclus=4,
                              seed = 100,
                              userStart = NULL,
                              s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker),
                              max_it = 10000L,
                              nstarts = 150L,
                              printing = FALSE,
                              partition = "hard",
                              endogenous_cov = TRUE,
                              endo_group_specific = TRUE,
                              sam_method = "local",
                              meanstr = FALSE,
                              rescaling = F)
#
##MMGSEM - 4 cluster - 150 starts - FIML
BasicModel.4clus.150S.FIML<-MMGSEM(dat=ESS8,
                              S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker),
                              S2 = Str_model,
                              group = "country",
                              nclus=4,
                              seed = 100,
                              userStart = NULL,
                              s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker),
                              max_it = 10000L,
                              nstarts = 150L,
                              printing = FALSE,
                              partition = "hard",
                              endogenous_cov = TRUE,
                              endo_group_specific = TRUE,
                              sam_method = "local",
                              meanstr = FALSE,
                              rescaling = F,
                              missing="FIML")

#
##MMGSEM - 5 cluster - 50 starts
BasicModel.5clus.50S<-MMGSEM(dat=ESS8_lw,
                             S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker),
                             S2 = Str_model,
                             group = "country",
                             nclus=5,
                             seed = 100,
                             userStart = NULL,
                             s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker),
                             max_it = 10000L,
                             nstarts = 50L,
                             printing = FALSE,
                             partition = "hard",
                             endogenous_cov = TRUE,
                             endo_group_specific = TRUE,
                             sam_method = "local",
                             meanstr = FALSE,
                             rescaling = F)

##MMGSEM - 5 cluster - 150 starts
BasicModel.5clus.150S<-MMGSEM(dat=ESS8_lw,
                              S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker),
                              S2 = Str_model,
                              group = "country",
                              nclus=5,
                              seed = 100,
                              userStart = NULL,
                              s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker),
                              max_it = 10000L,
                              nstarts = 150L,
                              printing = FALSE,
                              partition = "hard",
                              endogenous_cov = TRUE,
                              endo_group_specific = TRUE,
                              sam_method = "local",
                              meanstr = FALSE,
                              rescaling = F)

#
##Based on the new model selection with FIML, we go with 5 clusters
##150 starts - FIML
BasicModel.5clus.150S.FIML<-MMGSEM(dat=ESS8,
                             S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker),
                             S2 = Str_model,
                             group = "country",
                             nclus=5,
                             seed = 100,
                             userStart = NULL,
                             s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker),
                             max_it = 10000L,
                             nstarts = 150L,
                             printing = FALSE,
                             partition = "hard",
                             endogenous_cov = TRUE,
                             endo_group_specific = TRUE,
                             sam_method = "local",
                             meanstr = FALSE,
                             rescaling = F,
                             missing="FIML")



##Clustering membership
#
#2-cluster solution
clustering.2clus<-t(apply(BasicModel.2clus.150S$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.2clus[,2]<-ifelse(clustering.2clus[,2]==1,2,0)
ClusMembership.2clus<-apply(clustering.2clus,1,function(x) sum(x))
ClusterRes.2clus<-data.frame(group=c(1:23),
                       ClusMembership=ClusMembership.2clus)

countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.2clus<-merge(ClusterRes.2clus, countries,
                        by.x = "group", by.y = "group")

#
#3-cluster 150s FIML solution
clustering.3clus<-t(apply(BasicModel.3clus.150S.FIML$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.3clus[,2]<-ifelse(clustering.3clus[,2]==1,2,0)
clustering.3clus[,3]<-ifelse(clustering.3clus[,3]==1,3,0)
ClusMembership.3clus<-apply(clustering.3clus,1,function(x) sum(x))
ClusterRes.3clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.3clus)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.3clus<-merge(ClusterRes.3clus, countries,
                        by.x = "group", by.y = "group")

#
#4-cluster solution
clustering.4clus<-t(apply(BasicModel.4clus.150S$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.4clus[,2]<-ifelse(clustering.4clus[,2]==1,2,0)
clustering.4clus[,3]<-ifelse(clustering.4clus[,3]==1,3,0)
clustering.4clus[,4]<-ifelse(clustering.4clus[,4]==1,4,0)
ClusMembership.4clus<-apply(clustering.4clus,1,function(x) sum(x))
ClusterRes.4clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.4clus)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.4clus<-merge(ClusterRes.4clus, countries,
                        by.x = "group", by.y = "group")

#
#4-cluster solution - FIML
clustering.4clus<-t(apply(BasicModel.4clus.150S.FIML$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.4clus[,2]<-ifelse(clustering.4clus[,2]==1,2,0)
clustering.4clus[,3]<-ifelse(clustering.4clus[,3]==1,3,0)
clustering.4clus[,4]<-ifelse(clustering.4clus[,4]==1,4,0)
ClusMembership.4clus<-apply(clustering.4clus,1,function(x) sum(x))
ClusterRes.4clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.4clus)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.4clus<-merge(ClusterRes.4clus, countries,
                        by.x = "group", by.y = "group")


#
#5-cluster solution - 50 random starts
clustering.5clus.50s<-t(apply(BasicModel.5clus.50S$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.5clus.50s[,2]<-ifelse(clustering.5clus.50s[,2]==1,2,0)
clustering.5clus.50s[,3]<-ifelse(clustering.5clus.50s[,3]==1,3,0)
clustering.5clus.50s[,4]<-ifelse(clustering.5clus.50s[,4]==1,4,0)
clustering.5clus.50s[,5]<-ifelse(clustering.5clus.50s[,5]==1,5,0)

ClusMembership.5clus.50s<-apply(clustering.5clus.50s,1,function(x) sum(x))
ClusterRes.5clus.50s<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.5clus.50s)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.5clus.50s<-merge(ClusterRes.5clus.50s, countries,
                        by.x = "group", by.y = "group")


#5-cluster solution - 150 random starts
clustering.5clus.150s<-t(apply(BasicModel.5clus.150S$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.5clus.150s[,2]<-ifelse(clustering.5clus.150s[,2]==1,2,0)
clustering.5clus.150s[,3]<-ifelse(clustering.5clus.150s[,3]==1,3,0)
clustering.5clus.150s[,4]<-ifelse(clustering.5clus.150s[,4]==1,4,0)
clustering.5clus.150s[,5]<-ifelse(clustering.5clus.150s[,5]==1,5,0)

ClusMembership.5clus.150s<-apply(clustering.5clus.150s,1,function(x) sum(x))
ClusterRes.5clus.150s<-data.frame(group=c(1:23),
                                 ClusMembership=ClusMembership.5clus.150s)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.5clus.150s<-merge(ClusterRes.5clus.150s, countries,
                            by.x = "group", by.y = "group")

#
#5-cluster solution - 50 random starts - FIML
clustering.5clus.50s<-t(apply(BasicModel.5clus.50S.FIML$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.5clus.50s[,2]<-ifelse(clustering.5clus.50s[,2]==1,2,0)
clustering.5clus.50s[,3]<-ifelse(clustering.5clus.50s[,3]==1,3,0)
clustering.5clus.50s[,4]<-ifelse(clustering.5clus.50s[,4]==1,4,0)
clustering.5clus.50s[,5]<-ifelse(clustering.5clus.50s[,5]==1,5,0)

ClusMembership.5clus.50s<-apply(clustering.5clus.50s,1,function(x) sum(x))
ClusterRes.5clus.50s<-data.frame(group=c(1:23),
                                 ClusMembership=ClusMembership.5clus.50s)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.5clus.50s<-merge(ClusterRes.5clus.50s, countries,
                            by.x = "group", by.y = "group")


##Based on the new model selection with FIML, we go with 5 clusters
##150 starts - FIML
#5-cluster solution - 150 random starts - FIML
clustering.5clus.150s.FIML<-t(apply(BasicModel.5clus.150S.FIML$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.5clus.150s.FIML[,2]<-ifelse(clustering.5clus.150s.FIML[,2]==1,2,0)
clustering.5clus.150s.FIML[,3]<-ifelse(clustering.5clus.150s.FIML[,3]==1,3,0)
clustering.5clus.150s.FIML[,4]<-ifelse(clustering.5clus.150s.FIML[,4]==1,4,0)
clustering.5clus.150s.FIML[,5]<-ifelse(clustering.5clus.150s.FIML[,5]==1,5,0)

ClusMembership.5clus.150s<-apply(clustering.5clus.150s.FIML,1,function(x) sum(x))
ClusterRes.5clus.150s<-data.frame(group=c(1:23),
                                  ClusMembership=ClusMembership.5clus.150s)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.5clus.150s<-merge(ClusterRes.5clus.150s, countries,
                             by.x = "group", by.y = "group")

#####################################################################################
################### Basic Model: SAM estimation and comparison ######################
#####################################################################################

##First do the following step that is necessary for both 2-cluster and 6-cluster solution
#
##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
lambda_HV_23cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_23cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from CCBelief
EST_CCBelief<-lavInspect(CCBelief.Metric.Fit1.Marker, what = "est")
lambda_CCBelief_23cntry<-lapply(EST_CCBelief, "[[","lambda")
theta_CCBelief_23cntry<-lapply(EST_CCBelief, "[[","theta")
#
##initialize empty list to store the new lambda matrix, new theta matrix and the mapping matrix
lambda_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
theta_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
Mmatrix<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))){
  ##put lambda from two measurement blocks into into the same matrix for each group
  lambda_23cntry[[g]]<-lav_matrix_bdiag(lambda_HV_23cntry[[g]], lambda_CCBelief_23cntry[[g]])
  colnames(lambda_23cntry[[g]])<-c(colnames(lambda_HV_23cntry[[g]]), colnames(lambda_CCBelief_23cntry[[g]]))
  rownames(lambda_23cntry[[g]])<-c(rownames(lambda_HV_23cntry[[g]]), rownames(lambda_CCBelief_23cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_23cntry[[g]]<-lav_matrix_bdiag(theta_HV_23cntry[[g]],theta_CCBelief_23cntry[[g]])
  colnames(theta_23cntry[[g]])<-c(colnames(theta_HV_23cntry[[g]]), colnames(theta_CCBelief_23cntry[[g]]))
  rownames(theta_23cntry[[g]])<-c(rownames(theta_HV_23cntry[[g]]), rownames(theta_CCBelief_23cntry[[g]]))
  
  ##compute the mapping matrix for each group
  Mmatrix[[g]]<-solve(t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]]) %*% lambda_23cntry[[g]]) %*% t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]])
}
#
##run an empty sem to just extract the imputed sample covariance matrix
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCBelief=~ImpactBelief+TrendBelief+AttriBelief
'

fake<-cfa(model = fake_model,
          data = ESS8,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F)

S<-fake@SampleStats@cov

S<-lapply(S, function(x) {
  colnames(x)<-rownames(x)<-colnames(fitted(fake)[[19]]$cov)
  x
})

#
##compute the factor covariance matrix for each group that will be used for the second step:
Var_eta<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_23cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
FREEsam_str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan
'

BasicModel.FreeSAM<-cfa(model = FREEsam_str_model,
                    sample.cov = Var_eta,
                    sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/BasicModel_FreeSAM.txt")
summary(BasicModel.FreeSAM, fit.measures=T, standardized=T)
sink()



###Once we have the factor covariance matrix from step 1
##We can estimate the structural parameter for different cluster solution from now on:
##
##-------------------------------------------------------------------------------------------------------
##2-cluster: only group 1, 16, 19 are in cluster 1, the rest are in cluster 2
sam_str_model_2clus<-'
CCBelief~c(a1,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a1,a2,a2,a1,a2,a2,a2,a2)*SelfTran+
          c(b1,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b1,b2,b2,b1,b2,b2,b2,b2)*Conser+
          c(c1,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c1,c2,c2,c1,c2,c2,c2,c2)*SelfEnhan
'

BasicModel.SAM.2clus<-cfa(model = sam_str_model_2clus,
                    sample.cov = Var_eta,
                    sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/BasicModel_SAM_2clus.txt")
summary(BasicModel.SAM.2clus, fit.measures=T, standardized=T)
sink()


##faceted dot plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.2clus, 
                         by.x = "group", by.y = "group")
  
ggplot(FreeSAM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  labs(title = "SAM with clustering results - Human Values on Climate Change Belief")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

##3-D scatter plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.2clus, 
                         by.x = "group", by.y = "group")

plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
        type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM with clustering results - Human Values on Climate Change Belief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))



##
##-------------------------------------------------------------------------------------------------------
##4-cluster: 
##cluster 1: group 7,8,9,10,13,14,18,22,23
##cluster 2: group 1,4,11,15,19
##cluster 3: group 16
##cluster 4: group 2,3,5,6,12,17,20,21

sam_str_model_4clus<-'
CCBelief~c(a2,a4,a4,a2,a4,a4,a1,a1,a1,a1,a2,a4,a1,a1,a2,a3,a4,a1,a2,a4,a4,a1,a1)*SelfTran+
          c(b2,b4,b4,b2,b4,b4,b1,b1,b1,b1,b2,b4,b1,b1,b2,b3,b4,b1,b2,b4,b4,b1,b1)*Conser+
          c(c2,c4,c4,c2,c4,c4,c1,c1,c1,c1,c2,c4,c1,c1,c2,c3,c4,c1,c2,c4,c4,c1,c1)*SelfEnhan
'

BasicModel.SAM.4clus<-cfa(model = sam_str_model_4clus,
                          sample.cov = Var_eta,
                          sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/BasicModel_SAM_4clus.txt")
summary(BasicModel.SAM.4clus, fit.measures=T, standardized=T)
sink()


##faceted dot plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.4clus, 
                         by.x = "group", by.y = "group")

FreeSAM_reg_param$country <- fct_reorder(FreeSAM_reg_param$country, 
                                         FreeSAM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, -0.375, 0.5)                             # Line positions
)

ggplot(FreeSAM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - Human Values on Climate Change Belief")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

##3-D scatter plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.4clus, 
                         by.x = "group", by.y = "group")

SAM_4clus_3D<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
        type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM with clustering results - Human Values on Climate Change Belief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(SAM_4clus_3D), "SAM_4clus_3D.html")



##
##-------------------------------------------------------------------------------------------------------
##4-cluster - FIML: 
##cluster 1: group 16
##cluster 2: group 2,6,12,21,23
##cluster 3: group 4,7,8,10,13,14,15,17,18,22
##cluster 4: group 1,3,5,9,11,19,20

sam_str_model_4clus_FIML<-'
CCBelief~c(a4,a2,a4,a3,a4,a2,a3,a3,a4,a3,a4,a2,a3,a3,a3,a1,a3,a3,a4,a4,a2,a3,a2)*SelfTran+
          c(b4,b2,b4,b3,b4,b2,b3,b3,b4,b3,b4,b2,b3,b3,b3,b1,b3,b3,b4,b4,b2,b3,b2)*Conser+
          c(c4,c2,c4,c3,c4,c2,c3,c3,c4,c3,c4,c2,c3,c3,c3,c1,c3,c3,c4,c4,c2,c3,c2)*SelfEnhan
'

BasicModel.SAM.4clus.FIML<-cfa(model = sam_str_model_4clus_FIML,
                          sample.cov = Var_eta,
                          sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/BasicModel_SAM_4clus_FIML.txt")
summary(BasicModel.SAM.4clus.FIML, fit.measures=T, standardized=T)
sink()


##faceted dot plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.4clus, 
                         by.x = "group", by.y = "group")

FreeSAM_reg_param$country <- fct_reorder(FreeSAM_reg_param$country, 
                                         FreeSAM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, -0.375, 0.5)                             # Line positions
)

ggplot(FreeSAM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - Human Values on Climate Change Belief")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

##3-D scatter plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.4clus, 
                         by.x = "group", by.y = "group")

SAM_4clus_3D_FIML<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                      type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM with clustering results - Human Values on Climate Change Belief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(SAM_4clus_3D_FIML), "SAM_4clus_3D_FIML.html")


##
##-------------------------------------------------------------------------------------------------------
##5-cluster with 50 random starts: 
##cluster 1: group 4,7,10,12,15,17,23
##cluster 2: group 8,9,13,14,18,22
##cluster 3: group 2,6,21
##cluster 4: group 1,3,5,11,19,20
##cluster 5: group 16

sam_str_model_5clus.50s<-'
CCBelief~c(a4,a3,a4,a1,a4,a3,a1,a2,a2,a1,a4,a1,a2,a2,a1,a5,a1,a2,a4,a4,a3,a2,a1)*SelfTran+
          c(b4,b3,b4,b1,b4,b3,b1,b2,b2,b1,b4,b1,b2,b2,b1,b5,b1,b2,b4,b4,b3,b2,b1)*Conser+
          c(c4,c3,c4,c1,c4,c3,c1,c2,c2,c1,c4,c1,c2,c2,c1,c5,c1,c2,c4,c4,c3,c2,c1)*SelfEnhan
'

BasicModel.SAM.5clus.50s<-cfa(model = sam_str_model_5clus.50s,
                          sample.cov = Var_eta,
                          sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/BasicModel_SAM_5clus_50s.txt")
summary(BasicModel.SAM.5clus.50s, fit.measures=T, standardized=T)
sink()


##faceted dot plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.5clus.50s, 
                         by.x = "group", by.y = "group")

FreeSAM_reg_param$country <- fct_reorder(FreeSAM_reg_param$country, 
                                         FreeSAM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, -0.25, 0.5)                             # Line positions
)

ggplot(FreeSAM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x") +
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - Human Values on Climate Change Belief",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

##3-D scatter plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.5clus.50s, 
                         by.x = "group", by.y = "group")

SAM_5clus_50s_3D<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                      type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM 5 clusters 50 S with clustering results - Human Values on Climate Change Belief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(SAM_5clus_50s_3D), "SAM_5clus_50S_3D.html")

##
##-------------------------------------------------------------------------------------------------------
##5-cluster with 50 random starts - FIML: 
##cluster 1: group 1,3,5,11,19,20
##cluster 2: group 7,8,9,10,14,17,18,22
##cluster 3: group 16
##cluster 4: group 2,6,12,21,23
##cluster 5: group 4,13,15

sam_str_model_5clus.50s<-'
CCBelief~c(a4,a3,a4,a1,a4,a3,a1,a2,a2,a1,a4,a1,a2,a2,a1,a5,a1,a2,a4,a4,a3,a2,a1)*SelfTran+
          c(b4,b3,b4,b1,b4,b3,b1,b2,b2,b1,b4,b1,b2,b2,b1,b5,b1,b2,b4,b4,b3,b2,b1)*Conser+
          c(c4,c3,c4,c1,c4,c3,c1,c2,c2,c1,c4,c1,c2,c2,c1,c5,c1,c2,c4,c4,c3,c2,c1)*SelfEnhan
'

BasicModel.SAM.5clus.50s<-cfa(model = sam_str_model_5clus.50s,
                              sample.cov = Var_eta,
                              sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/BasicModel_SAM_5clus_50s.txt")
summary(BasicModel.SAM.5clus.50s, fit.measures=T, standardized=T)
sink()


##faceted dot plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.5clus.50s, 
                         by.x = "group", by.y = "group")

FreeSAM_reg_param$country <- fct_reorder(FreeSAM_reg_param$country, 
                                         FreeSAM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, -0.25, 0.5)                             # Line positions
)

ggplot(FreeSAM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x") +
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - Human Values on Climate Change Belief",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

##3-D scatter plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.5clus.50s, 
                         by.x = "group", by.y = "group")

SAM_5clus_50s_3D_FIML<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                          type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM 5 clusters 50 S with clustering results - Human Values on Climate Change Belief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(SAM_5clus_50s_3D_FIML), "SAM_5clus_50S_3D_FIML.html")




##
##-------------------------------------------------------------------------------------------------------
##5-cluster with 150 random starts: 
##cluster 1: group 4,15
##cluster 2: group 7,8,10,12,13,14,17,18,22,23
##cluster 3: group 1,3,5,9,11,19,20
##cluster 4: group 16
##cluster 5: group 2,6,21

sam_str_model_5clus.150s<-'
CCBelief~c(a3,a5,a3,a1,a3,a5,a2,a2,a3,a2,a3,a2,a2,a2,a1,a4,a2,a2,a3,a3,a5,a2,a2)*SelfTran+
          c(b3,b5,b3,b1,b3,b5,b2,b2,b3,b2,b3,b2,b2,b2,b1,b4,b2,b2,b3,b3,b5,b2,b2)*Conser+
          c(c3,c5,c3,c1,c3,c5,c2,c2,c3,c2,c3,c2,c2,c2,c1,c4,c2,c2,c3,c3,c5,c2,c2)*SelfEnhan
'

BasicModel.SAM.5clus.150s<-cfa(model = sam_str_model_5clus.150s,
                              sample.cov = Var_eta,
                              sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/BasicModel_SAM_5clus_150s.txt")
summary(BasicModel.SAM.5clus.150s, fit.measures=T, standardized=T)
sink()


##faceted dot plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.5clus.150s, 
                         by.x = "group", by.y = "group")

FreeSAM_reg_param$country <- fct_reorder(FreeSAM_reg_param$country, 
                                         FreeSAM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, -0.25, 0.5)                             # Line positions
)

ggplot(FreeSAM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x") +
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - Human Values on Climate Change Belief",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

##3-D scatter plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.5clus.150s, 
                         by.x = "group", by.y = "group")

SAM_5clus_150S_3D<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                      type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM 5 cluster 150 starts with clustering results - Human Values on Climate Change Belief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(SAM_5clus_150S_3D), "SAM_5clus_150S_3D.html")


##
##-------------------------------------------------------------------------------------------------------
##5-cluster with 150 random starts - FIML: 
##cluster 1: group 1,3,5,11,19,20
##cluster 2: group 7,8,9,10,14,17,18,22
##cluster 3: group 16
##cluster 4: group 2,6,12,21,23
##cluster 5: group 4,13,15

sam_str_model_5clus.150s.FIML<-'
CCBelief~c(a1,a4,a1,a5,a1,a4,a2,a2,a2,a2,a1,a4,a5,a2,a5,a3,a2,a2,a1,a1,a4,a2,a4)*SelfTran+
          c(b1,b4,b1,b5,b1,b4,b2,b2,b2,b2,b1,b4,b5,b2,b5,b3,b2,b2,b1,b1,b4,b2,b4)*Conser+
          c(c1,c4,c1,c5,c1,c4,c2,c2,c2,c2,c1,c4,c5,c2,c5,c3,c2,c2,c1,c1,c4,c2,c4)*SelfEnhan
'

BasicModel.SAM.5clus.150s.FIML<-cfa(model = sam_str_model_5clus.150s.FIML,
                              sample.cov = Var_eta,
                              sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/BasicModel_SAM_5clus_150s_FIML.txt")
summary(BasicModel.SAM.5clus.150s.FIML, fit.measures=T, standardized=T)
sink()


##faceted dot plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.5clus.150s, 
                         by.x = "group", by.y = "group")

FreeSAM_reg_param$country <- fct_reorder(FreeSAM_reg_param$country, 
                                         FreeSAM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, -0.3, 0.55)                             # Line positions
)

ggplot(FreeSAM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x") +
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - Human Values on Climate Change Belief",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

##3-D scatter plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.5clus.150s, 
                         by.x = "group", by.y = "group")

SAM_5clus_150s_3D_FIML<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                               type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM 5 clusters 150S FIML with clustering results - Human Values on Climate Change Belief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(SAM_5clus_150s_3D_FIML), "FINAL_SAM_5clus_150s_3D_FIML.html")



#####################################################################################
############## Basic Model: Simultaneously MGSEM estimation and comparison ##########
#####################################################################################

##Specify the model:
BasicModel.HV.CCBelief<-'
##human values
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

##Climate Change Belief
CCBelief=~ImpactBelief+TrendBelief+AttriBelief

##Structural Model:
CCBelief~SelfTran+Conser+SelfEnhan
'

##run regular MGSEM:
RegSEM.BasicModel.HV.CCBelief<-cfa(model = BasicModel.HV.CCBelief,
                                   data = ESS8,
                                   group = "country",
                                   estimator="MLR",
                                   missing="FIML",
                                   group.equal="loadings",
                                   group.partial=c("SelfEnhan=~SE3"))

sink("./Sink Output/ESS8/BasicModel_FreeRegSEM.txt")
summary(RegSEM.BasicModel.HV.CCBelief, fit.measures=T, standardized=T)
sink()


###------------------------------------------------------------------------------
##2-cluster solution

##faceted dot plot
FreeSEM_param<-parameterEstimates(RegSEM.BasicModel.HV.CCBelief)

FreeSEM_reg_param<-FreeSEM_param %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))

FreeSEM_reg_param<-merge(FreeSEM_reg_param, ClusterRes.2clus, 
                         by.x = "group", by.y = "group")

ggplot(FreeSEM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  labs(title = "Simultaneous MGSEM with clustering results - Human Values on Climate Change Belief")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()


##MGSEM with constrains within clusters:
BasicModel.HV.CCBelief.2Clus<-'
##human values
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

##Climate Change Belief
CCBelief=~ImpactBelief+TrendBelief+AttriBelief

##Structural Model:
CCBelief~c(a1,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a1,a2,a2,a1,a2,a2,a2,a2)*SelfTran+
          c(b1,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b1,b2,b2,b1,b2,b2,b2,b2)*Conser+
          c(c1,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c1,c2,c2,c1,c2,c2,c2,c2)*SelfEnhan
'

##run regular MGSEM:
RegSEM.BasicModel.HV.CCBelief.2clus<-cfa(model = BasicModel.HV.CCBelief.2Clus,
                                   data = ESS8,
                                   group = "country",
                                   estimator="MLR",
                                   missing="FIML",
                                   group.equal="loadings",
                                   group.partial=c("SelfEnhan=~SE3"))

sink("./Sink Output/ESS8/BasicModel_RegMGSEM_2clus.txt")
summary(RegSEM.BasicModel.HV.CCBelief.2clus, fit.measures=T, standardized=T)
sink()



###------------------------------------------------------------------------------
##4-cluster solution

##faceted dot plot
FreeSEM_param<-parameterEstimates(RegSEM.BasicModel.HV.CCBelief)

FreeSEM_reg_param<-FreeSEM_param %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))

FreeSEM_reg_param<-merge(FreeSEM_reg_param, ClusterRes.4clus, 
                         by.x = "group", by.y = "group")

FreeSEM_reg_param$country <- fct_reorder(FreeSEM_reg_param$country, 
                                         FreeSEM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, -0.375, 0.5)                             # Line positions
)


ggplot(FreeSEM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "Simultaneous MGSEM with clustering results - Human Values on Climate Change Belief")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()


##MGSEM with constrains within clusters:
BasicModel.HV.CCBelief.4Clus<-'
##human values
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

##Climate Change Belief
CCBelief=~ImpactBelief+TrendBelief+AttriBelief

##Structural Model:
CCBelief~c(a2,a4,a4,a2,a4,a4,a1,a1,a1,a1,a2,a4,a1,a1,a2,a3,a4,a1,a2,a4,a4,a1,a1)*SelfTran+
          c(b2,b4,b4,b2,b4,b4,b1,b1,b1,b1,b2,b4,b1,b1,b2,b3,b4,b1,b2,b4,b4,b1,b1)*Conser+
          c(c2,c4,c4,c2,c4,c4,c1,c1,c1,c1,c2,c4,c1,c1,c2,c3,c4,c1,c2,c4,c4,c1,c1)*SelfEnhan
'

##run regular MGSEM:
RegSEM.BasicModel.HV.CCBelief.4clus<-cfa(model = BasicModel.HV.CCBelief.4Clus,
                                         data = ESS8,
                                         group = "country",
                                         estimator="MLR",
                                         missing="FIML",
                                         group.equal="loadings",
                                         group.partial=c("SelfEnhan=~SE3"))

sink("./Sink Output/ESS8/BasicModel_RegMGSEM_4clus.txt")
summary(RegSEM.BasicModel.HV.CCBelief.4clus, fit.measures=T, standardized=T)
sink()

lavTestLRT(RegSEM.BasicModel.HV.CCBelief, RegSEM.BasicModel.HV.CCBelief.4clus)



###------------------------------------------------------------------------------
##5-cluster solution - 50 random starts

##faceted dot plot
FreeSEM_param<-parameterEstimates(RegSEM.BasicModel.HV.CCBelief)

FreeSEM_reg_param<-FreeSEM_param %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))

FreeSEM_reg_param<-merge(FreeSEM_reg_param, ClusterRes.5clus.50s, 
                         by.x = "group", by.y = "group")

FreeSEM_reg_param$country <- fct_reorder(FreeSEM_reg_param$country, 
                                         FreeSEM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, -0.25, 0.5)                             # Line positions
)


ggplot(FreeSEM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "Simultaneous MGSEM with clustering results - Human Values on Climate Change Belief")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()


##MGSEM with constrains within clusters:
BasicModel.HV.CCBelief.5Clus.50s<-'
##human values
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

##Climate Change Belief
CCBelief=~ImpactBelief+TrendBelief+AttriBelief

##Structural Model:
CCBelief~c(a4,a3,a4,a1,a4,a3,a1,a2,a2,a1,a4,a1,a2,a2,a1,a5,a1,a2,a4,a4,a3,a2,a1)*SelfTran+
          c(b4,b3,b4,b1,b4,b3,b1,b2,b2,b1,b4,b1,b2,b2,b1,b5,b1,b2,b4,b4,b3,b2,b1)*Conser+
          c(c4,c3,c4,c1,c4,c3,c1,c2,c2,c1,c4,c1,c2,c2,c1,c5,c1,c2,c4,c4,c3,c2,c1)*SelfEnhan
'

##run regular MGSEM:
RegSEM.BasicModel.HV.CCBelief.5clus.50s<-cfa(model = BasicModel.HV.CCBelief.5Clus.50s,
                                         data = ESS8,
                                         group = "country",
                                         estimator="MLR",
                                         missing="FIML",
                                         group.equal="loadings",
                                         group.partial=c("SelfEnhan=~SE3"))

sink("./Sink Output/ESS8/BasicModel_RegMGSEM_5clus_50s.txt")
summary(RegSEM.BasicModel.HV.CCBelief.5clus.50s, fit.measures=T, standardized=T)
sink()

lavTestLRT(RegSEM.BasicModel.HV.CCBelief, RegSEM.BasicModel.HV.CCBelief.5clus.50s)


###------------------------------------------------------------------------------
##5-cluster solution - 150 random starts

##faceted dot plot
FreeSEM_param<-parameterEstimates(RegSEM.BasicModel.HV.CCBelief)

FreeSEM_reg_param<-FreeSEM_param %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))

FreeSEM_reg_param<-merge(FreeSEM_reg_param, ClusterRes.5clus.150s, 
                         by.x = "group", by.y = "group")

FreeSEM_reg_param$country <- fct_reorder(FreeSEM_reg_param$country, 
                                         FreeSEM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, -0.25, 0.5)                             # Line positions
)


ggplot(FreeSEM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "Simultaneous MGSEM with clustering results - Human Values on Climate Change Belief",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()


##MGSEM with constrains within clusters:
BasicModel.HV.CCBelief.5Clus.150s<-'
##human values
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

##Climate Change Belief
CCBelief=~ImpactBelief+TrendBelief+AttriBelief

##Structural Model:
CCBelief~c(a3,a5,a3,a1,a3,a5,a2,a2,a3,a2,a3,a2,a2,a2,a1,a4,a2,a2,a3,a3,a5,a2,a2)*SelfTran+
          c(b3,b5,b3,b1,b3,b5,b2,b2,b3,b2,b3,b2,b2,b2,b1,b4,b2,b2,b3,b3,b5,b2,b2)*Conser+
          c(c3,c5,c3,c1,c3,c5,c2,c2,c3,c2,c3,c2,c2,c2,c1,c4,c2,c2,c3,c3,c5,c2,c2)*SelfEnhan
'

##run regular MGSEM:
RegSEM.BasicModel.HV.CCBelief.5clus.150s<-cfa(model = BasicModel.HV.CCBelief.5Clus.150s,
                                             data = ESS8,
                                             group = "country",
                                             estimator="MLR",
                                             missing="FIML",
                                             group.equal="loadings",
                                             group.partial=c("SelfEnhan=~SE3"))

sink("./Sink Output/ESS8/BasicModel_RegMGSEM_5clus_150s.txt")
summary(RegSEM.BasicModel.HV.CCBelief.5clus.150s, fit.measures=T, standardized=T)
sink()

lavTestLRT(RegSEM.BasicModel.HV.CCBelief, RegSEM.BasicModel.HV.CCBelief.5clus.150s)


###------------------------------------------------------------------------------
##5-cluster solution - 150 random starts - FIML

##faceted dot plot
FreeSEM_param<-parameterEstimates(RegSEM.BasicModel.HV.CCBelief)

FreeSEM_reg_param<-FreeSEM_param %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))

FreeSEM_reg_param<-merge(FreeSEM_reg_param, ClusterRes.5clus.150s, 
                         by.x = "group", by.y = "group")

FreeSEM_reg_param$country <- fct_reorder(FreeSEM_reg_param$country, 
                                         FreeSEM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, -0.3, 0.55)                              # Line positions
)


ggplot(FreeSEM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "Simultaneous MGSEM with clustering results - Human Values on Climate Change Belief",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()


##MGSEM with constrains within clusters:
BasicModel.HV.CCBelief.5Clus.150s.FIML<-'
##human values
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

##Climate Change Belief
CCBelief=~ImpactBelief+TrendBelief+AttriBelief

##Structural Model:
CCBelief~c(a1,a4,a1,a5,a1,a4,a2,a2,a2,a2,a1,a4,a5,a2,a5,a3,a2,a2,a1,a1,a4,a2,a4)*SelfTran+
          c(b1,b4,b1,b5,b1,b4,b2,b2,b2,b2,b1,b4,b5,b2,b5,b3,b2,b2,b1,b1,b4,b2,b4)*Conser+
          c(c1,c4,c1,c5,c1,c4,c2,c2,c2,c2,c1,c4,c5,c2,c5,c3,c2,c2,c1,c1,c4,c2,c4)*SelfEnhan
'

##run regular MGSEM:
RegSEM.BasicModel.HV.CCBelief.5clus.150s.FIML<-cfa(model = BasicModel.HV.CCBelief.5Clus.150s.FIML,
                                              data = ESS8,
                                              group = "country",
                                              estimator="MLR",
                                              missing="FIML",
                                              group.equal="loadings",
                                              group.partial=c("SelfEnhan=~SE3"))

sink("./Sink Output/ESS8/BasicModel_RegMGSEM_5clus_150s_FIML.txt")
summary(RegSEM.BasicModel.HV.CCBelief.5clus.150s.FIML, fit.measures=T, standardized=T)
sink()

lavTestLRT(RegSEM.BasicModel.HV.CCBelief, RegSEM.BasicModel.HV.CCBelief.5clus.150s.FIML)



#####################################################################################
############## CCBelief: Mapping the clustering results on Map  #####################
#####################################################################################


###----------------------------------------------------------------------------------
##4 cluster solution:

##take out the world map
world_map<-map_data("world")

##filter to be a eu map:
eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
  "Ireland", "Iceland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
  "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia",
  "Slovakia", "Slovenia", "Switzerland",
  "Spain", "Sweden", "UK", "Israel",
  "Turkey", "Lebanon", "Jordan", "Egypt", "Syria",
  "Ukraine", "Belarus", "Georgia", "Armenia", "Azerbaijan", "Moldova"
)

eu_map <- world_map %>%
  filter(region %in% eu_countries)

##add a new column called region to match the country names with the world map data country names
ClusterRes.4clus<-ClusterRes.4clus %>%
  mutate(region=case_when(
    country == "AT" ~ "Austria",
    country == "BE" ~ "Belgium",
    country == "CH" ~ "Switzerland",
    country == "CZ" ~ "Czech Republic",
    country == "DE" ~ "Germany",
    country == "EE" ~ "Estonia",
    country == "ES" ~ "Spain",
    country == "FI" ~ "Finland",
    country == "FR" ~ "France",
    country == "GB" ~ "UK",
    country == "HU" ~ "Hungary",
    country == "IE" ~ "Ireland",
    country == "IL" ~ "Israel",
    country == "IS" ~ "Iceland",
    country == "IT" ~ "Italy",
    country == "LT" ~ "Lithuania",
    country == "NL" ~ "Netherlands",
    country == "NO" ~ "Norway",
    country == "PL" ~ "Poland",
    country == "PT" ~ "Portugal",
    country == "RU" ~ "Russia",
    country == "SE" ~ "Sweden",
    country == "SI" ~ "Slovenia"
  )) %>%
  select(ClusMembership, region)

##merge the data:
map_with_4clusters <- eu_map %>%
  left_join(ClusterRes.4clus, by = "region")

##lay out on the map:
ggplot(map_with_4clusters, aes(long, lat, group = group, fill = factor(ClusMembership))) +
  geom_polygon(color = "white") +
  labs(
    title = "Clustering Results on the Map",
    fill = "Cluster"
  ) +
  theme_minimal()


###----------------------------------------------------------------------------------
##5 cluster solution - 150S - FIML:

##take out the world map
world_map<-map_data("world")

##filter to be a eu map:
eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
  "Ireland", "Iceland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
  "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia",
  "Slovakia", "Slovenia", "Switzerland",
  "Spain", "Sweden", "UK", "Israel",
  "Turkey", "Lebanon", "Jordan", "Egypt", "Syria",
  "Ukraine", "Belarus", "Georgia", "Armenia", "Azerbaijan", "Moldova"
)

eu_map <- world_map %>%
  filter(region %in% eu_countries)

##add a new column called region to match the country names with the world map data country names
ClusterRes.5clus.150s<-ClusterRes.5clus.150s %>%
  mutate(region=case_when(
    country == "AT" ~ "Austria",
    country == "BE" ~ "Belgium",
    country == "CH" ~ "Switzerland",
    country == "CZ" ~ "Czech Republic",
    country == "DE" ~ "Germany",
    country == "EE" ~ "Estonia",
    country == "ES" ~ "Spain",
    country == "FI" ~ "Finland",
    country == "FR" ~ "France",
    country == "GB" ~ "UK",
    country == "HU" ~ "Hungary",
    country == "IE" ~ "Ireland",
    country == "IL" ~ "Israel",
    country == "IS" ~ "Iceland",
    country == "IT" ~ "Italy",
    country == "LT" ~ "Lithuania",
    country == "NL" ~ "Netherlands",
    country == "NO" ~ "Norway",
    country == "PL" ~ "Poland",
    country == "PT" ~ "Portugal",
    country == "RU" ~ "Russia",
    country == "SE" ~ "Sweden",
    country == "SI" ~ "Slovenia"
  )) %>%
  select(ClusMembership, region)

##merge the data:
map_with_5clusters <- eu_map %>%
  left_join(ClusterRes.5clus.150s, by = "region")

##full results: lay out on the map:
ggplot(map_with_5clusters, aes(long, lat, group = group, fill = factor(ClusMembership))) +
  geom_polygon(color = "white") +
  labs(
    title = "5-clusters Results on the Map",
    fill = "Cluster"
  ) +
  theme_minimal()

##make a map focusing on the dimension of self-enhancement
map_with_5clusters_se<-map_with_5clusters %>%
  mutate(SE_char=case_when(
    ClusMembership == 1 | ClusMembership==5 ~ "SE_0-negative",
    ClusMembership == 2 | ClusMembership==4 ~ "SE_0-positive",
    ClusMembership == 3 ~ "LT: SE very negative"
  ))

#SE results: lay out on the map:
ggplot(map_with_5clusters_se, aes(long, lat, group = group, fill = factor(SE_char))) +
  geom_polygon(color = "white") +
  labs(
    title = "Self-enhancement dimension",
    fill = "SE Characteristics"
  ) +
  theme_minimal()


##make a map separating cluster 2 and 4 that are both SE 0-positive
map_with_5clusters_PositiveSE<-map_with_5clusters %>%
  mutate(ST_con=case_when(
    ClusMembership == 4 ~ "weak ST_Con",
    ClusMembership == 2 ~ "strong ST_con"
  ))

#SE 0-postive results to separate into cluster 2 and 4: lay out on the map:
ggplot(map_with_5clusters_PositiveSE, aes(long, lat, group = group, fill = factor(ST_con))) +
  geom_polygon(color = "white") +
  labs(
    title = "Self-Transcendence and Conservation dimension",
    fill = "SelfTran & Conser"
  ) +
  theme_minimal()


##make a map separating cluster 1,3,5 that are both SE 0-negative
map_with_5clusters_NegativeSE<-map_with_5clusters %>%
  mutate(ST_con=case_when(
    ClusMembership == 1 ~ "weaker SelfTran",
    ClusMembership == 5 ~ "stronger SelfTran",
    ClusMembership == 3 ~ "LT: strongest ST_Con"
  ))

#SE 0-postive results to separate into cluster 2 and 4: lay out on the map:
ggplot(map_with_5clusters_NegativeSE, aes(long, lat, group = group, fill = factor(ST_con))) +
  geom_polygon(color = "white") +
  labs(
    title = "Self-Transcendence and Conservation dimension",
    fill = "SelfTran & Conser"
  ) +
  theme_minimal()


#####################################################################################
############## Basic Model - Policy Support: MMGSEM  ####################
#####################################################################################

##First, take all the necessary measurement models and change to marker variable approach:
#
##Human Values without Openness to Change
#
NoOpen.HV.Metric.M2.Marker<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit2.Marker<-cfa(model = NoOpen.HV.Metric.M2.Marker,
                                  data = ESS8,
                                  group = "country",
                                  estimator="MLR",
                                  missing="FIML",
                                  group.equal="loadings",
                                  group.partial=c("SelfEnhan=~SE3"))

#sink("./Sink Output/ESS8/NoOpen_HV_Metric_fit2_Marker.txt")
#summary(NoOpen.HV.Metric.Fit2.Marker, fit.measures=T, standardized=T)
#sink()

##--------------------------------------------------------------------------------------------------------
##climate change policy support
#
##Option A: full metric invariance Model with support 3 as marker
CCPolSupport.FMetric.M1.MarkerSup3<-'
CCPolicySupport=~support3+support1+support2
'

CCPolSupport.FMetric.Fit1.MarkerSup3<-cfa(model = CCPolSupport.FMetric.M1.MarkerSup3,
                              data = ESS8,
                              group = "country",
                              estimator="MLR",
                              missing="FIML",
                              group.equal="loadings")

#
##Option B: full metric invariance Model with support 2 as marker
CCPolSupport.FMetric.M1.MarkerSup2<-'
CCPolicySupport=~support2+support1+support3
'

CCPolSupport.FMetric.Fit1.MarkerSup2<-cfa(model = CCPolSupport.FMetric.M1.MarkerSup2,
                                         data = ESS8,
                                         group = "country",
                                         estimator="MLR",
                                         missing="FIML",
                                         group.equal="loadings")

#
##Option C: partial metric invariance model with support 2 as marker and let support 3 freely estimated (wide bound estimation)
CCPolSupport.PMetric.M1.MarkerSup2<-'
CCPolicySupport=~support2+support1+support3
'

CCPolSupport.PMetric.Fit1.MarkerSup2<-cfa(model = CCPolSupport.PMetric.M1.MarkerSup2,
                                         data = ESS8,
                                         group = "country",
                                         estimator="MLR",
                                         missing="FIML",
                                         group.equal="loadings",
                                         group.partial=c("CCPolicySupport=~support3"),
                                         bounds="wide")

#sink("./Sink Output/ESS8/testwidebound1.txt")
#summary(CCPolSupport.PMetric.Fit1.MarkerSup2, fit.measures=T, standardized=T)
#sink()

##--------------------------------------------------------------------------------------------------------
##MMGSEM

##Structural model
Str_model<-'
CCPolicySupport~SelfTran+Conser+SelfEnhan
'

##Model selection 
#
##option A: CCPolSupport full metric with support 3 as marker
#
BasicModel.FMetricCCPolSup.marker3.Selection<-ModelSelection(dat=ESS8,
                                     S1 = list(NoOpen.HV.Metric.M2.Marker, CCPolSupport.FMetric.M1.MarkerSup3),
                                     S2 = Str_model,
                                     group = "country",
                                     clusters=c(1,8),
                                     seed = 100,
                                     userStart = NULL,
                                     s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCPolSupport.FMetric.Fit1.MarkerSup3),
                                     max_it = 10000L,
                                     nstarts = 150L,
                                     printing = FALSE,
                                     partition = "hard",
                                     endogenous_cov = TRUE,
                                     endo_group_specific = TRUE,
                                     sam_method = "local",
                                     meanstr = FALSE,
                                     rescaling = F,
                                     missing="FIML")

View(BasicModel.FMetricCCPolSup.marker3.Selection$Overview)

#
##option B: CCPolSupport full metric with support 2 as marker
BasicModel.FMetricCCPolSup.marker2.Selection<-ModelSelection(dat=ESS8,
                                                             S1 = list(NoOpen.HV.Metric.M2.Marker, CCPolSupport.FMetric.M1.MarkerSup2),
                                                             S2 = Str_model,
                                                             group = "country",
                                                             clusters=c(1,8),
                                                             seed = 100,
                                                             userStart = NULL,
                                                             s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCPolSupport.FMetric.Fit1.MarkerSup2),
                                                             max_it = 10000L,
                                                             nstarts = 150L,
                                                             printing = FALSE,
                                                             partition = "hard",
                                                             endogenous_cov = TRUE,
                                                             endo_group_specific = TRUE,
                                                             sam_method = "local",
                                                             meanstr = FALSE,
                                                             rescaling = F,
                                                             missing="FIML")

View(BasicModel.FMetricCCPolSup.marker2.Selection$Overview)

#
##option C: CCPolSupport partial metric with support 2 as marker:
BasicModel.PMetricCCPolSup.marker2.Selection<-ModelSelection(dat=ESS8,
                                                             S1 = list(NoOpen.HV.Metric.M2.Marker, CCPolSupport.PMetric.M1.MarkerSup2),
                                                             S2 = Str_model,
                                                             group = "country",
                                                             clusters=c(1,8),
                                                             seed = 100,
                                                             userStart = NULL,
                                                             s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCPolSupport.PMetric.Fit1.MarkerSup2),
                                                             max_it = 10000L,
                                                             nstarts = 150L,
                                                             printing = FALSE,
                                                             partition = "hard",
                                                             endogenous_cov = TRUE,
                                                             endo_group_specific = TRUE,
                                                             sam_method = "local",
                                                             meanstr = FALSE,
                                                             rescaling = F,
                                                             missing="FIML")

View(BasicModel.PMetricCCPolSup.marker2.Selection$Overview)



##expected CHull for cluster 4:
(-1156357)+(677-673)*(-1156339-(-1156357))/(681-673)
##-1156348

a<-BasicModel.FMetricCCPolSup.marker2.Selection$Overview
View(a)
a[4,3]<--1156346
ggplot(a, aes(x=nrpar, y=LL)) +
  geom_point()+
  geom_line()+
  labs(title = "Adjusted CHUll observed")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()

#
##plot for CHull observed
ggplot(BasicModel.PMetricCCPolSup.marker2.Selection$Overview, aes(x=nrpar, y=LL)) +
  geom_point()+
  geom_line()+
  labs(title = "Original CHUll observed")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()
#
##plot for CHull factor
ggplot(BasicModel.PMetricCCPolSup.marker2.Selection$Overview, aes(x=nrpar_fac, y=LL_fac)) +
  geom_point()+
  geom_line()+
  labs(title = "CHUll factor")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()
#
##plot for BIC_G observed
ggplot(BasicModel.PMetricCCPolSup.marker2.Selection$Overview, aes(x=Clusters, y=BIC_G))+
  geom_point()+geom_line()+
  labs(title = "BIC_G Observed")+xlab("Number of Clusters")+ylab("BIC_G")+
  theme_minimal()
#
##plot for BIC_G factor
ggplot(BasicModel.PMetricCCPolSup.marker2.Selection$Overview, aes(x=Clusters, y=BIC_G_fac))+
  geom_point()+geom_line()+
  labs(title = "BIC_G Factor")+xlab("Number of Clusters")+ylab("BIC_G")+
  theme_minimal()


####-------------------------------------------------------------------------------------------------------
#
##3 clusters 150 random starts

##Structural model
Str_model<-'
CCPolicySupport~SelfTran+Conser+SelfEnhan
'

#
##Option A: full metric invariance CCPolSupport with support3 as marker
CCPolicySupport.3clus.150S.MarkSup3.FM<-MMGSEM(dat=ESS8,
                                   S1 = list(NoOpen.HV.Metric.M2.Marker, CCPolSupport.FMetric.M1.MarkerSup3),
                                   S2 = Str_model,
                                   group = "country",
                                   nclus=3,
                                   seed = 100,
                                   userStart = NULL,
                                   s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCPolSupport.FMetric.Fit1.MarkerSup3),
                                   max_it = 10000L,
                                   nstarts = 150L,
                                   printing = FALSE,
                                   partition = "hard",
                                   endogenous_cov = TRUE,
                                   endo_group_specific = TRUE,
                                   sam_method = "local",
                                   meanstr = FALSE,
                                   rescaling = F,
                                   missing="FIML")
round(CCPolicySupport.3clus.150S.MarkSup3.FM$posteriors, digits = 5)
#
##Option B: full metric invariance CCPolSupport with support2 as marker
CCPolicySupport.3clus.150S.MarkSup2.FM<-MMGSEM(dat=ESS8,
                                            S1 = list(NoOpen.HV.Metric.M2.Marker, CCPolSupport.FMetric.M1.MarkerSup2),
                                            S2 = Str_model,
                                            group = "country",
                                            nclus=3,
                                            seed = 100,
                                            userStart = NULL,
                                            s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCPolSupport.FMetric.Fit1.MarkerSup2),
                                            max_it = 10000L,
                                            nstarts = 150L,
                                            printing = FALSE,
                                            partition = "hard",
                                            endogenous_cov = TRUE,
                                            endo_group_specific = TRUE,
                                            sam_method = "local",
                                            meanstr = FALSE,
                                            rescaling = F,
                                            missing="FIML")
round(CCPolicySupport.3clus.150S.MarkSup2.FM$posteriors, digits = 5)
#
##Option c: partial metric invariance CCPolSupport with support2 as marker
CCPolicySupport.3clus.150S.MarkSup2.PM<-MMGSEM(dat=ESS8,
                                            S1 = list(NoOpen.HV.Metric.M2.Marker, CCPolSupport.PMetric.M1.MarkerSup2),
                                            S2 = Str_model,
                                            group = "country",
                                            nclus=3,
                                            seed = 100,
                                            userStart = NULL,
                                            s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCPolSupport.PMetric.Fit1.MarkerSup2),
                                            max_it = 10000L,
                                            nstarts = 150L,
                                            printing = FALSE,
                                            partition = "hard",
                                            endogenous_cov = TRUE,
                                            endo_group_specific = TRUE,
                                            sam_method = "local",
                                            meanstr = FALSE,
                                            rescaling = F,
                                            missing="FIML")
round(CCPolicySupport.3clus.150S.MarkSup2.PM$posteriors, digits = 5)

###------------------------------------------------------------------------------------------------------------------------------
##Extra clustering run for 4-cluster solution with CCPolSupport Partial metric invariance
CCPolicySupport.4clus.150S.MarkSup2.PM<-MMGSEM(dat=ESS8,
                                               S1 = list(NoOpen.HV.Metric.M2.Marker, CCPolSupport.PMetric.M1.MarkerSup2),
                                               S2 = Str_model,
                                               group = "country",
                                               nclus=4,
                                               seed = 100,
                                               userStart = NULL,
                                               s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCPolSupport.PMetric.Fit1.MarkerSup2),
                                               max_it = 10000L,
                                               nstarts = 150L,
                                               printing = FALSE,
                                               partition = "hard",
                                               endogenous_cov = TRUE,
                                               endo_group_specific = TRUE,
                                               sam_method = "local",
                                               meanstr = FALSE,
                                               rescaling = F,
                                               missing="FIML")
round(CCPolicySupport.4clus.150S.MarkSup2.PM$posteriors, digits = 5)


###------------------------------------------------------------------------------------------------------------------------------
##Clustering membership
#
##Option A: CCPolSupport Full metric with support 3 as marker
##3 clusters solution
clustering.3clus<-t(apply(CCPolicySupport.3clus.150S.MarkSup3.FM$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.3clus[,2]<-ifelse(clustering.3clus[,2]==1,2,0)
clustering.3clus[,3]<-ifelse(clustering.3clus[,3]==1,3,0)

ClusMembership.3clus<-apply(clustering.3clus,1,function(x) sum(x))
ClusterRes.3clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.3clus)

countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.3clus<-merge(ClusterRes.3clus, countries,
                        by.x = "group", by.y = "group")

#
##Option B: CCPolSupport Full metric with support 2 as marker
##3 clusters solution
clustering.3clus<-t(apply(CCPolicySupport.3clus.150S.MarkSup2.FM$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.3clus[,2]<-ifelse(clustering.3clus[,2]==1,2,0)
clustering.3clus[,3]<-ifelse(clustering.3clus[,3]==1,3,0)

ClusMembership.3clus<-apply(clustering.3clus,1,function(x) sum(x))
ClusterRes.3clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.3clus)

countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.3clus<-merge(ClusterRes.3clus, countries,
                        by.x = "group", by.y = "group")


#
##Option C: CCPolSupport Partial metric with support 2 as marker
##3 clusters solution
clustering.3clus<-t(apply(CCPolicySupport.3clus.150S.MarkSup2.PM$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.3clus[,2]<-ifelse(clustering.3clus[,2]==1,2,0)
clustering.3clus[,3]<-ifelse(clustering.3clus[,3]==1,3,0)

ClusMembership.3clus<-apply(clustering.3clus,1,function(x) sum(x))
ClusterRes.3clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.3clus)

countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.3clus<-merge(ClusterRes.3clus, countries,
                        by.x = "group", by.y = "group")

#
##Extra Option: CCPolSupport Partial metric with support 2 as marker
##4 clusters solution
clustering.4clus<-t(apply(CCPolicySupport.4clus.150S.MarkSup2.PM$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.4clus[,2]<-ifelse(clustering.4clus[,2]==1,2,0)
clustering.4clus[,3]<-ifelse(clustering.4clus[,3]==1,3,0)
clustering.4clus[,4]<-ifelse(clustering.4clus[,4]==1,4,0)

ClusMembership.4clus<-apply(clustering.4clus,1,function(x) sum(x))
ClusterRes.4clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.4clus)

countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.4clus<-merge(ClusterRes.4clus, countries,
                        by.x = "group", by.y = "group")



###########################################################################################
############## Basic Model -- Policy Support: SAM estimation and comparison ###############
###########################################################################################


#####----------------------------------------------------------------------------------------------------
##Option A: CCPolSupport full metric with support 3 as the marker 
#
##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
lambda_HV_23cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_23cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from CCBelief
EST_CCPolSupport<-lavInspect(CCPolSupport.FMetric.Fit1.MarkerSup3, what = "est")
lambda_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","lambda")
theta_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","theta")
#
##initialize empty list to store the new lambda matrix, new theta matrix and the mapping matrix
lambda_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
theta_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
Mmatrix<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))){
  ##put lambda from two measurement blocks into into the same matrix for each group
  lambda_23cntry[[g]]<-lav_matrix_bdiag(lambda_HV_23cntry[[g]], lambda_CCPolSupport_23cntry[[g]])
  colnames(lambda_23cntry[[g]])<-c(colnames(lambda_HV_23cntry[[g]]), colnames(lambda_CCPolSupport_23cntry[[g]]))
  rownames(lambda_23cntry[[g]])<-c(rownames(lambda_HV_23cntry[[g]]), rownames(lambda_CCPolSupport_23cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_23cntry[[g]]<-lav_matrix_bdiag(theta_HV_23cntry[[g]],theta_CCPolSupport_23cntry[[g]])
  colnames(theta_23cntry[[g]])<-c(colnames(theta_HV_23cntry[[g]]), colnames(theta_CCPolSupport_23cntry[[g]]))
  rownames(theta_23cntry[[g]])<-c(rownames(theta_HV_23cntry[[g]]), rownames(theta_CCPolSupport_23cntry[[g]]))
  
  ##compute the mapping matrix for each group
  Mmatrix[[g]]<-solve(t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]]) %*% lambda_23cntry[[g]]) %*% t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]])
}
#
##run an empty sem to just extract the imputed sample covariance matrix
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCPolicySupport=~support3+support1+support2
'

fake<-cfa(model = fake_model,
          data = ESS8,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F)

S<-fake@SampleStats@cov

S<-lapply(S, function(x) {
  colnames(x)<-rownames(x)<-colnames(fitted(fake)[[1]]$cov)
  x
})

#
##compute the factor covariance matrix for each group that will be used for the second step:
Var_eta<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_23cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
CCPolSupport_FREEsam_str_model<-'
CCPolicySupport~SelfTran+Conser+SelfEnhan
'

CCPolSupport.FreeSAM.FM.Mark3<-cfa(model = CCPolSupport_FREEsam_str_model,
                        sample.cov = Var_eta,
                        sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/CCPolSupport_FM_markSup3_FreeSAM.txt")
summary(CCPolSupport.FreeSAM.FM.Mark3, fit.measures=T, standardized=T)
sink()

###Once we have the factor covariance matrix from step 1
##We can estimate the structural parameter for different cluster solution from now on:

##3-cluster: 
##cluster 1: group 16
##cluster 2: group 11,12,13,14,19,21
##cluster 3: group 1,2,3,4,5,6,7,8,9,10,15,17,18,20,22,23

sam_CCPolSupport_3clus_FMMark3<-'
CCPolicySupport~c(a3,a3,a3,a3,a3,a3,a3,a3,a3,a3,a2,a2,a2,a2,a3,a1,a3,a3,a2,a3,a2,a3,a3)*SelfTran+
                c(b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b2,b2,b2,b2,b3,b1,b3,b3,b2,b3,b2,b3,b3)*Conser+
                c(c3,c3,c3,c3,c3,c3,c3,c3,c3,c3,c2,c2,c2,c2,c3,c1,c3,c3,c2,c3,c2,c3,c3)*SelfEnhan
'

CCPolSupport.SAM.3clus.FMMark3<-cfa(model = sam_CCPolSupport_3clus_FMMark3,
                          sample.cov = Var_eta,
                          sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/CCPolSupport_SAM_FMMark3_3clus.txt")
summary(CCPolSupport.SAM.3clus.FMMark3, fit.measures=T, standardized=T)
sink()


##faceted dot plot
FreeSAMparam<-parameterEstimates(CCPolSupport.FreeSAM.FM.Mark3)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.3clus, 
                         by.x = "group", by.y = "group")

FreeSAM_reg_param$country <- fct_reorder(FreeSAM_reg_param$country, 
                                         FreeSAM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.20, 0.375)                             # Line positions
)

ggplot(FreeSAM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - Human Values on Climate Change Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()



##3-D scatter plot
FreeSAMparam<-parameterEstimates(CCPolSupport.FreeSAM.FM.Mark3)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.3clus, 
                         by.x = "group", by.y = "group")

CCPolSupport_3clus_3D_FMMark3<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
        type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM with clustering results - Human Values on Climate Change Policy Support",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))


htmlwidgets::saveWidget(as_widget(CCPolSupport_3clus_3D_FMMark3), "CCPolSupport_3clus_3D_FMMark3.html")




#####----------------------------------------------------------------------------------------------------
##Option B: CCPolSupport full metric with support 2 as the marker 
#
##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
lambda_HV_23cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_23cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from CCBelief
EST_CCPolSupport<-lavInspect(CCPolSupport.FMetric.Fit1.MarkerSup2, what = "est")
lambda_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","lambda")
theta_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","theta")
#
##initialize empty list to store the new lambda matrix, new theta matrix and the mapping matrix
lambda_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
theta_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
Mmatrix<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))){
  ##put lambda from two measurement blocks into into the same matrix for each group
  lambda_23cntry[[g]]<-lav_matrix_bdiag(lambda_HV_23cntry[[g]], lambda_CCPolSupport_23cntry[[g]])
  colnames(lambda_23cntry[[g]])<-c(colnames(lambda_HV_23cntry[[g]]), colnames(lambda_CCPolSupport_23cntry[[g]]))
  rownames(lambda_23cntry[[g]])<-c(rownames(lambda_HV_23cntry[[g]]), rownames(lambda_CCPolSupport_23cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_23cntry[[g]]<-lav_matrix_bdiag(theta_HV_23cntry[[g]],theta_CCPolSupport_23cntry[[g]])
  colnames(theta_23cntry[[g]])<-c(colnames(theta_HV_23cntry[[g]]), colnames(theta_CCPolSupport_23cntry[[g]]))
  rownames(theta_23cntry[[g]])<-c(rownames(theta_HV_23cntry[[g]]), rownames(theta_CCPolSupport_23cntry[[g]]))
  
  ##compute the mapping matrix for each group
  Mmatrix[[g]]<-solve(t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]]) %*% lambda_23cntry[[g]]) %*% t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]])
}
#
##run an empty sem to just extract the imputed sample covariance matrix
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCPolicySupport=~support2+support1+support3
'

fake<-cfa(model = fake_model,
          data = ESS8,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F)

S<-fake@SampleStats@cov

S<-lapply(S, function(x) {
  colnames(x)<-rownames(x)<-colnames(fitted(fake)[[1]]$cov)
  x
})

#
##compute the factor covariance matrix for each group that will be used for the second step:
Var_eta<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_23cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
CCPolSupport_FREEsam_str_model<-'
CCPolicySupport~SelfTran+Conser+SelfEnhan
'

CCPolSupport.FreeSAM.FM.Mark2<-cfa(model = CCPolSupport_FREEsam_str_model,
                                   sample.cov = Var_eta,
                                   sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/CCPolSupport_FM_markSup2_FreeSAM.txt")
summary(CCPolSupport.FreeSAM.FM.Mark2, fit.measures=T, standardized=T)
sink()

###Once we have the factor covariance matrix from step 1
##We can estimate the structural parameter for different cluster solution from now on:

##3-cluster: 
##cluster 1: group 16
##cluster 2: group 11,12,13,14,19,21
##cluster 3: group 1,2,3,4,5,6,7,8,9,10,15,17,18,20,22,23

sam_CCPolSupport_3clus_FMMark2<-'
CCPolicySupport~c(a3,a3,a3,a3,a3,a3,a3,a3,a3,a3,a2,a2,a2,a2,a3,a1,a3,a3,a2,a3,a2,a3,a3)*SelfTran+
                c(b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b2,b2,b2,b2,b3,b1,b3,b3,b2,b3,b2,b3,b3)*Conser+
                c(c3,c3,c3,c3,c3,c3,c3,c3,c3,c3,c2,c2,c2,c2,c3,c1,c3,c3,c2,c3,c2,c3,c3)*SelfEnhan
'

CCPolSupport.SAM.3clus.FMMark2<-cfa(model = sam_CCPolSupport_3clus_FMMark2,
                                    sample.cov = Var_eta,
                                    sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/CCPolSupport_SAM_FMMark2_3clus.txt")
summary(CCPolSupport.SAM.3clus.FMMark2, fit.measures=T, standardized=T)
sink()


##faceted dot plot
FreeSAMparam<-parameterEstimates(CCPolSupport.FreeSAM.FM.Mark2)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.3clus, 
                         by.x = "group", by.y = "group")

FreeSAM_reg_param$country <- fct_reorder(FreeSAM_reg_param$country, 
                                         FreeSAM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.125, 0.375)                             # Line positions
)

ggplot(FreeSAM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - Human Values on Climate Change Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()



##3-D scatter plot
FreeSAMparam<-parameterEstimates(CCPolSupport.FreeSAM.FM.Mark2)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.3clus, 
                         by.x = "group", by.y = "group")

CCPolSupport_3clus_3D_FMMark2<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                                       type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM with clustering results - Human Values on Climate Change Policy Support",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))


htmlwidgets::saveWidget(as_widget(CCPolSupport_3clus_3D_FMMark2), "CCPolSupport_3clus_3D_FMMark2.html")



#####----------------------------------------------------------------------------------------------------
##Option C: CCPolSupport partial metric with support 2 as the marker 
#
##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
lambda_HV_23cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_23cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from CCBelief
EST_CCPolSupport<-lavInspect(CCPolSupport.PMetric.Fit1.MarkerSup2, what = "est")
lambda_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","lambda")
theta_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","theta")
#
##initialize empty list to store the new lambda matrix, new theta matrix and the mapping matrix
lambda_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
theta_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
Mmatrix<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))){
  ##put lambda from two measurement blocks into into the same matrix for each group
  lambda_23cntry[[g]]<-lav_matrix_bdiag(lambda_HV_23cntry[[g]], lambda_CCPolSupport_23cntry[[g]])
  colnames(lambda_23cntry[[g]])<-c(colnames(lambda_HV_23cntry[[g]]), colnames(lambda_CCPolSupport_23cntry[[g]]))
  rownames(lambda_23cntry[[g]])<-c(rownames(lambda_HV_23cntry[[g]]), rownames(lambda_CCPolSupport_23cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_23cntry[[g]]<-lav_matrix_bdiag(theta_HV_23cntry[[g]],theta_CCPolSupport_23cntry[[g]])
  colnames(theta_23cntry[[g]])<-c(colnames(theta_HV_23cntry[[g]]), colnames(theta_CCPolSupport_23cntry[[g]]))
  rownames(theta_23cntry[[g]])<-c(rownames(theta_HV_23cntry[[g]]), rownames(theta_CCPolSupport_23cntry[[g]]))
  
  ##compute the mapping matrix for each group
  Mmatrix[[g]]<-solve(t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]]) %*% lambda_23cntry[[g]]) %*% t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]])
}
#
##run an empty sem to just extract the imputed sample covariance matrix
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCPolicySupport=~support2+support1+support3
'

fake<-cfa(model = fake_model,
          data = ESS8,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F)

S<-fake@SampleStats@cov

S<-lapply(S, function(x) {
  colnames(x)<-rownames(x)<-colnames(fitted(fake)[[1]]$cov)
  x
})

#
##compute the factor covariance matrix for each group that will be used for the second step:
Var_eta<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_23cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
CCPolSupport_FREEsam_str_model<-'
CCPolicySupport~SelfTran+Conser+SelfEnhan
'

CCPolSupport.FreeSAM.PM.Mark2<-cfa(model = CCPolSupport_FREEsam_str_model,
                                   sample.cov = Var_eta,
                                   sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/CCPolSupport_PM_markSup2_FreeSAM.txt")
summary(CCPolSupport.FreeSAM.PM.Mark2, fit.measures=T, standardized=T)
sink()

###Once we have the factor covariance matrix from step 1
##We can estimate the structural parameter for different cluster solution from now on:

##3-cluster: 
##cluster 1: group 6,12,13,14,19,21,23
##cluster 2: group 11,16
##cluster 3: group 1,2,3,4,5,7,8,9,10,15,17,18,20,22

sam_CCPolSupport_3clus_PMMark2<-'
CCPolicySupport~c(a3,a3,a3,a3,a3,a1,a3,a3,a3,a3,a2,a1,a1,a1,a3,a2,a3,a3,a1,a3,a1,a3,a1)*SelfTran+
                c(b3,b3,b3,b3,b3,b1,b3,b3,b3,b3,b2,b1,b1,b1,b3,b2,b3,b3,b1,b3,b1,b3,b1)*Conser+
                c(c3,c3,c3,c3,c3,c1,c3,c3,c3,c3,c2,c1,c1,c1,c3,c2,c3,c3,c1,c3,c1,c3,c1)*SelfEnhan
'

CCPolSupport.SAM.3clus.PMMark2<-cfa(model = sam_CCPolSupport_3clus_PMMark2,
                                    sample.cov = Var_eta,
                                    sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/CCPolSupport_SAM_PMMark2_3clus.txt")
summary(CCPolSupport.SAM.3clus.PMMark2, fit.measures=T, standardized=T)
sink()


##faceted dot plot
FreeSAMparam<-parameterEstimates(CCPolSupport.FreeSAM.PM.Mark2)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.3clus, 
                         by.x = "group", by.y = "group")

FreeSAM_reg_param$country <- fct_reorder(FreeSAM_reg_param$country, 
                                         FreeSAM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(c(-0.6,0), c(0.07,0.65),c(-0.21,0.4))                             # Line positions
)

ggplot(FreeSAM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - Human Values on Climate Change Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()



##3-D scatter plot
FreeSAMparam<-parameterEstimates(CCPolSupport.FreeSAM.PM.Mark2)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.3clus, 
                         by.x = "group", by.y = "group")

CCPolSupport_3clus_3D_PMMark2<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                                       type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM with clustering results - Human Values on Climate Change Policy Support",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))


htmlwidgets::saveWidget(as_widget(CCPolSupport_3clus_3D_PMMark2), "CCPolSupport_3clus_3D_PMMark2.html")


#####----------------------------------------------------------------------------------------------------
##Extra Option: CCPolSupport partial metric with support 2 as the marker but FOR 4 CLUSTERS 
#
##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
lambda_HV_23cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_23cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from CCBelief
EST_CCPolSupport<-lavInspect(CCPolSupport.PMetric.Fit1.MarkerSup2, what = "est")
lambda_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","lambda")
theta_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","theta")
#
##initialize empty list to store the new lambda matrix, new theta matrix and the mapping matrix
lambda_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
theta_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
Mmatrix<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))){
  ##put lambda from two measurement blocks into into the same matrix for each group
  lambda_23cntry[[g]]<-lav_matrix_bdiag(lambda_HV_23cntry[[g]], lambda_CCPolSupport_23cntry[[g]])
  colnames(lambda_23cntry[[g]])<-c(colnames(lambda_HV_23cntry[[g]]), colnames(lambda_CCPolSupport_23cntry[[g]]))
  rownames(lambda_23cntry[[g]])<-c(rownames(lambda_HV_23cntry[[g]]), rownames(lambda_CCPolSupport_23cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_23cntry[[g]]<-lav_matrix_bdiag(theta_HV_23cntry[[g]],theta_CCPolSupport_23cntry[[g]])
  colnames(theta_23cntry[[g]])<-c(colnames(theta_HV_23cntry[[g]]), colnames(theta_CCPolSupport_23cntry[[g]]))
  rownames(theta_23cntry[[g]])<-c(rownames(theta_HV_23cntry[[g]]), rownames(theta_CCPolSupport_23cntry[[g]]))
  
  ##compute the mapping matrix for each group
  Mmatrix[[g]]<-solve(t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]]) %*% lambda_23cntry[[g]]) %*% t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]])
}
#
##run an empty sem to just extract the imputed sample covariance matrix
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCPolicySupport=~support2+support1+support3
'

fake<-cfa(model = fake_model,
          data = ESS8,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F)

S<-fake@SampleStats@cov

S<-lapply(S, function(x) {
  colnames(x)<-rownames(x)<-colnames(fitted(fake)[[1]]$cov)
  x
})

#
##compute the factor covariance matrix for each group that will be used for the second step:
Var_eta<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_23cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
CCPolSupport_FREEsam_str_model<-'
CCPolicySupport~SelfTran+Conser+SelfEnhan
'

CCPolSupport.FreeSAM.PM.Mark2<-cfa(model = CCPolSupport_FREEsam_str_model,
                                   sample.cov = Var_eta,
                                   sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/CCPolSupport_PM_markSup2_FreeSAM.txt")
summary(CCPolSupport.FreeSAM.PM.Mark2, fit.measures=T, standardized=T)
sink()

###Once we have the factor covariance matrix from step 1
##We can estimate the structural parameter for different cluster solution from now on:

##4-cluster: 
##cluster 1: group 16
##cluster 2: group 11
##cluster 3: group 1,2,3,4,5,7,8,9,10,15,17,18,20,22
##cluster 4: group 6,12,13,14,19,21,23

sam_CCPolSupport_4clus_PMMark2<-'
CCPolicySupport~c(a3,a3,a3,a3,a3,a4,a3,a3,a3,a3,a2,a4,a4,a4,a3,a1,a3,a3,a4,a3,a4,a3,a4)*SelfTran+
                c(b3,b3,b3,b3,b3,b4,b3,b3,b3,b3,b2,b4,b4,b4,b3,b1,b3,b3,b4,b3,b4,b3,b4)*Conser+
                c(c3,c3,c3,c3,c3,c4,c3,c3,c3,c3,c2,c4,c4,c4,c3,c1,c3,c3,c4,c3,c4,c3,c4)*SelfEnhan
'

CCPolSupport.SAM.4clus.PMMark2<-cfa(model = sam_CCPolSupport_4clus_PMMark2,
                                    sample.cov = Var_eta,
                                    sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/CCPolSupport_SAM_PMMark2_4clus.txt")
summary(CCPolSupport.SAM.4clus.PMMark2, fit.measures=T, standardized=T)
sink()


##faceted dot plot
FreeSAMparam<-parameterEstimates(CCPolSupport.FreeSAM.PM.Mark2)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.4clus, 
                         by.x = "group", by.y = "group")

FreeSAM_reg_param$country <- fct_reorder(FreeSAM_reg_param$country, 
                                         FreeSAM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(c(-0.6,0), c(0.07,0.65),c(-0.21,0.4))                             # Line positions
)

vline_data <- data.frame(
  Human.Values = c(rep("Conservation", 3), rep("Self-Transcendence", 3), "Self-Enhancement"), # Assign facets
  xintercept = c(-0.6, -0.21, 0.07, 0, 0.37, 0.65, -0.05)  # Line positions
)

ggplot(FreeSAM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - Human Values on Climate Change Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()



##3-D scatter plot
FreeSAMparam<-parameterEstimates(CCPolSupport.FreeSAM.PM.Mark2)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.4clus, 
                         by.x = "group", by.y = "group")

CCPolSupport_4clus_3D_PMMark2<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                                       type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM with clustering results - Human Values on Climate Change Policy Support",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))


htmlwidgets::saveWidget(as_widget(CCPolSupport_4clus_3D_PMMark2), "CCPolSupport_4clus_3D_PMMark2.html")


#####################################################################################
######### TO BE UPDATE Basic Model - Metric CC Policy Support: Simultaneously MGSEM #########
#####################################################################################

##Specify the model:
BasicModel.HV.CCPolSupport<-'
##human values
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

##Climate Change Policy Support
CCPolicySupport=~support2+support1+support3

##Structural Model:
CCPolicySupport~SelfTran+Conser+SelfEnhan
'

##run regular MGSEM:
RegSEM.BasicModel.HV.CCPolSupport<-cfa(model = BasicModel.HV.CCPolSupport,
                                   data = ESS8,
                                   group = "country",
                                   estimator="MLR",
                                   missing="FIML",
                                   group.equal="loadings",
                                   group.partial=c("SelfEnhan=~SE3"))

sink("./Sink Output/ESS8/CCPolSupport_BasicModel_FreeRegSEM2.txt")
summary(RegSEM.BasicModel.HV.CCPolSupport, fit.measures=T, standardized=T)
sink()


###------------------------------------------------------------------------------
##3-cluster solution

##faceted dot plot
FreeSEM_param<-parameterEstimates(RegSEM.BasicModel.HV.CCPolSupport)

FreeSEM_reg_param<-FreeSEM_param %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))

FreeSEM_reg_param<-merge(FreeSEM_reg_param, ClusterRes.3clus, 
                         by.x = "group", by.y = "group")

FreeSEM_reg_param$country <- fct_reorder(FreeSEM_reg_param$country, 
                                         FreeSEM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, -0.25, 0.5)                             # Line positions
)


ggplot(FreeSEM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "Simultaneous MGSEM with clustering results - Human Values on Climate Change Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()


##MGSEM with constrains within clusters:
BasicModel.HV.CCPolSupport.3Clus<-'
##human values
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

##Climate Change Belief
CCPolicySupport=~support3+support1+support2

##Structural Model:
CCPolicySupport~c(a1,a1,a1,a1,a1,a2,a1,a1,a1,a1,a2,a2,a2,a2,a1,a3,a1,a1,a2,a1,a2,a1,a1)*SelfTran+
          c(b1,b1,b1,b1,b1,b2,b1,b1,b1,b1,b2,b2,b2,b2,b1,b3,b1,b1,b2,b1,b2,b1,b1)*Conser+
          c(c1,c1,c1,c1,c1,c2,c1,c1,c1,c1,c2,c2,c2,c2,c1,c3,c1,c1,c2,c1,c2,c1,c1)*SelfEnhan
'

##run regular MGSEM:
RegSEM.BasicModel.HV.CCPolSupport.3clus<-cfa(model = BasicModel.HV.CCPolSupport.3Clus,
                                         data = ESS8,
                                         group = "country",
                                         estimator="MLR",
                                         missing="FIML",
                                         group.equal="loadings",
                                         group.partial=c("SelfEnhan=~SE3"))

sink("./Sink Output/ESS8/CCPolSupport_BasicModel_RegMGSEM_3clus.txt")
summary(RegSEM.BasicModel.HV.CCPolSupport.3clus, fit.measures=T, standardized=T)
sink()

lavTestLRT(RegSEM.BasicModel.HV.CCPolSupport.3clus, RegSEM.BasicModel.HV.CCPolSupport)


###------------------------------------------------------------------------------
##3-clusters solution with FIML
FreeSEM_param<-parameterEstimates(RegSEM.BasicModel.HV.CCPolSupport)

FreeSEM_reg_param<-FreeSEM_param %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))

FreeSEM_reg_param<-merge(FreeSEM_reg_param, ClusterRes.3clus, 
                         by.x = "group", by.y = "group")

FreeSEM_reg_param$country <- fct_reorder(FreeSEM_reg_param$country, 
                                         FreeSEM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, -0.1, 0.375)                             # Line positions
)


ggplot(FreeSEM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "Simultaneous MGSEM with clustering results - Human Values on Climate Change Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()


##MGSEM with constrains within clusters:
BasicModel.HV.CCPolSupport.3Clus.FIML<-'
##human values
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

##Climate Change Belief
CCPolicySupport=~support2+support1+support3

##Structural Model:
CCPolicySupport~c(a3,a3,a3,a3,a3,a3,a3,a3,a3,a3,a2,a2,a2,a2,a3,a1,a3,a3,a2,a3,a2,a3,a3)*SelfTran+
                c(b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b2,b2,b2,b2,b3,b1,b3,b3,b2,b3,b2,b3,b3)*Conser+
                c(c3,c3,c3,c3,c3,c3,c3,c3,c3,c3,c2,c2,c2,c2,c3,c1,c3,c3,c2,c3,c2,c3,c3)*SelfEnhan
'

##run regular MGSEM:
RegSEM.BasicModel.HV.CCPolSupport.3clus.FIML<-cfa(model = BasicModel.HV.CCPolSupport.3Clus.FIML,
                                             data = ESS8,
                                             group = "country",
                                             estimator="MLR",
                                             missing="FIML",
                                             group.equal="loadings",
                                             group.partial=c("SelfEnhan=~SE3"))

sink("./Sink Output/ESS8/CCPolSupport_BasicModel_RegMGSEM_3clus_FIML2.txt")
summary(RegSEM.BasicModel.HV.CCPolSupport.3clus.FIML, fit.measures=T, standardized=T)
sink()


lavTestLRT(RegSEM.BasicModel.HV.CCPolSupport, RegSEM.BasicModel.HV.CCPolSupport.3clus.FIML)

###------------------------------------------------------------------------------
##4-cluster solution - 50 random starts

##faceted dot plot
FreeSEM_param<-parameterEstimates(RegSEM.BasicModel.HV.CCPolSupport)

FreeSEM_reg_param<-FreeSEM_param %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))

FreeSEM_reg_param<-merge(FreeSEM_reg_param, ClusterRes.4clus.50s, 
                         by.x = "group", by.y = "group")

FreeSEM_reg_param$country <- fct_reorder(FreeSEM_reg_param$country, 
                                         FreeSEM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, -0.25, 0.5)                             # Line positions
)


ggplot(FreeSEM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "Simultaneous MGSEM with clustering results - Human Values on Climate Change Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()


##MGSEM with constrains within clusters:
BasicModel.HV.CCPolSupport.4Clus.50s<-'
##human values
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

##Climate Change Belief
CCPolicySupport=~support3+support1+support2

##Structural Model:
CCPolicySupport~c(a4,a4,a4,a4,a4,a3,a4,a4,a4,a4,a1,a1,a3,a3,a4,a2,a4,a4,a3,a4,a3,a4,a3)*SelfTran+
                c(b4,b4,b4,b4,b4,b3,b4,b4,b4,b4,b1,b1,b3,b3,b4,b2,b4,b4,b3,b4,b3,b4,b3)*Conser+
                c(c4,c4,c4,c4,c4,c3,c4,c4,c4,c4,c1,c1,c3,c3,c4,c2,c4,c4,c3,c4,c3,c4,c3)*SelfEnhan
'

##run regular MGSEM:
RegSEM.BasicModel.HV.CCPolSupport.4clus.50s<-cfa(model = BasicModel.HV.CCPolSupport.4Clus.50s,
                                             data = ESS8,
                                             group = "country",
                                             estimator="MLR",
                                             missing="FIML",
                                             group.equal="loadings",
                                             group.partial=c("SelfEnhan=~SE3"))

sink("./Sink Output/ESS8/CCPolSupport_BasicModel_RegMGSEM_4clus_50s.txt")
summary(RegSEM.BasicModel.HV.CCPolSupport.4clus.50s, fit.measures=T, standardized=T)
sink()

lavTestLRT(RegSEM.BasicModel.HV.CCPolSupport.4clus.50s, RegSEM.BasicModel.HV.CCPolSupport)



###------------------------------------------------------------------------------
##4-cluster solution - 150 random starts

##faceted dot plot
FreeSEM_param<-parameterEstimates(RegSEM.BasicModel.HV.CCPolSupport)

FreeSEM_reg_param<-FreeSEM_param %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))

FreeSEM_reg_param<-merge(FreeSEM_reg_param, ClusterRes.4clus.150s, 
                         by.x = "group", by.y = "group")

FreeSEM_reg_param$country <- fct_reorder(FreeSEM_reg_param$country, 
                                         FreeSEM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0.05, -0.25, 0.5)                             # Line positions
)


ggplot(FreeSEM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "Simultaneous MGSEM with clustering results - Human Values on Climate Change Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()


##MGSEM with constrains within clusters:
BasicModel.HV.CCPolSupport.4Clus.150s<-'
##human values
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

##Climate Change Belief
CCPolicySupport=~support3+support1+support2

##Structural Model:
CCPolicySupport~c(a2,a2,a2,a2,a2,a3,a2,a2,a2,a2,a1,a1,a3,a3,a2,a4,a2,a2,a3,a2,a3,a2,a2)*SelfTran+
                c(b2,b2,b2,b2,b2,b3,b2,b2,b2,b2,b1,b1,b3,b3,b2,b4,b2,b2,b3,b2,b3,b2,b2)*Conser+
                c(c2,c2,c2,c2,c2,c3,c2,c2,c2,c2,c1,c1,c3,c3,c2,c4,c2,c2,c3,c2,c3,c2,c2)*SelfEnhan
'

##run regular MGSEM:
RegSEM.BasicModel.HV.CCPolSupport.4clus.150s<-cfa(model = BasicModel.HV.CCPolSupport.4Clus.150s,
                                                 data = ESS8,
                                                 group = "country",
                                                 estimator="MLR",
                                                 missing="FIML",
                                                 group.equal="loadings",
                                                 group.partial=c("SelfEnhan=~SE3"))

sink("./Sink Output/ESS8/CCPolSupport_BasicModel_RegMGSEM_4clus_150s.txt")
summary(RegSEM.BasicModel.HV.CCPolSupport.4clus.150s, fit.measures=T, standardized=T)
sink()

lavTestLRT(RegSEM.BasicModel.HV.CCPolSupport.4clus.150s, RegSEM.BasicModel.HV.CCPolSupport)


###------------------------------------------------------------------------------
##6-cluster solution - 150 random starts

##faceted dot plot
FreeSEM_param<-parameterEstimates(RegSEM.BasicModel.HV.CCPolSupport)

FreeSEM_reg_param<-FreeSEM_param %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))

FreeSEM_reg_param<-merge(FreeSEM_reg_param, ClusterRes.6clus.150s, 
                         by.x = "group", by.y = "group")

FreeSEM_reg_param$country <- fct_reorder(FreeSEM_reg_param$country, 
                                         FreeSEM_reg_param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Self-Enhancement", "Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0.05, -0.25, 0.5)                             # Line positions
)


ggplot(FreeSEM_reg_param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "Simultaneous MGSEM with clustering results - Human Values on Climate Change Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()


##MGSEM with constrains within clusters:
BasicModel.HV.CCPolSupport.6Clus.150s<-'
##human values
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

##Climate Change Belief
CCPolicySupport=~support3+support1+support2

##Structural Model:
CCPolicySupport~c(a1,a6,a6,a1,a1,a4,a1,a6,a1,a1,a3,a3,a4,a4,a2,a5,a6,a6,a4,a1,a4,a1,a6)*SelfTran+
                c(b1,b6,b6,b1,b1,b4,b1,b6,b1,b1,b3,b3,b4,b4,b2,b5,b6,b6,b4,b1,b4,b1,b6)*Conser+
                c(c1,c6,c6,c1,c1,c4,c1,c6,c1,c1,c3,c3,c4,c4,c2,c5,c6,c6,c4,c1,c4,c1,c6)*SelfEnhan
'

##run regular MGSEM:
RegSEM.BasicModel.HV.CCPolSupport.6clus.150s<-cfa(model = BasicModel.HV.CCPolSupport.6Clus.150s,
                                                  data = ESS8,
                                                  group = "country",
                                                  estimator="MLR",
                                                  missing="FIML",
                                                  group.equal="loadings",
                                                  group.partial=c("SelfEnhan=~SE3"))

sink("./Sink Output/ESS8/CCPolSupport_BasicModel_RegMGSEM_6clus_150s.txt")
summary(RegSEM.BasicModel.HV.CCPolSupport.6clus.150s, fit.measures=T, standardized=T)
sink()

lavTestLRT(RegSEM.BasicModel.HV.CCPolSupport.6clus.150s, RegSEM.BasicModel.HV.CCPolSupport)



#####################################################################################
##################### CC Policy Support: Mapping   ##################################
#####################################################################################


###----------------------------------------------------------------------------------
##Option A and Option B: CCPolSupport full metric with either support 3 or support 2 as marker
#3 cluster solution:
##take out the world map
world_map<-map_data("world")

##filter to be a eu map:
eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
  "Ireland", "Iceland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
  "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia",
  "Slovakia", "Slovenia", "Switzerland",
  "Spain", "Sweden", "UK", "Israel",
  "Turkey", "Lebanon", "Jordan", "Egypt", "Syria",
  "Ukraine", "Belarus", "Georgia", "Armenia", "Azerbaijan", "Moldova"
)

eu_map <- world_map %>%
  filter(region %in% eu_countries)

##add a new column called region to match the country names with the world map data country names
ClusterRes.3clus<-ClusterRes.3clus %>%
  mutate(region=case_when(
    country == "AT" ~ "Austria",
    country == "BE" ~ "Belgium",
    country == "CH" ~ "Switzerland",
    country == "CZ" ~ "Czech Republic",
    country == "DE" ~ "Germany",
    country == "EE" ~ "Estonia",
    country == "ES" ~ "Spain",
    country == "FI" ~ "Finland",
    country == "FR" ~ "France",
    country == "GB" ~ "UK",
    country == "HU" ~ "Hungary",
    country == "IE" ~ "Ireland",
    country == "IL" ~ "Israel",
    country == "IS" ~ "Iceland",
    country == "IT" ~ "Italy",
    country == "LT" ~ "Lithuania",
    country == "NL" ~ "Netherlands",
    country == "NO" ~ "Norway",
    country == "PL" ~ "Poland",
    country == "PT" ~ "Portugal",
    country == "RU" ~ "Russia",
    country == "SE" ~ "Sweden",
    country == "SI" ~ "Slovenia"
  )) %>%
  select(ClusMembership, region)

##merge the data:
map_with_3clusters.FM <- eu_map %>%
  left_join(ClusterRes.3clus, by = "region")

##lay out on the map:
ggplot(map_with_3clusters.FM, aes(long, lat, group = group, fill = factor(ClusMembership))) +
  geom_polygon(color = "white") +
  labs(
    title = "Clustering Results on the Map",
    fill = "Cluster"
  ) +
  theme_minimal()


###----------------------------------------------------------------------------------
##Option C: CCPolSupport partial metric with support 2 as marker
#3 cluster solution:
##take out the world map
world_map<-map_data("world")

##filter to be a eu map:
eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
  "Ireland", "Iceland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
  "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia",
  "Slovakia", "Slovenia", "Switzerland",
  "Spain", "Sweden", "UK", "Israel",
  "Turkey", "Lebanon", "Jordan", "Egypt", "Syria",
  "Ukraine", "Belarus", "Georgia", "Armenia", "Azerbaijan", "Moldova"
)

eu_map <- world_map %>%
  filter(region %in% eu_countries)

##add a new column called region to match the country names with the world map data country names
ClusterRes.3clus<-ClusterRes.3clus %>%
  mutate(region=case_when(
    country == "AT" ~ "Austria",
    country == "BE" ~ "Belgium",
    country == "CH" ~ "Switzerland",
    country == "CZ" ~ "Czech Republic",
    country == "DE" ~ "Germany",
    country == "EE" ~ "Estonia",
    country == "ES" ~ "Spain",
    country == "FI" ~ "Finland",
    country == "FR" ~ "France",
    country == "GB" ~ "UK",
    country == "HU" ~ "Hungary",
    country == "IE" ~ "Ireland",
    country == "IL" ~ "Israel",
    country == "IS" ~ "Iceland",
    country == "IT" ~ "Italy",
    country == "LT" ~ "Lithuania",
    country == "NL" ~ "Netherlands",
    country == "NO" ~ "Norway",
    country == "PL" ~ "Poland",
    country == "PT" ~ "Portugal",
    country == "RU" ~ "Russia",
    country == "SE" ~ "Sweden",
    country == "SI" ~ "Slovenia"
  )) %>%
  select(ClusMembership, region)

##merge the data:
map_with_3clusters.PM <- eu_map %>%
  left_join(ClusterRes.3clus, by = "region")

##lay out on the map:
ggplot(map_with_3clusters.PM, aes(long, lat, group = group, fill = factor(ClusMembership))) +
  geom_polygon(color = "white") +
  labs(
    title = "Clustering Results on the Map",
    fill = "Cluster"
  ) +
  theme_minimal()



###----------------------------------------------------------------------------------
##Extra option: CCPolSupport partial metric with support 2 as marker
#4 cluster solution:
##take out the world map
world_map<-map_data("world")

##filter to be a eu map:
eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
  "Ireland", "Iceland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
  "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia",
  "Slovakia", "Slovenia", "Switzerland",
  "Spain", "Sweden", "UK", "Israel",
  "Turkey", "Lebanon", "Jordan", "Egypt", "Syria",
  "Ukraine", "Belarus", "Georgia", "Armenia", "Azerbaijan", "Moldova"
)

eu_map <- world_map %>%
  filter(region %in% eu_countries)

##add a new column called region to match the country names with the world map data country names
ClusterRes.4clus<-ClusterRes.4clus %>%
  mutate(region=case_when(
    country == "AT" ~ "Austria",
    country == "BE" ~ "Belgium",
    country == "CH" ~ "Switzerland",
    country == "CZ" ~ "Czech Republic",
    country == "DE" ~ "Germany",
    country == "EE" ~ "Estonia",
    country == "ES" ~ "Spain",
    country == "FI" ~ "Finland",
    country == "FR" ~ "France",
    country == "GB" ~ "UK",
    country == "HU" ~ "Hungary",
    country == "IE" ~ "Ireland",
    country == "IL" ~ "Israel",
    country == "IS" ~ "Iceland",
    country == "IT" ~ "Italy",
    country == "LT" ~ "Lithuania",
    country == "NL" ~ "Netherlands",
    country == "NO" ~ "Norway",
    country == "PL" ~ "Poland",
    country == "PT" ~ "Portugal",
    country == "RU" ~ "Russia",
    country == "SE" ~ "Sweden",
    country == "SI" ~ "Slovenia"
  )) %>%
  select(ClusMembership, region)

##merge the data:
map_with_4clusters.PM <- eu_map %>%
  left_join(ClusterRes.4clus, by = "region")

##lay out on the map:
ggplot(map_with_4clusters.PM, aes(long, lat, group = group, fill = factor(ClusMembership))) +
  geom_polygon(color = "white") +
  labs(
    title = "Clustering Results on the Map",
    fill = "Cluster"
  ) +
  theme_minimal()


#####################################################################################
##################### Mediation Model: MMGSEM #######################################
#####################################################################################


###----------------------------------------------------------------------------------------
##starting with the measurement model for human values and CCBelief:
#
###Human values: marker approach:
#
NoOpen.HV.Metric.M2.Marker<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit2.Marker<-cfa(model = NoOpen.HV.Metric.M2.Marker,
                                  data = ESS8,
                                  group = "country",
                                  estimator="MLR",
                                  missing="FIML",
                                  group.equal="loadings",
                                  group.partial=c("SelfEnhan=~SE3"))

#sink("./Sink Output/ESS8/NoOpen_HV_Metric_fit2_Marker.txt")
#summary(NoOpen.HV.Metric.Fit2.Marker, fit.measures=T, standardized=T)
#sink()

#
##Climate Change Belief
#
CCBelief.Metric.M1.Marker<-'
CCBelief=~ImpactBelief+TrendBelief+AttriBelief
'

CCBelief.Metric.Fit1.Marker<-cfa(model = CCBelief.Metric.M1.Marker,
                                 data = ESS8,
                                 group = "country",
                                 estimator="MLR",
                                 missing="FIML",
                                 group.equal="loadings")

#sink("./Sink Output/ESS8/CCBelief_Metric_fit1_marker.txt")
#summary(CCBelief.Metric.Fit1.Marker, fit.measures=T, standardized=T)
#sink()

##--------------------------------------------------------------------------------------------------------
##climate change policy support
#
##Option A: full metric invariance Model with support 3 as marker
CCPolSupport.FMetric.M1.MarkerSup3<-'
CCPolicySupport=~support3+support1+support2
'

CCPolSupport.FMetric.Fit1.MarkerSup3<-cfa(model = CCPolSupport.FMetric.M1.MarkerSup3,
                                          data = ESS8,
                                          group = "country",
                                          estimator="MLR",
                                          missing="FIML",
                                          group.equal="loadings")

#
##Option B: full metric invariance Model with support 2 as marker
CCPolSupport.FMetric.M1.MarkerSup2<-'
CCPolicySupport=~support2+support1+support3
'

CCPolSupport.FMetric.Fit1.MarkerSup2<-cfa(model = CCPolSupport.FMetric.M1.MarkerSup2,
                                          data = ESS8,
                                          group = "country",
                                          estimator="MLR",
                                          missing="FIML",
                                          group.equal="loadings")

#
##Option C: partial metric invariance model with support 2 as marker and let support 3 freely estimated (wide bound estimation)
CCPolSupport.PMetric.M1.MarkerSup2<-'
CCPolicySupport=~support2+support1+support3
'

CCPolSupport.PMetric.Fit1.MarkerSup2<-cfa(model = CCPolSupport.PMetric.M1.MarkerSup2,
                                          data = ESS8,
                                          group = "country",
                                          estimator="MLR",
                                          missing="FIML",
                                          group.equal="loadings",
                                          group.partial=c("CCPolicySupport=~support3"),
                                          bounds="wide")


####----------------------------------------------------------------------------------------------------
##Model selection:

##specify the structural model:
Str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan

CCPolicySupport~CCBelief+SelfTran+Conser+SelfEnhan
'

##Model selection:
#
##Option A: CCPolSupport full metric with support 3 as marker
MediationModel.Selection.FM.MarkSup3<-ModelSelection(dat=ESS8,
                                     S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.FMetric.M1.MarkerSup3),
                                     S2 = Str_model,
                                     group = "country",
                                     clusters=c(1,8),
                                     seed = 100,
                                     userStart = NULL,
                                     s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.FMetric.Fit1.MarkerSup3),
                                     max_it = 10000L,
                                     nstarts = 150L,
                                     printing = FALSE,
                                     partition = "hard",
                                     endogenous_cov = TRUE,
                                     endo_group_specific = TRUE,
                                     sam_method = "local",
                                     meanstr = FALSE,
                                     rescaling = F,
                                     missing="FIML")
View(MediationModel.Selection.FM.MarkSup3$Overview)
#
##Option B: CCPolSupport full metric with support 2 as marker
MediationModel.Selection.FM.MarkSup2<-ModelSelection(dat=ESS8,
                                                     S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.FMetric.M1.MarkerSup2),
                                                     S2 = Str_model,
                                                     group = "country",
                                                     clusters=c(1,8),
                                                     seed = 100,
                                                     userStart = NULL,
                                                     s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.FMetric.Fit1.MarkerSup2),
                                                     max_it = 10000L,
                                                     nstarts = 150L,
                                                     printing = FALSE,
                                                     partition = "hard",
                                                     endogenous_cov = TRUE,
                                                     endo_group_specific = TRUE,
                                                     sam_method = "local",
                                                     meanstr = FALSE,
                                                     rescaling = F,
                                                     missing="FIML")
View(MediationModel.Selection.FM.MarkSup2$Overview)
#
##Option C: CCPolSupport partial metric with support 2 as marker
MediationModel.Selection.PM.MarkSup2<-ModelSelection(dat=ESS8,
                                                     S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                                                     S2 = Str_model,
                                                     group = "country",
                                                     clusters=c(1,8),
                                                     seed = 100,
                                                     userStart = NULL,
                                                     s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                                                     max_it = 10000L,
                                                     nstarts = 1000L,
                                                     printing = FALSE,
                                                     partition = "hard",
                                                     endogenous_cov = TRUE,
                                                     endo_group_specific = TRUE,
                                                     sam_method = "local",
                                                     meanstr = FALSE,
                                                     rescaling = F,
                                                     missing="FIML")
View(MediationModel.Selection.PM.MarkSup2$Overview)


##CHull expected value for the 2-clusters solution:
(-1311938)+(771-763)*(-1311764-(-1311938))/(779-763)
#-1311851

a<-MediationModel.Selection.FM.MarkSup3$Overview
a[2,3]<--1311850

#
##plot for adjusted CHull observed
ggplot(a, aes(x=nrpar, y=LL)) +
  geom_point()+
  geom_line()+
  labs(title = "Adjusted CHUll observed")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()

#
##plot for CHull observed
ggplot(MediationModel.Selection.PM.MarkSup2$Overview, aes(x=nrpar, y=LL)) +
  geom_point()+
  geom_line()+
  labs(title = "Original CHUll observed")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()

#
##plot for CHull factor
ggplot(MediationModel.Selection.PM.MarkSup2$Overview, aes(x=nrpar_fac, y=LL_fac)) +
  geom_point()+
  geom_line()+
  labs(title = "CHUll factor")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()
#
##plot for BIC_G observed
ggplot(MediationModel.Selection.PM.MarkSup2$Overview, aes(x=Clusters, y=BIC_G))+
  geom_point()+geom_line()+
  labs(title = "BIC_G Observed")+xlab("Number of Clusters")+ylab("BIC_G")+
  theme_minimal()
#
##plot for BIC_G factor
ggplot(MediationModel.Selection.PM.MarkSup2$Overview, aes(x=Clusters, y=BIC_G_fac))+
  geom_point()+geom_line()+
  labs(title = "BIC_G Factor")+xlab("Number of Clusters")+ylab("BIC_G")+
  theme_minimal()


####----------------------------------------------------------------------------------------------------
##MMGSEM for the 2 full metric CCPolSupport options:

##specify structural model
Str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan

CCPolicySupport~CCBelief+SelfTran+Conser+SelfEnhan
'

#
##Option A: full metric CCPolSupport with support3 as marker
##5 clusters
Mediation.5clus.FM.MarkSup3<-MMGSEM(dat=ESS8,
                            S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.FMetric.M1.MarkerSup3),
                            S2 = Str_model,
                            group = "country",
                            nclus=5,
                            seed = 100,
                            userStart = NULL,
                            s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.FMetric.Fit1.MarkerSup3),
                            max_it = 10000L,
                            nstarts = 150L,
                            printing = FALSE,
                            partition = "hard",
                            endogenous_cov = TRUE,
                            endo_group_specific = TRUE,
                            sam_method = "local",
                            meanstr = FALSE,
                            rescaling = F,
                            missing="FIML")
round(Mediation.5clus.FM.MarkSup3$posteriors, digits = 5)
#
##Option B: full metric CCPolSupport with support2 as marker
##5 clusters
Mediation.5clus.FM.MarkSup2<-MMGSEM(dat=ESS8,
                                    S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.FMetric.M1.MarkerSup2),
                                    S2 = Str_model,
                                    group = "country",
                                    nclus=5,
                                    seed = 100,
                                    userStart = NULL,
                                    s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.FMetric.Fit1.MarkerSup2),
                                    max_it = 10000L,
                                    nstarts = 150L,
                                    printing = FALSE,
                                    partition = "hard",
                                    endogenous_cov = TRUE,
                                    endo_group_specific = TRUE,
                                    sam_method = "local",
                                    meanstr = FALSE,
                                    rescaling = F,
                                    missing="FIML")
round(Mediation.5clus.FM.MarkSup2$posteriors, digits = 5)





####----------------------------------------------------------------------------------------------------
##MMGSEM for the partial metric CCPolSupport with support2 as marker:
#run cluster 1-7 one by one

##specify structural model
Str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan

CCPolicySupport~CCBelief+SelfTran+Conser+SelfEnhan
'

##2 clusters:
Mediation.2clus.PM.MarkSup2<-MMGSEM(dat=ESS8,
                                    S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                                    S2 = Str_model,
                                    group = "country",
                                    nclus=2,
                                    seed = 100,
                                    userStart = NULL,
                                    s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                                    max_it = 10000L,
                                    nstarts = 150L,
                                    printing = T,
                                    partition = "hard",
                                    endogenous_cov = TRUE,
                                    endo_group_specific = TRUE,
                                    sam_method = "local",
                                    meanstr = FALSE,
                                    rescaling = F,
                                    missing="FIML")
round(Mediation.2clus.PM.MarkSup2$posteriors, digits = 5)
##converged

##3 clusters:
Mediation.3clus.PM.MarkSup2<-MMGSEM(dat=ESS8,
                                    S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                                    S2 = Str_model,
                                    group = "country",
                                    nclus=3,
                                    seed = 100,
                                    userStart = NULL,
                                    s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                                    max_it = 10000L,
                                    nstarts = 150L,
                                    printing = T,
                                    partition = "hard",
                                    endogenous_cov = TRUE,
                                    endo_group_specific = TRUE,
                                    sam_method = "local",
                                    meanstr = FALSE,
                                    rescaling = F,
                                    missing="FIML")
round(Mediation.3clus.PM.MarkSup2$posteriors, digits = 10)
##converge

##4 clusters:
Mediation.4clus.PM.MarkSup2<-MMGSEM(dat=ESS8,
                                    S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                                    S2 = Str_model,
                                    group = "country",
                                    nclus=4,
                                    seed = 100,
                                    userStart = NULL,
                                    s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                                    max_it = 10000L,
                                    nstarts = 150L,
                                    printing = T,
                                    partition = "hard",
                                    endogenous_cov = TRUE,
                                    endo_group_specific = TRUE,
                                    sam_method = "local",
                                    meanstr = FALSE,
                                    rescaling = F,
                                    missing="FIML")
round(Mediation.4clus.PM.MarkSup2$posteriors, digits = 10)
##converge


##5 clusters:
Mediation.5clus.PM.MarkSup2<-MMGSEM(dat=ESS8,
                                    S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                                    S2 = Str_model,
                                    group = "country",
                                    nclus=5,
                                    seed = 100,
                                    userStart = NULL,
                                    s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                                    max_it = 10000L,
                                    nstarts = 150L,
                                    printing = T,
                                    partition = "hard",
                                    endogenous_cov = TRUE,
                                    endo_group_specific = TRUE,
                                    sam_method = "local",
                                    meanstr = FALSE,
                                    rescaling = F,
                                    missing="FIML")
round(Mediation.5clus.PM.MarkSup2$posteriors, digits = 10)
##converge


##6 clusters:
Mediation.6clus.PM.MarkSup2<-MMGSEM(dat=ESS8,
                                    S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                                    S2 = Str_model,
                                    group = "country",
                                    nclus=6,
                                    seed = 100,
                                    userStart = NULL,
                                    s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                                    max_it = 10000L,
                                    nstarts = 150L,
                                    printing = T,
                                    partition = "hard",
                                    endogenous_cov = TRUE,
                                    endo_group_specific = TRUE,
                                    sam_method = "local",
                                    meanstr = FALSE,
                                    rescaling = F,
                                    missing="FIML")
round(Mediation.6clus.PM.MarkSup2$posteriors, digits = 10)
##converge


####----------------------------------------------------------------------------------------------------
##Clustering membership for the two full metric CCPolSupport options

##Option A: Full Metric CCPolSupport with support3 as marker
#5-cluster solution
clustering.5clus<-t(apply(Mediation.5clus.FM.MarkSup3$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.5clus[,2]<-ifelse(clustering.5clus[,2]==1,2,0)
clustering.5clus[,3]<-ifelse(clustering.5clus[,3]==1,3,0)
clustering.5clus[,4]<-ifelse(clustering.5clus[,4]==1,4,0)
clustering.5clus[,5]<-ifelse(clustering.5clus[,5]==1,5,0)
ClusMembership.5clus<-apply(clustering.5clus,1,function(x) sum(x))
ClusterRes.5clus<-data.frame(group=c(1:23),
                                 ClusMembership=ClusMembership.5clus)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.5clus<-merge(ClusterRes.5clus, countries,
                            by.x = "group", by.y = "group")

##Option B: Full Metric CCPolSupport with support2 as marker
#5-cluster solution
clustering.5clus<-t(apply(Mediation.5clus.FM.MarkSup2$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.5clus[,2]<-ifelse(clustering.5clus[,2]==1,2,0)
clustering.5clus[,3]<-ifelse(clustering.5clus[,3]==1,3,0)
clustering.5clus[,4]<-ifelse(clustering.5clus[,4]==1,4,0)
clustering.5clus[,5]<-ifelse(clustering.5clus[,5]==1,5,0)
ClusMembership.5clus<-apply(clustering.5clus,1,function(x) sum(x))
ClusterRes.5clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.5clus)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.5clus<-merge(ClusterRes.5clus, countries,
                        by.x = "group", by.y = "group")


##Option C: Partial Metric CCPolSupport with support2 as marker
#
##2-cluster solution:
clustering.2clus<-t(apply(Mediation.2clus.PM.MarkSup2$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.2clus[,2]<-ifelse(clustering.2clus[,2]==1,2,0)
ClusMembership.2clus<-apply(clustering.2clus,1,function(x) sum(x))
ClusterRes.2clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.2clus)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.2clus<-merge(ClusterRes.2clus, countries,
                        by.x = "group", by.y = "group")
#
##3-cluster solution:
clustering.3clus<-t(apply(Mediation.3clus.PM.MarkSup2$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.3clus[,2]<-ifelse(clustering.3clus[,2]==1,2,0)
clustering.3clus[,3]<-ifelse(clustering.3clus[,3]==1,3,0)
ClusMembership.3clus<-apply(clustering.3clus,1,function(x) sum(x))
ClusterRes.3clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.3clus)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.3clus<-merge(ClusterRes.3clus, countries,
                        by.x = "group", by.y = "group")


#
##4-cluster solution:
clustering.4clus<-t(apply(Mediation.4clus.PM.MarkSup2$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.4clus[,2]<-ifelse(clustering.4clus[,2]==1,2,0)
clustering.4clus[,3]<-ifelse(clustering.4clus[,3]==1,3,0)
clustering.4clus[,4]<-ifelse(clustering.4clus[,4]==1,4,0)
ClusMembership.4clus<-apply(clustering.4clus,1,function(x) sum(x))
ClusterRes.4clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.4clus)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.4clus<-merge(ClusterRes.4clus, countries,
                        by.x = "group", by.y = "group")

#
##5-cluster solution:
clustering.5clus<-t(apply(Mediation.5clus.PM.MarkSup2$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.5clus[,2]<-ifelse(clustering.5clus[,2]==1,2,0)
clustering.5clus[,3]<-ifelse(clustering.5clus[,3]==1,3,0)
clustering.5clus[,4]<-ifelse(clustering.5clus[,4]==1,4,0)
clustering.5clus[,5]<-ifelse(clustering.5clus[,5]==1,5,0)
ClusMembership.5clus<-apply(clustering.5clus,1,function(x) sum(x))
ClusterRes.5clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.5clus)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.5clus<-merge(ClusterRes.5clus, countries,
                        by.x = "group", by.y = "group")


#
##5-cluster solution:
clustering.6clus<-t(apply(Mediation.6clus.PM.MarkSup2$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.6clus[,2]<-ifelse(clustering.6clus[,2]==1,2,0)
clustering.6clus[,3]<-ifelse(clustering.6clus[,3]==1,3,0)
clustering.6clus[,4]<-ifelse(clustering.6clus[,4]==1,4,0)
clustering.6clus[,5]<-ifelse(clustering.6clus[,5]==1,5,0)
clustering.6clus[,6]<-ifelse(clustering.6clus[,6]==1,6,0)
ClusMembership.6clus<-apply(clustering.6clus,1,function(x) sum(x))
ClusterRes.6clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.6clus)
countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.6clus<-merge(ClusterRes.6clus, countries,
                        by.x = "group", by.y = "group")


####################################################################################
################### Mediation Model: SAM estimation and comparison #################
####################################################################################


###------------------------------------------------------------------------------------
##Option A: CCPolSupport full metric with support3 as marker
##First do the following step that is necessary for all clusters solution
#
##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
lambda_HV_23cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_23cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from CCBelief
EST_CCBelief<-lavInspect(CCBelief.Metric.Fit1.Marker, what = "est")
lambda_CCBelief_23cntry<-lapply(EST_CCBelief, "[[","lambda")
theta_CCBelief_23cntry<-lapply(EST_CCBelief, "[[","theta")
#
##extract the loadings and residual variances from CCPolSupport
EST_CCPolSupport<-lavInspect(CCPolSupport.FMetric.Fit1.MarkerSup3, what = "est")
lambda_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","lambda")
theta_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","theta")
#
##initialize empty list to store the new lambda matrix, new theta matrix and the mapping matrix
lambda_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
theta_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
Mmatrix<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))){
  ##put lambda from two measurement blocks into into the same matrix for each group
  lambda_23cntry[[g]]<-lav_matrix_bdiag(lambda_HV_23cntry[[g]], lambda_CCBelief_23cntry[[g]], lambda_CCPolSupport_23cntry[[g]])
  colnames(lambda_23cntry[[g]])<-c(colnames(lambda_HV_23cntry[[g]]),colnames(lambda_CCBelief_23cntry[[g]]), colnames(lambda_CCPolSupport_23cntry[[g]]))
  rownames(lambda_23cntry[[g]])<-c(rownames(lambda_HV_23cntry[[g]]),rownames(lambda_CCBelief_23cntry[[g]]), rownames(lambda_CCPolSupport_23cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_23cntry[[g]]<-lav_matrix_bdiag(theta_HV_23cntry[[g]],theta_CCBelief_23cntry[[g]],theta_CCPolSupport_23cntry[[g]])
  colnames(theta_23cntry[[g]])<-c(colnames(theta_HV_23cntry[[g]]),colnames(theta_CCBelief_23cntry[[g]]), colnames(theta_CCPolSupport_23cntry[[g]]))
  rownames(theta_23cntry[[g]])<-c(rownames(theta_HV_23cntry[[g]]),rownames(theta_CCBelief_23cntry[[g]]), rownames(theta_CCPolSupport_23cntry[[g]]))
  
  ##compute the mapping matrix for each group
  Mmatrix[[g]]<-solve(t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]]) %*% lambda_23cntry[[g]]) %*% t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]])
}

#
##run an empty sem to just extract the imputed sample covariance matrix
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCBelief=~ImpactBelief+TrendBelief+AttriBelief

CCPolicySupport=~support3+support1+support2
'

fake<-cfa(model = fake_model,
          data = ESS8,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F)

S<-fake@SampleStats@cov

S<-lapply(S, function(x) {
  colnames(x)<-rownames(x)<-colnames(fitted(fake)[[1]]$cov)
  x
})

#
##compute the factor covariance matrix for each group that will be used for the second step:
Var_eta<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_23cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
Mediation_FREEsam_str_model_FM_MarkSup3<-'
CCBelief~c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23)*SelfTran+
          c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23)*Conser+
          c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23)*SelfEnhan

CCPolicySupport~c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23)*CCBelief+SelfTran+Conser+SelfEnhan

STindirect_g1:=a1*d1
STindirect_g2:=a2*d2
STindirect_g3:=a3*d3
STindirect_g4:=a4*d4
STindirect_g5:=a5*d5
STindirect_g6:=a6*d6
STindirect_g7:=a7*d7
STindirect_g8:=a8*d8
STindirect_g9:=a9*d9
STindirect_g10:=a10*d10
STindirect_g11:=a11*d11
STindirect_g12:=a12*d12
STindirect_g13:=a13*d13
STindirect_g14:=a14*d14
STindirect_g15:=a15*d15
STindirect_g16:=a16*d16
STindirect_g17:=a17*d17
STindirect_g18:=a18*d18
STindirect_g19:=a19*d19
STindirect_g20:=a20*d20
STindirect_g21:=a21*d21
STindirect_g22:=a22*d22
STindirect_g23:=a23*d23

ConIndirect_g1:=b1*d1
ConIndirect_g2:=b2*d2
ConIndirect_g3:=b3*d3
ConIndirect_g4:=b4*d4
ConIndirect_g5:=b5*d5
ConIndirect_g6:=b6*d6
ConIndirect_g7:=b7*d7
ConIndirect_g8:=b8*d8
ConIndirect_g9:=b9*d9
ConIndirect_g10:=b10*d10
ConIndirect_g11:=b11*d11
ConIndirect_g12:=b12*d12
ConIndirect_g13:=b13*d13
ConIndirect_g14:=b14*d14
ConIndirect_g15:=b15*d15
ConIndirect_g16:=b16*d16
ConIndirect_g17:=b17*d17
ConIndirect_g18:=b18*d18
ConIndirect_g19:=b19*d19
ConIndirect_g20:=b20*d20
ConIndirect_g21:=b21*d21
ConIndirect_g22:=b22*d22
ConIndirect_g23:=b23*d23

SEindirect_g1:=c1*d1
SEindirect_g2:=c2*d2
SEindirect_g3:=c3*d3
SEindirect_g4:=c4*d4
SEindirect_g5:=c5*d5
SEindirect_g6:=c6*d6
SEindirect_g7:=c7*d7
SEindirect_g8:=c8*d8
SEindirect_g9:=c9*d9
SEindirect_g10:=c10*d10
SEindirect_g11:=c11*d11
SEindirect_g12:=c12*d12
SEindirect_g13:=c13*d13
SEindirect_g14:=c14*d14
SEindirect_g15:=c15*d15
SEindirect_g16:=c16*d16
SEindirect_g17:=c17*d17
SEindirect_g18:=c18*d18
SEindirect_g19:=c19*d19
SEindirect_g20:=c20*d20
SEindirect_g21:=c21*d21
SEindirect_g22:=c22*d22
SEindirect_g23:=c23*d23
'

Mediation.FreeSAM.FM.MarkSup3<-cfa(model = Mediation_FREEsam_str_model_FM_MarkSup3,
                          sample.cov = Var_eta,
                          sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/Mediation_FreeSAM_FM_MarkSup3.txt")
summary(Mediation.FreeSAM.FM.MarkSup3, fit.measures=T, standardized=T)
sink()

##5-cluster: 
##cluster 1: group 1,4,7,15
##cluster 2: group 13,14
##cluster 3: group 2,3,5,6,8,9,10,17,18,19,20,22,23
##cluster 4: group 11,12,21
##cluster 5: group 16

sam_mediation_5clus_FM_MarkSup3<-'
CCBelief~c(a1,a3,a3,a1,a3,a3,a1,a3,a3,a3,a4,a4,a2,a2,a1,a5,a3,a3,a3,a3,a4,a3,a3)*SelfTran+
          c(b1,b3,b3,b1,b3,b3,b1,b3,b3,b3,b4,b4,b2,b2,b1,b5,b3,b3,b3,b3,b4,b3,b3)*Conser+
          c(c1,c3,c3,c1,c3,c3,c1,c3,c3,c3,c4,c4,c2,c2,c1,c5,c3,c3,c3,c3,c4,c3,c3)*SelfEnhan

CCPolicySupport~c(d1,d3,d3,d1,d3,d3,d1,d3,d3,d3,d4,d4,d2,d2,d1,d5,d3,d3,d3,d3,d4,d3,d3)*CCBelief+
                c(e1,e3,e3,e1,e3,e3,e1,e3,e3,e3,e4,e4,e2,e2,e1,e5,e3,e3,e3,e3,e4,e3,e3)*SelfTran+
                c(f1,f3,f3,f1,f3,f3,f1,f3,f3,f3,f4,f4,f2,f2,f1,f5,f3,f3,f3,f3,f4,f3,f3)*Conser+
                c(g1,g3,g3,g1,g3,g3,g1,g3,g3,g3,g4,g4,g2,g2,g1,g5,g3,g3,g3,g3,g4,g3,g3)*SelfEnhan
'

Mediation.SAM.5clus.FM.MarkSup3<-cfa(model = sam_mediation_5clus_FM_MarkSup3,
                            sample.cov = Var_eta,
                            sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/Mediation_SAM_5clus_FM_MarkSup3.txt")
summary(Mediation.SAM.5clus.FM.MarkSup3, fit.measures=T, standardized=T)
sink()

#
#
####facet plotting
#
##first extract all the estimate:
FreeSAMparam<-parameterEstimates(Mediation.FreeSAM.FM.MarkSup3)


##Facet plot for the direct effect of the 3 human values on CCPolicySupport
#
#make a table with direct effect of the 3 human values on CCPolicySupport
HVDirect.param<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs!="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


HVDirect.param<-merge(HVDirect.param, ClusterRes.5clus, 
                         by.x = "group", by.y = "group")

HVDirect.param$country <- fct_reorder(HVDirect.param$country, 
                                         HVDirect.param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, 0)                             # Line positions
)

vline_data2 <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.125,0.15)                             # Line positions
)

ggplot(HVDirect.param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  geom_vline(data = vline_data2, aes(xintercept = xintercept), color="blue", linetype="dashed")+
  labs(title = "SAM with clustering results - Direct Effects of Human Values on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for only the effect of CCBelief on CCPolicySupport
CCBelief_regPar<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs=="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper)

CCBelief_regPar<-merge(CCBelief_regPar, ClusterRes.5clus, 
                         by.x = "group", by.y = "group")

CCBelief_regPar$country <- fct_reorder(CCBelief_regPar$country, 
                                       CCBelief_regPar$ClusMembership)

ggplot(CCBelief_regPar, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  geom_vline(xintercept = 0.5, color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - effect of CCBelief on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for the indirect effect of the 3 human values on CCPolicySupport VIA CCBelief
HVIndirect.par<-FreeSAMparam %>%
  filter(op==":=")

HVIndirect.par$group <- as.numeric(gsub(".*_g(\\d+).*", "\\1", HVIndirect.par$lhs))

HVIndirect.par<-HVIndirect.par %>%
  select(lhs, group, est, ci.lower, ci.upper)

HVIndirect.par$human_values <- gsub("_g.*", "", HVIndirect.par$lhs)

HVIndirect.par<-HVIndirect.par %>%
  mutate(human_values=case_when(
    human_values=="STindirect"~"Self-Transcendence",
    human_values=="ConIndirect"~"Conservation",
    human_values=="SEindirect"~"Self-Enhancement"
  ))

HVIndirect.par<-merge(HVIndirect.par, ClusterRes.5clus, 
                      by.x = "group", by.y = "group")

HVIndirect.par$country <- fct_reorder(HVIndirect.par$country, 
                                      HVIndirect.par$ClusMembership)

vline_data <- data.frame(
  human_values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.12, 0.20)                             # Line positions
)

ggplot(HVIndirect.par, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~human_values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="blue", linetype="dashed")+
  labs(title = "SAM Indirect Effects of Human Values on CC Policy Support via CCBelief",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()



##-------------------------------------------------------------------------------------------------------
####3D plot
#
##first extract all the estimate:
FreeSAMparam<-parameterEstimates(Mediation.FreeSAM.FM.MarkSup3)

##3D plot for the direct effects of HV:
HVDirect_reg_param<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs!="CCBelief") %>%
  select(lhs,rhs,group,est) %>%
  pivot_wider(names_from = rhs, values_from = est)


HVDirect_reg_param<-merge(HVDirect_reg_param, ClusterRes.5clus, 
                          by.x = "group", by.y = "group")

HVDirect_5clus_3D_FM3<-plot_ly(HVDirect_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                               type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM 4 clusters - Direct effect of Human Values on CCPolicy after controlling for CCBelief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))


#htmlwidgets::saveWidget(as_widget(HVDirect_4clus_3D), "HVDirect_4clus_3D.html")


##3D plot for the indirect effect of HV:
HVIndirect.RegPar<-FreeSAMparam %>%
  filter(op==":=")

HVIndirect.RegPar$group <- as.numeric(gsub(".*_g(\\d+).*", "\\1", HVIndirect.RegPar$lhs))

HVIndirect.RegPar$human_values <- gsub("_g.*", "", HVIndirect.RegPar$lhs)

HVIndirect.RegPar<-HVIndirect.RegPar %>%
  select(human_values, group, est) %>%
  pivot_wider(names_from = human_values, values_from = est) %>%
  rename(SelfTran=STindirect,
         Conser=ConIndirect,
         SelfEnhan=SEindirect)

HVIndirect.RegPar<-merge(HVIndirect.RegPar, ClusterRes.5clus, 
                         by.x = "group", by.y = "group")

HVIndirect_5clus_3D_FM3<-plot_ly(HVIndirect.RegPar, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                                 type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM 4 clusters - Indirect effect of Human Values on CCPolicy through CCBelief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

#htmlwidgets::saveWidget(as_widget(HVIndirect_4clus_3D), "HVIndirect_4clus_3D.html")



###------------------------------------------------------------------------------------
##Option B: CCPolSupport full metric with support2 as marker
##First do the following step that is necessary for all clusters solution
#
##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
lambda_HV_23cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_23cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from CCBelief
EST_CCBelief<-lavInspect(CCBelief.Metric.Fit1.Marker, what = "est")
lambda_CCBelief_23cntry<-lapply(EST_CCBelief, "[[","lambda")
theta_CCBelief_23cntry<-lapply(EST_CCBelief, "[[","theta")
#
##extract the loadings and residual variances from CCPolSupport
EST_CCPolSupport<-lavInspect(CCPolSupport.FMetric.Fit1.MarkerSup2, what = "est")
lambda_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","lambda")
theta_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","theta")
#
##initialize empty list to store the new lambda matrix, new theta matrix and the mapping matrix
lambda_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
theta_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
Mmatrix<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))){
  ##put lambda from two measurement blocks into into the same matrix for each group
  lambda_23cntry[[g]]<-lav_matrix_bdiag(lambda_HV_23cntry[[g]], lambda_CCBelief_23cntry[[g]], lambda_CCPolSupport_23cntry[[g]])
  colnames(lambda_23cntry[[g]])<-c(colnames(lambda_HV_23cntry[[g]]),colnames(lambda_CCBelief_23cntry[[g]]), colnames(lambda_CCPolSupport_23cntry[[g]]))
  rownames(lambda_23cntry[[g]])<-c(rownames(lambda_HV_23cntry[[g]]),rownames(lambda_CCBelief_23cntry[[g]]), rownames(lambda_CCPolSupport_23cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_23cntry[[g]]<-lav_matrix_bdiag(theta_HV_23cntry[[g]],theta_CCBelief_23cntry[[g]],theta_CCPolSupport_23cntry[[g]])
  colnames(theta_23cntry[[g]])<-c(colnames(theta_HV_23cntry[[g]]),colnames(theta_CCBelief_23cntry[[g]]), colnames(theta_CCPolSupport_23cntry[[g]]))
  rownames(theta_23cntry[[g]])<-c(rownames(theta_HV_23cntry[[g]]),rownames(theta_CCBelief_23cntry[[g]]), rownames(theta_CCPolSupport_23cntry[[g]]))
  
  ##compute the mapping matrix for each group
  Mmatrix[[g]]<-solve(t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]]) %*% lambda_23cntry[[g]]) %*% t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]])
}

#
##run an empty sem to just extract the imputed sample covariance matrix
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCBelief=~ImpactBelief+TrendBelief+AttriBelief

CCPolicySupport=~support3+support1+support2
'

fake<-cfa(model = fake_model,
          data = ESS8,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F)

S<-fake@SampleStats@cov

S<-lapply(S, function(x) {
  colnames(x)<-rownames(x)<-colnames(fitted(fake)[[1]]$cov)
  x
})

#
##compute the factor covariance matrix for each group that will be used for the second step:
Var_eta<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_23cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
Mediation_FREEsam_str_model_FM_MarkSup2<-'
CCBelief~c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23)*SelfTran+
          c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23)*Conser+
          c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23)*SelfEnhan

CCPolicySupport~c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23)*CCBelief+SelfTran+Conser+SelfEnhan

STindirect_g1:=a1*d1
STindirect_g2:=a2*d2
STindirect_g3:=a3*d3
STindirect_g4:=a4*d4
STindirect_g5:=a5*d5
STindirect_g6:=a6*d6
STindirect_g7:=a7*d7
STindirect_g8:=a8*d8
STindirect_g9:=a9*d9
STindirect_g10:=a10*d10
STindirect_g11:=a11*d11
STindirect_g12:=a12*d12
STindirect_g13:=a13*d13
STindirect_g14:=a14*d14
STindirect_g15:=a15*d15
STindirect_g16:=a16*d16
STindirect_g17:=a17*d17
STindirect_g18:=a18*d18
STindirect_g19:=a19*d19
STindirect_g20:=a20*d20
STindirect_g21:=a21*d21
STindirect_g22:=a22*d22
STindirect_g23:=a23*d23

ConIndirect_g1:=b1*d1
ConIndirect_g2:=b2*d2
ConIndirect_g3:=b3*d3
ConIndirect_g4:=b4*d4
ConIndirect_g5:=b5*d5
ConIndirect_g6:=b6*d6
ConIndirect_g7:=b7*d7
ConIndirect_g8:=b8*d8
ConIndirect_g9:=b9*d9
ConIndirect_g10:=b10*d10
ConIndirect_g11:=b11*d11
ConIndirect_g12:=b12*d12
ConIndirect_g13:=b13*d13
ConIndirect_g14:=b14*d14
ConIndirect_g15:=b15*d15
ConIndirect_g16:=b16*d16
ConIndirect_g17:=b17*d17
ConIndirect_g18:=b18*d18
ConIndirect_g19:=b19*d19
ConIndirect_g20:=b20*d20
ConIndirect_g21:=b21*d21
ConIndirect_g22:=b22*d22
ConIndirect_g23:=b23*d23

SEindirect_g1:=c1*d1
SEindirect_g2:=c2*d2
SEindirect_g3:=c3*d3
SEindirect_g4:=c4*d4
SEindirect_g5:=c5*d5
SEindirect_g6:=c6*d6
SEindirect_g7:=c7*d7
SEindirect_g8:=c8*d8
SEindirect_g9:=c9*d9
SEindirect_g10:=c10*d10
SEindirect_g11:=c11*d11
SEindirect_g12:=c12*d12
SEindirect_g13:=c13*d13
SEindirect_g14:=c14*d14
SEindirect_g15:=c15*d15
SEindirect_g16:=c16*d16
SEindirect_g17:=c17*d17
SEindirect_g18:=c18*d18
SEindirect_g19:=c19*d19
SEindirect_g20:=c20*d20
SEindirect_g21:=c21*d21
SEindirect_g22:=c22*d22
SEindirect_g23:=c23*d23
'

Mediation.FreeSAM.FM.MarkSup2<-cfa(model = Mediation_FREEsam_str_model_FM_MarkSup2,
                                   sample.cov = Var_eta,
                                   sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/Mediation_FreeSAM_FM_MarkSup2.txt")
summary(Mediation.FreeSAM.FM.MarkSup2, fit.measures=T, standardized=T)
sink()

##5-cluster: 
##cluster 1: group 1,4,7,15
##cluster 2: group 13,14
##cluster 3: group 2,3,5,6,8,9,10,17,18,19,20,22,23
##cluster 4: group 11,12,21
##cluster 5: group 16

sam_mediation_5clus_FM_MarkSup2<-'
CCBelief~c(a1,a3,a3,a1,a3,a3,a1,a3,a3,a3,a4,a4,a2,a2,a1,a5,a3,a3,a3,a3,a4,a3,a3)*SelfTran+
          c(b1,b3,b3,b1,b3,b3,b1,b3,b3,b3,b4,b4,b2,b2,b1,b5,b3,b3,b3,b3,b4,b3,b3)*Conser+
          c(c1,c3,c3,c1,c3,c3,c1,c3,c3,c3,c4,c4,c2,c2,c1,c5,c3,c3,c3,c3,c4,c3,c3)*SelfEnhan

CCPolicySupport~c(d1,d3,d3,d1,d3,d3,d1,d3,d3,d3,d4,d4,d2,d2,d1,d5,d3,d3,d3,d3,d4,d3,d3)*CCBelief+
                c(e1,e3,e3,e1,e3,e3,e1,e3,e3,e3,e4,e4,e2,e2,e1,e5,e3,e3,e3,e3,e4,e3,e3)*SelfTran+
                c(f1,f3,f3,f1,f3,f3,f1,f3,f3,f3,f4,f4,f2,f2,f1,f5,f3,f3,f3,f3,f4,f3,f3)*Conser+
                c(g1,g3,g3,g1,g3,g3,g1,g3,g3,g3,g4,g4,g2,g2,g1,g5,g3,g3,g3,g3,g4,g3,g3)*SelfEnhan
'

Mediation.SAM.5clus.FM.MarkSup2<-cfa(model = sam_mediation_5clus_FM_MarkSup2,
                                     sample.cov = Var_eta,
                                     sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/Mediation_SAM_5clus_FM_MarkSup2.txt")
summary(Mediation.SAM.5clus.FM.MarkSup2, fit.measures=T, standardized=T)
sink()

#
#
####facet plotting
#
##first extract all the estimate:
FreeSAMparam<-parameterEstimates(Mediation.FreeSAM.FM.MarkSup2)


##Facet plot for the direct effect of the 3 human values on CCPolicySupport
#
#make a table with direct effect of the 3 human values on CCPolicySupport
HVDirect.param<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs!="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


HVDirect.param<-merge(HVDirect.param, ClusterRes.5clus, 
                      by.x = "group", by.y = "group")

HVDirect.param$country <- fct_reorder(HVDirect.param$country, 
                                      HVDirect.param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, 0)                             # Line positions
)

vline_data2 <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.125,0.125)                             # Line positions
)

ggplot(HVDirect.param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  geom_vline(data = vline_data2, aes(xintercept = xintercept), color="blue", linetype="dashed")+
  labs(title = "SAM with clustering results - Direct Effects of Human Values on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for only the effect of CCBelief on CCPolicySupport
CCBelief_regPar<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs=="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper)

CCBelief_regPar<-merge(CCBelief_regPar, ClusterRes.5clus, 
                       by.x = "group", by.y = "group")

CCBelief_regPar$country <- fct_reorder(CCBelief_regPar$country, 
                                       CCBelief_regPar$ClusMembership)

ggplot(CCBelief_regPar, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  geom_vline(xintercept = 0.44, color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - effect of CCBelief on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for the indirect effect of the 3 human values on CCPolicySupport VIA CCBelief
HVIndirect.par<-FreeSAMparam %>%
  filter(op==":=")

HVIndirect.par$group <- as.numeric(gsub(".*_g(\\d+).*", "\\1", HVIndirect.par$lhs))

HVIndirect.par<-HVIndirect.par %>%
  select(lhs, group, est, ci.lower, ci.upper)

HVIndirect.par$human_values <- gsub("_g.*", "", HVIndirect.par$lhs)

HVIndirect.par<-HVIndirect.par %>%
  mutate(human_values=case_when(
    human_values=="STindirect"~"Self-Transcendence",
    human_values=="ConIndirect"~"Conservation",
    human_values=="SEindirect"~"Self-Enhancement"
  ))

HVIndirect.par<-merge(HVIndirect.par, ClusterRes.5clus, 
                      by.x = "group", by.y = "group")

HVIndirect.par$country <- fct_reorder(HVIndirect.par$country, 
                                      HVIndirect.par$ClusMembership)

vline_data <- data.frame(
  human_values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.11, 0.20)                             # Line positions
)

ggplot(HVIndirect.par, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~human_values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="blue", linetype="dashed")+
  labs(title = "SAM Indirect Effects of Human Values on CC Policy Support via CCBelief",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()



##-------------------------------------------------------------------------------------------------------
####3D plot
#
##first extract all the estimate:
FreeSAMparam<-parameterEstimates(Mediation.FreeSAM.FM.MarkSup2)

##3D plot for the direct effects of HV:
HVDirect_reg_param<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs!="CCBelief") %>%
  select(lhs,rhs,group,est) %>%
  pivot_wider(names_from = rhs, values_from = est)


HVDirect_reg_param<-merge(HVDirect_reg_param, ClusterRes.5clus, 
                         by.x = "group", by.y = "group")

HVDirect_5clus_3D_FM2<-plot_ly(HVDirect_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                               type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM 4 clusters - Direct effect of Human Values on CCPolicy after controlling for CCBelief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))


#htmlwidgets::saveWidget(as_widget(HVDirect_4clus_3D), "HVDirect_4clus_3D.html")


##3D plot for the indirect effect of HV:
HVIndirect.RegPar<-FreeSAMparam %>%
  filter(op==":=")

HVIndirect.RegPar$group <- as.numeric(gsub(".*_g(\\d+).*", "\\1", HVIndirect.RegPar$lhs))

HVIndirect.RegPar$human_values <- gsub("_g.*", "", HVIndirect.RegPar$lhs)

HVIndirect.RegPar<-HVIndirect.RegPar %>%
  select(human_values, group, est) %>%
  pivot_wider(names_from = human_values, values_from = est) %>%
  rename(SelfTran=STindirect,
         Conser=ConIndirect,
         SelfEnhan=SEindirect)

HVIndirect.RegPar<-merge(HVIndirect.RegPar, ClusterRes.5clus, 
                          by.x = "group", by.y = "group")

HVIndirect_5clus_3D_FM2<-plot_ly(HVIndirect.RegPar, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                           type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM 4 clusters - Indirect effect of Human Values on CCPolicy through CCBelief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

#htmlwidgets::saveWidget(as_widget(HVIndirect_4clus_3D), "HVIndirect_4clus_3D.html")



###------------------------------------------------------------------------------------
##Option C: CCPolSupport partial metric with support2 as marker
##First do the following step that is necessary for all clusters solution
#
##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
lambda_HV_23cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_23cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from CCBelief
EST_CCBelief<-lavInspect(CCBelief.Metric.Fit1.Marker, what = "est")
lambda_CCBelief_23cntry<-lapply(EST_CCBelief, "[[","lambda")
theta_CCBelief_23cntry<-lapply(EST_CCBelief, "[[","theta")
#
##extract the loadings and residual variances from CCPolSupport
EST_CCPolSupport<-lavInspect(CCPolSupport.PMetric.Fit1.MarkerSup2, what = "est")
lambda_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","lambda")
theta_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","theta")
#
##initialize empty list to store the new lambda matrix, new theta matrix and the mapping matrix
lambda_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
theta_23cntry<-vector(mode = "list", length=length(unique(ESS8$country)))
Mmatrix<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))){
  ##put lambda from two measurement blocks into into the same matrix for each group
  lambda_23cntry[[g]]<-lav_matrix_bdiag(lambda_HV_23cntry[[g]], lambda_CCBelief_23cntry[[g]], lambda_CCPolSupport_23cntry[[g]])
  colnames(lambda_23cntry[[g]])<-c(colnames(lambda_HV_23cntry[[g]]),colnames(lambda_CCBelief_23cntry[[g]]), colnames(lambda_CCPolSupport_23cntry[[g]]))
  rownames(lambda_23cntry[[g]])<-c(rownames(lambda_HV_23cntry[[g]]),rownames(lambda_CCBelief_23cntry[[g]]), rownames(lambda_CCPolSupport_23cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_23cntry[[g]]<-lav_matrix_bdiag(theta_HV_23cntry[[g]],theta_CCBelief_23cntry[[g]],theta_CCPolSupport_23cntry[[g]])
  colnames(theta_23cntry[[g]])<-c(colnames(theta_HV_23cntry[[g]]),colnames(theta_CCBelief_23cntry[[g]]), colnames(theta_CCPolSupport_23cntry[[g]]))
  rownames(theta_23cntry[[g]])<-c(rownames(theta_HV_23cntry[[g]]),rownames(theta_CCBelief_23cntry[[g]]), rownames(theta_CCPolSupport_23cntry[[g]]))
  
  ##compute the mapping matrix for each group
  Mmatrix[[g]]<-solve(t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]]) %*% lambda_23cntry[[g]]) %*% t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]])
}

#
##run an empty sem to just extract the imputed sample covariance matrix
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCBelief=~ImpactBelief+TrendBelief+AttriBelief

CCPolicySupport=~support3+support1+support2
'

fake<-cfa(model = fake_model,
          data = ESS8,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F)

S<-fake@SampleStats@cov

S<-lapply(S, function(x) {
  colnames(x)<-rownames(x)<-colnames(fitted(fake)[[1]]$cov)
  x
})

#
##compute the factor covariance matrix for each group that will be used for the second step:
Var_eta<-vector(mode = "list", length = length(unique(ESS8$country)))

for (g in 1:length(unique(ESS8$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_23cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
Mediation_FREEsam_str_model_PM_MarkSup2<-'
CCBelief~c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22,a23)*SelfTran+
          c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23)*Conser+
          c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23)*SelfEnhan

CCPolicySupport~c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23)*CCBelief+SelfTran+Conser+SelfEnhan

STindirect_g1:=a1*d1
STindirect_g2:=a2*d2
STindirect_g3:=a3*d3
STindirect_g4:=a4*d4
STindirect_g5:=a5*d5
STindirect_g6:=a6*d6
STindirect_g7:=a7*d7
STindirect_g8:=a8*d8
STindirect_g9:=a9*d9
STindirect_g10:=a10*d10
STindirect_g11:=a11*d11
STindirect_g12:=a12*d12
STindirect_g13:=a13*d13
STindirect_g14:=a14*d14
STindirect_g15:=a15*d15
STindirect_g16:=a16*d16
STindirect_g17:=a17*d17
STindirect_g18:=a18*d18
STindirect_g19:=a19*d19
STindirect_g20:=a20*d20
STindirect_g21:=a21*d21
STindirect_g22:=a22*d22
STindirect_g23:=a23*d23

ConIndirect_g1:=b1*d1
ConIndirect_g2:=b2*d2
ConIndirect_g3:=b3*d3
ConIndirect_g4:=b4*d4
ConIndirect_g5:=b5*d5
ConIndirect_g6:=b6*d6
ConIndirect_g7:=b7*d7
ConIndirect_g8:=b8*d8
ConIndirect_g9:=b9*d9
ConIndirect_g10:=b10*d10
ConIndirect_g11:=b11*d11
ConIndirect_g12:=b12*d12
ConIndirect_g13:=b13*d13
ConIndirect_g14:=b14*d14
ConIndirect_g15:=b15*d15
ConIndirect_g16:=b16*d16
ConIndirect_g17:=b17*d17
ConIndirect_g18:=b18*d18
ConIndirect_g19:=b19*d19
ConIndirect_g20:=b20*d20
ConIndirect_g21:=b21*d21
ConIndirect_g22:=b22*d22
ConIndirect_g23:=b23*d23

SEindirect_g1:=c1*d1
SEindirect_g2:=c2*d2
SEindirect_g3:=c3*d3
SEindirect_g4:=c4*d4
SEindirect_g5:=c5*d5
SEindirect_g6:=c6*d6
SEindirect_g7:=c7*d7
SEindirect_g8:=c8*d8
SEindirect_g9:=c9*d9
SEindirect_g10:=c10*d10
SEindirect_g11:=c11*d11
SEindirect_g12:=c12*d12
SEindirect_g13:=c13*d13
SEindirect_g14:=c14*d14
SEindirect_g15:=c15*d15
SEindirect_g16:=c16*d16
SEindirect_g17:=c17*d17
SEindirect_g18:=c18*d18
SEindirect_g19:=c19*d19
SEindirect_g20:=c20*d20
SEindirect_g21:=c21*d21
SEindirect_g22:=c22*d22
SEindirect_g23:=c23*d23
'

Mediation.FreeSAM.PM.MarkSup2<-cfa(model = Mediation_FREEsam_str_model_PM_MarkSup2,
                                   sample.cov = Var_eta,
                                   sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/Mediation_FreeSAM_PM_MarkSup2.txt")
summary(Mediation.FreeSAM.PM.MarkSup2, fit.measures=T, standardized=T)
sink()



####------------------------------------------------------------------------------------------
####------------------------------------------------------------------------------------------
####------------------------------------------------------------------------------------------
##cluster by cluster
####------------------------------------------------------------------------------------------

##2-cluster: 
##cluster 1: group 2,3,5,7,8,9,10,13,14,15,16,17,18,20,22
##cluster 2: group 1,4,6,11,12,19,21,23

sam_mediation_2clus_PM_MarkSup2<-'
CCBelief~c(a2,a1,a1,a2,a1,a2,a1,a1,a1,a1,a2,a2,a1,a1,a1,a1,a1,a1,a2,a1,a2,a1,a2)*SelfTran+
          c(b2,b1,b1,b2,b1,b2,b1,b1,b1,b1,b2,b2,b1,b1,b1,b1,b1,b1,b2,b1,b2,b1,b2)*Conser+
          c(c2,c1,c1,c2,c1,c2,c1,c1,c1,c1,c2,c2,c1,c1,c1,c1,c1,c1,c2,c1,c2,c1,c2)*SelfEnhan

CCPolicySupport~c(d2,d1,d1,d2,d1,d2,d1,d1,d1,d1,d2,d2,d1,d1,d1,d1,d1,d1,d2,d1,d2,d1,d2)*CCBelief+
                c(e2,e1,e1,e2,e1,e2,e1,e1,e1,e1,e2,e2,e1,e1,e1,e1,e1,e1,e2,e1,e2,e1,e2)*SelfTran+
                c(f2,f1,f1,f2,f1,f2,f1,f1,f1,f1,f2,f2,f1,f1,f1,f1,f1,f1,f2,f1,f2,f1,f2)*Conser+
                c(g2,g1,g1,g2,g1,g2,g1,g1,g1,g1,g2,g2,g1,g1,g1,g1,g1,g1,g2,g1,g2,g1,g2)*SelfEnhan
'

Mediation.SAM.2clus.PM.MarkSup2<-cfa(model = sam_mediation_2clus_PM_MarkSup2,
                                     sample.cov = Var_eta,
                                     sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/Mediation_SAM_2clus_PM_MarkSup2.txt")
summary(Mediation.SAM.2clus.PM.MarkSup2, fit.measures=T, standardized=T)
sink()

#
#
####facet plotting
#
##first extract all the estimate:
FreeSAMparam<-parameterEstimates(Mediation.FreeSAM.PM.MarkSup2)


##Facet plot for the direct effect of the 3 human values on CCPolicySupport
#
#make a table with direct effect of the 3 human values on CCPolicySupport
HVDirect.param<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs!="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


HVDirect.param<-merge(HVDirect.param, ClusterRes.2clus, 
                      by.x = "group", by.y = "group")

HVDirect.param$country <- fct_reorder(HVDirect.param$country, 
                                      HVDirect.param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, 0)                             # Line positions
)

vline_data2 <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.125,0.125)                             # Line positions
)

ggplot(HVDirect.param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  #geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  #geom_vline(data = vline_data2, aes(xintercept = xintercept), color="blue", linetype="dashed")+
  labs(title = "SAM with clustering results - Direct Effects of Human Values on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for only the effect of CCBelief on CCPolicySupport
CCBelief_regPar<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs=="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper)

CCBelief_regPar<-merge(CCBelief_regPar, ClusterRes.2clus, 
                       by.x = "group", by.y = "group")

CCBelief_regPar$country <- fct_reorder(CCBelief_regPar$country, 
                                       CCBelief_regPar$ClusMembership)

ggplot(CCBelief_regPar, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  geom_vline(xintercept = 0.375, color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - effect of CCBelief on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for the indirect effect of the 3 human values on CCPolicySupport VIA CCBelief
HVIndirect.par<-FreeSAMparam %>%
  filter(op==":=")

HVIndirect.par$group <- as.numeric(gsub(".*_g(\\d+).*", "\\1", HVIndirect.par$lhs))

HVIndirect.par<-HVIndirect.par %>%
  select(lhs, group, est, ci.lower, ci.upper)

HVIndirect.par$human_values <- gsub("_g.*", "", HVIndirect.par$lhs)

HVIndirect.par<-HVIndirect.par %>%
  mutate(human_values=case_when(
    human_values=="STindirect"~"Self-Transcendence",
    human_values=="ConIndirect"~"Conservation",
    human_values=="SEindirect"~"Self-Enhancement"
  ))

HVIndirect.par<-merge(HVIndirect.par, ClusterRes.2clus, 
                      by.x = "group", by.y = "group")

HVIndirect.par$country <- fct_reorder(HVIndirect.par$country, 
                                      HVIndirect.par$ClusMembership)

vline_data <- data.frame(
  human_values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.13, 0.20)                             # Line positions
)

ggplot(HVIndirect.par, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~human_values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="blue", linetype="dashed")+
  labs(title = "SAM Indirect Effects of Human Values on CC Policy Support via CCBelief",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()



####------------------------------------------------------------------------------------------

##3-cluster: 
##cluster 1: group 1,4,6,11,12,19,21,23
##cluster 2: group 2,3,5,7,8,9,10,15,16,17,18,20,22
##cluster 3: group 13, 14


sam_mediation_3clus_PM_MarkSup2<-'
CCBelief~c(a1,a2,a2,a1,a2,a1,a2,a2,a2,a2,a1,a1,a3,a3,a2,a2,a2,a2,a1,a2,a1,a2,a1)*SelfTran+
          c(b1,b2,b2,b1,b2,b1,b2,b2,b2,b2,b1,b1,b3,b3,b2,b2,b2,b2,b1,b2,b1,b2,b1)*Conser+
          c(c1,c2,c2,c1,c2,c1,c2,c2,c2,c2,c1,c1,c3,c3,c2,c2,c2,c2,c1,c2,c1,c2,c1)*SelfEnhan

CCPolicySupport~c(d1,d2,d2,d1,d2,d1,d2,d2,d2,d2,d1,d1,d3,d3,d2,d2,d2,d2,d1,d2,d1,d2,d1)*CCBelief+
                c(e1,e2,e2,e1,e2,e1,e2,e2,e2,e2,e1,e1,e3,e3,e2,e2,e2,e2,e1,e2,e1,e2,e1)*SelfTran+
                c(f1,f2,f2,f1,f2,f1,f2,f2,f2,f2,f1,f1,f3,f3,f2,f2,f2,f2,f1,f2,f1,f2,f1)*Conser+
                c(g1,g2,g2,g1,g2,g1,g2,g2,g2,g2,g1,g1,g3,g3,g2,g2,g2,g2,g1,g2,g1,g2,g1)*SelfEnhan
'

Mediation.SAM.3clus.PM.MarkSup2<-cfa(model = sam_mediation_3clus_PM_MarkSup2,
                                     sample.cov = Var_eta,
                                     sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/Mediation_SAM_3clus_PM_MarkSup2.txt")
summary(Mediation.SAM.3clus.PM.MarkSup2, fit.measures=T, standardized=T)
sink()

#
#
####facet plotting
#
##first extract all the estimate:
FreeSAMparam<-parameterEstimates(Mediation.FreeSAM.PM.MarkSup2)


##Facet plot for the direct effect of the 3 human values on CCPolicySupport
#
#make a table with direct effect of the 3 human values on CCPolicySupport
HVDirect.param<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs!="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


HVDirect.param<-merge(HVDirect.param, ClusterRes.3clus, 
                      by.x = "group", by.y = "group")

HVDirect.param$country <- fct_reorder(HVDirect.param$country, 
                                      HVDirect.param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, 0)                             # Line positions
)

vline_data2 <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.125,0.125)                             # Line positions
)

ggplot(HVDirect.param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  #geom_vline(data = vline_data2, aes(xintercept = xintercept), color="blue", linetype="dashed")+
  labs(title = "SAM with clustering results - Direct Effects of Human Values on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for only the effect of CCBelief on CCPolicySupport
CCBelief_regPar<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs=="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper)

CCBelief_regPar<-merge(CCBelief_regPar, ClusterRes.3clus, 
                       by.x = "group", by.y = "group")

CCBelief_regPar$country <- fct_reorder(CCBelief_regPar$country, 
                                       CCBelief_regPar$ClusMembership)

ggplot(CCBelief_regPar, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  geom_vline(xintercept = 0.375, color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - effect of CCBelief on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for the indirect effect of the 3 human values on CCPolicySupport VIA CCBelief
HVIndirect.par<-FreeSAMparam %>%
  filter(op==":=")

HVIndirect.par$group <- as.numeric(gsub(".*_g(\\d+).*", "\\1", HVIndirect.par$lhs))

HVIndirect.par<-HVIndirect.par %>%
  select(lhs, group, est, ci.lower, ci.upper)

HVIndirect.par$human_values <- gsub("_g.*", "", HVIndirect.par$lhs)

HVIndirect.par<-HVIndirect.par %>%
  mutate(human_values=case_when(
    human_values=="STindirect"~"Self-Transcendence",
    human_values=="ConIndirect"~"Conservation",
    human_values=="SEindirect"~"Self-Enhancement"
  ))

HVIndirect.par<-merge(HVIndirect.par, ClusterRes.3clus, 
                      by.x = "group", by.y = "group")

HVIndirect.par$country <- fct_reorder(HVIndirect.par$country, 
                                      HVIndirect.par$ClusMembership)

vline_data <- data.frame(
  human_values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.13, 0.20)                             # Line positions
)

ggplot(HVIndirect.par, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~human_values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="blue", linetype="dashed")+
  labs(title = "SAM Indirect Effects of Human Values on CC Policy Support via CCBelief",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()




####------------------------------------------------------------------------------------------

##4-cluster: 
##cluster 1: group 2,3,5,7,8,9,10,15,17,18,20,22
##cluster 2: group 16
##cluster 3: group 13, 14
##cluster 4: group 1,4,6,11,12,19,21,23



sam_mediation_4clus_PM_MarkSup2<-'
CCBelief~c(a4,a1,a1,a4,a1,a4,a1,a1,a1,a1,a4,a4,a3,a3,a1,a2,a1,a1,a4,a1,a4,a1,a4)*SelfTran+
          c(b4,b1,b1,b4,b1,b4,b1,b1,b1,b1,b4,b4,b3,b3,b1,b2,b1,b1,b4,b1,b4,b1,b4)*Conser+
          c(c4,c1,c1,c4,c1,c4,c1,c1,c1,c1,c4,c4,c3,c3,c1,c2,c1,c1,c4,c1,c4,c1,c4)*SelfEnhan

CCPolicySupport~c(d4,d1,d1,d4,d1,d4,d1,d1,d1,d1,d4,d4,d3,d3,d1,d2,d1,d1,d4,d1,d4,d1,d4)*CCBelief+
                c(e4,e1,e1,e4,e1,e4,e1,e1,e1,e1,e4,e4,e3,e3,e1,e2,e1,e1,e4,e1,e4,e1,e4)*SelfTran+
                c(f4,f1,f1,f4,f1,f4,f1,f1,f1,f1,f4,f4,f3,f3,f1,f2,f1,f1,f4,f1,f4,f1,f4)*Conser+
                c(g4,g1,g1,g4,g1,g4,g1,g1,g1,g1,g4,g4,g3,g3,g1,g2,g1,g1,g4,g1,g4,g1,g4)*SelfEnhan
'

Mediation.SAM.4clus.PM.MarkSup2<-cfa(model = sam_mediation_4clus_PM_MarkSup2,
                                     sample.cov = Var_eta,
                                     sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/Mediation_SAM_4clus_PM_MarkSup2.txt")
summary(Mediation.SAM.4clus.PM.MarkSup2, fit.measures=T, standardized=T)
sink()

#
#
####facet plotting
#
##first extract all the estimate:
FreeSAMparam<-parameterEstimates(Mediation.FreeSAM.PM.MarkSup2)


##Facet plot for the direct effect of the 3 human values on CCPolicySupport
#
#make a table with direct effect of the 3 human values on CCPolicySupport
HVDirect.param<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs!="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


HVDirect.param<-merge(HVDirect.param, ClusterRes.4clus, 
                      by.x = "group", by.y = "group")

HVDirect.param$country <- fct_reorder(HVDirect.param$country, 
                                      HVDirect.param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, 0)                             # Line positions
)

vline_data2 <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.125,0.125)                             # Line positions
)

ggplot(HVDirect.param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  #geom_vline(data = vline_data2, aes(xintercept = xintercept), color="blue", linetype="dashed")+
  labs(title = "SAM with clustering results - Direct Effects of Human Values on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for only the effect of CCBelief on CCPolicySupport
CCBelief_regPar<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs=="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper)

CCBelief_regPar<-merge(CCBelief_regPar, ClusterRes.4clus, 
                       by.x = "group", by.y = "group")

CCBelief_regPar$country <- fct_reorder(CCBelief_regPar$country, 
                                       CCBelief_regPar$ClusMembership)

ggplot(CCBelief_regPar, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  geom_vline(xintercept = 0.375, color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - effect of CCBelief on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for the indirect effect of the 3 human values on CCPolicySupport VIA CCBelief
HVIndirect.par<-FreeSAMparam %>%
  filter(op==":=")

HVIndirect.par$group <- as.numeric(gsub(".*_g(\\d+).*", "\\1", HVIndirect.par$lhs))

HVIndirect.par<-HVIndirect.par %>%
  select(lhs, group, est, ci.lower, ci.upper)

HVIndirect.par$human_values <- gsub("_g.*", "", HVIndirect.par$lhs)

HVIndirect.par<-HVIndirect.par %>%
  mutate(human_values=case_when(
    human_values=="STindirect"~"Self-Transcendence",
    human_values=="ConIndirect"~"Conservation",
    human_values=="SEindirect"~"Self-Enhancement"
  ))

HVIndirect.par<-merge(HVIndirect.par, ClusterRes.4clus, 
                      by.x = "group", by.y = "group")

HVIndirect.par$country <- fct_reorder(HVIndirect.par$country, 
                                      HVIndirect.par$ClusMembership)

vline_data <- data.frame(
  human_values = c("Self-Enhancement"), # Facet names
  xintercept = c(-0.070)                             # Line positions
)

ggplot(HVIndirect.par, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~human_values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="blue", linetype="dashed")+
  labs(title = "SAM Indirect Effects of Human Values on CC Policy Support via CCBelief",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()



####------------------------------------------------------------------------------------------

##5-cluster: 
##cluster 1: group 1,4,6,#7, 12,#15, 19,21,23
##cluster 2: group 16, 20
##cluster 3: group 11
##cluster 4: group 13, 14
##cluster 5: group 2,3,5,8,9,10,17,18,22



sam_mediation_5clus_PM_MarkSup2<-'
CCBelief~c(a1,a5,a5,a1,a5,a1,a1,a5,a5,a5,a3,a1,a4,a4,a1,a2,a5,a5,a1,a2,a1,a5,a1)*SelfTran+
          c(b1,b5,b5,b1,b5,b1,b1,b5,b5,b5,b3,b1,b4,b4,b1,b2,b5,b5,b1,b2,b1,b5,b1)*Conser+
          c(c1,c5,c5,c1,c5,c1,c1,c5,c5,c5,c3,c1,c4,c4,c1,c2,c5,c5,c1,c2,c1,c5,c1)*SelfEnhan

CCPolicySupport~c(d1,d5,d5,d1,d5,d1,d1,d5,d5,d5,d3,d1,d4,d4,d1,d2,d5,d5,d1,d2,d1,d5,d1)*CCBelief+
                c(e1,e5,e5,e1,e5,e1,e1,e5,e5,e5,e3,e1,e4,e4,e1,e2,e5,e5,e1,e2,e1,e5,e1)*SelfTran+
                c(f1,f5,f5,f1,f5,f1,f1,f5,f5,f5,f3,f1,f4,f4,f1,f2,f5,f5,f1,f2,f1,f5,f1)*Conser+
                c(g1,g5,g5,g1,g5,g1,g1,g5,g5,g5,g3,g1,g4,g4,g1,g2,g5,g5,g1,g2,g1,g5,g1)*SelfEnhan
'

Mediation.SAM.5clus.PM.MarkSup2<-cfa(model = sam_mediation_5clus_PM_MarkSup2,
                                     sample.cov = Var_eta,
                                     sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/Mediation_SAM_5clus_PM_MarkSup2.txt")
summary(Mediation.SAM.5clus.PM.MarkSup2, fit.measures=T, standardized=T)
sink()

#
#
####facet plotting
#
##first extract all the estimate:
FreeSAMparam<-parameterEstimates(Mediation.FreeSAM.PM.MarkSup2)


##Facet plot for the direct effect of the 3 human values on CCPolicySupport
#
#make a table with direct effect of the 3 human values on CCPolicySupport
HVDirect.param<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs!="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


HVDirect.param<-merge(HVDirect.param, ClusterRes.5clus, 
                      by.x = "group", by.y = "group")

HVDirect.param$country <- fct_reorder(HVDirect.param$country, 
                                      HVDirect.param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence","Self-Enhancement"), # Facet names
  xintercept = c(0, 0, 0)                             # Line positions
)

vline_data2 <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.125,0.125)                             # Line positions
)

ggplot(HVDirect.param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  #geom_vline(data = vline_data2, aes(xintercept = xintercept), color="blue", linetype="dashed")+
  labs(title = "SAM with clustering results - Direct Effects of Human Values on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for only the effect of CCBelief on CCPolicySupport
CCBelief_regPar<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs=="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper)

CCBelief_regPar<-merge(CCBelief_regPar, ClusterRes.5clus, 
                       by.x = "group", by.y = "group")

CCBelief_regPar$country <- fct_reorder(CCBelief_regPar$country, 
                                       CCBelief_regPar$ClusMembership)

ggplot(CCBelief_regPar, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  geom_vline(xintercept = c(0.125,0.47), color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - effect of CCBelief on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for the indirect effect of the 3 human values on CCPolicySupport VIA CCBelief
HVIndirect.par<-FreeSAMparam %>%
  filter(op==":=")

HVIndirect.par$group <- as.numeric(gsub(".*_g(\\d+).*", "\\1", HVIndirect.par$lhs))

HVIndirect.par<-HVIndirect.par %>%
  select(lhs, group, est, ci.lower, ci.upper)

HVIndirect.par$human_values <- gsub("_g.*", "", HVIndirect.par$lhs)

HVIndirect.par<-HVIndirect.par %>%
  mutate(human_values=case_when(
    human_values=="STindirect"~"Self-Transcendence",
    human_values=="ConIndirect"~"Conservation",
    human_values=="SEindirect"~"Self-Enhancement"
  ))

HVIndirect.par<-merge(HVIndirect.par, ClusterRes.5clus, 
                      by.x = "group", by.y = "group")

HVIndirect.par$country <- fct_reorder(HVIndirect.par$country, 
                                      HVIndirect.par$ClusMembership)

vline_data <- data.frame(
  human_values = c("Self-Enhancement","Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0,0,0)                             # Line positions
)

ggplot(HVIndirect.par, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~human_values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "SAM Indirect Effects of Human Values on CC Policy Support via CCBelief",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()



####------------------------------------------------------------------------------------------

##6-cluster: 
##cluster 1: group 11
##cluster 2: group 16
##cluster 3: group 1,4,#7, #15
##cluster 4: group 13
##cluster 5: group 2,3,5,8,9,10, #14, 17,18,#20,22 
##cluster 6: group 6,12,19,21,23



##5-cluster: 
##cluster 1: group 1,4,6,#7, 12,#15, 19,21,23
##cluster 2: group 16, 20
##cluster 3: group 11
##cluster 4: group 13, 14
##cluster 5: group 2,3,5,8,9,10,17,18,22




sam_mediation_6clus_PM_MarkSup2<-'
CCBelief~c(a3,a5,a5,a3,a5,a6,a3,a5,a5,a5,a1,a6,a4,a5,a3,a2,a5,a5,a6,a5,a6,a5,a6)*SelfTran+
          c(b3,b5,b5,b3,b5,b6,b3,b5,b5,b5,b1,b6,b4,b5,b3,b2,b5,b5,b6,b5,b6,b5,b6)*Conser+
          c(c3,c5,c5,c3,c5,c6,c3,c5,c5,c5,c1,c6,c4,c5,c3,c2,c5,c5,c6,c5,c6,c5,c6)*SelfEnhan

CCPolicySupport~c(d3,d5,d5,d3,d5,d6,d3,d5,d5,d5,d1,d6,d4,d5,d3,d2,d5,d5,d6,d5,d6,d5,d6)*CCBelief+
                c(e3,e5,e5,e3,e5,e6,e3,e5,e5,e5,e1,e6,e4,e5,e3,e2,e5,e5,e6,e5,e6,e5,e6)*SelfTran+
                c(f3,f5,f5,f3,f5,f6,f3,f5,f5,f5,f1,f6,f4,f5,f3,f2,f5,f5,f6,f5,f6,f5,f6)*Conser+
                c(g3,g5,g5,g3,g5,g6,g3,g5,g5,g5,g1,g6,g4,g5,g3,g2,g5,g5,g6,g5,g6,g5,g6)*SelfEnhan
'

Mediation.SAM.6clus.PM.MarkSup2<-cfa(model = sam_mediation_6clus_PM_MarkSup2,
                                     sample.cov = Var_eta,
                                     sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/Mediation_SAM_6clus_PM_MarkSup2.txt")
summary(Mediation.SAM.6clus.PM.MarkSup2, fit.measures=T, standardized=T)
sink()

#
#
####facet plotting
#
##first extract all the estimate:
FreeSAMparam<-parameterEstimates(Mediation.FreeSAM.PM.MarkSup2)


##Facet plot for the direct effect of the 3 human values on CCPolicySupport
#
#make a table with direct effect of the 3 human values on CCPolicySupport
HVDirect.param<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs!="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


HVDirect.param<-merge(HVDirect.param, ClusterRes.6clus, 
                      by.x = "group", by.y = "group")

HVDirect.param$country <- fct_reorder(HVDirect.param$country, 
                                      HVDirect.param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence","Self-Enhancement"), # Facet names
  xintercept = c(0, 0, 0)                             # Line positions
)

vline_data2 <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.125,0.125)                             # Line positions
)

ggplot(HVDirect.param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  #geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  #geom_vline(data = vline_data2, aes(xintercept = xintercept), color="blue", linetype="dashed")+
  labs(title = "SAM with clustering results - Direct Effects of Human Values on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for only the effect of CCBelief on CCPolicySupport
CCBelief_regPar<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs=="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper)

CCBelief_regPar<-merge(CCBelief_regPar, ClusterRes.6clus, 
                       by.x = "group", by.y = "group")

CCBelief_regPar$country <- fct_reorder(CCBelief_regPar$country, 
                                       CCBelief_regPar$ClusMembership)

ggplot(CCBelief_regPar, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  geom_vline(xintercept = c(0.20,0.465,0.86), color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - effect of CCBelief on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for the indirect effect of the 3 human values on CCPolicySupport VIA CCBelief
HVIndirect.par<-FreeSAMparam %>%
  filter(op==":=")

HVIndirect.par$group <- as.numeric(gsub(".*_g(\\d+).*", "\\1", HVIndirect.par$lhs))

HVIndirect.par<-HVIndirect.par %>%
  select(lhs, group, est, ci.lower, ci.upper)

HVIndirect.par$human_values <- gsub("_g.*", "", HVIndirect.par$lhs)

HVIndirect.par<-HVIndirect.par %>%
  mutate(human_values=case_when(
    human_values=="STindirect"~"Self-Transcendence",
    human_values=="ConIndirect"~"Conservation",
    human_values=="SEindirect"~"Self-Enhancement"
  ))

HVIndirect.par<-merge(HVIndirect.par, ClusterRes.6clus, 
                      by.x = "group", by.y = "group")

HVIndirect.par$country <- fct_reorder(HVIndirect.par$country, 
                                      HVIndirect.par$ClusMembership)

vline_data <- data.frame(
  human_values = c("Self-Enhancement","Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0,-0.1,0.17)                             # Line positions
)

ggplot(HVIndirect.par, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~human_values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="violet", linetype="dashed")+
  labs(title = "SAM Indirect Effects of Human Values on CC Policy Support via CCBelief",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()



##############################################################################################################
################ TO UPDATE Mediation: Simultaneous MGSEM estimation ##########################
##############################################################################################################


##5-cluster solution:
Mediation_RegSEM_5clus_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCBelief=~ImpactBelief+TrendBelief+AttriBelief

CCPolicySupport=~support2+support1+support3

CCBelief~c(a1,a3,a3,a1,a3,a3,a1,a3,a3,a3,a4,a4,a2,a2,a1,a5,a3,a3,a3,a3,a4,a3,a3)*SelfTran+
          c(b1,b3,b3,b1,b3,b3,b1,b3,b3,b3,b4,b4,b2,b2,b1,b5,b3,b3,b3,b3,b4,b3,b3)*Conser+
          c(c1,c3,c3,c1,c3,c3,c1,c3,c3,c3,c4,c4,c2,c2,c1,c5,c3,c3,c3,c3,c4,c3,c3)*SelfEnhan

CCPolicySupport~c(d1,d3,d3,d1,d3,d3,d1,d3,d3,d3,d4,d4,d2,d2,d1,d5,d3,d3,d3,d3,d4,d3,d3)*CCBelief+
                c(e1,e3,e3,e1,e3,e3,e1,e3,e3,e3,e4,e4,e2,e2,e1,e5,e3,e3,e3,e3,e4,e3,e3)*SelfTran+
                c(f1,f3,f3,f1,f3,f3,f1,f3,f3,f3,f4,f4,f2,f2,f1,f5,f3,f3,f3,f3,f4,f3,f3)*Conser+
                c(g1,g3,g3,g1,g3,g3,g1,g3,g3,g3,g4,g4,g2,g2,g1,g5,g3,g3,g3,g3,g4,g3,g3)*SelfEnhan
'

RegSEM.Mediation.5Clus<-cfa(model = Mediation_RegSEM_5clus_model,
                                             data = ESS8,
                                             group = "country",
                                             estimator="MLR",
                                             missing="FIML",
                                             group.equal="loadings",
                                             group.partial=c("SelfEnhan=~SE3"))

sink("./Sink Output/ESS8/JustFOrRun.txt")
summary(RegSEM.Mediation.5Clus, fit.measures=T, standardized=T)
sink()

sink("./Sink Output/ESS8/Mediation_FullMCCPolSup_RegMGSEM_5clus.txt")
summary(RegSEM.Mediation.5Clus, fit.measures=T, standardized=T)
sink()



#####################################################################################
#################### Mediation Mapping ##############################################
#####################################################################################


###----------------------------------------------------------------------------------
##6 cluster solution:

##take out the world map
world_map<-map_data("world")

##filter to be a eu map:
eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
  "Ireland", "Iceland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
  "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia",
  "Slovakia", "Slovenia", "Switzerland",
  "Spain", "Sweden", "UK", "Israel",
  "Turkey", "Lebanon", "Jordan", "Egypt", "Syria",
  "Ukraine", "Belarus", "Georgia", "Armenia", "Azerbaijan", "Moldova"
)

eu_map <- world_map %>%
  filter(region %in% eu_countries)

##add a new column called region to match the country names with the world map data country names
ClusterRes.6clus<-ClusterRes.6clus %>%
  mutate(region=case_when(
    country == "AT" ~ "Austria",
    country == "BE" ~ "Belgium",
    country == "CH" ~ "Switzerland",
    country == "CZ" ~ "Czech Republic",
    country == "DE" ~ "Germany",
    country == "EE" ~ "Estonia",
    country == "ES" ~ "Spain",
    country == "FI" ~ "Finland",
    country == "FR" ~ "France",
    country == "GB" ~ "UK",
    country == "HU" ~ "Hungary",
    country == "IE" ~ "Ireland",
    country == "IL" ~ "Israel",
    country == "IS" ~ "Iceland",
    country == "IT" ~ "Italy",
    country == "LT" ~ "Lithuania",
    country == "NL" ~ "Netherlands",
    country == "NO" ~ "Norway",
    country == "PL" ~ "Poland",
    country == "PT" ~ "Portugal",
    country == "RU" ~ "Russia",
    country == "SE" ~ "Sweden",
    country == "SI" ~ "Slovenia"
  )) %>%
  select(ClusMembership, region)

##merge the data:
map_with_6clusters <- eu_map %>%
  left_join(ClusterRes.6clus, by = "region")

##lay out on the map:
ggplot(map_with_6clusters, aes(long, lat, group = group, fill = factor(ClusMembership))) +
  geom_polygon(color = "white") +
  labs(
    title = "Clustering Results on the Map",
    fill = "Cluster"
  ) +
  theme_minimal()

##mapping with translation of CCBelief into CCPolSupport:
map_with_6clusters <- map_with_6clusters %>%
  mutate(CCBelief_Into_CCPolSupport=case_when(
    ClusMembership == 1 ~ "weakest",
    ClusMembership == 2 ~ "medium-weak",
    ClusMembership == 3 ~ "medium-weak",
    ClusMembership == 4 ~ "strongest",
    ClusMembership == 5 ~ "medium-strong",
    ClusMembership == 6 ~ "medium-weak"
  ))

map_with_6clusters$CCBelief_Into_CCPolSupport<-factor(map_with_6clusters$CCBelief_Into_CCPolSupport,
                                                      levels = c("weakest","medium-weak","medium-strong","strongest"))


ggplot(map_with_6clusters, aes(long, lat, group = group, fill = CCBelief_Into_CCPolSupport)) +
  geom_polygon(color = "white") +
  labs(
    title = "Clustering Results on the Map",
    fill = "CCBelief into CCPolSupport"
  ) +
  theme_minimal()



##mapping within the medium-weak group:
map_with_6clusters <- map_with_6clusters %>%
  mutate(Characteristics=case_when(
    ClusMembership == 1 ~ NA,
    ClusMembership == 2 ~ "strong neg. ind. SE",
    ClusMembership == 3 ~ "no other char.",
    ClusMembership == 4 ~ NA,
    ClusMembership == 5 ~ NA,
    ClusMembership == 6 ~ "weak ind. Con_ST"
  ))


ggplot(map_with_6clusters, aes(long, lat, group = group, fill = factor(Characteristics))) +
  geom_polygon(color = "white") +
  labs(
    title = "Clustering Results on the Map",
    fill = "characteristics"
  ) +
  theme_minimal()


###################################################################################################################
#################### First Trail to rerun all MMGSEM model with no Hungary ########################################
###################################################################################################################


####----------------------------------------------------------------------------------------------------------------------
##basic model with CCBelief as DV
##First, take all the necessary measurement models and change to marker variable approach:

ESS8_noHU<-ESS8 %>%
  filter(country != "HU")
rm(ESS8)

##Human Values without Openness to Change
#
NoOpen.HV.Metric.M2.Marker<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit2.Marker<-cfa(model = NoOpen.HV.Metric.M2.Marker,
                                  data = ESS8_noHU,
                                  group = "country",
                                  estimator="MLR",
                                  missing="FIML",
                                  group.equal="loadings",
                                  group.partial=c("SelfEnhan=~SE3"))

#sink("./Sink Output/ESS8/NoHU_HV_Metric_fit2_Marker.txt")
#summary(NoOpen.HV.Metric.Fit2.Marker, fit.measures=T, standardized=T)
#sink()

#
##Climate Change Belief
#
CCBelief.Metric.M1.Marker<-'
CCBelief=~ImpactBelief+TrendBelief+AttriBelief
'

CCBelief.Metric.Fit1.Marker<-cfa(model = CCBelief.Metric.M1.Marker,
                                 data = ESS8_noHU,
                                 group = "country",
                                 estimator="MLR",
                                 missing="FIML",
                                 group.equal="loadings")


#sink("./Sink Output/ESS8/NoHU_CCBelief_Metric_fit1_Marker.txt")
#summary(CCBelief.Metric.Fit1.Marker, fit.measures=T, standardized=T)
#sink()


##re-run MMGSEM

##Structural model
Str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan
'

##Model selection 
BasicModel.Selection<-ModelSelection(dat=ESS8_noHU,
                                     S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker),
                                     S2 = Str_model,
                                     group = "country",
                                     clusters=c(1,8),
                                     seed = 100,
                                     userStart = NULL,
                                     s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker),
                                     max_it = 10000L,
                                     nstarts = 150L,
                                     printing = FALSE,
                                     partition = "hard",
                                     endogenous_cov = TRUE,
                                     endo_group_specific = TRUE,
                                     sam_method = "local",
                                     meanstr = FALSE,
                                     rescaling = F,
                                     missing="FIML")
#
View(BasicModel.Selection$Overview)

##plot for CHull observed
ggplot(BasicModel.Selection$Overview, aes(x=nrpar, y=LL)) +
  geom_point()+
  geom_line()+
  labs(title = "CHUll observed")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()
#
##plot for CHull factor
ggplot(BasicModel.Selection$Overview, aes(x=nrpar_fac, y=LL_fac)) +
  geom_point()+
  geom_line()+
  labs(title = "CHUll factor")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()
#
##plot for BIC_G observed
ggplot(BasicModel.Selection$Overview, aes(x=Clusters, y=BIC_G))+
  geom_point()+geom_line()+
  labs(title = "BIC_G Observed")+xlab("Number of Clusters")+ylab("BIC_G")+
  theme_minimal()
#
##plot for BIC_G factor
ggplot(BasicModel.Selection$Overview, aes(x=Clusters, y=BIC_G_fac))+
  geom_point()+geom_line()+
  labs(title = "BIC_G Factor")+xlab("Number of Clusters")+ylab("BIC_G")+
  theme_minimal()


##go for 5 clusters:
BasicModel.5clus.150S.NoHU<-MMGSEM(dat=ESS8_noHU,
                                   S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker),
                                   S2 = Str_model,
                                   group = "country",
                                   nclus=5,
                                   seed = 100,
                                   userStart = NULL,
                                   s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker),
                                   max_it = 10000L,
                                   nstarts = 150L,
                                   printing = FALSE,
                                   partition = "hard",
                                   endogenous_cov = TRUE,
                                   endo_group_specific = TRUE,
                                   sam_method = "local",
                                   meanstr = FALSE,
                                   rescaling = F,
                                   missing="FIML")
round(BasicModel.5clus.150S.NoHU$posteriors, digits = 5)

#cluster membership --> 5 clusters
clustering.5clus.150s.NoHU<-t(apply(BasicModel.5clus.150S.NoHU$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.5clus.150s.NoHU[,2]<-ifelse(clustering.5clus.150s.NoHU[,2]==1,2,0)
clustering.5clus.150s.NoHU[,3]<-ifelse(clustering.5clus.150s.NoHU[,3]==1,3,0)
clustering.5clus.150s.NoHU[,4]<-ifelse(clustering.5clus.150s.NoHU[,4]==1,4,0)
clustering.5clus.150s.NoHU[,5]<-ifelse(clustering.5clus.150s.NoHU[,5]==1,5,0)

ClusMembership.5clus.150s<-apply(clustering.5clus.150s.NoHU,1,function(x) sum(x))
ClusterRes.5clus.150s<-data.frame(group=c(1:22),
                                  ClusMembership=ClusMembership.5clus.150s)
countries<-data.frame(group=c(1:22),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.5clus.150s<-merge(ClusterRes.5clus.150s, countries,
                             by.x = "group", by.y = "group")


##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
lambda_HV_23cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_23cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from CCBelief
EST_CCBelief<-lavInspect(CCBelief.Metric.Fit1.Marker, what = "est")
lambda_CCBelief_23cntry<-lapply(EST_CCBelief, "[[","lambda")
theta_CCBelief_23cntry<-lapply(EST_CCBelief, "[[","theta")
#
##initialize empty list to store the new lambda matrix, new theta matrix and the mapping matrix
lambda_23cntry<-vector(mode = "list", length=length(unique(ESS8_noHU$country)))
theta_23cntry<-vector(mode = "list", length=length(unique(ESS8_noHU$country)))
Mmatrix<-vector(mode = "list", length = length(unique(ESS8_noHU$country)))

for (g in 1:length(unique(ESS8_noHU$country))){
  ##put lambda from two measurement blocks into into the same matrix for each group
  lambda_23cntry[[g]]<-lav_matrix_bdiag(lambda_HV_23cntry[[g]], lambda_CCBelief_23cntry[[g]])
  colnames(lambda_23cntry[[g]])<-c(colnames(lambda_HV_23cntry[[g]]), colnames(lambda_CCBelief_23cntry[[g]]))
  rownames(lambda_23cntry[[g]])<-c(rownames(lambda_HV_23cntry[[g]]), rownames(lambda_CCBelief_23cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_23cntry[[g]]<-lav_matrix_bdiag(theta_HV_23cntry[[g]],theta_CCBelief_23cntry[[g]])
  colnames(theta_23cntry[[g]])<-c(colnames(theta_HV_23cntry[[g]]), colnames(theta_CCBelief_23cntry[[g]]))
  rownames(theta_23cntry[[g]])<-c(rownames(theta_HV_23cntry[[g]]), rownames(theta_CCBelief_23cntry[[g]]))
  
  ##compute the mapping matrix for each group
  Mmatrix[[g]]<-solve(t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]]) %*% lambda_23cntry[[g]]) %*% t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]])
}
#
##run an empty sem to just extract the imputed sample covariance matrix
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCBelief=~ImpactBelief+TrendBelief+AttriBelief
'

fake<-cfa(model = fake_model,
          data = ESS8_noHU,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F)

S<-fake@SampleStats@cov

S<-lapply(S, function(x) {
  colnames(x)<-rownames(x)<-colnames(fitted(fake)[[19]]$cov)
  x
})

#
##compute the factor covariance matrix for each group that will be used for the second step:
Var_eta<-vector(mode = "list", length = length(unique(ESS8_noHU$country)))

for (g in 1:length(unique(ESS8_noHU$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_23cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
FREEsam_str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan
'

BasicModel.FreeSAM<-cfa(model = FREEsam_str_model,
                        sample.cov = Var_eta,
                        sample.nobs = lavInspect(fake, "nobs"))


##constrain for the 5-cluster solution:
##cluster 1: group 4,12,14
##cluster 2: group 2,6,11,20,22
##cluster 3: group 7,8,9,10,13,16,17,21
##cluster 4: group 15
##cluster 5: group 1,3,5,18,19

sam_str_model_5clus.NoHU<-'
CCBelief~c(a5,a2,a5,a1,a5,a2,a3,a3,a3,a3,a2,a1,a3,a1,a4,a3,a3,a5,a5,a2,a3,a2)*SelfTran+
          c(b5,b2,b5,b1,b5,b2,b3,b3,b3,b3,b2,b1,b3,b1,b4,b3,b3,b5,b5,b2,b3,b2)*Conser+
          c(c5,c2,c5,c1,c5,c2,c3,c3,c3,c3,c2,c1,c3,c1,c4,c3,c3,c5,c5,c2,c3,c2)*SelfEnhan
'

BasicModel.SAM.5clus.NoHU<-cfa(model = sam_str_model_5clus.NoHU,
                              sample.cov = Var_eta,
                              sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/NoHU_CCBeliefBasicModel_SAM_5clus.txt")
summary(BasicModel.SAM.5clus.NoHU, fit.measures=T, standardized=T)
sink()




##3-D scatter plot
FreeSAMparam<-parameterEstimates(BasicModel.FreeSAM)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.5clus.150s, 
                         by.x = "group", by.y = "group")

SAM_5clus_noHU_3D_FIML<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                               type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM 5 clusters no Hungary with clustering results - Human Values on Climate Change Belief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(SAM_5clus_noHU_3D_FIML), "NoHU_CCBeliefBasicModel_3D.html")





####-------------------------------------------------------------------------------------------------------------------------
####-------------------------------------------------------------------------------------------------------------------------
####-------------------------------------------------------------------------------------------------------------------------
##now re-run the model with CCPolsupport as the DV for the basic model

ESS8_noHU<-ESS8 %>%
  filter(country != "HU")
rm(ESS8)


NoOpen.HV.Metric.M2.Marker<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit2.Marker<-cfa(model = NoOpen.HV.Metric.M2.Marker,
                                  data = ESS8_noHU,
                                  group = "country",
                                  estimator="MLR",
                                  missing="FIML",
                                  group.equal="loadings",
                                  group.partial=c("SelfEnhan=~SE3"))

#sink("./Sink Output/ESS8/NoOpen_HV_Metric_fit2_Marker.txt")
#summary(NoOpen.HV.Metric.Fit2.Marker, fit.measures=T, standardized=T)
#sink()

CCPolSupport.PMetric.M1.MarkerSup2<-'
CCPolicySupport=~support2+support1+support3
'

CCPolSupport.PMetric.Fit1.MarkerSup2<-cfa(model = CCPolSupport.PMetric.M1.MarkerSup2,
                                          data = ESS8_noHU,
                                          group = "country",
                                          estimator="MLR",
                                          missing="FIML",
                                          group.equal="loadings",
                                          group.partial=c("CCPolicySupport=~support3"))

##Structural model
Str_model<-'
CCPolicySupport~SelfTran+Conser+SelfEnhan
'

##Model selection 
#
BasicModel.PMetricCCPolSup.marker2.Selection<-ModelSelection(dat=ESS8_noHU,
                                                             S1 = list(NoOpen.HV.Metric.M2.Marker, CCPolSupport.PMetric.M1.MarkerSup2),
                                                             S2 = Str_model,
                                                             group = "country",
                                                             clusters=c(1,8),
                                                             seed = 100,
                                                             userStart = NULL,
                                                             s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCPolSupport.PMetric.Fit1.MarkerSup2),
                                                             max_it = 10000L,
                                                             nstarts = 150L,
                                                             printing = FALSE,
                                                             partition = "hard",
                                                             endogenous_cov = TRUE,
                                                             endo_group_specific = TRUE,
                                                             sam_method = "local",
                                                             meanstr = FALSE,
                                                             rescaling = F,
                                                             missing="FIML")

View(BasicModel.PMetricCCPolSup.marker2.Selection$Overview)

#
##plot for CHull observed
ggplot(BasicModel.PMetricCCPolSup.marker2.Selection$Overview, aes(x=nrpar, y=LL)) +
  geom_point()+
  geom_line()+
  labs(title = "Original CHUll observed")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()
#
##plot for CHull factor
ggplot(BasicModel.PMetricCCPolSup.marker2.Selection$Overview, aes(x=nrpar_fac, y=LL_fac)) +
  geom_point()+
  geom_line()+
  labs(title = "CHUll factor")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()
#
##plot for BIC_G observed
ggplot(BasicModel.PMetricCCPolSup.marker2.Selection$Overview, aes(x=Clusters, y=BIC_G))+
  geom_point()+geom_line()+
  labs(title = "BIC_G Observed")+xlab("Number of Clusters")+ylab("BIC_G")+
  theme_minimal()
#
##plot for BIC_G factor
ggplot(BasicModel.PMetricCCPolSup.marker2.Selection$Overview, aes(x=Clusters, y=BIC_G_fac))+
  geom_point()+geom_line()+
  labs(title = "BIC_G Factor")+xlab("Number of Clusters")+ylab("BIC_G")+
  theme_minimal()



##run 3 clusters
CCPolicySupport.3clus.NoHU.MarkSup2.PM<-MMGSEM(dat=ESS8_noHU,
                                               S1 = list(NoOpen.HV.Metric.M2.Marker, CCPolSupport.PMetric.M1.MarkerSup2),
                                               S2 = Str_model,
                                               group = "country",
                                               nclus=3,
                                               seed = 100,
                                               userStart = NULL,
                                               s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCPolSupport.PMetric.Fit1.MarkerSup2),
                                               max_it = 10000L,
                                               nstarts = 150L,
                                               printing = FALSE,
                                               partition = "hard",
                                               endogenous_cov = TRUE,
                                               endo_group_specific = TRUE,
                                               sam_method = "local",
                                               meanstr = FALSE,
                                               rescaling = F,
                                               missing="FIML")
round(CCPolicySupport.3clus.NoHU.MarkSup2.PM$posteriors, digits = 5)


#
##clustering membership for 3 clusters solution
clustering.3clus<-t(apply(CCPolicySupport.3clus.NoHU.MarkSup2.PM$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.3clus[,2]<-ifelse(clustering.3clus[,2]==1,2,0)
clustering.3clus[,3]<-ifelse(clustering.3clus[,3]==1,3,0)

ClusMembership.3clus<-apply(clustering.3clus,1,function(x) sum(x))
ClusterRes.3clus<-data.frame(group=c(1:22),
                             ClusMembership=ClusMembership.3clus)

countries<-data.frame(group=c(1:22),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.3clus<-merge(ClusterRes.3clus, countries,
                        by.x = "group", by.y = "group")

##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
lambda_HV_23cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_23cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from CCBelief
EST_CCPolSupport<-lavInspect(CCPolSupport.PMetric.Fit1.MarkerSup2, what = "est")
lambda_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","lambda")
theta_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","theta")
#
##initialize empty list to store the new lambda matrix, new theta matrix and the mapping matrix
lambda_23cntry<-vector(mode = "list", length=length(unique(ESS8_noHU$country)))
theta_23cntry<-vector(mode = "list", length=length(unique(ESS8_noHU$country)))
Mmatrix<-vector(mode = "list", length = length(unique(ESS8_noHU$country)))

for (g in 1:length(unique(ESS8_noHU$country))){
  ##put lambda from two measurement blocks into into the same matrix for each group
  lambda_23cntry[[g]]<-lav_matrix_bdiag(lambda_HV_23cntry[[g]], lambda_CCPolSupport_23cntry[[g]])
  colnames(lambda_23cntry[[g]])<-c(colnames(lambda_HV_23cntry[[g]]), colnames(lambda_CCPolSupport_23cntry[[g]]))
  rownames(lambda_23cntry[[g]])<-c(rownames(lambda_HV_23cntry[[g]]), rownames(lambda_CCPolSupport_23cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_23cntry[[g]]<-lav_matrix_bdiag(theta_HV_23cntry[[g]],theta_CCPolSupport_23cntry[[g]])
  colnames(theta_23cntry[[g]])<-c(colnames(theta_HV_23cntry[[g]]), colnames(theta_CCPolSupport_23cntry[[g]]))
  rownames(theta_23cntry[[g]])<-c(rownames(theta_HV_23cntry[[g]]), rownames(theta_CCPolSupport_23cntry[[g]]))
  
  ##compute the mapping matrix for each group
  Mmatrix[[g]]<-solve(t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]]) %*% lambda_23cntry[[g]]) %*% t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]])
}
#
##run an empty sem to just extract the imputed sample covariance matrix
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCPolicySupport=~support2+support1+support3
'

fake<-cfa(model = fake_model,
          data = ESS8_noHU,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F)

S<-fake@SampleStats@cov

S<-lapply(S, function(x) {
  colnames(x)<-rownames(x)<-colnames(fitted(fake)[[1]]$cov)
  x
})

#
##compute the factor covariance matrix for each group that will be used for the second step:
Var_eta<-vector(mode = "list", length = length(unique(ESS8_noHU$country)))

for (g in 1:length(unique(ESS8_noHU$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_23cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
CCPolSupport_FREEsam_str_model<-'
CCPolicySupport~SelfTran+Conser+SelfEnhan
'

CCPolSupport.FreeSAM.PM.Mark2<-cfa(model = CCPolSupport_FREEsam_str_model,
                                   sample.cov = Var_eta,
                                   sample.nobs = lavInspect(fake, "nobs"))


##SAM constrain estimation for 3-cluster solution:
##3-cluster: 
##cluster 1: group 1,2,3,4,5,7,8,9,10,14,16,17,19,21
##cluster 2: group 6,11,12,13,18,20,22
##cluster 3: group 15

sam_CCPolSupport_3clus_NoHU<-'
CCPolicySupport~c(a1,a1,a1,a1,a1,a2,a1,a1,a1,a1,a2,a2,a2,a1,a3,a1,a1,a2,a1,a2,a1,a2)*SelfTran+
                c(b1,b1,b1,b1,b1,b2,b1,b1,b1,b1,b2,b2,b2,b1,b3,b1,b1,b2,b1,b2,b1,b2)*Conser+
                c(c1,c1,c1,c1,c1,c2,c1,c1,c1,c1,c2,c2,c2,c1,c3,c1,c1,c2,c1,c2,c1,c2)*SelfEnhan
'

CCPolSupport.SAM.3clus.NoHU<-cfa(model = sam_CCPolSupport_3clus_NoHU,
                                    sample.cov = Var_eta,
                                    sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/NoHU_CCPolSupBasicModel_SAM_3clus.txt")
summary(CCPolSupport.SAM.3clus.NoHU, fit.measures=T, standardized=T)
sink()



##3-D scatter plot
FreeSAMparam<-parameterEstimates(CCPolSupport.FreeSAM.PM.Mark2)
FreeSAM_reg_param<-FreeSAMparam %>%
  filter(op=="~") %>%
  select(lhs, rhs, group, est) %>%
  pivot_wider(names_from = rhs, values_from = est)

FreeSAM_reg_param<-merge(FreeSAM_reg_param, ClusterRes.3clus, 
                         by.x = "group", by.y = "group")

CCPolSupport_3clus_3D_PMMark2_NoHU<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                                       type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM with clustering results - Human Values on Climate Change Policy Support",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))


htmlwidgets::saveWidget(as_widget(CCPolSupport_3clus_3D_PMMark2_NoHU), "NoHU_CCPolSupBasicModel_3D.html")





###----------------------------------------------------------------------------------------
###----------------------------------------------------------------------------------------
###----------------------------------------------------------------------------------------
##re-run the mediation model without Hungary:

ESS8_noHU<-ESS8 %>%
  filter(country != "HU")
rm(ESS8)

###Human values: marker approach:
NoOpen.HV.Metric.M2.Marker<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit2.Marker<-cfa(model = NoOpen.HV.Metric.M2.Marker,
                                  data = ESS8_noHU,
                                  group = "country",
                                  estimator="MLR",
                                  missing="FIML",
                                  group.equal="loadings",
                                  group.partial=c("SelfEnhan=~SE3"))

#sink("./Sink Output/ESS8/NoOpen_HV_Metric_fit2_Marker.txt")
#summary(NoOpen.HV.Metric.Fit2.Marker, fit.measures=T, standardized=T)
#sink()

#
##Climate Change Belief
#
CCBelief.Metric.M1.Marker<-'
CCBelief=~ImpactBelief+TrendBelief+AttriBelief
'

CCBelief.Metric.Fit1.Marker<-cfa(model = CCBelief.Metric.M1.Marker,
                                 data = ESS8_noHU,
                                 group = "country",
                                 estimator="MLR",
                                 missing="FIML",
                                 group.equal="loadings")

#sink("./Sink Output/ESS8/CCBelief_Metric_fit1_marker.txt")
#summary(CCBelief.Metric.Fit1.Marker, fit.measures=T, standardized=T)
#sink()

##partial metric invariance model with support 2 as marker and let support 3 freely estimated
CCPolSupport.PMetric.M1.MarkerSup2<-'
CCPolicySupport=~support2+support1+support3
'

CCPolSupport.PMetric.Fit1.MarkerSup2<-cfa(model = CCPolSupport.PMetric.M1.MarkerSup2,
                                          data = ESS8_noHU,
                                          group = "country",
                                          estimator="MLR",
                                          missing="FIML",
                                          group.equal="loadings",
                                          group.partial=c("CCPolicySupport=~support3"))

##specify the structural model:
Str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan

CCPolicySupport~CCBelief+SelfTran+Conser+SelfEnhan
'

##Model selection:
MediationModel.Selection.PM.MarkSup2<-ModelSelection(dat=ESS8_noHU,
                                                     S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                                                     S2 = Str_model,
                                                     group = "country",
                                                     clusters=c(1,8),
                                                     seed = 100,
                                                     userStart = NULL,
                                                     s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                                                     max_it = 10000L,
                                                     nstarts = 300L,
                                                     printing = FALSE,
                                                     partition = "hard",
                                                     endogenous_cov = TRUE,
                                                     endo_group_specific = TRUE,
                                                     sam_method = "local",
                                                     meanstr = FALSE,
                                                     rescaling = F,
                                                     missing="FIML")
View(MediationModel.Selection.PM.MarkSup2$Overview)

a<-MediationModel.Selection.PM.MarkSup2$Overview
##expected CHull at the 3-clusters
(-1269185)+(768-760)*((-1269022-(-1269185))/(776-760))
##-1269104
a[3,3]<--1269104+1
#
##expected CHull at 7-clusters
(-1268943)+(800-792)*((-1268913-(-1268943))/(808-792))
##-1268928
a[7,3]<--1268928+1

ggplot(a, aes(x=nrpar, y=LL)) +
  geom_point()+
  geom_line()+
  labs(title = "Adjusted CHUll observed")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()

#
##plot for CHull observed
ggplot(MediationModel.Selection.PM.MarkSup2$Overview, aes(x=nrpar, y=LL)) +
  geom_point()+
  geom_line()+
  labs(title = "Original CHUll observed")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()

#
##plot for CHull factor
ggplot(MediationModel.Selection.PM.MarkSup2$Overview, aes(x=nrpar_fac, y=LL_fac)) +
  geom_point()+
  geom_line()+
  labs(title = "CHUll factor")+xlab("number of parameters")+ylab("Log-Likelihood")+
  theme_minimal()
#
##plot for BIC_G observed
ggplot(MediationModel.Selection.PM.MarkSup2$Overview, aes(x=Clusters, y=BIC_G))+
  geom_point()+geom_line()+
  labs(title = "BIC_G Observed")+xlab("Number of Clusters")+ylab("BIC_G")+
  theme_minimal()
#
##plot for BIC_G factor
ggplot(MediationModel.Selection.PM.MarkSup2$Overview, aes(x=Clusters, y=BIC_G_fac))+
  geom_point()+geom_line()+
  labs(title = "BIC_G Factor")+xlab("Number of Clusters")+ylab("BIC_G")+
  theme_minimal()



##Mediation model from 1-6 clusters run one by one to compare with the previous results with Hungary

#
##2 clusters:
Mediation.2clus.NoHU<-MMGSEM(dat=ESS8_noHU,
                                    S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                                    S2 = Str_model,
                                    group = "country",
                                    nclus=2,
                                    seed = 100,
                                    userStart = NULL,
                                    s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                                    max_it = 10000L,
                                    nstarts = 300L,
                                    printing = T,
                                    partition = "hard",
                                    endogenous_cov = TRUE,
                                    endo_group_specific = TRUE,
                                    sam_method = "local",
                                    meanstr = FALSE,
                                    rescaling = F,
                                    missing="FIML")
round(Mediation.2clus.NoHU$posteriors, digits = 10)
##converged

#
##3 clusters:
Mediation.3clus.NoHU<-MMGSEM(dat=ESS8_noHU,
                             S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                             S2 = Str_model,
                             group = "country",
                             nclus=3,
                             seed = 100,
                             userStart = NULL,
                             s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                             max_it = 10000L,
                             nstarts = 300L,
                             printing = T,
                             partition = "hard",
                             endogenous_cov = TRUE,
                             endo_group_specific = TRUE,
                             sam_method = "local",
                             meanstr = FALSE,
                             rescaling = F,
                             missing="FIML")
round(Mediation.3clus.NoHU$posteriors, digits = 10)
##converged

#
##4 clusters:
Mediation.4clus.NoHU<-MMGSEM(dat=ESS8_noHU,
                             S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                             S2 = Str_model,
                             group = "country",
                             nclus=4,
                             seed = 100,
                             userStart = NULL,
                             s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                             max_it = 10000L,
                             nstarts = 300L,
                             printing = T,
                             partition = "hard",
                             endogenous_cov = TRUE,
                             endo_group_specific = TRUE,
                             sam_method = "local",
                             meanstr = FALSE,
                             rescaling = F,
                             missing="FIML")
round(Mediation.4clus.NoHU$posteriors, digits = 10)
##converged


#
##5 clusters:
Mediation.5clus.NoHU<-MMGSEM(dat=ESS8_noHU,
                             S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker,CCPolSupport.PMetric.M1.MarkerSup2),
                             S2 = Str_model,
                             group = "country",
                             nclus=5,
                             seed = 100,
                             userStart = NULL,
                             s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker,CCPolSupport.PMetric.Fit1.MarkerSup2),
                             max_it = 10000L,
                             nstarts = 300L,
                             printing = T,
                             partition = "hard",
                             endogenous_cov = TRUE,
                             endo_group_specific = TRUE,
                             sam_method = "local",
                             meanstr = FALSE,
                             rescaling = F,
                             missing="FIML")
round(Mediation.5clus.NoHU$posteriors, digits = 15)
##converged



##clustering membership:
#
##2-clusters:
clustering.2clus<-t(apply(Mediation.2clus.NoHU$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.2clus[,2]<-ifelse(clustering.2clus[,2]==1,2,0)
ClusMembership.2clus<-apply(clustering.2clus,1,function(x) sum(x))
ClusterRes.2clus<-data.frame(group=c(1:22),
                             ClusMembership=ClusMembership.2clus)
countries<-data.frame(group=c(1:22),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.2clus<-merge(ClusterRes.2clus, countries,
                        by.x = "group", by.y = "group")

#
##3-clusters:
clustering.3clus<-t(apply(Mediation.3clus.NoHU$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.3clus[,2]<-ifelse(clustering.3clus[,2]==1,2,0)
clustering.3clus[,3]<-ifelse(clustering.3clus[,3]==1,3,0)
ClusMembership.3clus<-apply(clustering.3clus,1,function(x) sum(x))

ClusterRes.3clus<-data.frame(group=c(1:22),
                             ClusMembership=ClusMembership.3clus)
countries<-data.frame(group=c(1:22),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.3clus<-merge(ClusterRes.3clus, countries,
                        by.x = "group", by.y = "group")

#
##4-clusters:
clustering.4clus<-t(apply(Mediation.4clus.NoHU$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.4clus[,2]<-ifelse(clustering.4clus[,2]==1,2,0)
clustering.4clus[,3]<-ifelse(clustering.4clus[,3]==1,3,0)
clustering.4clus[,4]<-ifelse(clustering.4clus[,4]==1,4,0)
ClusMembership.4clus<-apply(clustering.4clus,1,function(x) sum(x))

ClusterRes.4clus<-data.frame(group=c(1:22),
                             ClusMembership=ClusMembership.4clus)
countries<-data.frame(group=c(1:22),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.4clus<-merge(ClusterRes.4clus, countries,
                        by.x = "group", by.y = "group")


#
##5-clusters:
clustering.5clus<-t(apply(Mediation.5clus.NoHU$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.5clus[,2]<-ifelse(clustering.5clus[,2]==1,2,0)
clustering.5clus[,3]<-ifelse(clustering.5clus[,3]==1,3,0)
clustering.5clus[,4]<-ifelse(clustering.5clus[,4]==1,4,0)
clustering.5clus[,5]<-ifelse(clustering.5clus[,5]==1,5,0)
ClusMembership.5clus<-apply(clustering.5clus,1,function(x) sum(x))

ClusterRes.5clus<-data.frame(group=c(1:22),
                             ClusMembership=ClusMembership.5clus)
countries<-data.frame(group=c(1:22),
                      country=lavInspect(NoOpen.HV.Metric.Fit2.Marker, "group.label"))

ClusterRes.5clus<-merge(ClusterRes.5clus, countries,
                        by.x = "group", by.y = "group")




##First do the following step that is necessary for all clusters solution
#
##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
lambda_HV_23cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_23cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from CCBelief
EST_CCBelief<-lavInspect(CCBelief.Metric.Fit1.Marker, what = "est")
lambda_CCBelief_23cntry<-lapply(EST_CCBelief, "[[","lambda")
theta_CCBelief_23cntry<-lapply(EST_CCBelief, "[[","theta")
#
##extract the loadings and residual variances from CCPolSupport
EST_CCPolSupport<-lavInspect(CCPolSupport.PMetric.Fit1.MarkerSup2, what = "est")
lambda_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","lambda")
theta_CCPolSupport_23cntry<-lapply(EST_CCPolSupport, "[[","theta")
#
##initialize empty list to store the new lambda matrix, new theta matrix and the mapping matrix
lambda_23cntry<-vector(mode = "list", length=length(unique(ESS8_noHU$country)))
theta_23cntry<-vector(mode = "list", length=length(unique(ESS8_noHU$country)))
Mmatrix<-vector(mode = "list", length = length(unique(ESS8_noHU$country)))

for (g in 1:length(unique(ESS8_noHU$country))){
  ##put lambda from two measurement blocks into into the same matrix for each group
  lambda_23cntry[[g]]<-lav_matrix_bdiag(lambda_HV_23cntry[[g]], lambda_CCBelief_23cntry[[g]], lambda_CCPolSupport_23cntry[[g]])
  colnames(lambda_23cntry[[g]])<-c(colnames(lambda_HV_23cntry[[g]]),colnames(lambda_CCBelief_23cntry[[g]]), colnames(lambda_CCPolSupport_23cntry[[g]]))
  rownames(lambda_23cntry[[g]])<-c(rownames(lambda_HV_23cntry[[g]]),rownames(lambda_CCBelief_23cntry[[g]]), rownames(lambda_CCPolSupport_23cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_23cntry[[g]]<-lav_matrix_bdiag(theta_HV_23cntry[[g]],theta_CCBelief_23cntry[[g]],theta_CCPolSupport_23cntry[[g]])
  colnames(theta_23cntry[[g]])<-c(colnames(theta_HV_23cntry[[g]]),colnames(theta_CCBelief_23cntry[[g]]), colnames(theta_CCPolSupport_23cntry[[g]]))
  rownames(theta_23cntry[[g]])<-c(rownames(theta_HV_23cntry[[g]]),rownames(theta_CCBelief_23cntry[[g]]), rownames(theta_CCPolSupport_23cntry[[g]]))
  
  ##compute the mapping matrix for each group
  Mmatrix[[g]]<-solve(t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]]) %*% lambda_23cntry[[g]]) %*% t(lambda_23cntry[[g]]) %*% solve(theta_23cntry[[g]])
}

#
##run an empty sem to just extract the imputed sample covariance matrix
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCBelief=~ImpactBelief+TrendBelief+AttriBelief

CCPolicySupport=~support3+support1+support2
'

fake<-cfa(model = fake_model,
          data = ESS8_noHU,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F)

S<-fake@SampleStats@cov

S<-lapply(S, function(x) {
  colnames(x)<-rownames(x)<-colnames(fitted(fake)[[1]]$cov)
  x
})

#
##compute the factor covariance matrix for each group that will be used for the second step:
Var_eta<-vector(mode = "list", length = length(unique(ESS8_noHU$country)))

for (g in 1:length(unique(ESS8_noHU$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_23cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
Mediation_FREEsam_str_model_NoHU<-'
CCBelief~c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21,a22)*SelfTran+
          c(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22)*Conser+
          c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22)*SelfEnhan

CCPolicySupport~c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22)*CCBelief+SelfTran+Conser+SelfEnhan

STindirect_g1:=a1*d1
STindirect_g2:=a2*d2
STindirect_g3:=a3*d3
STindirect_g4:=a4*d4
STindirect_g5:=a5*d5
STindirect_g6:=a6*d6
STindirect_g7:=a7*d7
STindirect_g8:=a8*d8
STindirect_g9:=a9*d9
STindirect_g10:=a10*d10
STindirect_g11:=a11*d11
STindirect_g12:=a12*d12
STindirect_g13:=a13*d13
STindirect_g14:=a14*d14
STindirect_g15:=a15*d15
STindirect_g16:=a16*d16
STindirect_g17:=a17*d17
STindirect_g18:=a18*d18
STindirect_g19:=a19*d19
STindirect_g20:=a20*d20
STindirect_g21:=a21*d21
STindirect_g22:=a22*d22

ConIndirect_g1:=b1*d1
ConIndirect_g2:=b2*d2
ConIndirect_g3:=b3*d3
ConIndirect_g4:=b4*d4
ConIndirect_g5:=b5*d5
ConIndirect_g6:=b6*d6
ConIndirect_g7:=b7*d7
ConIndirect_g8:=b8*d8
ConIndirect_g9:=b9*d9
ConIndirect_g10:=b10*d10
ConIndirect_g11:=b11*d11
ConIndirect_g12:=b12*d12
ConIndirect_g13:=b13*d13
ConIndirect_g14:=b14*d14
ConIndirect_g15:=b15*d15
ConIndirect_g16:=b16*d16
ConIndirect_g17:=b17*d17
ConIndirect_g18:=b18*d18
ConIndirect_g19:=b19*d19
ConIndirect_g20:=b20*d20
ConIndirect_g21:=b21*d21
ConIndirect_g22:=b22*d22

SEindirect_g1:=c1*d1
SEindirect_g2:=c2*d2
SEindirect_g3:=c3*d3
SEindirect_g4:=c4*d4
SEindirect_g5:=c5*d5
SEindirect_g6:=c6*d6
SEindirect_g7:=c7*d7
SEindirect_g8:=c8*d8
SEindirect_g9:=c9*d9
SEindirect_g10:=c10*d10
SEindirect_g11:=c11*d11
SEindirect_g12:=c12*d12
SEindirect_g13:=c13*d13
SEindirect_g14:=c14*d14
SEindirect_g15:=c15*d15
SEindirect_g16:=c16*d16
SEindirect_g17:=c17*d17
SEindirect_g18:=c18*d18
SEindirect_g19:=c19*d19
SEindirect_g20:=c20*d20
SEindirect_g21:=c21*d21
SEindirect_g22:=c22*d22
'

Mediation.FreeSAM.PM.NoHU<-cfa(model = Mediation_FREEsam_str_model_NoHU,
                                   sample.cov = Var_eta,
                                   sample.nobs = lavInspect(fake, "nobs"))


sink("./Sink Output/ESS8/NoHU_Mediation_FreeSAM.txt")
summary(Mediation.FreeSAM.PM.NoHU, fit.measures=T, standardized=T)
sink()


#
####facet plotting
#
##first extract all the estimate:
FreeSAMparam<-parameterEstimates(Mediation.FreeSAM.PM.NoHU)


#
#
##Facet plot for only the effect of CCBelief on CCPolicySupport
CCBelief_regPar<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs=="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper)

CCBelief_regPar<-merge(CCBelief_regPar, ClusterRes.5clus, 
                       by.x = "group", by.y = "group")

CCBelief_regPar$country <- fct_reorder(CCBelief_regPar$country, 
                                       CCBelief_regPar$ClusMembership)

ggplot(CCBelief_regPar, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  geom_vline(xintercept = c(0.475,0.85), color="red", linetype="dashed")+
  labs(title = "SAM with clustering results - effect of CCBelief on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()


##Facet plot for the direct effect of the 3 human values on CCPolicySupport
#
#make a table with direct effect of the 3 human values on CCPolicySupport
HVDirect.param<-FreeSAMparam %>%
  filter(op=="~" & lhs=="CCPolicySupport" & rhs!="CCBelief") %>%
  select(lhs, rhs, group, est, ci.lower, ci.upper) %>%
  mutate(Human.Values=case_when(
    rhs=="SelfTran" ~ "Self-Transcendence",
    rhs=="Conser" ~ "Conservation",
    rhs=="SelfEnhan" ~ "Self-Enhancement"
  ))


HVDirect.param<-merge(HVDirect.param, ClusterRes.5clus, 
                      by.x = "group", by.y = "group")

HVDirect.param$country <- fct_reorder(HVDirect.param$country, 
                                      HVDirect.param$ClusMembership)

vline_data <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, 0)                             # Line positions
)

vline_data2 <- data.frame(
  Human.Values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.125,0.125)                             # Line positions
)

ggplot(HVDirect.param, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~Human.Values, scales = "free_x")+
  #geom_vline(data = vline_data, aes(xintercept = xintercept), color="red", linetype="dashed")+
  #geom_vline(data = vline_data2, aes(xintercept = xintercept), color="blue", linetype="dashed")+
  labs(title = "SAM with clustering results - Direct Effects of Human Values on CC Policy Support",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()

#
#
##Facet plot for the indirect effect of the 3 human values on CCPolicySupport VIA CCBelief
HVIndirect.par<-FreeSAMparam %>%
  filter(op==":=")

HVIndirect.par$group <- as.numeric(gsub(".*_g(\\d+).*", "\\1", HVIndirect.par$lhs))

HVIndirect.par<-HVIndirect.par %>%
  select(lhs, group, est, ci.lower, ci.upper)

HVIndirect.par$human_values <- gsub("_g.*", "", HVIndirect.par$lhs)

HVIndirect.par<-HVIndirect.par %>%
  mutate(human_values=case_when(
    human_values=="STindirect"~"Self-Transcendence",
    human_values=="ConIndirect"~"Conservation",
    human_values=="SEindirect"~"Self-Enhancement"
  ))

HVIndirect.par<-merge(HVIndirect.par, ClusterRes.5clus, 
                      by.x = "group", by.y = "group")

HVIndirect.par$country <- fct_reorder(HVIndirect.par$country, 
                                      HVIndirect.par$ClusMembership)

vline_data <- data.frame(
  human_values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(-0.1, 0.17)                             # Line positions
)

vline_data2 <- data.frame(
  human_values = c("Conservation","Self-Transcendence"), # Facet names
  xintercept = c(0, 0)                             # Line positions
)

ggplot(HVIndirect.par, aes(x=est, y=country, color=factor(ClusMembership)))+
  geom_point(size=3) +
  geom_errorbarh(aes(xmin = ci.lower, xmax = ci.upper), height=0.2)+
  facet_wrap(~human_values, scales = "free_x")+
  geom_vline(data = vline_data, aes(xintercept = xintercept), color="blue", linetype="dashed")+
  #geom_vline(data = vline_data2, aes(xintercept = xintercept), color="red", linetype="dashed")+
  labs(title = "SAM Indirect Effects of Human Values on CC Policy Support via CCBelief",
       color="cluster")+
  xlab("regression coefficients")+ylab("country")+
  theme_bw()


##constrained SAM:
#
##2 clusters:
## cluster 1: group 1,4,6,7,11,14,18,20,22
## cluster 2: group 2,3,5,8,9,10,12,13,15,16,17,19,21

sam_mediation_2clus_NoHU<-'
CCBelief~c(a1,a2,a2,a1,a2,a1,a1,a2,a2,a2,a1,a2,a2,a1,a2,a2,a2,a1,a2,a1,a2,a1)*SelfTran+
          c(b1,b2,b2,b1,b2,b1,b1,b2,b2,b2,b1,b2,b2,b1,b2,b2,b2,b1,b2,b1,b2,b1)*Conser+
          c(c1,c2,c2,c1,c2,c1,c1,c2,c2,c2,c1,c2,c2,c1,c2,c2,c2,c1,c2,c1,c2,c1)*SelfEnhan

CCPolicySupport~c(d1,d2,d2,d1,d2,d1,d1,d2,d2,d2,d1,d2,d2,d1,d2,d2,d2,d1,d2,d1,d2,d1)*CCBelief+
                c(e1,e2,e2,e1,e2,e1,e1,e2,e2,e2,e1,e2,e2,e1,e2,e2,e2,e1,e2,e1,e2,e1)*SelfTran+
                c(f1,f2,f2,f1,f2,f1,f1,f2,f2,f2,f1,f2,f2,f1,f2,f2,f2,f1,f2,f1,f2,f1)*Conser+
                c(g1,g2,g2,g1,g2,g1,g1,g2,g2,g2,g1,g2,g2,g1,g2,g2,g2,g1,g2,g1,g2,g1)*SelfEnhan
'

Mediation.SAM.2clus.NoHU<-cfa(model = sam_mediation_2clus_NoHU,
                                     sample.cov = Var_eta,
                                     sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/NoHU_Mediation_2clus.txt")
summary(Mediation.SAM.2clus.NoHU, fit.measures=T, standardized=T)
sink()


#
##3 clusters:
## cluster 1: group 1,4,6,7,11,14,18,20,22
## cluster 2: group 12,13
## cluster 3: group 2,3,5,8,9,10,15,16,17,19,21


sam_mediation_3clus_NoHU<-'
CCBelief~c(a1,a3,a3,a1,a3,a1,a1,a3,a3,a3,a1,a2,a2,a1,a3,a3,a3,a1,a3,a1,a3,a1)*SelfTran+
          c(b1,b3,b3,b1,b3,b1,b1,b3,b3,b3,b1,b2,b2,b1,b3,b3,b3,b1,b3,b1,b3,b1)*Conser+
          c(c1,c3,c3,c1,c3,c1,c1,c3,c3,c3,c1,c2,c2,c1,c3,c3,c3,c1,c3,c1,c3,c1)*SelfEnhan

CCPolicySupport~c(d1,d3,d3,d1,d3,d1,d1,d3,d3,d3,d1,d2,d2,d1,d3,d3,d3,d1,d3,d1,d3,d1)*CCBelief+
                c(e1,e3,e3,e1,e3,e1,e1,e3,e3,e3,e1,e2,e2,e1,e3,e3,e3,e1,e3,e1,e3,e1)*SelfTran+
                c(f1,f3,f3,f1,f3,f1,f1,f3,f3,f3,f1,f2,f2,f1,f3,f3,f3,f1,f3,f1,f3,f1)*Conser+
                c(g1,g3,g3,g1,g3,g1,g1,g3,g3,g3,g1,g2,g2,g1,g3,g3,g3,g1,g3,g1,g3,g1)*SelfEnhan
'

Mediation.SAM.3clus.NoHU<-cfa(model = sam_mediation_3clus_NoHU,
                              sample.cov = Var_eta,
                              sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/NoHU_Mediation_3clus.txt")
summary(Mediation.SAM.3clus.NoHU, fit.measures=T, standardized=T)
sink()


#
##4 clusters:
## cluster 1: group 2,3,5,8,9,10,13,16,17,19,21
## cluster 2: group 1,4,6,7,11,14,18,20,22
## cluster 3: group 12
## cluster 4: group 15


sam_mediation_4clus_NoHU<-'
CCBelief~c(a2,a1,a1,a2,a1,a2,a2,a1,a1,a1,a2,a3,a1,a2,a4,a1,a1,a2,a1,a2,a1,a2)*SelfTran+
          c(b2,b1,b1,b2,b1,b2,b2,b1,b1,b1,b2,b3,b1,b2,b4,b1,b1,b2,b1,b2,b1,b2)*Conser+
          c(c2,c1,c1,c2,c1,c2,c2,c1,c1,c1,c2,c3,c1,c2,c4,c1,c1,c2,c1,c2,c1,c2)*SelfEnhan

CCPolicySupport~c(d2,d1,d1,d2,d1,d2,d2,d1,d1,d1,d2,d3,d1,d2,d4,d1,d1,d2,d1,d2,d1,d2)*CCBelief+
                c(e2,e1,e1,e2,e1,e2,e2,e1,e1,e1,e2,e3,e1,e2,e4,e1,e1,e2,e1,e2,e1,e2)*SelfTran+
                c(f2,f1,f1,f2,f1,f2,f2,f1,f1,f1,f2,f3,f1,f2,f4,f1,f1,f2,f1,f2,f1,f2)*Conser+
                c(g2,g1,g1,g2,g1,g2,g2,g1,g1,g1,g2,g3,g1,g2,g4,g1,g1,g2,g1,g2,g1,g2)*SelfEnhan
'

Mediation.SAM.4clus.NoHU<-cfa(model = sam_mediation_4clus_NoHU,
                              sample.cov = Var_eta,
                              sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/NoHU_Mediation_4clus.txt")
summary(Mediation.SAM.4clus.NoHU, fit.measures=T, standardized=T)
sink()


#
##5 clusters:
## cluster 1: group 2,3,5,8,9,10,13,16,17,19,21
## cluster 2: group 15
## cluster 3: group 12
## cluster 4: group 1,4,7,14
## cluster 5: group 6,11,18,20,22



sam_mediation_5clus_NoHU<-'
CCBelief~c(a4,a1,a1,a4,a1,a5,a4,a1,a1,a1,a5,a3,a1,a4,a2,a1,a1,a5,a1,a5,a1,a5)*SelfTran+
          c(b4,b1,b1,b4,b1,b5,b4,b1,b1,b1,b5,b3,b1,b4,b2,b1,b1,b5,b1,b5,b1,b5)*Conser+
          c(c4,c1,c1,c4,c1,c5,c4,c1,c1,c1,c5,c3,c1,c4,c2,c1,c1,c5,c1,c5,c1,c5)*SelfEnhan

CCPolicySupport~c(d4,d1,d1,d4,d1,d5,d4,d1,d1,d1,d5,d3,d1,d4,d2,d1,d1,d5,d1,d5,d1,d5)*CCBelief+
                c(e4,e1,e1,e4,e1,e5,e4,e1,e1,e1,e5,e3,e1,e4,e2,e1,e1,e5,e1,e5,e1,e5)*SelfTran+
                c(f4,f1,f1,f4,f1,f5,f4,f1,f1,f1,f5,f3,f1,f4,f2,f1,f1,f5,f1,f5,f1,f5)*Conser+
                c(g4,g1,g1,g4,g1,g5,g4,g1,g1,g1,g5,g3,g1,g4,g2,g1,g1,g5,g1,g5,g1,g5)*SelfEnhan
'

Mediation.SAM.5clus.NoHU<-cfa(model = sam_mediation_5clus_NoHU,
                              sample.cov = Var_eta,
                              sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/NoHU_Mediation_5clus.txt")
summary(Mediation.SAM.5clus.NoHU, fit.measures=T, standardized=T)
sink()



###----------------------------------------------------------------------------------
##mapping
##5 cluster solution:

##take out the world map
world_map<-map_data("world")

##filter to be a eu map:
eu_countries <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", 
  "Ireland", "Iceland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
  "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russia",
  "Slovakia", "Slovenia", "Switzerland",
  "Spain", "Sweden", "UK", "Israel",
  "Turkey", "Lebanon", "Jordan", "Egypt", "Syria",
  "Ukraine", "Belarus", "Georgia", "Armenia", "Azerbaijan", "Moldova"
)

eu_map <- world_map %>%
  filter(region %in% eu_countries)

##add a new column called region to match the country names with the world map data country names
ClusterRes.5clus<-ClusterRes.5clus %>%
  mutate(region=case_when(
    country == "AT" ~ "Austria",
    country == "BE" ~ "Belgium",
    country == "CH" ~ "Switzerland",
    country == "CZ" ~ "Czech Republic",
    country == "DE" ~ "Germany",
    country == "EE" ~ "Estonia",
    country == "ES" ~ "Spain",
    country == "FI" ~ "Finland",
    country == "FR" ~ "France",
    country == "GB" ~ "UK",
    country == "HU" ~ "Hungary",
    country == "IE" ~ "Ireland",
    country == "IL" ~ "Israel",
    country == "IS" ~ "Iceland",
    country == "IT" ~ "Italy",
    country == "LT" ~ "Lithuania",
    country == "NL" ~ "Netherlands",
    country == "NO" ~ "Norway",
    country == "PL" ~ "Poland",
    country == "PT" ~ "Portugal",
    country == "RU" ~ "Russia",
    country == "SE" ~ "Sweden",
    country == "SI" ~ "Slovenia"
  )) %>%
  select(ClusMembership, region)

##merge the data:
map_with_5clusters <- eu_map %>%
  left_join(ClusterRes.5clus, by = "region")

##lay out on the map:
ggplot(map_with_5clusters, aes(long, lat, group = group, fill = factor(ClusMembership))) +
  geom_polygon(color = "white") +
  labs(
    title = "Clustering Results on the Map",
    fill = "Cluster"
  ) +
  theme_minimal()

##mapping with translation of CCBelief into CCPolSupport:
map_with_5clusters <- map_with_5clusters %>%
  mutate(CCBelief_Into_CCPolSupport=case_when(
    ClusMembership == 1 ~ "medium-strong",
    ClusMembership == 2 ~ "medium-weak",
    ClusMembership == 3 ~ "strongest",
    ClusMembership == 4 ~ "medium-weak",
    ClusMembership == 5 ~ "medium-weak"
  ))

map_with_5clusters$CCBelief_Into_CCPolSupport<-factor(map_with_5clusters$CCBelief_Into_CCPolSupport,
                                                      levels = c("medium-weak","medium-strong","strongest"))


ggplot(map_with_5clusters, aes(long, lat, group = group, fill = CCBelief_Into_CCPolSupport)) +
  geom_polygon(color = "white") +
  labs(
    title = "Clustering Results on the Map",
    fill = "CCBelief into CCPolSupport"
  ) +
  theme_minimal()



##mapping within the medium-weak group:
map_with_5clusters <- map_with_5clusters %>%
  mutate(Characteristics=case_when(
    ClusMembership == 1 ~ NA,
    ClusMembership == 2 ~ "Strong Neg. Ind. SE",
    ClusMembership == 3 ~ NA,
    ClusMembership == 4 ~ "stronger ind. ST_Con",
    ClusMembership == 5 ~ "weaker ind. ST_Con"
  ))


ggplot(map_with_5clusters, aes(long, lat, group = group, fill = factor(Characteristics))) +
  geom_polygon(color = "white") +
  labs(
    title = "Clustering Results on the Map",
    fill = "characteristics"
  ) +
  theme_minimal()



#####################################################################################
########## Single Indicator Approach for Basic Model: CCBelief ######################
#####################################################################################


###------------------------------------------------------------------------------------
##Start with the Human Values model:
#
##Original model with cross-loadings and allow covariance between factors are as below:
NoOpen.HV.Metric.M2.Marker<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit2.Marker<-cfa(model = NoOpen.HV.Metric.M2.Marker,
                                  data = ESS8,
                                  group = "country",
                                  estimator="MLR",
                                  missing="FIML",
                                  group.equal="loadings",
                                  group.partial=c("SelfEnhan=~SE3"))

##Extract for each group:
#the factor covariance matrix Φ 
#the factor loadings Λ
#the unique variance θ
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
Phi_HV<-lapply(EST_HV, "[[", "psi")
lambda_HV<-lapply(EST_HV, "[[","lambda")
theta_HV<-lapply(EST_HV, "[[","theta")

##Take only the diagonal from the Phi_HV matrix:
Phi_HV<-lapply(Phi_HV, function(x) diag(diag(x)))

##compute the factor score matrix with regression Ar for each group:
##compute the factor scores for each group:
#
AR_Matrix_HV<-vector(mode = "list", length=length(unique(ESS8$country)))
y_HV_items<-vector(mode = "list", length=length(unique(ESS8$country)))
FactorScores_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  #factor score matrix
  AR_Matrix_HV[[g]]<-solve(solve(Phi_HV[[g]])+t(lambda_HV[[g]]) %*% solve(theta_HV[[g]]) %*% lambda_HV[[g]]) %*% t(lambda_HV[[g]]) %*% solve(theta_HV[[g]])
  
  ##factor scores
  y_HV_items[[g]]<-ESS8[ESS8$country==unique(ESS8$country)[g],colnames(AR_Matrix_HV[[g]])]
  FactorScores_HV[[g]]<-AR_Matrix_HV[[g]] %*% t(y_HV_items[[g]])
  }

##compute the posterior Varf in equation 2.3
ModelImplied_Sigma_HV<-vector(mode = "list", length=length(unique(ESS8$country)))
Posterior_Varf_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  ##before computing the posterior Varf, first compute the model-implied covariance matrix for each group:
  ModelImplied_Sigma_HV[[g]]<-lambda_HV[[g]] %*% Phi_HV[[g]] %*% t(lambda_HV[[g]])+theta_HV[[g]]
  
  ##posterior Varf:
  Posterior_Varf_HV[[g]]<-Phi_HV[[g]]-t(Phi_HV[[g]]) %*% t(lambda_HV[[g]]) %*% solve(ModelImplied_Sigma_HV[[g]]) %*% lambda_HV[[g]] %*% Phi_HV[[g]]
}

##Take only the diagonal of the Posterior Varf for the next step:
Posterior_Varf_HV<-lapply(Posterior_Varf_HV, function(x) diag(diag(x)))

##request covariance matrix from the factor scores to constrcut var(f)
Var_f_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
    Var_f_HV[[g]]<-cov(t(FactorScores_HV[[g]]), use = "pairwise.complete.obs")
}

##Take only the diagonal of the var(f)
Var_f_HV<-lapply(Var_f_HV, function(x) diag(diag(x)))


##compute the true factor variance 
TruePhi_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  TruePhi_HV[[g]]<-Var_f_HV[[g]]+Posterior_Varf_HV[[g]]
}

##compute the ρ:
Rho_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Rho_HV[[g]]<-Var_f_HV[[g]]%*% solve(TruePhi_HV[[g]])
}

##Compute the residual variances for the single indicator:
Theta_g_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Theta_g_HV[[g]]<-TruePhi_HV[[g]] %*% Rho_HV[[g]] %*% (diag(1, nrow = 3, ncol = 3)-Rho_HV[[g]])
  
}



###------------------------------------------------------------------------------------
##Repeat the same steps with the Climate Change Belief:
##Original (Full) Metric Model 1: 
##Can be used directly for the single indicator approach
CCBelief.Metric.M1.Marker<-'
CCBelief=~ImpactBelief+TrendBelief+AttriBelief
'

CCBelief.Metric.Fit1.Marker<-cfa(model = CCBelief.Metric.M1.Marker,
                                 data = ESS8,
                                 group = "country",
                                 estimator="MLR",
                                 missing="FIML",
                                 group.equal="loadings")


##Extract for each group:
#the factor covariance matrix Φ 
#the factor loadings Λ
#the unique variance θ
EST_CCBelief<-lavInspect(CCBelief.Metric.Fit1.Marker, what = "est")
Phi_CCBelief<-lapply(EST_CCBelief, "[[", "psi")
lambda_CCBelief<-lapply(EST_CCBelief, "[[","lambda")
theta_CCBelief<-lapply(EST_CCBelief, "[[","theta")

##compute the factor score matrix with regression Ar for each group:
##compute the factor scores for each group:
#
AR_Matrix_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))
y_CCBelief_items<-vector(mode = "list", length=length(unique(ESS8$country)))
FactorScores_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  #factor score matrix
  AR_Matrix_CCBelief[[g]]<-solve(solve(Phi_CCBelief[[g]])+t(lambda_CCBelief[[g]]) %*% solve(theta_CCBelief[[g]]) %*% lambda_CCBelief[[g]]) %*% t(lambda_CCBelief[[g]]) %*% solve(theta_CCBelief[[g]])
  
  ##factor scores
  y_CCBelief_items[[g]]<-ESS8[ESS8$country==unique(ESS8$country)[g],colnames(AR_Matrix_CCBelief[[g]])]
  FactorScores_CCBelief[[g]]<-AR_Matrix_CCBelief[[g]] %*% t(y_CCBelief_items[[g]])
}

##compute the posterior Varf in equation 2.3
ModelImplied_Sigma_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))
Posterior_Varf_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  ##before computing the posterior Varf, first compute the model-implied covariance matrix for each group:
  ModelImplied_Sigma_CCBelief[[g]]<-lambda_CCBelief[[g]] %*% Phi_CCBelief[[g]] %*% t(lambda_CCBelief[[g]])+theta_CCBelief[[g]]
  
  ##posterior Varf:
  Posterior_Varf_CCBelief[[g]]<-Phi_CCBelief[[g]]-t(Phi_CCBelief[[g]]) %*% t(lambda_CCBelief[[g]]) %*% solve(ModelImplied_Sigma_CCBelief[[g]]) %*% lambda_CCBelief[[g]] %*% Phi_CCBelief[[g]]
}

##request covariance matrix from the factor scores to constrcut var(f)
Var_f_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  Var_f_CCBelief[[g]]<-cov(t(FactorScores_CCBelief[[g]]), use = "pairwise.complete.obs")
}


##compute the true factor variance 
TruePhi_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  TruePhi_CCBelief[[g]]<-Var_f_CCBelief[[g]]+Posterior_Varf_CCBelief[[g]]
}

##compute the ρ:
Rho_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Rho_CCBelief[[g]]<-Var_f_CCBelief[[g]]%*% solve(TruePhi_CCBelief[[g]])
}

##Compute the residual variances for the single indicator:
Theta_g_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Theta_g_CCBelief[[g]]<-TruePhi_CCBelief[[g]] * Rho_CCBelief[[g]] %*% (diag(1, nrow = 1, ncol = 1)-Rho_CCBelief[[g]])
  
}




###------------------------------------------------------------------------------------
##At this point, we need to put each pair together in the same matrix:
#Rho_HV, Rho_CCBelief --> Rho_g
#Theta_g_HV, Theta_g_CCbelief --> Theta_g
#for Cov_fg: we need to first put the factor scores together and then ask for the Cov_fg

Rho_g<-vector(mode = "list", length=length(unique(ESS8$country)))
Theta_g<-vector(mode = "list", length=length(unique(ESS8$country)))
factorScore_g<-vector(mode = "list", length=length(unique(ESS8$country)))
Cov_fg<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  ##first focus on Rho_g
  Rho_g[[g]]<-lav_matrix_bdiag(Rho_HV[[g]], Rho_CCBelief[[g]])
  colnames(Rho_g[[g]])<-rownames(Rho_g[[g]])<-c(rownames(AR_Matrix_HV[[g]]), rownames(AR_Matrix_CCBelief[[g]]))
  
  ##then focus on Theta_g
  Theta_g[[g]]<-lav_matrix_bdiag(Theta_g_HV[[g]], Theta_g_CCBelief[[g]])
  colnames(Theta_g[[g]])<-rownames(Theta_g[[g]])<-c(rownames(AR_Matrix_HV[[g]]), rownames(AR_Matrix_CCBelief[[g]]))
  
  ##lastly focus on Cov_fg
  #first put the factor scores together:
  factorScore_g[[g]]<-rbind(FactorScores_HV[[g]],FactorScores_CCBelief[[g]])
  rownames(factorScore_g[[g]])<-c(rownames(AR_Matrix_HV[[g]]), rownames(AR_Matrix_CCBelief[[g]]))
  
  Cov_fg[[g]]<-cov(t(factorScore_g[[g]]), use = "pairwise.complete.obs")
}

##compute the factor covariance matrix that could be used to estimate the structural model through lavaan
Phi_BasicModel_Step2<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  Phi_BasicModel_Step2[[g]]<-solve(Rho_g[[g]]) %*% (Cov_fg[[g]]-Theta_g[[g]]) %*% solve(t(Rho_g[[g]]))
}




###----------------------------------------------------------------------------------------------------------
##Take out LT as the sample covariance matrix is not positive-definite
ESS_noLT<-ESS8 %>%
  filter(country != "LT")

##Take 16th out of the Phi_BasicModel_Step2
NEWPhi_BasicModel_Step2<-Phi_BasicModel_Step2[-16]

##To extract the new nobs
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCBelief=~ImpactBelief+TrendBelief+AttriBelief
'

fake<-cfa(model = fake_model,
          data = ESS_noLT,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F) ##to extract the nobs per country


##run the Structural model without LT
SI_str_model_5clus.150s.FIML<-'
CCBelief~c(a1,a4,a1,a5,a1,a4,a2,a2,a2,a2,a1,a4,a5,a2,a5,a2,a2,a1,a1,a4,a2,a4)*SelfTran+
          c(b1,b4,b1,b5,b1,b4,b2,b2,b2,b2,b1,b4,b5,b2,b5,b2,b2,b1,b1,b4,b2,b4)*Conser+
          c(c1,c4,c1,c5,c1,c4,c2,c2,c2,c2,c1,c4,c5,c2,c5,c2,c2,c1,c1,c4,c2,c4)*SelfEnhan
'

BasicModel.SI.5clus.150s.FIML<-cfa(model = SI_str_model_5clus.150s.FIML,
                                    sample.cov = NEWPhi_BasicModel_Step2,
                                    sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/BasicModel_SingleIndicator_5clus_noLT.txt")
summary(BasicModel.SI.5clus.150s.FIML, fit.measures=T, standardized=T)
sink()






#####################################################################################
##### Single Indicator Approach for Basic Model: Full Metric CCPolSupport ###########
#####################################################################################

###------------------------------------------------------------------------------------
##Start with the Human Values model:
#
##Original model with cross-loadings and allow covariance between factors are as below:
NoOpen.HV.Metric.M2.Marker<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit2.Marker<-cfa(model = NoOpen.HV.Metric.M2.Marker,
                                  data = ESS8,
                                  group = "country",
                                  estimator="MLR",
                                  missing="FIML",
                                  group.equal="loadings",
                                  group.partial=c("SelfEnhan=~SE3"))

##Extract for each group:
#the factor covariance matrix Φ 
#the factor loadings Λ
#the unique variance θ
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
Phi_HV<-lapply(EST_HV, "[[", "psi")
lambda_HV<-lapply(EST_HV, "[[","lambda")
theta_HV<-lapply(EST_HV, "[[","theta")

##Take only the diagonal from the Phi_HV matrix:
Phi_HV<-lapply(Phi_HV, function(x) diag(diag(x)))

##compute the factor score matrix with regression Ar for each group:
##compute the factor scores for each group:
#
AR_Matrix_HV<-vector(mode = "list", length=length(unique(ESS8$country)))
y_HV_items<-vector(mode = "list", length=length(unique(ESS8$country)))
FactorScores_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  #factor score matrix
  AR_Matrix_HV[[g]]<-solve(solve(Phi_HV[[g]])+t(lambda_HV[[g]]) %*% solve(theta_HV[[g]]) %*% lambda_HV[[g]]) %*% t(lambda_HV[[g]]) %*% solve(theta_HV[[g]])
  
  ##factor scores
  y_HV_items[[g]]<-ESS8[ESS8$country==unique(ESS8$country)[g],colnames(AR_Matrix_HV[[g]])]
  FactorScores_HV[[g]]<-AR_Matrix_HV[[g]] %*% t(y_HV_items[[g]])
}

##compute the posterior Varf in equation 2.3
ModelImplied_Sigma_HV<-vector(mode = "list", length=length(unique(ESS8$country)))
Posterior_Varf_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  ##before computing the posterior Varf, first compute the model-implied covariance matrix for each group:
  ModelImplied_Sigma_HV[[g]]<-lambda_HV[[g]] %*% Phi_HV[[g]] %*% t(lambda_HV[[g]])+theta_HV[[g]]
  
  ##posterior Varf:
  Posterior_Varf_HV[[g]]<-Phi_HV[[g]]-t(Phi_HV[[g]]) %*% t(lambda_HV[[g]]) %*% solve(ModelImplied_Sigma_HV[[g]]) %*% lambda_HV[[g]] %*% Phi_HV[[g]]
}

##Take only the diagonal of the Posterior Varf for the next step:
Posterior_Varf_HV<-lapply(Posterior_Varf_HV, function(x) diag(diag(x)))

##request covariance matrix from the factor scores to constrcut var(f)
Var_f_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  Var_f_HV[[g]]<-cov(t(FactorScores_HV[[g]]), use = "pairwise.complete.obs")
}

##Take only the diagonal of the var(f)
Var_f_HV<-lapply(Var_f_HV, function(x) diag(diag(x)))


##compute the true factor variance 
TruePhi_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  TruePhi_HV[[g]]<-Var_f_HV[[g]]+Posterior_Varf_HV[[g]]
}

##compute the ρ:
Rho_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Rho_HV[[g]]<-Var_f_HV[[g]]%*% solve(TruePhi_HV[[g]])
}

##Compute the residual variances for the single indicator:
Theta_g_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Theta_g_HV[[g]]<-TruePhi_HV[[g]] %*% Rho_HV[[g]] %*% (diag(1, nrow = 3, ncol = 3)-Rho_HV[[g]])
  
}



###------------------------------------------------------------------------------------
##Repeat the same steps with the full metric Climate Change Policy Support:
CCPolSupport.Metric.M1.Marker<-'
CCPolicySupport=~support2+support1+support3
'

CCPolSupport.Metric.Fit1.Marker<-cfa(model = CCPolSupport.Metric.M1.Marker,
                                     data = ESS8,
                                     group = "country",
                                     estimator="MLR",
                                     missing="FIML",
                                     group.equal="loadings")



##Extract for each group:
#the factor covariance matrix Φ 
#the factor loadings Λ
#the unique variance θ
EST_CCPolSupport<-lavInspect(CCPolSupport.Metric.Fit1.Marker, what = "est")
Phi_CCPolSupport<-lapply(EST_CCPolSupport, "[[", "psi")
lambda_CCPolSupport<-lapply(EST_CCPolSupport, "[[","lambda")
theta_CCPolSupport<-lapply(EST_CCPolSupport, "[[","theta")

##compute the factor score matrix with regression Ar for each group:
##compute the factor scores for each group:
#
AR_Matrix_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))
y_CCPolSupport_items<-vector(mode = "list", length=length(unique(ESS8$country)))
FactorScores_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  #factor score matrix
  AR_Matrix_CCPolSupport[[g]]<-solve(solve(Phi_CCPolSupport[[g]])+t(lambda_CCPolSupport[[g]]) %*% solve(theta_CCPolSupport[[g]]) %*% lambda_CCPolSupport[[g]]) %*% t(lambda_CCPolSupport[[g]]) %*% solve(theta_CCPolSupport[[g]])
  
  ##factor scores
  y_CCPolSupport_items[[g]]<-ESS8[ESS8$country==unique(ESS8$country)[g],colnames(AR_Matrix_CCPolSupport[[g]])]
  FactorScores_CCPolSupport[[g]]<-AR_Matrix_CCPolSupport[[g]] %*% t(y_CCPolSupport_items[[g]])
}

##compute the posterior Varf in equation 2.3
ModelImplied_Sigma_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))
Posterior_Varf_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  ##before computing the posterior Varf, first compute the model-implied covariance matrix for each group:
  ModelImplied_Sigma_CCPolSupport[[g]]<-lambda_CCPolSupport[[g]] %*% Phi_CCPolSupport[[g]] %*% t(lambda_CCPolSupport[[g]])+theta_CCPolSupport[[g]]
  
  ##posterior Varf:
  Posterior_Varf_CCPolSupport[[g]]<-Phi_CCPolSupport[[g]]-t(Phi_CCPolSupport[[g]]) %*% t(lambda_CCPolSupport[[g]]) %*% solve(ModelImplied_Sigma_CCPolSupport[[g]]) %*% lambda_CCPolSupport[[g]] %*% Phi_CCPolSupport[[g]]
}

##request covariance matrix from the factor scores to constrcut var(f)
Var_f_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  Var_f_CCPolSupport[[g]]<-cov(t(FactorScores_CCPolSupport[[g]]), use = "pairwise.complete.obs")
}


##compute the true factor variance 
TruePhi_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  TruePhi_CCPolSupport[[g]]<-Var_f_CCPolSupport[[g]]+Posterior_Varf_CCPolSupport[[g]]
}

##compute the ρ:
Rho_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Rho_CCPolSupport[[g]]<-Var_f_CCPolSupport[[g]]%*% solve(TruePhi_CCPolSupport[[g]])
}

##Compute the residual variances for the single indicator:
Theta_g_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Theta_g_CCPolSupport[[g]]<-TruePhi_CCPolSupport[[g]] * Rho_CCPolSupport[[g]] %*% (diag(1, nrow = 1, ncol = 1)-Rho_CCPolSupport[[g]])
  
}



###------------------------------------------------------------------------------------
##At this point, we need to put each pair together in the same matrix:
#Rho_HV, Rho_CCPolSupport --> Rho_g
#Theta_g_HV, Theta_g_CCPolSupport --> Theta_g
#for Cov_fg: we need to first put the factor scores together and then ask for the Cov_fg

Rho_g<-vector(mode = "list", length=length(unique(ESS8$country)))
Theta_g<-vector(mode = "list", length=length(unique(ESS8$country)))
factorScore_g<-vector(mode = "list", length=length(unique(ESS8$country)))
Cov_fg<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  ##first focus on Rho_g
  Rho_g[[g]]<-lav_matrix_bdiag(Rho_HV[[g]], Rho_CCPolSupport[[g]])
  colnames(Rho_g[[g]])<-rownames(Rho_g[[g]])<-c(rownames(AR_Matrix_HV[[g]]), rownames(AR_Matrix_CCPolSupport[[g]]))
  
  ##then focus on Theta_g
  Theta_g[[g]]<-lav_matrix_bdiag(Theta_g_HV[[g]], Theta_g_CCPolSupport[[g]])
  colnames(Theta_g[[g]])<-rownames(Theta_g[[g]])<-c(rownames(AR_Matrix_HV[[g]]), rownames(AR_Matrix_CCPolSupport[[g]]))
  
  ##lastly focus on Cov_fg
  #first put the factor scores together:
  factorScore_g[[g]]<-rbind(FactorScores_HV[[g]],FactorScores_CCPolSupport[[g]])
  rownames(factorScore_g[[g]])<-c(rownames(AR_Matrix_HV[[g]]), rownames(AR_Matrix_CCPolSupport[[g]]))
  
  Cov_fg[[g]]<-cov(t(factorScore_g[[g]]), use = "pairwise.complete.obs")
}

##compute the factor covariance matrix that could be used to estimate the structural model through lavaan
Phi_BasicModel_Step2<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  Phi_BasicModel_Step2[[g]]<-solve(Rho_g[[g]]) %*% (Cov_fg[[g]]-Theta_g[[g]]) %*% solve(t(Rho_g[[g]]))
}


###----------------------------------------------------------------------------------------------------------
##Take out LT as the sample covariance matrix is not positive-definite
ESS_noLT<-ESS8 %>%
  filter(country != "LT")

##Take 16th out of the Phi_BasicModel_Step2
NEWPhi_BasicModel_Step2<-Phi_BasicModel_Step2[-16]

##To extract the new nobs
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCPolicySupport=~support2+support1+support3
'

fake<-cfa(model = fake_model,
          data = ESS_noLT,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F) ##to extract the nobs per country


##run the Structural model without LT
SI_CCPolSupportModel_3clus<-'
CCPolicySupport~c(a3,a3,a3,a3,a3,a3,a3,a3,a3,a3,a2,a2,a2,a2,a3,a3,a3,a2,a3,a2,a3,a3)*SelfTran+
                c(b3,b3,b3,b3,b3,b3,b3,b3,b3,b3,b2,b2,b2,b2,b3,b3,b3,b2,b3,b2,b3,b3)*Conser+
                c(c3,c3,c3,c3,c3,c3,c3,c3,c3,c3,c2,c2,c2,c2,c3,c3,c3,c2,c3,c2,c3,c3)*SelfEnhan
'

CCPolSupport.SI.3clus.fit<-cfa(model = SI_CCPolSupportModel_3clus,
                                   sample.cov = NEWPhi_BasicModel_Step2,
                                   sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/CCPolSupport_BasicModel_SingleIndicator_3clus_noLT.txt")
summary(CCPolSupport.SI.3clus.fit, fit.measures=T, standardized=T)
sink()




#####################################################################################
##### Single Indicator Approach for Basic Model: Partial Metric CCPolSupport ###########
#####################################################################################

###------------------------------------------------------------------------------------
##Start with the Human Values model:
#
##Original model with cross-loadings and allow covariance between factors are as below:
NoOpen.HV.Metric.M2.Marker<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit2.Marker<-cfa(model = NoOpen.HV.Metric.M2.Marker,
                                  data = ESS8,
                                  group = "country",
                                  estimator="MLR",
                                  missing="FIML",
                                  group.equal="loadings",
                                  group.partial=c("SelfEnhan=~SE3"))

##Extract for each group:
#the factor covariance matrix Φ 
#the factor loadings Λ
#the unique variance θ
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
Phi_HV<-lapply(EST_HV, "[[", "psi")
lambda_HV<-lapply(EST_HV, "[[","lambda")
theta_HV<-lapply(EST_HV, "[[","theta")

##Take only the diagonal from the Phi_HV matrix:
Phi_HV<-lapply(Phi_HV, function(x) diag(diag(x)))

##compute the factor score matrix with regression Ar for each group:
##compute the factor scores for each group:
#
AR_Matrix_HV<-vector(mode = "list", length=length(unique(ESS8$country)))
y_HV_items<-vector(mode = "list", length=length(unique(ESS8$country)))
FactorScores_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  #factor score matrix
  AR_Matrix_HV[[g]]<-solve(solve(Phi_HV[[g]])+t(lambda_HV[[g]]) %*% solve(theta_HV[[g]]) %*% lambda_HV[[g]]) %*% t(lambda_HV[[g]]) %*% solve(theta_HV[[g]])
  
  ##factor scores
  y_HV_items[[g]]<-ESS8[ESS8$country==unique(ESS8$country)[g],colnames(AR_Matrix_HV[[g]])]
  FactorScores_HV[[g]]<-AR_Matrix_HV[[g]] %*% t(y_HV_items[[g]])
}

##compute the posterior Varf in equation 2.3
ModelImplied_Sigma_HV<-vector(mode = "list", length=length(unique(ESS8$country)))
Posterior_Varf_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  ##before computing the posterior Varf, first compute the model-implied covariance matrix for each group:
  ModelImplied_Sigma_HV[[g]]<-lambda_HV[[g]] %*% Phi_HV[[g]] %*% t(lambda_HV[[g]])+theta_HV[[g]]
  
  ##posterior Varf:
  Posterior_Varf_HV[[g]]<-Phi_HV[[g]]-t(Phi_HV[[g]]) %*% t(lambda_HV[[g]]) %*% solve(ModelImplied_Sigma_HV[[g]]) %*% lambda_HV[[g]] %*% Phi_HV[[g]]
}

##Take only the diagonal of the Posterior Varf for the next step:
Posterior_Varf_HV<-lapply(Posterior_Varf_HV, function(x) diag(diag(x)))

##request covariance matrix from the factor scores to constrcut var(f)
Var_f_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  Var_f_HV[[g]]<-cov(t(FactorScores_HV[[g]]), use = "pairwise.complete.obs")
}

##Take only the diagonal of the var(f)
Var_f_HV<-lapply(Var_f_HV, function(x) diag(diag(x)))


##compute the true factor variance 
TruePhi_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  TruePhi_HV[[g]]<-Var_f_HV[[g]]+Posterior_Varf_HV[[g]]
}

##compute the ρ:
Rho_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Rho_HV[[g]]<-Var_f_HV[[g]]%*% solve(TruePhi_HV[[g]])
}

##Compute the residual variances for the single indicator:
Theta_g_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Theta_g_HV[[g]]<-TruePhi_HV[[g]] %*% Rho_HV[[g]] %*% (diag(1, nrow = 3, ncol = 3)-Rho_HV[[g]])
  
}



###------------------------------------------------------------------------------------
##Repeat the same steps with the partial metric Climate Change Policy Support:
CCPolSupport.Metric.M2.Marker<-'
CCPolicySupport=~support2+support1+support3
'

CCPolSupport.Metric.Fit2.Marker.WideBound<-cfa(model = CCPolSupport.Metric.M2.Marker,
                                               data = ESS8,
                                               group = "country",
                                               estimator="MLR",
                                               missing="FIML",
                                               bounds="wide",
                                               group.equal="loadings",
                                               group.partial=c("CCPolicySupport=~support3"))




##Extract for each group:
#the factor covariance matrix Φ 
#the factor loadings Λ
#the unique variance θ
EST_CCPolSupport<-lavInspect(CCPolSupport.Metric.Fit2.Marker.WideBound, what = "est")
Phi_CCPolSupport<-lapply(EST_CCPolSupport, "[[", "psi")
lambda_CCPolSupport<-lapply(EST_CCPolSupport, "[[","lambda")
theta_CCPolSupport<-lapply(EST_CCPolSupport, "[[","theta")

##compute the factor score matrix with regression Ar for each group:
##compute the factor scores for each group:
#
AR_Matrix_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))
y_CCPolSupport_items<-vector(mode = "list", length=length(unique(ESS8$country)))
FactorScores_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  #factor score matrix
  AR_Matrix_CCPolSupport[[g]]<-solve(solve(Phi_CCPolSupport[[g]])+t(lambda_CCPolSupport[[g]]) %*% solve(theta_CCPolSupport[[g]]) %*% lambda_CCPolSupport[[g]]) %*% t(lambda_CCPolSupport[[g]]) %*% solve(theta_CCPolSupport[[g]])
  
  ##factor scores
  y_CCPolSupport_items[[g]]<-ESS8[ESS8$country==unique(ESS8$country)[g],colnames(AR_Matrix_CCPolSupport[[g]])]
  FactorScores_CCPolSupport[[g]]<-AR_Matrix_CCPolSupport[[g]] %*% t(y_CCPolSupport_items[[g]])
}

##compute the posterior Varf in equation 2.3
ModelImplied_Sigma_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))
Posterior_Varf_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  ##before computing the posterior Varf, first compute the model-implied covariance matrix for each group:
  ModelImplied_Sigma_CCPolSupport[[g]]<-lambda_CCPolSupport[[g]] %*% Phi_CCPolSupport[[g]] %*% t(lambda_CCPolSupport[[g]])+theta_CCPolSupport[[g]]
  
  ##posterior Varf:
  Posterior_Varf_CCPolSupport[[g]]<-Phi_CCPolSupport[[g]]-t(Phi_CCPolSupport[[g]]) %*% t(lambda_CCPolSupport[[g]]) %*% solve(ModelImplied_Sigma_CCPolSupport[[g]]) %*% lambda_CCPolSupport[[g]] %*% Phi_CCPolSupport[[g]]
}

##request covariance matrix from the factor scores to constrcut var(f)
Var_f_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  Var_f_CCPolSupport[[g]]<-cov(t(FactorScores_CCPolSupport[[g]]), use = "pairwise.complete.obs")
}


##compute the true factor variance 
TruePhi_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  TruePhi_CCPolSupport[[g]]<-Var_f_CCPolSupport[[g]]+Posterior_Varf_CCPolSupport[[g]]
}

##compute the ρ:
Rho_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Rho_CCPolSupport[[g]]<-Var_f_CCPolSupport[[g]]%*% solve(TruePhi_CCPolSupport[[g]])
}

##Compute the residual variances for the single indicator:
Theta_g_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Theta_g_CCPolSupport[[g]]<-TruePhi_CCPolSupport[[g]] * Rho_CCPolSupport[[g]] %*% (diag(1, nrow = 1, ncol = 1)-Rho_CCPolSupport[[g]])
  
}



###------------------------------------------------------------------------------------
##At this point, we need to put each pair together in the same matrix:
#Rho_HV, Rho_CCPolSupport --> Rho_g
#Theta_g_HV, Theta_g_CCPolSupport --> Theta_g
#for Cov_fg: we need to first put the factor scores together and then ask for the Cov_fg

Rho_g<-vector(mode = "list", length=length(unique(ESS8$country)))
Theta_g<-vector(mode = "list", length=length(unique(ESS8$country)))
factorScore_g<-vector(mode = "list", length=length(unique(ESS8$country)))
Cov_fg<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  ##first focus on Rho_g
  Rho_g[[g]]<-lav_matrix_bdiag(Rho_HV[[g]], Rho_CCPolSupport[[g]])
  colnames(Rho_g[[g]])<-rownames(Rho_g[[g]])<-c(rownames(AR_Matrix_HV[[g]]), rownames(AR_Matrix_CCPolSupport[[g]]))
  
  ##then focus on Theta_g
  Theta_g[[g]]<-lav_matrix_bdiag(Theta_g_HV[[g]], Theta_g_CCPolSupport[[g]])
  colnames(Theta_g[[g]])<-rownames(Theta_g[[g]])<-c(rownames(AR_Matrix_HV[[g]]), rownames(AR_Matrix_CCPolSupport[[g]]))
  
  ##lastly focus on Cov_fg
  #first put the factor scores together:
  factorScore_g[[g]]<-rbind(FactorScores_HV[[g]],FactorScores_CCPolSupport[[g]])
  rownames(factorScore_g[[g]])<-c(rownames(AR_Matrix_HV[[g]]), rownames(AR_Matrix_CCPolSupport[[g]]))
  
  Cov_fg[[g]]<-cov(t(factorScore_g[[g]]), use = "pairwise.complete.obs")
}

##compute the factor covariance matrix that could be used to estimate the structural model through lavaan
Phi_BasicModel_Step2<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  Phi_BasicModel_Step2[[g]]<-solve(Rho_g[[g]]) %*% (Cov_fg[[g]]-Theta_g[[g]]) %*% solve(t(Rho_g[[g]]))
}


###----------------------------------------------------------------------------------------------------------
##Take out LT as the sample covariance matrix is not positive-definite
ESS_noLT<-ESS8 %>%
  filter(country != "LT")

##Take 16th out of the Phi_BasicModel_Step2
NEWPhi_BasicModel_Step2<-Phi_BasicModel_Step2[-16]

##To extract the new nobs
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCPolicySupport=~support2+support1+support3
'

fake<-cfa(model = fake_model,
          data = ESS_noLT,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F) ##to extract the nobs per country


##run the Structural model without LT
SI_PartCCPolSupportModel_3clus<-'
CCPolicySupport~c(a3,a3,a3,a3,a3,a1,a3,a3,a3,a3,a2,a1,a1,a1,a3,a3,a3,a1,a3,a1,a3,a1)*SelfTran+
                c(b3,b3,b3,b3,b3,b1,b3,b3,b3,b3,b2,b1,b1,b1,b3,b3,b3,b1,b3,b1,b3,b1)*Conser+
                c(c3,c3,c3,c3,c3,c1,c3,c3,c3,c3,c2,c1,c1,c1,c3,c3,c3,c1,c3,c1,c3,c1)*SelfEnhan
'

PartCCPolSupport.SI.3clus.fit<-cfa(model = SI_PartCCPolSupportModel_3clus,
                               sample.cov = NEWPhi_BasicModel_Step2,
                               sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/PartCCPolSupport_BasicModel_SingleIndicator_3clus_noLT.txt")
summary(PartCCPolSupport.SI.3clus.fit, fit.measures=T, standardized=T)
sink()


#####################################################################################
##### Single Indicator Approach for Mediation: Full Metric CCPolSupport ###########
#####################################################################################

###------------------------------------------------------------------------------------
##Start with the Human Values model:
#
##Original model with cross-loadings and allow covariance between factors are as below:
NoOpen.HV.Metric.M2.Marker<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit2.Marker<-cfa(model = NoOpen.HV.Metric.M2.Marker,
                                  data = ESS8,
                                  group = "country",
                                  estimator="MLR",
                                  missing="FIML",
                                  group.equal="loadings",
                                  group.partial=c("SelfEnhan=~SE3"))

##Extract for each group:
#the factor covariance matrix Φ 
#the factor loadings Λ
#the unique variance θ
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit2.Marker, what = "est")
Phi_HV<-lapply(EST_HV, "[[", "psi")
lambda_HV<-lapply(EST_HV, "[[","lambda")
theta_HV<-lapply(EST_HV, "[[","theta")

##Take only the diagonal from the Phi_HV matrix:
Phi_HV<-lapply(Phi_HV, function(x) diag(diag(x)))

##compute the factor score matrix with regression Ar for each group:
##compute the factor scores for each group:
#
AR_Matrix_HV<-vector(mode = "list", length=length(unique(ESS8$country)))
y_HV_items<-vector(mode = "list", length=length(unique(ESS8$country)))
FactorScores_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  #factor score matrix
  AR_Matrix_HV[[g]]<-solve(solve(Phi_HV[[g]])+t(lambda_HV[[g]]) %*% solve(theta_HV[[g]]) %*% lambda_HV[[g]]) %*% t(lambda_HV[[g]]) %*% solve(theta_HV[[g]])
  
  ##factor scores
  y_HV_items[[g]]<-ESS8[ESS8$country==unique(ESS8$country)[g],colnames(AR_Matrix_HV[[g]])]
  FactorScores_HV[[g]]<-AR_Matrix_HV[[g]] %*% t(y_HV_items[[g]])
}

##compute the posterior Varf in equation 2.3
ModelImplied_Sigma_HV<-vector(mode = "list", length=length(unique(ESS8$country)))
Posterior_Varf_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  ##before computing the posterior Varf, first compute the model-implied covariance matrix for each group:
  ModelImplied_Sigma_HV[[g]]<-lambda_HV[[g]] %*% Phi_HV[[g]] %*% t(lambda_HV[[g]])+theta_HV[[g]]
  
  ##posterior Varf:
  Posterior_Varf_HV[[g]]<-Phi_HV[[g]]-t(Phi_HV[[g]]) %*% t(lambda_HV[[g]]) %*% solve(ModelImplied_Sigma_HV[[g]]) %*% lambda_HV[[g]] %*% Phi_HV[[g]]
}

##Take only the diagonal of the Posterior Varf for the next step:
Posterior_Varf_HV<-lapply(Posterior_Varf_HV, function(x) diag(diag(x)))

##request covariance matrix from the factor scores to constrcut var(f)
Var_f_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  Var_f_HV[[g]]<-cov(t(FactorScores_HV[[g]]), use = "pairwise.complete.obs")
}

##Take only the diagonal of the var(f)
Var_f_HV<-lapply(Var_f_HV, function(x) diag(diag(x)))


##compute the true factor variance 
TruePhi_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  TruePhi_HV[[g]]<-Var_f_HV[[g]]+Posterior_Varf_HV[[g]]
}

##compute the ρ:
Rho_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Rho_HV[[g]]<-Var_f_HV[[g]]%*% solve(TruePhi_HV[[g]])
}

##Compute the residual variances for the single indicator:
Theta_g_HV<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Theta_g_HV[[g]]<-TruePhi_HV[[g]] %*% Rho_HV[[g]] %*% (diag(1, nrow = 3, ncol = 3)-Rho_HV[[g]])
  
}


###------------------------------------------------------------------------------------
##Repeat the same steps with the Climate Change Belief:
##Original (Full) Metric Model 1: 
##Can be used directly for the single indicator approach
CCBelief.Metric.M1.Marker<-'
CCBelief=~ImpactBelief+TrendBelief+AttriBelief
'

CCBelief.Metric.Fit1.Marker<-cfa(model = CCBelief.Metric.M1.Marker,
                                 data = ESS8,
                                 group = "country",
                                 estimator="MLR",
                                 missing="FIML",
                                 group.equal="loadings")


##Extract for each group:
#the factor covariance matrix Φ 
#the factor loadings Λ
#the unique variance θ
EST_CCBelief<-lavInspect(CCBelief.Metric.Fit1.Marker, what = "est")
Phi_CCBelief<-lapply(EST_CCBelief, "[[", "psi")
lambda_CCBelief<-lapply(EST_CCBelief, "[[","lambda")
theta_CCBelief<-lapply(EST_CCBelief, "[[","theta")

##compute the factor score matrix with regression Ar for each group:
##compute the factor scores for each group:
#
AR_Matrix_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))
y_CCBelief_items<-vector(mode = "list", length=length(unique(ESS8$country)))
FactorScores_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  #factor score matrix
  AR_Matrix_CCBelief[[g]]<-solve(solve(Phi_CCBelief[[g]])+t(lambda_CCBelief[[g]]) %*% solve(theta_CCBelief[[g]]) %*% lambda_CCBelief[[g]]) %*% t(lambda_CCBelief[[g]]) %*% solve(theta_CCBelief[[g]])
  
  ##factor scores
  y_CCBelief_items[[g]]<-ESS8[ESS8$country==unique(ESS8$country)[g],colnames(AR_Matrix_CCBelief[[g]])]
  FactorScores_CCBelief[[g]]<-AR_Matrix_CCBelief[[g]] %*% t(y_CCBelief_items[[g]])
}

##compute the posterior Varf in equation 2.3
ModelImplied_Sigma_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))
Posterior_Varf_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  ##before computing the posterior Varf, first compute the model-implied covariance matrix for each group:
  ModelImplied_Sigma_CCBelief[[g]]<-lambda_CCBelief[[g]] %*% Phi_CCBelief[[g]] %*% t(lambda_CCBelief[[g]])+theta_CCBelief[[g]]
  
  ##posterior Varf:
  Posterior_Varf_CCBelief[[g]]<-Phi_CCBelief[[g]]-t(Phi_CCBelief[[g]]) %*% t(lambda_CCBelief[[g]]) %*% solve(ModelImplied_Sigma_CCBelief[[g]]) %*% lambda_CCBelief[[g]] %*% Phi_CCBelief[[g]]
}

##request covariance matrix from the factor scores to constrcut var(f)
Var_f_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  Var_f_CCBelief[[g]]<-cov(t(FactorScores_CCBelief[[g]]), use = "pairwise.complete.obs")
}


##compute the true factor variance 
TruePhi_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  TruePhi_CCBelief[[g]]<-Var_f_CCBelief[[g]]+Posterior_Varf_CCBelief[[g]]
}

##compute the ρ:
Rho_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Rho_CCBelief[[g]]<-Var_f_CCBelief[[g]]%*% solve(TruePhi_CCBelief[[g]])
}

##Compute the residual variances for the single indicator:
Theta_g_CCBelief<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Theta_g_CCBelief[[g]]<-TruePhi_CCBelief[[g]] * Rho_CCBelief[[g]] %*% (diag(1, nrow = 1, ncol = 1)-Rho_CCBelief[[g]])
  
}




###------------------------------------------------------------------------------------
##Repeat the same steps with the full metric Climate Change Policy Support:
CCPolSupport.Metric.M1.Marker<-'
CCPolicySupport=~support2+support1+support3
'

CCPolSupport.Metric.Fit1.Marker<-cfa(model = CCPolSupport.Metric.M1.Marker,
                                     data = ESS8,
                                     group = "country",
                                     estimator="MLR",
                                     missing="FIML",
                                     group.equal="loadings")



##Extract for each group:
#the factor covariance matrix Φ 
#the factor loadings Λ
#the unique variance θ
EST_CCPolSupport<-lavInspect(CCPolSupport.Metric.Fit1.Marker, what = "est")
Phi_CCPolSupport<-lapply(EST_CCPolSupport, "[[", "psi")
lambda_CCPolSupport<-lapply(EST_CCPolSupport, "[[","lambda")
theta_CCPolSupport<-lapply(EST_CCPolSupport, "[[","theta")

##compute the factor score matrix with regression Ar for each group:
##compute the factor scores for each group:
#
AR_Matrix_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))
y_CCPolSupport_items<-vector(mode = "list", length=length(unique(ESS8$country)))
FactorScores_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  #factor score matrix
  AR_Matrix_CCPolSupport[[g]]<-solve(solve(Phi_CCPolSupport[[g]])+t(lambda_CCPolSupport[[g]]) %*% solve(theta_CCPolSupport[[g]]) %*% lambda_CCPolSupport[[g]]) %*% t(lambda_CCPolSupport[[g]]) %*% solve(theta_CCPolSupport[[g]])
  
  ##factor scores
  y_CCPolSupport_items[[g]]<-ESS8[ESS8$country==unique(ESS8$country)[g],colnames(AR_Matrix_CCPolSupport[[g]])]
  FactorScores_CCPolSupport[[g]]<-AR_Matrix_CCPolSupport[[g]] %*% t(y_CCPolSupport_items[[g]])
}

##compute the posterior Varf in equation 2.3
ModelImplied_Sigma_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))
Posterior_Varf_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  ##before computing the posterior Varf, first compute the model-implied covariance matrix for each group:
  ModelImplied_Sigma_CCPolSupport[[g]]<-lambda_CCPolSupport[[g]] %*% Phi_CCPolSupport[[g]] %*% t(lambda_CCPolSupport[[g]])+theta_CCPolSupport[[g]]
  
  ##posterior Varf:
  Posterior_Varf_CCPolSupport[[g]]<-Phi_CCPolSupport[[g]]-t(Phi_CCPolSupport[[g]]) %*% t(lambda_CCPolSupport[[g]]) %*% solve(ModelImplied_Sigma_CCPolSupport[[g]]) %*% lambda_CCPolSupport[[g]] %*% Phi_CCPolSupport[[g]]
}

##request covariance matrix from the factor scores to constrcut var(f)
Var_f_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  Var_f_CCPolSupport[[g]]<-cov(t(FactorScores_CCPolSupport[[g]]), use = "pairwise.complete.obs")
}


##compute the true factor variance 
TruePhi_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  TruePhi_CCPolSupport[[g]]<-Var_f_CCPolSupport[[g]]+Posterior_Varf_CCPolSupport[[g]]
}

##compute the ρ:
Rho_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Rho_CCPolSupport[[g]]<-Var_f_CCPolSupport[[g]]%*% solve(TruePhi_CCPolSupport[[g]])
}

##Compute the residual variances for the single indicator:
Theta_g_CCPolSupport<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))) {
  
  Theta_g_CCPolSupport[[g]]<-TruePhi_CCPolSupport[[g]] * Rho_CCPolSupport[[g]] %*% (diag(1, nrow = 1, ncol = 1)-Rho_CCPolSupport[[g]])
  
}



###------------------------------------------------------------------------------------
##At this point, we need to put each pair together in the same matrix:
#Rho_HV, Rho_CCBelief, Rho_CCPolSupport --> Rho_g
#Theta_g_HV, Theta_g_CCBelief, Theta_g_CCPolSupport --> Theta_g
#for Cov_fg: we need to first put the factor scores together and then ask for the Cov_fg

Rho_g<-vector(mode = "list", length=length(unique(ESS8$country)))
Theta_g<-vector(mode = "list", length=length(unique(ESS8$country)))
factorScore_g<-vector(mode = "list", length=length(unique(ESS8$country)))
Cov_fg<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  ##first focus on Rho_g
  Rho_g[[g]]<-lav_matrix_bdiag(Rho_HV[[g]], Rho_CCBelief[[g]],Rho_CCPolSupport[[g]])
  colnames(Rho_g[[g]])<-rownames(Rho_g[[g]])<-c(rownames(AR_Matrix_HV[[g]]), rownames(AR_Matrix_CCBelief[[g]]), rownames(AR_Matrix_CCPolSupport[[g]]))
  
  ##then focus on Theta_g
  Theta_g[[g]]<-lav_matrix_bdiag(Theta_g_HV[[g]], Theta_g_CCBelief[[g]],Theta_g_CCPolSupport[[g]])
  colnames(Theta_g[[g]])<-rownames(Theta_g[[g]])<-c(rownames(AR_Matrix_HV[[g]]), rownames(AR_Matrix_CCBelief[[g]]), rownames(AR_Matrix_CCPolSupport[[g]]))
  
  ##lastly focus on Cov_fg
  #first put the factor scores together:
  factorScore_g[[g]]<-rbind(FactorScores_HV[[g]],FactorScores_CCBelief[[g]],FactorScores_CCPolSupport[[g]])
  rownames(factorScore_g[[g]])<-c(rownames(AR_Matrix_HV[[g]]), rownames(AR_Matrix_CCBelief[[g]]), rownames(AR_Matrix_CCPolSupport[[g]]))
  
  Cov_fg[[g]]<-cov(t(factorScore_g[[g]]), use = "pairwise.complete.obs")
}

##compute the factor covariance matrix that could be used to estimate the structural model through lavaan
Phi_Mediation_Step2<-vector(mode = "list", length=length(unique(ESS8$country)))

for(g in 1:length(unique(ESS8$country))){
  
  Phi_Mediation_Step2[[g]]<-solve(Rho_g[[g]]) %*% (Cov_fg[[g]]-Theta_g[[g]]) %*% solve(t(Rho_g[[g]]))
}


###----------------------------------------------------------------------------------------------------------
##Take out LT as the sample covariance matrix is not positive-definite
ESS_noLT<-ESS8 %>%
  filter(country != "LT")

##Take 16th out of the Phi_BasicModel_Step2
NEWPhi_Mediation_Step2<-Phi_Mediation_Step2[-16]

##To extract the new nobs
fake_model<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6

CCBelief=~ImpactBelief+TrendBelief+AttriBelief

CCPolicySupport=~support2+support1+support3
'

fake<-cfa(model = fake_model,
          data = ESS_noLT,
          group = "country",
          estimator="MLR",
          missing="FIML",
          do.fit=F) ##to extract the nobs per country


##run the Structural model without LT
SI_Mediation_FullMetricCPolSupport_5clus<-'
CCBelief~c(a1,a3,a3,a1,a3,a3,a1,a3,a3,a3,a4,a4,a2,a2,a1,a3,a3,a3,a3,a4,a3,a3)*SelfTran+
          c(b1,b3,b3,b1,b3,b3,b1,b3,b3,b3,b4,b4,b2,b2,b1,b3,b3,b3,b3,b4,b3,b3)*Conser+
          c(c1,c3,c3,c1,c3,c3,c1,c3,c3,c3,c4,c4,c2,c2,c1,c3,c3,c3,c3,c4,c3,c3)*SelfEnhan

CCPolicySupport~c(d1,d3,d3,d1,d3,d3,d1,d3,d3,d3,d4,d4,d2,d2,d1,d3,d3,d3,d3,d4,d3,d3)*CCBelief+
                c(e1,e3,e3,e1,e3,e3,e1,e3,e3,e3,e4,e4,e2,e2,e1,e3,e3,e3,e3,e4,e3,e3)*SelfTran+
                c(f1,f3,f3,f1,f3,f3,f1,f3,f3,f3,f4,f4,f2,f2,f1,f3,f3,f3,f3,f4,f3,f3)*Conser+
                c(g1,g3,g3,g1,g3,g3,g1,g3,g3,g3,g4,g4,g2,g2,g1,g3,g3,g3,g3,g4,g3,g3)*SelfEnhan
'

Mediation.SI.FullMCCPolSup.5clus.fit<-cfa(model = SI_Mediation_FullMetricCPolSupport_5clus,
                               sample.cov = NEWPhi_Mediation_Step2,
                               sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/Mediation_FullMetricCCPolSup_SingleIndicator_5clus_noLT.txt")
summary(Mediation.SI.FullMCCPolSup.5clus.fit, fit.measures=T, standardized=T)
sink()

