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

##Configural Invariance Model 1: perfect fit since the model is just identified
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


##(partial) metric Invariance Model 2: free the loading CCPolicySupport=~Support3
CCPolSupport.Metric.M2<-'
CCPolicySupport=~support1+support2+support3
'

CCPolSupport.Metric.Fit2<-cfa(model = CCPolSupport.Metric.M2,
                              data = ESS8,
                              group = "country",
                              estimator="MLR",
                              missing="FIML",
                              group.equal="loadings",
                              group.partial=c("CCPolicySupport=~support3"),
                              std.lv=T)

sink("./Sink Output/ESS8/CCPolicySupport_Metric_fit2.txt")
summary(CCPolSupport.Metric.Fit2, fit.measures=T, standardized=T)
sink()

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
ESS8_lw<-na.omit(ESS8)

##Structural model
Str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan
'

##Model selection 
BasicModel.Selection<-ModelSelection(dat=ESS8_lw,
                                     S1 = list(NoOpen.HV.Metric.M2.Marker, CCBelief.Metric.M1.Marker),
                                     S2 = Str_model,
                                     group = "country",
                                     clusters=c(1,8),
                                     seed = 100,
                                     userStart = NULL,
                                     s1_fit = list(NoOpen.HV.Metric.Fit2.Marker, CCBelief.Metric.Fit1.Marker),
                                     max_it = 10000L,
                                     nstarts = 70L,
                                     printing = FALSE,
                                     partition = "hard",
                                     endogenous_cov = TRUE,
                                     endo_group_specific = TRUE,
                                     sam_method = "local",
                                     meanstr = FALSE,
                                     rescaling = F)
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

SAM_5clus_3D<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                      type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM with clustering results - Human Values on Climate Change Belief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(SAM_5clus_3D), "SAM_5clus_3D.html")

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

SAM_5clus_3D<-plot_ly(FreeSAM_reg_param, x= ~SelfTran, y= ~Conser, z= ~SelfEnhan, text= ~country, color = ~factor(ClusMembership),
                      type = "scatter3d", mode="markers+text") %>%
  layout(title="SAM with clustering results - Human Values on Climate Change Belief",
         scene=list(xaxis=list(title="Self-Transcendence"),
                    yaxis=list(title="Conservation"),
                    zaxis=list(title="Self-Enhancement")))

htmlwidgets::saveWidget(as_widget(SAM_5clus_3D), "SAM_5clus_3D.html")



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




#####################################################################################
############## Mapping the clustering results on Map  ###############################
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
