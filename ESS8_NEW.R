library(dplyr)
library(lavaan)
library(tidyr)
library(ggplot2)
library(plotly)
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
  mutate(ImpactBelief2=case_when(
    ImpactBelief<=2 ~ 1,
    ImpactBelief>=3 & ImpactBelief <=4 ~ 2,
    ImpactBelief>=5 & ImpactBelief <=6 ~ 3,
    ImpactBelief>=7 & ImpactBelief <=8 ~ 4,
    ImpactBelief>=9 & ImpactBelief <=10 ~ 5
  ))

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

##Test with the shriken scale of Impact Belief:
TESTCCBelief.Metric.M1<-'
CCBelief=~TrendBelief+AttriBelief+ImpactBelief2
'

TESTCCBelief.Metric.Fit1<-cfa(model = TESTCCBelief.Metric.M1,
                          data = ESS8,
                          group = "country",
                          estimator="MLR",
                          missing="FIML",
                          group.equal="loadings",
                          std.lv=T)

sink("./Sink Output/ESS8/TESTCCBelief_Metric_fit1.txt")
summary(TESTCCBelief.Metric.Fit1, fit.measures=T, standardized=T)
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
NoOpen.HV.Metric.M5.marker<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

NoOpen.HV.Metric.Fit5.marker<-cfa(model = NoOpen.HV.Metric.M5.marker,
                                  data = ESS8,
                                  group = "country",
                                  estimator="MLR",
                                  missing="FIML",
                                  group.equal="loadings",
                                  group.partial=c("SelfEnhan=~SE3",
                                                  "SelfEnhan=~C1",
                                                  "SelfTran=~SE3",
                                                  "Conser=~C1"))

#sink("./Sink Output/ESS8/NoOpen_HV_Metric_fit5_marker.txt")
#summary(NoOpen.HV.Metric.Fit5.marker, fit.measures=T, standardized=T)
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
#
##Climate Change Belief 2
CCBelief.Metric.M1.Marker2<-'
CCBelief=~ImpactBelief2+TrendBelief+AttriBelief
'

CCBelief.Metric.Fit1.Marker2<-cfa(model = CCBelief.Metric.M1.Marker2,
                                 data = ESS8,
                                 group = "country",
                                 estimator="MLR",
                                 missing="FIML",
                                 group.equal="loadings")

#sink("./Sink Output/ESS8/CCBelief_Metric_fit1_marker2.txt")
#summary(CCBelief.Metric.Fit1.Marker2, fit.measures=T, standardized=T)
#sink()

##listwise deletion:
ESS8_lw<-na.omit(ESS8)

##Structural model
Str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan
'

##Model selection 
BasicModel.Selection<-ModelSelection(dat=ESS8_lw,
                                     S1 = list(NoOpen.HV.Metric.M5.marker, CCBelief.Metric.M1.Marker),
                                     S2 = Str_model,
                                     group = "country",
                                     clusters=c(1,8),
                                     seed = 100,
                                     userStart = NULL,
                                     s1_fit = list(NoOpen.HV.Metric.Fit5.marker, CCBelief.Metric.Fit1.Marker),
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
##plot for CHull:Questions, why there is no CHull scree ratio for 5 clusters
ggplot(BasicModel.Selection$Overview, aes(x=nrpar, y=LL)) +
  geom_point()+
  geom_line()+
  labs(title = "CHUll")+xlab("number of parameters")+ylab("Log-Likelihood")+
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


##MMGSEM - 2 cluster
BasicModel.2clus<-MMGSEM(dat=ESS8_lw,
                         S1 = list(NoOpen.HV.Metric.M5.marker, CCBelief.Metric.M1.Marker),
                         S2 = Str_model,
                         group = "country",
                         nclus=2,
                         seed = 100,
                         userStart = NULL,
                         s1_fit = list(NoOpen.HV.Metric.Fit5.marker, CCBelief.Metric.Fit1.Marker),
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
##MMGSEM-6 cluster
BasicModel.6clus<-MMGSEM(dat=ESS8_lw,
                         S1 = list(NoOpen.HV.Metric.M5.marker, CCBelief.Metric.M1.Marker),
                         S2 = Str_model,
                         group = "country",
                         nclus=6,
                         seed = 100,
                         userStart = NULL,
                         s1_fit = list(NoOpen.HV.Metric.Fit5.marker, CCBelief.Metric.Fit1.Marker),
                         max_it = 10000L,
                         nstarts = 50L,
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
clustering.2clus<-t(apply(BasicModel.2clus$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.2clus[,2]<-ifelse(clustering.2clus[,2]==1,2,0)
ClusMembership.2clus<-apply(clustering.2clus,1,function(x) sum(x))
ClusterRes.2clus<-data.frame(group=c(1:23),
                       ClusMembership=ClusMembership.2clus)

countries<-data.frame(group=c(1:23),
                      country=lavInspect(NoOpen.HV.Metric.Fit5.marker, "group.label"))

ClusterRes.2clus<-merge(ClusterRes.2clus, countries,
                        by.x = "group", by.y = "group")

#
#6-cluster solution
clustering.6clus<-t(apply(BasicModel.6clus$posteriors,1,function(x) as.numeric(x==max(x))))
clustering.6clus[,2]<-ifelse(clustering.6clus[,2]==1,2,0)
clustering.6clus[,3]<-ifelse(clustering.6clus[,3]==1,3,0)
clustering.6clus[,4]<-ifelse(clustering.6clus[,4]==1,4,0)
clustering.6clus[,5]<-ifelse(clustering.6clus[,5]==1,5,0)
clustering.6clus[,6]<-ifelse(clustering.6clus[,6]==1,6,0)
ClusMembership.6clus<-apply(clustering.6clus,1,function(x) sum(x))
ClusterRes.6clus<-data.frame(group=c(1:23),
                             ClusMembership=ClusMembership.6clus)


#####################################################################################
################### Basic Model: SAM estimation and comparison ######################
#####################################################################################

##First do the following step that is necessary for both 2-cluster and 6-cluster solution
#
##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(NoOpen.HV.Metric.Fit5.marker, what = "est")
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

Var_eta

##In order to map the cluster solution, we also need to do a free SAM:
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
##
##2-cluster: only group 1, 16, 19 are in cluster 1, the rest are in cluster 2
sam_str_model<-'
CCBelief~c(a1,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a2,a1,a2,a2,a1,a2,a2,a2,a2)*SelfTran+
          c(b1,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2,b1,b2,b2,b1,b2,b2,b2,b2)*Conser+
          c(c1,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c2,c1,c2,c2,c1,c2,c2,c2,c2)*SelfEnhan
'

BasicModel.SAM<-cfa(model = sam_str_model,
                    sample.cov = Var_eta,
                    sample.nobs = lavInspect(fake, "nobs"))

sink("./Sink Output/ESS8/BasicModel_SAM.txt")
summary(BasicModel.SAM, fit.measures=T, standardized=T)
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

