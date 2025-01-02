##first run the data management for ESS_NEW.R

##select 3 countries
ESS_3cntry<-ESS8 %>%
  filter(country== "NL" | country == "SE" | country == "SI")


##run the measurement model with these three countries only
HV.3cntry.M1.Marker<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

HV.3cntry.M1.Marker<-cfa(model = HV.3cntry.M1.Marker,
                         data = ESS_3cntry,
                         group = "country",
                         estimator="MLR",
                         missing="FIML",
                         group.equal="loadings",
                         group.partial=c("SelfEnhan=~SE3"))

summary(HV.3cntry.M1.Marker, fit.measures=T, standardized=T)

#
##Climate Change Belief
#
CCBelief.3cntry.M1.Marker<-'
CCBelief=~ImpactBelief+TrendBelief+AttriBelief
'

CCBelief.3cntry.Fit1.Marker<-cfa(model = CCBelief.3cntry.M1.Marker,
                                 data = ESS_3cntry,
                                 group = "country",
                                 estimator="MLR",
                                 missing="FIML",
                                 group.equal="loadings")

summary(CCBelief.3cntry.Fit1.Marker, fit.measures=T, standardized=T)


##SAM long process below:
#
##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(HV.3cntry.M1.Marker, what = "est")
lambda_HV_3cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_3cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from CCBelief
EST_CCBelief<-lavInspect(CCBelief.3cntry.Fit1.Marker, what = "est")
lambda_CCBelief_3cntry<-lapply(EST_CCBelief, "[[","lambda")
theta_CCBelief_3cntry<-lapply(EST_CCBelief, "[[","theta")
#
##initialize empty list to store the new lambda matrix, new theta matrix and the mapping matrix
lambda_3cntry<-vector(mode = "list", length=length(unique(ESS_3cntry$country)))
theta_3cntry<-vector(mode = "list", length=length(unique(ESS_3cntry$country)))
Mmatrix<-vector(mode = "list", length = length(unique(ESS_3cntry$country)))

for (g in 1:length(unique(ESS_3cntry$country))){
  ##put lambda from two measurement blocks into into the same matrix for each group
  lambda_3cntry[[g]]<-lav_matrix_bdiag(lambda_HV_3cntry[[g]], lambda_CCBelief_3cntry[[g]])
  colnames(lambda_3cntry[[g]])<-c(colnames(lambda_HV_3cntry[[g]]), colnames(lambda_CCBelief_3cntry[[g]]))
  rownames(lambda_3cntry[[g]])<-c(rownames(lambda_HV_3cntry[[g]]), rownames(lambda_CCBelief_3cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_3cntry[[g]]<-lav_matrix_bdiag(theta_HV_3cntry[[g]],theta_CCBelief_3cntry[[g]])
  colnames(theta_3cntry[[g]])<-c(colnames(theta_HV_3cntry[[g]]), colnames(theta_CCBelief_3cntry[[g]]))
  rownames(theta_3cntry[[g]])<-c(rownames(theta_HV_3cntry[[g]]), rownames(theta_CCBelief_3cntry[[g]]))
  
  ##compute the mapping matrix for each group
  Mmatrix[[g]]<-solve(t(lambda_3cntry[[g]]) %*% solve(theta_3cntry[[g]]) %*% lambda_3cntry[[g]]) %*% t(lambda_3cntry[[g]]) %*% solve(theta_3cntry[[g]])
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
          data = ESS_3cntry,
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
Var_eta<-vector(mode = "list", length = length(unique(ESS_3cntry$country)))

for (g in 1:length(unique(ESS_3cntry$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_3cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
FREEsam_str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan
'

FreeSAM.3cntry<-cfa(model = FREEsam_str_model,
                        sample.cov = Var_eta,
                        sample.nobs = lavInspect(fake, "nobs"))

summary(FreeSAM.3cntry, fit.measures=T, standardized=T)


##constrain the coefficient to be the same in all 3 groups:
ConstrainSAM_str_model<-'
CCBelief~c(a1,a1,a1)*SelfTran+
          c(b1,b1,b1)*Conser+
          c(c1,c1,c1)*SelfEnhan
'

ConstrainSAM_fit<-cfa(model = ConstrainSAM_str_model,
                      sample.cov = Var_eta,
                      sample.nobs = lavInspect(fake, "nobs"))

summary(ConstrainSAM_fit, fit.measures=T, standardized=T)



################################################################################
###################### Re-do SAM with only 2 countries #########################
################################################################################

##select 2 countries
ESS_2cntry<-ESS8 %>%
  filter(country== "NL" | country == "SE")


##run the measurement model with these three countries only
HV.2cntry.M1.Marker<-'
SelfTran=~ST4+ST1+ST2+ST3+ST5+SE3+C3+C4
Conser=~C2+C1+C3+C4+C5+C6+SE4
SelfEnhan=~SE2+SE1+SE3+SE4+C1

##Add Error Term Correlation
C5~~C6
'

HV.2cntry.M1.Marker<-cfa(model = HV.2cntry.M1.Marker,
                         data = ESS_2cntry,
                         group = "country",
                         estimator="MLR",
                         missing="FIML",
                         group.equal="loadings",
                         group.partial=c("SelfEnhan=~SE3"))

summary(HV.2cntry.M1.Marker, fit.measures=T, standardized=T)

#
##Climate Change Belief
#
CCBelief.2cntry.M1.Marker<-'
CCBelief=~ImpactBelief+TrendBelief+AttriBelief
'

CCBelief.2cntry.Fit1.Marker<-cfa(model = CCBelief.2cntry.M1.Marker,
                                 data = ESS_2cntry,
                                 group = "country",
                                 estimator="MLR",
                                 missing="FIML",
                                 group.equal="loadings")

summary(CCBelief.2cntry.Fit1.Marker, fit.measures=T, standardized=T)


##SAM long process below:
#
##extract the loadings and residual variances from HV 
EST_HV<-lavInspect(HV.2cntry.M1.Marker, what = "est")
lambda_HV_2cntry<-lapply(EST_HV, "[[", "lambda")
theta_HV_2cntry<-lapply(EST_HV, "[[", "theta")
#
##extract the loadings and residual variances from CCBelief
EST_CCBelief<-lavInspect(CCBelief.2cntry.Fit1.Marker, what = "est")
lambda_CCBelief_2cntry<-lapply(EST_CCBelief, "[[","lambda")
theta_CCBelief_2cntry<-lapply(EST_CCBelief, "[[","theta")
#
##initialize empty list to store the new lambda matrix, new theta matrix and the mapping matrix
lambda_2cntry<-vector(mode = "list", length=length(unique(ESS_2cntry$country)))
theta_2cntry<-vector(mode = "list", length=length(unique(ESS_2cntry$country)))
Mmatrix<-vector(mode = "list", length = length(unique(ESS_2cntry$country)))

for (g in 1:length(unique(ESS_2cntry$country))){
  ##put lambda from two measurement blocks into into the same matrix for each group
  lambda_2cntry[[g]]<-lav_matrix_bdiag(lambda_HV_2cntry[[g]], lambda_CCBelief_2cntry[[g]])
  colnames(lambda_2cntry[[g]])<-c(colnames(lambda_HV_2cntry[[g]]), colnames(lambda_CCBelief_2cntry[[g]]))
  rownames(lambda_2cntry[[g]])<-c(rownames(lambda_HV_2cntry[[g]]), rownames(lambda_CCBelief_2cntry[[g]]))
  
  ##put theta from two measurement blocks into the same matrix for each group
  theta_2cntry[[g]]<-lav_matrix_bdiag(theta_HV_2cntry[[g]],theta_CCBelief_2cntry[[g]])
  colnames(theta_2cntry[[g]])<-c(colnames(theta_HV_2cntry[[g]]), colnames(theta_CCBelief_2cntry[[g]]))
  rownames(theta_2cntry[[g]])<-c(rownames(theta_HV_2cntry[[g]]), rownames(theta_CCBelief_2cntry[[g]]))
  
  ##compute the mapping matrix for each group
  Mmatrix[[g]]<-solve(t(lambda_2cntry[[g]]) %*% solve(theta_2cntry[[g]]) %*% lambda_2cntry[[g]]) %*% t(lambda_2cntry[[g]]) %*% solve(theta_2cntry[[g]])
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
          data = ESS_2cntry,
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
Var_eta<-vector(mode = "list", length = length(unique(ESS_2cntry$country)))

for (g in 1:length(unique(ESS_2cntry$country))) {
  Var_eta[[g]]<-Mmatrix[[g]] %*% (S[[g]]-theta_2cntry[[g]]) %*% t(Mmatrix[[g]])
}

##In order to map the cluster solution, we also need to do a free SAM for all sorts of clustering solution:
FREEsam_str_model<-'
CCBelief~SelfTran+Conser+SelfEnhan
'

FreeSAM.2cntry<-cfa(model = FREEsam_str_model,
                    sample.cov = Var_eta,
                    sample.nobs = lavInspect(fake, "nobs"))

summary(FreeSAM.2cntry, fit.measures=T, standardized=T)


##constrain the coefficient to be the same in all 3 groups:
ConstrainSAM_str_model<-'
CCBelief~c(a1,a1)*SelfTran+
          c(b1,b1)*Conser+
          c(c1,c1)*SelfEnhan
'

ConstrainSAM_fit<-cfa(model = ConstrainSAM_str_model,
                      sample.cov = Var_eta,
                      sample.nobs = lavInspect(fake, "nobs"))

summary(ConstrainSAM_fit, fit.measures=T, standardized=T)

