
#' CAMPSIS model suite.
#'
#' A library of models of all kinds, ready to be simulated in Campsis. These model
#' templates are sorted into the following categories: pharmacokinetic (PK), pharmacodynamic (PD), 
#' target-mediated drug disposition (TMDD), NONMEM, literature  and other (custom models).
#'
#' @format A list with all the models:
#' \describe{
#'    \item{pk}{extensive list of pharmacokinetic (PK) model templates}
#'    \item{pd}{list of pharmacodynamic (PD) model templates, to be plugged into any pharmacokinetic (PK) model}
#'    \item{tmdd}{extensive list of target-mediated drug disposition (TMDD) model templates}
#'    \item{nonmem}{list of model templates translated from standard NONMEM control streams}
#'    \item{literature}{a couple of models coming from the literature}
#'    \item{other}{a couple of custom models}
#' }
#' @source \url{https://calvagone.github.io/campsis.doc/}
#' @source \url{https://www.iconplc.com/solutions/technologies/nonmem/}
#' @source \url{http://repository.ddmore.eu/model/DDMODEL00000077/}
"model_suite"


#' CAMPSIS model library.
#'
#' A list containing pharmacokinetic (PK) and pharmacodynamic (PD) model templates.
#'
#' @format A list with all the models:
#' \describe{
#'    \item{advan1_trans1}{1-compartment PK model (K,V)}
#'    \item{advan1_trans2}{1-compartment PK model (CL,V)}
#'    \item{advan2_trans1}{1-compartment PK model with absorption compartment (KA,K,V)}
#'    \item{advan2_trans2}{1-compartment PK model with absorption compartment (KA,CL,V)}
#'    \item{advan3_trans1}{2-compartment PK model (K,V,K12,K21)}
#'    \item{advan3_trans3}{2-compartment PK model (CL,V,Q,VSS)}
#'    \item{advan3_trans4}{2-compartment PK model (CL,V1,V2,Q)}
#'    \item{advan4_trans1}{2-compartment PK model with absorption compartment (KA,K,V,K12,K21)}
#'    \item{advan4_trans3}{2-compartment PK model with absorption compartment (KA,CL,V,Q,VSS)}
#'    \item{advan4_trans4}{2-compartment PK model with absorption compartment (KA,CL,V1,V2,Q)}
#'    \item{advan11_trans4}{3-compartment PK model (CL,V1,V2,V3,Q2,Q3)}
#'    \item{advan12_trans4}{3-compartment PK model with absorption compartment (KA,CL,V1,V2,V3,Q2,Q3)}
#'    \item{direct_effect_model}{direct effect PD model (EMAX,EC50,GAMMA,E0)}
#'    \item{effect_cmt_model}{effect compartment PD model (KE0)}
#'    \item{irm_kin_inhibition}{indirect response PD model - inhibition of KIN (IC50,KIN,KOUT)}
#'    \item{irm_kin_stimulation}{indirect response PD model - stimulation of KIN (EMAX,EC50,KIN,KOUT)}
#'    \item{irm_kout_inhibition}{indirect response PD model - inhibition of KOUT (IC50,KIN,KOUT)}
#'    \item{irm_kout_stimulation}{indirect response PD model - stimulation of KOUT (EMAX,EC50,KIN,KOUT)}
#'    \item{transit_cmt_model}{transit compartment PD model (BASE,POWER,MTT,SLOPE,KTR)}
#'    \item{filgrastim_pkpd_krzyzanski}{PK/PD model of filgrastim (Krzyzanski et al., see URL below)}
#'    \item{my_model1}{Example of 2-compartment PK model with variance-covariance matrix}
#' }
#' @source \url{https://www.iconplc.com/solutions/technologies/nonmem/}
#' @source \url{http://repository.ddmore.eu/model/DDMODEL00000077/}
#' @source \url{https://calvagone.github.io/campsis.doc/}
"model_library"
