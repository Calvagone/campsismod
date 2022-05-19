
library(testthat)

context("Test the conversion of the simulation time when mrgsolve is used")

test_that("Time-dependent ODE's are correctly translated", {
  
  posMain <- Position(MainRecord())
  posOde <- Position(OdeRecord())
  weibullEquation <- Equation("WF", "DOSE * exp(-pow(t/TAU, SH)) * (SH/TAU) * pow(t/TAU, SH-1)")
  
  model <- CampsisModel() %>%
    add(Ode("A_ABS", "-WF*A_ABS"), pos=posOde) %>%
    add(Ode("A_CENTRAL", "WF*A_ABS - CL/VC*A_CENTRAL"), pos=posOde) %>%
    add(weibullEquation, pos=Position(OdeRecord(), after=FALSE)) %>% # WF will start the ODE block
    add(Equation("CP", "A_CENTRAL/VC"), pos=posOde) %>%
    add(Equation("TAU", "10"), pos=posMain) %>%
    add(Equation("SH", "5"), pos=posMain) %>%
    add(Equation("CL", "5"), pos=posMain) %>%
    add(Equation("VC", "100"), pos=posMain)
  
  ode <- model %>% find(OdeRecord())
  main <- model %>% find(MainRecord())
  expect_equal(ode@statements %>% getNames(), c("EQUATION (WF)", "ODE (A_ABS)", "ODE (A_CENTRAL)", "EQUATION (CP)"))
  expect_equal(main@statements %>% getNames(), c("EQUATION (TAU)", "EQUATION (SH)", "EQUATION (CL)", "EQUATION (VC)"))
  
  mrgmod <- model %>% export(dest="mrgsolve")
  expected <- c("[ODE]",
                "double WF=DOSE * exp(-pow(SOLVERTIME/TAU, SH)) * (SH/TAU) * pow(SOLVERTIME/TAU, SH-1);",
                "dxdt_A_ABS=-WF*A_ABS;",
                "dxdt_A_CENTRAL=WF*A_ABS - CL/VC*A_CENTRAL;",
                "double CP=A_CENTRAL/VC;")
  
  # Possibly run a simulation with Campsis
  # ds <- Dataset(1) %>%
  #   add(Bolus(time=0, amount=100)) %>%
  #   add(Observations(0:24)) %>%
  #   add(Covariate("DOSE", 100))
  # 
  # spaghettiPlot(simulate(model=model, dataset=ds, dest="mrgsolve", outvars="CP"), "CP")
  # spaghettiPlot(simulate(model=model, dataset=ds, dest="RxODE", outvars="CP"), "CP")
    
})

