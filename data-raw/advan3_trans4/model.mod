[PK]
CL=THETA_1*exp(ETA_1)
V1=THETA_2*exp(ETA_2)
V2=THETA_3*exp(ETA_3)
Q=THETA_4*exp(ETA_4)
S1=V1

[DES]
d/dt(A_CENTRAL)=Q*A_PERIPHERAL/V2 + (-CL/V1 - Q/V1)*A_CENTRAL
d/dt(A_PERIPHERAL)=-Q*A_PERIPHERAL/V2 + Q*A_CENTRAL/V1
d/dt(A_OUTPUT)=CL*A_CENTRAL/V1
F=A_CENTRAL/S1

[ERROR]
CP=F
OBS_CP=CP*(EPS_1 + 1)
Y=OBS_CP
