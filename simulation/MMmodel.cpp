$PARAM @annotated


P_F : 1 : Bioavailability
KA : 1.5 : Absorption rate constant (1/h)
VC : 10 : Central volume (V)
CL : 0 : Clearance (L/h)
Q : 4 : Inter-compartmental clearance (L/h)
Q2 : 0 : Inter-compartmenta clearance2
VP : 10 : Peripheral volume of distribution (L)
VP2  : 20 : Peripheral volume of distribution2 (L)
VMAX : 1000 : CL_Vmax
KM : 20 : CL_Km
DUMMY3 : 1 : MM no (0) yes (1)
KOUTtv : 0.5 : Kout in tumor growth model
DUMMY_TV : 0 : Response model(0) or Tumor growth model (1)

Fzero : 1 : Zero-order bioavailability
DUAL : 0 : Dual order absorption
P_F2 : 0.5 : Dual bioavailability

DUMMY_TS : 0 : Transit

ALAG1 : 0 :
ALAG2 : 0 :
D : 5 :

$SET delta= 0.05

$CMT GUT CENT PERI PERI2

$GLOBAL

double podo = 0;


$MAIN

double CLi = exp(log(CL) + ECL);
double VCi = exp(log(VC) + EVC);
double VPi = exp(log(VP) + EVP);
double VP2i = exp(log(VP2) + EVP2);
double Qi = exp(log(Q) + EQ);
double Q2i = exp(log(Q2) + EQ2);
double KAi = exp(log(KA) + EKA);
double Fi = exp(log(P_F) + EF);
double Fzeroi = exp(log(Fzero) + EFzero);
double VMAXi = exp(log(VMAX) + EVMAX);
double Di = exp(log(D) + ED);
double KMi = exp(log(KM) + EKM);
double K23 = Qi / VCi;
double K24 = Q2i / VCi;
double K32 = Qi / VPi;
double K42 = Q2i / VP2i;


if(DUAL == 0){
    F_GUT = Fi;
  } else {
    F_GUT = P_F2;
  }

if(DUAL == 0){
  F_CENT = Fzeroi;
} else {
  F_CENT = 1 - P_F2;
}

ALAG_GUT = ALAG1;
ALAG_CENT = ALAG2;

D_CENT = Di;

$OMEGA @name IIV @labels ECL EVC EVP EVP2 EQ EQ2 EKA EF EFzero EVMAX EKM ED

0.1 0.1 0.04 0.04 0.04 0.04 0.04 0.04 0.04 0 0 0.04

$SIGMA @labels ADD PROP
0 0


$ODE

double CPi = std::max(0.0001, CENT/VCi);
double CLNL = (VMAXi*CPi/(KMi+CPi));


dxdt_GUT = -KAi * GUT;
dxdt_CENT = KAi * GUT - (CLi/VCi) * CENT - CLNL * DUMMY3 - K23 * CENT + K32 * PERI - K24 * CENT + K42 * PERI2;
dxdt_PERI = K23 * CENT - K32 * PERI;
dxdt_PERI2 = K24 * CENT - K42 * PERI2;

$TABLE
capture CP = CENT/VCi*(1 + PROP) + ADD;


$CAPTURE CP;