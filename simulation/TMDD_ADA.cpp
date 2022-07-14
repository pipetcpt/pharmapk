$PARAM
KPT = 0.064, KTP = 0.123, VC=0.032, KA1 = 0.142, KA2 = 0.6
KEL = 0.106
R0 = 64.31, KDEG = 0.079, KINT = 2, KON=0.101, KOFF = 10.1
R02 = 0
KDEG2 = 0.00001
KINT2 = 0.01
KON2 = 5
KOFF2 = 0
ADA = 0
KSYN2 = 0.005
$CMT EV1 CENT TISS REC2 RC2 REC RC EV2 

$GLOBAL
namespace tmdd {
  double _dxdt_CP=0;
  double TMDDR0 = 0;  
}
#define KSYN (R0*KDEG)
#define CP (CENT/VC)

$MAIN
REC_0 = R0;
REC2_0 = R02;
tmdd::TMDDR0 = _R(3);
double VCi = exp(log(VC) + EVC);
double KDEGi = exp(log(KDEG) + EKDEG);


$OMEGA  @annotated

EVC : 0.09 : 
EKDEG : 0.1 : 

$ODE
dxdt_EV1 = -KA1*EV1;
dxdt_EV2 = -KA2*EV2;
tmdd::_dxdt_CP = (tmdd::TMDDR0+KA1*EV1 + KA2*EV2)/VCi - (KEL + KPT*(1-ADA))*CP - KON*CP*REC*(1-ADA) - KON2*CP*REC2 + KOFF2*RC2 + KOFF*RC + KTP*TISS/VCi;
dxdt_CENT = tmdd::_dxdt_CP * VCi;
dxdt_TISS = KPT*CP*VCi - KTP*TISS;
dxdt_REC = (1-ADA)*KSYN - KDEGi*REC - KON*CP*REC + KOFF*RC;
dxdt_RC = KON*CP*REC*(1-ADA) - (KINT+KOFF)*RC;
dxdt_REC2 = ADA*KSYN2 - KDEG2 * REC2 - KON2 * CP * REC2 + KOFF2 * RC2;
dxdt_RC2 = KON2 * CP * REC2 - (KINT2 + KOFF2) * RC2;
$TABLE
double TOTAL = REC+RC;
double TOTAL2 = REC2 + RC2;

$CAPTURE CP TOTAL TOTAL2