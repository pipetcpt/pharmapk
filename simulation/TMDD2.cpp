$PARAM
KPT = 0.064, KTP = 0.123, VC=0.032, KA1 = 0.142, KA2 = 0.6
KEL = 0.106
R0 = 64.31, KDEG = 0.079, KINT = 2, KON=0.101, KOFF = 10.1
KDEG2 = 0.1
KINT2 = 20
KON2 = 5
KOFF2 = 10

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
tmdd::TMDDR0 = _R(3);
double VCi = exp(log(VC) + EVC);
double KDEGi =exp(log(KDEG) + EKDEG);

$OMEGA  @annotated

EVC : 0.1 : 
EKDEG : 0.1 : 

$ODE
dxdt_EV1 = -KA1*EV1;
dxdt_EV2 = -KA2*EV2;
tmdd::_dxdt_CP = (tmdd::TMDDR0+KA1*EV1 + KA2*EV2)/VCi - (KEL+KPT)*CP - KON*CP*REC - KON2*CP*REC2 + KOFF2*RC2 + KOFF*RC + KTP*TISS/VCi;
dxdt_CENT = tmdd::_dxdt_CP * VCi;
dxdt_TISS = KPT*CP*VCi - KTP*TISS;
dxdt_REC = KSYN - KDEGi*REC - KON*CP*REC + KOFF*RC;
dxdt_RC = KON*CP*REC - (KINT+KOFF)*RC;
dxdt_REC2 = - KDEG2 * REC2 - KON2 * CP * REC2 + KOFF2 * RC2;
dxdt_RC2 = KON2 * CP * REC2 - (KINT2 + KOFF2) * RC2;
$TABLE
double TOTAL = REC+RC;
double TOTAL2 = REC2 + RC2;

$CAPTURE CP TOTAL TOTAL2