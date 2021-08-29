#grep -rl 'xyz' ./*.Rmd | xargs gsed -i '/xyz/ s/$/\\index{xyz}/'
#grep -rl '용량용법' ./*.Rmd | xargs gsed -i '/용량용법/ s/$/\\index{용량용법(dosage regimen)}/'
#grep -rl '최종반감기' ./*.Rmd | xargs gsed -i '/최종반감기/ s/$/\\index{최종반감기}/'
#grep -rl '조직결합' ./*.Rmd | xargs gsed -i '/조직결합/ s/$/\\index{조직결합}/'
#grep -rl '유효용량' ./*.Rmd | xargs gsed -i '/유효용량/ s/$/\\index{유효용량}/'

# 신장과 콩팥이 혼용되어 쓰이고 있었는데 전체에서 신장을 콩팥으로 바꿈
# - "CL을" -> "CL를", "CL과" -> "CL와", "CL은" -> "CL는": CL이 clearance로 읽히므로 전체에서 바꿈
# - 용량용법(dosage regimen)에서 괄호안 내용 처음에만 사용

# 2nd ----
#grep -rl '약동학' ./*.Rmd | xargs gsed -i '/약동학/ s/$/\\index{약동학}/'
#grep -rl '약력학' ./*.Rmd | xargs gsed -i '/약력학/ s/$/\\index{약력학}/'
#grep -rl '가능도' ./*.Rmd | xargs gsed -i '/가능도/ s/$/\\index{가능도}/'
#grep -rl '청소율' ./*.Rmd | xargs gsed -i '/청소율/ s/$/\\index{청소율}/'
#grep -rl '혈장단백결합' ./*.Rmd | xargs gsed -i '/혈장단백결합/ s/$/\\index{혈장단백결합}/'
#grep -rl '요독증' ./*.Rmd | xargs gsed -i '/요독증/ s/$/\\index{요독증}/'
#grep -rl '건강인' ./*.Rmd | xargs gsed -i '/건강인/ s/$/\\index{건강인}/'
#grep -rl '노인' ./*.Rmd | xargs gsed -i '/노인/ s/$/\\index{노인}/'
#grep -rl '신기능' ./*.Rmd | xargs gsed -i '/신기능/ s/$/\\index{신기능}/'
#grep -rl '체중' ./*.Rmd | xargs gsed -i '/체중/ s/$/\\index{체중}/'
#grep -rl '산점도' ./*.Rmd | xargs gsed -i '/산점도/ s/$/\\index{산점도}/'
#grep -rl 'Michelis-Menten kinetics' ./*.Rmd | xargs gsed -i '/Michelis-Menten kinetics/ s/$/\\index{Michelis-Menten kinetics}/'
#grep -rl '반감기' ./*.Rmd | xargs gsed -i '/반감기/ s/$/\\index{반감기}/'
#grep -rl '비선형약동학' ./*.Rmd | xargs gsed -i '/비선형약동학/ s/$/\\index{비선형약동학}/'
#grep -rl '흡수속도상수' ./*.Rmd | xargs gsed -i '/흡수속도상수/ s/$/\\index{흡수속도상수}/'
#grep -rl '독성약동학' ./*.Rmd | xargs gsed -i '/독성약동학/ s/$/\\index{독성약동학}/'
#grep -rl '임상개발' ./*.Rmd | xargs gsed -i '/임상개발/ s/$/\\index{임상개발}/'
#grep -rl '단클론항체' ./*.Rmd | xargs gsed -i '/단클론항체/ s/$/\\index{단클론항체}/'
#grep -rl '모델링' ./*.Rmd | xargs gsed -i '/모델링/ s/$/\\index{모델링}/'
#grep -rl '시뮬레이션' ./*.Rmd | xargs gsed -i '/시뮬레이션/ s/$/\\index{시뮬레이션}/'
#grep -rl '계량약리학' ./*.Rmd | xargs gsed -i '/계량약리학/ s/$/\\index{계량약리학}/'
#grep -rl '분포구획' ./*.Rmd | xargs gsed -i '/분포구획/ s/$/\\index{분포구획}/'
#grep -rl '확산속도상수' ./*.Rmd | xargs gsed -i '/확산속도상수/ s/$/\\index{확산속도상수}/'
#grep -rl '의사결정' ./*.Rmd | xargs gsed -i '/의사결정/ s/$/\\index{의사결정}/'
#grep -rl '항정상태' ./*.Rmd | xargs gsed -i '/항정상태/ s/$/\\index{항정상태}/'
#grep -rl '중심구획' ./*.Rmd | xargs gsed -i '/중심구획/ s/$/\\index{중심구획}/'
#grep -rl '말초구획' ./*.Rmd | xargs gsed -i '/말초구획/ s/$/\\index{말초구획}/'

# 1st ----
#grep -rl '지연 시간' ./*.Rmd | xargs gsed -i '/지연 시간/ s/$/\\index{지연 시간}/'

