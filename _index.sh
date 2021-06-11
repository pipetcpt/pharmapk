grep -rl '항정상태' ./*.Rmd | xargs sed -i '/척도 파라미터/ s/$/\\index{항정상태}/'
