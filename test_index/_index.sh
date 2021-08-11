grep -rl '비구획' ./*.Rmd | xargs sed -i '/비구획/ s/$/\\index{비구획}/'
