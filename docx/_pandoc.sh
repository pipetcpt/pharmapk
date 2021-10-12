#rm -rf media
#pandoc --extract-media ./ -s "실전약동학1 머리말_20210217 임동석.docx" -t markdown -o preface.md 
#pandoc --extract-media ./ -s 1장_20210217임동석.docx -t markdown -o 01.md ; mv media media-01
#pandoc --extract-media ./ -s 4장_20210217임동석.docx -t markdown -o 04.md ; mv media media-04
#pandoc --extract-media ./ -s "비구획 분석 교재.docx" -t markdown -o 02.md ; mv media media-02
#pandoc --extract-media ./ -s "NCA-FDA.docx" -t markdown -o nca-fda.md 
#pandoc --extract-media ./ -s "05-1.docx" -t markdown -o 05-1.md 

#pandoc --extract-media ./ -s chapter1-2021-10-11-added-yimds.docx -t markdown -o chapter1-2021-10-11-added-yimds.md
pandoc --extract-media ./ -s chapter1-2021-10-11-added-yimds-new.docx -t markdown -o chapter1-2021-10-11-added-yimds-new.md


