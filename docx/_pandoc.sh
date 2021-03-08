rm -rf media
pandoc --extract-media ./ -s "실전약동학1 머리말_20210217 임동석.docx" -t markdown -o preface.md 
pandoc --extract-media ./ -s 1장_20210217임동석.docx -t markdown -o 01.md ; mv media media-01
pandoc --extract-media ./ -s 4장_20210217임동석.docx -t markdown -o 04.md ; mv media media-04

