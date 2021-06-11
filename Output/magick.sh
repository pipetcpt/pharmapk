for file in *.pdf; do   
  magick -density 150 "$file[0]" -background white -alpha remove -shave 1x1 -bordercolor black -border 1 "${file/pdf/png}"
done

for file in *.tiff; do
  magick -density 150 "$file" -background white -alpha remove -shave 1x1 -bordercolor black -border 1 "${file/tiff/png}"
done
