sed 's/\"\[/\[/g' dayDistance.json > temp.json
sed 's/\]\",/\],/g' temp.json > temp2.json
sed 's/\]\"\]\}/\]\]\}/g' temp2.json > dayDistance2.json
cp dayDistance2.json dayDistance.json
rm dayDistance2.json
rm temp.json
rm temp2.json