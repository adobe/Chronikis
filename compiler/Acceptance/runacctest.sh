#!/bin/bash
set -e
echo "Run compiler..."
for f in ./*.cks; do
  echo ""
  echo "--- $f ---"
  stack exec chronikisc < $f -- \
	--stan ~/Tmp/$(basename $f .cks).stan \
	--R ~/Tmp/$(basename $f .cks).R
done
set +e
echo ""
echo "Compare..."
COUNTER=0
for f in ./*.cks; do
  fname=$(basename $f .cks).stan
  fnameR=$(basename $f .cks).R
  echo ""
  echo "--- $fname ---"
  echo ".stan:"
  diff ./Reference/$fname ~/Tmp/$fname
  let COUNTER=COUNTER+$?
  echo ".R:"
  diff ./Reference/$fnameR ~/Tmp/$fnameR
  let COUNTER=COUNTER+$?
done
echo ""
if [ $COUNTER -eq 0 ]; then
    echo "PASSED"
else
    echo "FAILED!!!"
fi
exit $COUNTER
