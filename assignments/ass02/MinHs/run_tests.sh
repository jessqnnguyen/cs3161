#!/bin/bash
if test "$1" == "--no-color"; then
  runhaskell -cpp -DNOCOLOR -i./driver ./driver/Check.hs $2 $3 $4 $5 $6
else
  runhaskell -cpp -i./driver ./driver/Check.hs $1 $2 $3 $4 $5 $6
fi
