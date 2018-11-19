#!/bin/bash
fpdoc --package=rxnet --format=html  \
  --input=../smpp.pas --descr=rxnet.xml \
  --input=../smspduconstsunit.pas --descr=rxnet.xml \
  --input=../smspduoptparamtypesunit.pas --descr=rxnet.xml \
  --input=../smspdutypesunit.pas --descr=rxnet.xml \
  --input=../smsutils.pas --descr=rxnet.xml 
  
