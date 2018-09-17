@echo off
SET dir='homework 1'
SET files=hw1.mli hw1.ml hw2.mli hw2.ml test.ml
SET run=run
SET script="cd %dir%; ocamlc -g -linkall -o %run% %files%; ./%run%"
echo Running script %script%
bash -c %script%