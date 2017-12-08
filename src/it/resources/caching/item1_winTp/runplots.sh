#!/bin/sh
items=1
pre=1
runs=2
tmsFile="plot_tms.txt"
clingoFile="plot_clingo.txt"
classpath='../../../../../../target/scala-2.11/classes/:../../../../../../out/artifacts/steen_jar/*'
program=iclp.evaluation.LarsEvaluation
winSizes=(20 40 60 80 100 120 140 160 180 200)

java -cp $classpath $program inst content1 pre $pre runs $runs tp 10 winsize 20 items $items impl ClingoPush header only >> $tmsFile

for winsize in ${winSizes[@]}
do
   tp=$(($winsize*10))
    echo "evaluating Doyle - content 1 " $winsize $tp

    java -cp $classpath $program inst content1 pre $pre runs $runs tp $tp winsize $winsize items $items impl Doyle >> $tmsFile
done

for winsize in ${winSizes[@]}
do
    tp=$(($winsize*10))
    echo "evaluating Doyle - content 2 " $winsize $tp

    java -cp $classpath $program inst content2 pre $pre runs $runs tp $tp winsize $winsize items $items impl Doyle >> $tmsFile
done

java -cp $classpath $program inst content1 pre $pre runs $runs tp $tp winsize 20 items $items impl ClingoPush header only >> $clingoFile

for winsize in ${winSizes[@]}
do
    tp=$(($winsize*10))
    echo "evaluating clingo - content 1 " $winsize $tp

    java -Xmx10g -cp $classpath $program inst content1 pre $pre runs $runs tp $tp winsize $winsize items $items impl ClingoPush >> $clingoFile
done

for winsize in ${winSizes[@]}
do
    tp=$(($winsize*10))
    echo "evaluating clingo - content 2 " $winsize $tp

    java -Xmx10g -cp $classpath $program inst content2 pre $pre runs $runs tp $tp winsize $winsize items $items impl ClingoPush >> $clingoFile
done

