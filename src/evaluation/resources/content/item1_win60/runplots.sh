#!/bin/sh
items=1
pre=1
runs=2
winSize=60
tmsFile="plot_tms.txt"
clingoFile="plot_clingo.txt"
classpath='../../../../../../target/scala-2.11/classes/:../../../../../../out/artifacts/steen_jar/*'
program=iclp.evaluation.LarsEvaluation
timePoints=(100 200 300 400 500 600 700 800 900 1000)

java -cp $classpath $program inst content1 pre $pre runs $runs tp 10 winsize 20 items $items impl ClingoPush header only >> $tmsFile

for tp in ${timePoints[@]}
do
    echo "evaluating Doyle - content 1 " $tp

    java -cp $classpath $program inst content1 pre $pre runs $runs tp $tp winsize $winSize items $items impl Doyle >> $tmsFile
done

for tp in ${timePoints[@]}
do
    echo "evaluating Doyle - content 2 " $tp

    java -cp $classpath $program inst content2 pre $pre runs $runs tp $tp winsize $winSize items $items impl Doyle >> $tmsFile
done

java -cp $classpath $program inst content1 pre $pre runs $runs tp $tp winsize 20 items $items impl ClingoPush header only >> $clingoFile

for tp in ${timePoints[@]}
do
    echo "evaluating clingo - content 1 " $tp

    java -cp $classpath $program inst content1 pre $pre runs $runs tp $tp winsize $winSize items $items impl ClingoPush >> $clingoFile
done

for tp in ${timePoints[@]}
do
    echo "evaluating clingo - content 2 " $tp

    java -cp $classpath $program inst content2 pre $pre runs $runs tp $tp winsize $winSize items $items impl ClingoPush >> $clingoFile
done

