#!/bin/sh
items=1
pre=1
runs=2
tp=1000
tmsFile="plot_tms.txt"
clingoFile="plot_clingo.txt"
classpath='../../../../../../target/scala-2.11/classes/:../../../../../../out/artifacts/steen_jar/*'
program=iclp.evaluation.LarsEvaluation
winSizes=(20 40 80 120 160 200)

java -cp $classpath $program inst content1 pre $pre runs $runs tp $tp winsize 20 items $items impl ClingoPush header only >> $tmsFile

for winSize in ${winSizes[@]}
do
    echo "evaluating Doyle - content 1 " $winSize

    java -Xmx10g -cp $classpath $program inst cachingStatTupleNonDet pre $pre runs $runs tp $tp winsize $winSize items $items impl Doyle >> $tmsFile
done

for winSize in ${winSizes[@]}
do
    echo "evaluating Doyle - content 2 " $winSize

    java -Xmx10g -cp $classpath $program inst cachingStatTimeNonDet pre $pre runs $runs tp $tp winsize $winSize items $items impl Doyle >> $tmsFile
done

java -cp $classpath $program inst content1 pre $pre runs $runs tp $tp winsize 20 items $items impl ClingoPush header only >> $clingoFile

for winSize in ${winSizes[@]}
do
    echo "evaluating clingo - content 1 " $winSize

    java -Xmx10g -cp $classpath $program inst cachingStatTupleNonDet pre $pre runs $runs tp $tp winsize $winSize items $items impl ClingoPush >> $clingoFile
done

for winSize in ${winSizes[@]}
do
    echo "evaluating clingo - content 2 " $winSize

    java -Xmx10g -cp $classpath $program inst cachingStatTimeNonDet pre $pre runs $runs tp $tp winsize $winSize items $items impl ClingoPush >> $clingoFile
done

