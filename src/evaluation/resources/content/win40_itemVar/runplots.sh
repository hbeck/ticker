#!/bin/sh
pre=1
runs=2
tmsFile="plot_tms.txt"
clingoFile="plot_clingo.txt"
classpath='../../../../../../target/scala-2.11/classes/:../../../../../../out/artifacts/steen_jar/*'
program=iclp.evaluation.LarsEvaluation
winsize=40
tp=400
itemVariations=(1 2 4 6 8 10 12 14 16 18 20)


java -Xmx10g -cp $classpath $program inst content2 pre $pre runs $runs tp $tp winsize $winsize items 12 impl Doyle >> $tmsFile

# java -cp $classpath $program inst content1 pre $pre runs $runs tp 10 winsize 20 items $items impl ClingoPush header only >> $tmsFile

# for items in ${itemVariations[@]}
# do
#     echo "evaluating Doyle - content 1 " $items

#     java -Xmx10g -cp $classpath $program inst content1 pre $pre runs $runs tp $tp winsize $winsize items $items impl Doyle >> $tmsFile
# done

# for items in ${itemVariations[@]}
# do
#     echo "evaluating Doyle - content 2 " $items

#     java -Xmx10g -cp $classpath $program inst content2 pre $pre runs $runs tp $tp winsize $winsize items $items impl Doyle >> $tmsFile
# done

# java -cp $classpath $program inst content1 pre $pre runs $runs tp $tp winsize 20 items $items impl ClingoPush header only >> $clingoFile

# for items in ${itemVariations[@]}
# do
#     echo "evaluating clingo - content 1 " $items

#     java -Xmx10g -cp $classpath $program inst content1 pre $pre runs $runs tp $tp winsize $winsize items $items impl ClingoPush >> $clingoFile
# done

# for items in ${itemVariations[@]}
# do
#     echo "evaluating clingo - content 2 " $items

#     java -Xmx10g -cp $classpath $program inst content2 pre $pre runs $runs tp $tp winsize $winsize items $items impl ClingoPush >> $clingoFile
# done

