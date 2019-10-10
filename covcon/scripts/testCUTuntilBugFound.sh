#!/bin/bash

benchmarkDir=$1
seedBase=$2
c=$3
reportFile=$4
testToExecute=$5
maxRuns=$6
archiveDir=$7
# 仪器痕迹？？什么留下的痕迹
rm -rf Instrument_Traces/

#cut=`cat ${benchmarkDir}/cut.txt`
cut=$8
b=$9
b=$((b + 1))
echo "检测第 ("${b}"/"${maxRuns}")个bug"
echo "Testing ${cut} until a bug is found ("${c}")"
# 最大后缀（生成）/（根）尝试
maxSuffixGenTries="10"
# 起始时间
timeStart=$(date +%s)
# 运行指定的命令，如果在指定时间后仍在运行，则杀死该进程。
# 如果testWithSeed在一小时内结束则结束，否则kill
# timeout $timeout ./scripts/testWithSeed.sh $seedBase $maxSuffixGenTries $benchmarkDir
./scripts/testWithSeed.sh $seedBase $maxSuffixGenTries $benchmarkDir
# 结束时间
timeEnd=$(date +%s) 
# 花费时间
timeTaken=$(( timeEnd - timeStart )) 

./scripts/time_fetch.sh results/${cut}_seed${seedBase}_tries${maxSuffixGenTries}.out results/${cut}_seed${seedBase}_tries${maxSuffixGenTries}.result $timeTaken ${reportFile} ${testToExecute} ${c} $archiveDir

#wc=`wc -l results/${cut}_seed${seedBase}_tries${maxSuffixGenTries}.result`
#lines=`echo ${wc} | cut -d" " -f1`
#
#mv results/${cut}_seed${seedBase}_tries${maxSuffixGenTries}.result $archiveDir/${cut}_seed${seedBase}_tries${maxSuffixGenTries}_count${c}_${timeStart}.result
#mv results/${cut}_seed${seedBase}_tries${maxSuffixGenTries}.out $archiveDir/${cut}_seed${seedBase}_tries${maxSuffixGenTries}_count${c}_${timeStart}.out
#
#if [ "$lines" -gt "2" ]
#then
#  echo "Found BUG! Stopping to test."
#  exit 0
#else
#  echo "         ... nothing"
#fi
#


