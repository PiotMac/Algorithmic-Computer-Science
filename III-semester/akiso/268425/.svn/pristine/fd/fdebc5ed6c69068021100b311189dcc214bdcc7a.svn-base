#!/bin/bash

touch file
echo "PPID|PID|COMM|STATE|TTY|RSS|PGID|SID|OPEN_FILES" > file
#Searching only for processes
pattern="/proc/[0-9]+"

for files in /proc/*
do
	if [[ $files =~ $pattern ]] && [[ -d $files ]]
	then
		#We read process attributes
		#'^*' - beginning of a line
		ppid=$(cat $files/status | grep '^PPid' | awk '{print $2}')
		pid=$(cat $files/status | grep '^Pid' | awk '{print $2}')
		comm=$(cat $files/status | grep 'Name' | awk '{print $2}')
		state=$(cat $files/status | grep 'State' | awk '{print $2}')
		tty=$(cat $files/stat | awk '{print $7}')
		if [[ $tty -eq 0 ]]
		then
			ttyid="0"
		else
			let tty=tty-1024
			ttyid="tty$tty"
		fi
		rss=$(cat $files/status | grep 'VmRSS' | awk '{print $2}')
		pgid=$(cat $files/status | grep 'NSpgid' | awk '{print $2}')
		sid=$(cat $files/status | grep 'NSsid' | awk '{print $2}')
		openfiles=$(sudo ls -l $files/fd | wc -l)
		openfiles=$(( openfiles-1 ))
		echo "$ppid|$pid|$comm|$state|$ttyid|$rss|$pgid|$sid|$openfiles" >> file
	fi
done

cat file | column -t -s "|"
rm file
