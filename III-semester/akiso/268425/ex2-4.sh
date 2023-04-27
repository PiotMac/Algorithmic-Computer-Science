#!/bin/bash

drawDownloadChart () {
	(for i in {0..9}
	do
		local number=$(echo "$max_download/10*(10-$i)" | bc)
		echo -n "$number B         ->|"
		for j in "${download[@]}"
		do
			if [[ $j -gt $number ]]
			then
				echo -en "\e[102m \e[40m"
			else
				echo -en "\e[107m \e[40m"
			fi
		done
		echo -en "\n"
	done
	echo "|------------------------------") | column -t -s "|"
}

drawUploadChart () {
	(for i in {0..9}
	do
		local number=$(echo "$max_upload/10*(10-$i)" | bc)
		echo -n "$number B         ->|"
		for j in "${upload[@]}"
		do
			if [[ $j -gt $number ]]
			then
				echo -en "\e[102m \e[40m"
			else
				echo -en "\e[107m \e[40m"
			fi
		done
		echo -en "\n"
	done
	echo "|------------------------------") | column -t -s "|"
}

declare -a download
declare -a upload

for i in  {0..29}
do
	download[i]=0
	upload[i]=0
done


line="---------------------------------------------|--------"


while :
do
#Info about internet speed

first_tick_receive=$(cat /proc/net/dev | sed '5q;d' | awk '{print $2}')
first_tick_transmit=$(cat /proc/net/dev | sed '5q;d' | awk '{print $10}')
sleep 1
second_tick_receive=$(cat /proc/net/dev | sed '5q;d' | awk '{print $2}')
second_tick_transmit=$(cat /proc/net/dev | sed '5q;d' | awk '{print $10}')
receive=$(echo "$second_tick_receive - $first_tick_receive" | bc -l)
transmit=$(echo "$second_tick_transmit - $first_tick_transmit" | bc -l)

max_download=0
max_upload=0
avg_download=0
avg_upload=0

for i in {29..0}
do
	avg_download=$(echo "$avg_download+${download[i]}" | bc)
	avg_upload=$(echo "$avg_upload+${upload[i]}" | bc)
	if [ $i -ne 0 ]
	then
		download[i]=${download[i-1]}
		upload[i]=${upload[i-1]}
	else
		download[i]=$receive
		upload[i]=$transmit
	fi
		
	if [[ ${download[i]} -gt $max_download ]]
	then
		max_download=${download[i]}
	fi

	if [[ ${upload[i]} -gt $max_upload ]]
	then
		max_upload=${upload[i]}
	fi
done

echo "		Download data chart"
echo -n "	Average amount of download data (bytes): "
echo "scale=2; $avg_download/30" | bc -l

drawDownloadChart
echo "		Upload data chart"
echo -n "	Average amount of upload data (bytes): "
echo "scale=2; $avg_upload/30" | bc -l

drawUploadChart

#B or KB or MB?
if (( $receive < 1000 ))
then
	speedr="B/s"
	elif (( $receive < 1000000 ))
	then
		receive=$(echo "scale=2;$receive / 1000" | bc -l)
		speedr="KB/s"
		else
		receive=$(echo "scale=2;$receive / 1000000" | bc -l)
		speedr="MB/s"
fi

if (( $transmit < 1000 ))
then
	speedt="B/s"
	elif (( $transmit < 1000000 ))
	then
		transmit=$(echo "scale=2;$transmit / 1000" | bc -l)
		speedt="KB/s"
		else
		transmit=$(echo "scale=2;$transmit / 1000000" | bc -l)
		speedt="MB/s"
fi
echo "#####INTERNET SPEED INFO#####"
echo "Download speed: $receive $speedr"
echo "Upload speed: $transmit $speedt"

#Info about CPU usage

notidle=$(cat /proc/stat | sed '2q;d' | awk '{print $2+$3+$4+$6+$7+$8}')
total=$(cat /proc/stat | sed '2q;d' | awk '{print $2+$3+$4+$5+$6+$7+$8}')
cpuusage1=$(echo "x=($notidle / $total) * 100.0; scale=2; x/1" | bc -l)
notidle=$(cat /proc/stat | sed '3q;d' | awk '{print $2+$3+$4+$6+$7+$8}')
total=$(cat /proc/stat | sed '3q;d' | awk '{print $2+$3+$4+$5+$6+$7+$8}')
cpuusage2=$(echo "x=($notidle / $total) * 100.0; scale=2; x/1" | bc -l)
notidle=$(cat /proc/stat | sed '4q;d' | awk '{print $2+$3+$4+$6+$7+$8}')
total=$(cat /proc/stat | sed '4q;d' | awk '{print $2+$3+$4+$5+$6+$7+$8}')
cpuusage3=$(echo "x=($notidle / $total) * 100.0; scale=2; x/1" | bc -l)
notidle=$(cat /proc/stat | sed '5q;d' | awk '{print $2+$3+$4+$6+$7+$8}')
total=$(cat /proc/stat | sed '5q;d' | awk '{print $2+$3+$4+$5+$6+$7+$8}')
cpuusage4=$(echo "x=($notidle / $total) * 100.0; scale=2; x/1" | bc -l)

MHz1=$(cat /proc/cpuinfo | grep "MHz" | sed '1q;d')
MHz2=$(cat /proc/cpuinfo | grep "MHz" | sed '2q;d')
MHz3=$(cat /proc/cpuinfo | grep "MHz" | sed '3q;d')
MHz4=$(cat /proc/cpuinfo | grep "MHz" | sed '4q;d')

echo "#####CPU USAGE INFO#####"
echo "CPU1: $cpuusage1%   $MHz1"
echo "CPU2: $cpuusage2%   $MHz2"
echo "CPU3: $cpuusage3%   $MHz3"
echo "CPU4: $cpuusage4%   $MHz4"

#Info about uptime
seconds=$(cat /proc/uptime | awk '{print $1}')
minutes=$(echo "scale=2;$seconds / 60.0" | bc)
hours=$(echo "scale=2;$minutes / 60.0" | bc)
days=$(echo "scale=2;$hours / 24.0" | bc)

printf "#####UPTIME INFO#####\nDays: $days\nHours: $hours\nMinutes: $minutes\nSeconds: $seconds\n"

#Info about battery
line=$(sed '13q;d' /sys/class/power_supply/BAT0/uevent)
prefix="POWER_SUPPLY_CAPACITY="
capacity=${line#"$prefix"}
echo "#####BATTERY INFO#####"
echo "$capacity%"

#Info about system load
one_minute=$(cat /proc/loadavg | awk '{print $1}')
five_minutes=$(cat /proc/loadavg | awk '{print $2}')
fifteen_minutes=$(cat /proc/loadavg | awk '{print $3}')

echo -e "#####SYSTEM LOAD INFO#####\nAverage number of jobs in the state R or D in last:\n1 minute: $one_minute\n5 minutes: $five_minutes\n15 minutes: $fifteen_minutes\n"

#Info about memory usage
line=$(sed '1q;d' /proc/meminfo)
prefix="MemTotal:"
memory=${line#"$prefix"}
echo "#####MEMORY USAGE INFO#####"
echo "Total usable RAM: $memory"
line=$(sed '2q;d' /proc/meminfo)
prefix="MemFree:"
memory=${line#"$prefix"}
echo "Amount of RAM unused by the system:$memory"
line=$(sed '3q;d' /proc/meminfo)
prefix="MemAvailable:"
memory=${line#"$prefix"}
echo "Amount of memory available for starting new applications:$memory"
sleep 1
done
