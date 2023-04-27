#!/bin/bash

wypiszWykresPobrane () {
	(for j in {1..10}
	do
		local liczba=$(echo "$max_pobrane/10*(10-$j)" | bc)
		echo -n "$liczba-|"
		for i in "${pobrane[@]}"
		do
			if [[ $i -gt $liczba ]]
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

wypiszWykresWyslane () {
	(for j in {1..10}
	do
		local liczba=$(echo "$max_wyslane/10*(10-$j)" | bc)
		echo -n "$liczba-|"
		for i in "${wyslane[@]}"
		do
			if [[ $i -gt $liczba ]]
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

declare -a pobrane
declare -a wyslane
declare -i iwyslane
declare -i ipobrane
declare -i ipobrane_stare
declare -i iwyslane_stare

ipobrane_stare=$(cat /proc/net/dev | grep "wlp3s0" | awk '{print $2}')
iwyslane_stare=$(cat /proc/net/dev | grep "wlp3s0" | awk -v x=10 '{print $x}')

for i in  {0..29}
do
	pobrane[i]=0
	wyslane[i]=0
done

while true
do

echo -e "\n\e[33mWykres danych sieci\e[37m"

	ipobrane=$(cat /proc/net/dev | grep "wlp3s0" | awk '{print $2}')
	iwyslane=$(cat /proc/net/dev | grep "wlp3s0" | awk -v x=10 '{print $x}')

	max_pobrane=0
	avg_wyslane=0
	max_wyslane=0
	avg_pobrane=0

	for i in {29..0}
	do
		avg_pobrane=$(echo "$avg_pobrane+${pobrane[i]}" | bc)
		avg_wyslane=$(echo "$avg_wyslane+${wyslane[i]}" | bc)
		if [ $i -ne 0 ]
		then
			pobrane[i]=${pobrane[i-1]}
			wyslane[i]=${wyslane[i-1]}
		else
			pobrane[i]=$(echo "$ipobrane-$ipobrane_stare" | bc)
			wyslane[i]=$(echo "$iwyslane-$iwyslane_stare" | bc)
		fi
		
		if [[ ${pobrane[i]} -gt $max_pobrane ]]
		then
			max_pobrane=${pobrane[i]}
		fi

		if [[ ${wyslane[i]} -gt $max_wyslane ]]
		then
			max_wyslane=${wyslane[i]}
		fi
	done

	echo -e "\e[33m	Wykres danych pobranych\e[37m"
	echo -n "	Srednia danych pobranych: "
	echo "scale=2; $avg_pobrane/30" | bc -l
	wypiszWykresPobrane
	echo -e "\e[33m	Wykres danych wyslanych\e[37m"
	echo -n "	Srednia danych wyslanych: "
	echo "scale=2; $avg_wyslane/30" | bc -l
	wypiszWykresWyslane
	
	ipobrane_stare=$ipobrane
	iwyslane_stare=$iwyslane
	sleep 1
done
