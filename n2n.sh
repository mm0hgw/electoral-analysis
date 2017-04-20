#!/bin/bash
sudo killall edge
sudo edge -f -l remoteqth.com:82 -a 192.168.41.254 -d tun0 -c verylittleindustry -k bumsbums
for i in `seq 10`; do
	ping -c 1 192.168.41.$i
	if [ $? -eq 1 ] ; then
		echo ip 192.168.41.$i detected free
		sudo killall edge
		sudo edge -f -l remoteqth.com:82 -a 192.168.41.$i -d tun0 -c verylittleindustry -k bumsbums
		exit 0
	fi
done
exit 1
