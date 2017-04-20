#!/bin/bash
killall edge
export N2N_KEY=bumsbums
edge -f -l remoteqth.com:82 -a 192.168.41.254 -d tun0 -c verylittleindustry 
for i in `seq 253`; do
	ping -c 1 192.168.41.$i
	if [ $? -eq 1 ] ; then
		echo ip 192.168.41.$i detected free
		killall edge
		edge -f -l remoteqth.com:82 -a 192.168.41.$i -d tun0 -c verylittleindustry
		exit 0
	fi
done
exit 1
