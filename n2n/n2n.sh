#!/bin/bash
killall edge
[ -r /etc/default/n2n ] & . /etc/default/n2n
edge -f -l $N2N_SUPERNODE:$N2N_SUPERNODE_PORT -a $N2N_SUBNET_PREFIX.254 -c $N2N_COMMUNITY $N2N_DAEMON_OPTS
for i in `seq 253`; do
	ping -c 1 192.168.41.$i
	if [ $? -eq 1 ] ; then
		echo ip 192.168.41.$i detected free
		killall edge
		edge -f -l $N2N_SUPERNODE:$N2N_SUPERNODE_PORT -a $N2N_SUBNET_PREFIX.$i -c $N2N_COMMUNITY $N2N_DAEMON_OPTS
		exit 0
	fi
done
exit 1
