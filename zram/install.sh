#!/bin/sh
# I don't work without root permissions
cp zram /etc/init.d/
cp zram.rules /etc/udev/rules.d/
udevadm control -R
/etc/init.d/zram start
