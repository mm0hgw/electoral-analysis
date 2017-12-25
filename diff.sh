#!/bin/bash 
# un-comment one diff tool you'd like to use 

xhost >> /dev/null
XHOST=$?

# side-by-side diff with custom options: 
if [ '$XHOST' != '0' ] ; then
 /usr/bin/sdiff -w200 -l "$2" "$5" 
fi
# using kdiff3 as the side-by-side diff: 
if [ '$XHOST' == '0' ] ; then
 /usr/bin/kdiff3 "$2" "$5" 
fi
# using Meld 
# /usr/bin/meld "$2" "$5" 
# using VIM 
# /usr/bin/vim -d "$2" "$5"
