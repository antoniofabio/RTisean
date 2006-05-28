! echo ""
! echo "**********************************************************************"
! echo "*  Orbits of period six (or subperiod)                               *"
! echo "**********************************************************************"
! echo ""
pause 1

set data style points
set pointsize 3
set title "Orbits of period six (or subperiod)"
! henon -l1000 | addnoise -v0.1 | upo -p6 -m2 -v0.1 -n100 -V -o
plot '< henon -l1000 | addnoise -v0.1 | delay' notitle w do, \
     '< cat stdin_upo_06 | upoembed -p1 -d1' title "fixed point",\
     '< cat stdin_upo_06 | upoembed -p2 -d1' title "period 2",\
     '< cat stdin_upo_06 | upoembed -p6 -d1' index 0 title "period 6",\
     '< cat stdin_upo_06 | upoembed -p6 -d1' index 1 title "period 6"
pause -1 "Press <return> when finished"
! rm stdin_*
