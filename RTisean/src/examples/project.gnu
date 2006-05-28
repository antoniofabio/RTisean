! echo ""
! echo "**********************************************************************"
! echo "*  Noise reduction. First: noisy data                                *"
! echo "**********************************************************************"
! echo ""
pause 1

set title "Noisy data"
set data style dots
plot '< henon -l5000 | addnoise -v0.02 | delay'
pause -1 "Press <return> to continue"

! echo ""
! echo "**********************************************************************"
! echo "*  Noise reduction                                                   *"
! echo "**********************************************************************"
! echo ""
pause 1

set title "Noise reduction"
plot '< henon -l5000 | addnoise -v0.02 | project -m7 -q2 -r0.05 -k20 -i3 | delay'
pause -1 "Press <return> when finished"
