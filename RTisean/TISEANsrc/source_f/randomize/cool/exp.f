c exponential cooling scheme
c Copyright (C) T. Schreiber (1999)

c-------------------------------------------------------------------
c get options specific for cooling scheme
C
      subroutine opts_cool()
      common /coolcom/ 
     .   itini,tini,iafac,afac,cgoal,mtot,msucc,ntot,nsucc,mstop

      tini=fcan("T",0.)
      afac=fcan("a",0.)
      mtot=ican("S",20000)
      msucc=ican("s",2000)
      mstop=ican("z",200)
      cgoal=fcan("C",0.)
      end

c-------------------------------------------------------------------
c print version information on cooling scheme
C
      subroutine what_cool()
      call ptext("Cooling scheme: exponential")
      end

c-------------------------------------------------------------------
c print usage message specific for cooling scheme
C
      subroutine usage_cool()
      call ptext("Cooling options: [-T# -a# -S# -s# -z# -C#]")
      call popt("T","initial temperature (auto)")
      call popt("a","cooling factor (auto)")
      call popt("S","total steps before cooling (20000)")
      call popt("s","successful steps before cooling (2000)")
      call popt("z","minimal successful steps before cooling (200)")
      call popt("C","goal value of cost function (0.0)")
      end

c-------------------------------------------------------------------
c initialise all that is needed for cooling scheme
C
      function cool_init()
      common /coolcom/ 
     .   itini,tini,iafac,afac,cgoal,mtot,msucc,ntot,nsucc,mstop

      ntot=0
      nsucc=0
      itini=1
      if(tini.eq.0.) then
         tini=1e-4
         itini=0
      endif
      iafac=1
      if(afac.eq.0.) then
         afac=0.5
         iafac=0
      endif
      temp=tini
      cool_init=temp
      end
      
c-------------------------------------------------------------------
c determine new temperature depending on current cost function,
c acceptance status and history
c par can be used to pass information to the permutation scheme
c
      function cool(iaccept,iend,iv)
      common /coolcom/ 
     .   itini,tini,iafac,afac,cgoal,mtot,msucc,ntot,nsucc,mstop
      common nmax,cost,temp,cmin,rate

      iend=0
      cool=temp
      nsucc=nsucc+iaccept
      ntot=ntot+1
      if(ntot.lt.mtot.and.nsucc.lt.msucc) return
      rate=real(nsucc)/real(ntot)
      iend=1
      if(cost.le.cgoal) return
      if(itini.eq.0.and.temp.eq.tini.and.ntot.gt.1.5*nsucc) then
         tini=10*temp
         if(iv.ne.0) write(istderr(),*) 
     .      "increased initial temperature from ",
     .      temp, " to ", tini, " for melting"
         temp=tini
      else if(nsucc.le.mstop) then
         if(iafac.eq.1) return
         afac=sqrt(afac)
         mtot=mtot*sqrt(2.)
         temp=tini
         if(iv.ne.0) write(istderr(),*) "starting over: "
         if(iv.ne.0) write(istderr(),*) "   Cooling rate: ", afac, 
     .      " S:", mtot, " s: ", msucc
      else         
         temp=temp*afac
         if(iv.ne.0) write(istderr(),
     .      '(3hT: ,g15.6,4h S: ,i15,4h s: , i15,8h  cost: ,g15.6)') 
     .      temp, ntot, nsucc, cost
      endif
      iend=0
      ntot=0
      nsucc=0
      cool=temp
      end

