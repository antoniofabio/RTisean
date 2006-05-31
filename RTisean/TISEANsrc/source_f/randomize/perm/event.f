C permutation scheme for event times
c one event time is changed such that the two adjacent intervals are swapped
c Copyright (C) T. Schreiber (1999)

c-------------------------------------------------------------------
c get permutation specific options
c
      subroutine opts_permute()
      end

c-------------------------------------------------------------------
c print version information on permutation scheme
c
      subroutine what_permute()
      call ptext("Permutation scheme: event time preserving intervals")
      end

c-------------------------------------------------------------------
c print permutation specific usage message
c
      subroutine usage_permute()
      end

c-------------------------------------------------------------------
c initialise all that is needed for permutation scheme 
c
      subroutine permute_init()
      parameter(nx=100000) 
      dimension x(nx)
      common nmax,cost,temp,cmin,rate,x

      do 10 n=1,nmax*log(nmax*1.)
         call permute(n1,n2)
 10      call exch(n1,n2)
      end

c-------------------------------------------------------------------
c find two indices n1, n2 to be exchanged, maybe using a parameter 
c par provided by the cooling schedule
c
c here, n2 is not used at all; event 1 and nmax are never changed
c
      subroutine permute(n1,n2)
      common nmax
      external rand

      n1=min(int(rand(0.0)*nmax)+2,nmax-1)
      end

c-------------------------------------------------------------------
c given two indices n1, n2, actually perform the exchange
c
      subroutine exch(n1,n2)
      parameter(nx=100000)
      dimension x(nx)
      common nmax,cost,temp,cmin,rate,x

      x(n1)=x(n1-1)+x(n1+1)-x(n1)
      end
