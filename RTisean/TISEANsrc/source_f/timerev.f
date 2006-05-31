c Statistics for time reversibility
c Copyright (C) T. Schreiber (1999)

      parameter(nx=1000000)
      dimension x(nx)
      character*72 file
      data iverb/1/

      call whatido("time reversal asymmetry statistic",iverb)
      id=abs(ican("d",1))
      nmaxx=ican("l",nx)
      nexcl=ican("x",0)
      jcol=ican("c",0)
c      isout=igetout(fout,iverb)

      do 10 ifi=1,nstrings()
         call nthstring(ifi,file)
         nmax=nmaxx
         call readfile(nmax,x,nexcl,jcol,file,iverb)
         t2=0
         t3=0
         do 20 n=id+1,nmax
            t2=t2+(x(n)-x(n-id))**2
 20         t3=t3+(x(n)-x(n-id))**3
 10         write(*,*) t3/t2, " "//file(1:index(file," ")-1)
      end

      subroutine usage()
c usage message

      call whatineed(
     .   "[-d# -l# -x# -c# -V# -h] file(s)")
      call popt("d","delay (1)")
      call popt("l","number of values to be read (all)")
      call popt("x","number of values to be skipped (0)")
      call popt("c","column to be read (1 or file,#)")
      call pall()
      stop
      end

