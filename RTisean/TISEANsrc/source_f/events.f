c convert inter-event intervals to event times
c Copyright (C) T. Schreiber (1999)

      parameter(nx=1000000)
      dimension x(nx)
      character*72 file, fout
      data iverb/1/

      call whatido("interval to event time conversion",iverb)
      nmaxx=ican("l",nx)
      nexcl=ican("x",0)
      jcol=ican("c",0)
      isout=igetout(fout,iverb)

      do 10 ifi=1,nstrings()
         call nthstring(ifi,file)
         nmax=nmaxx
         call readfile(nmax,x,nexcl,jcol,file,iverb)
         nmax=nmax+1
         do 20 n=nmax,2,-1
 20         x(n)=x(n-1)
         x(1)=0
         do 30 n=2,nmax
 30         x(n)=x(n)+x(n-1)
         if(file.eq."-") file="stdin"
         if(isout.eq.1) call addsuff(fout,file,"_st")
 10      call writefile(nmax,x,fout,iverb)
      end

      subroutine usage()
c usage message

      call whatineed(
     .   "[-o outfile -l# -x# -c# -V# -h] file(s)")
      call popt("l","number of values to be read (all)")
      call popt("x","number of values to be skipped (0)")
      call popt("c","column to be read (1 or file,#)")
      call pout("file_st")
      call pall()
      stop
      end


