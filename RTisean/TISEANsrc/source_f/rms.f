c subtract mean, normalise to unit variance, or
c print mean, standard deviation, and range of series in file(s)
c Copyright (C) T. Schreiber (1997)

      parameter(nx=1000000)
      dimension x(nx)
      character*72 file, fout
      data lmean/0/, lvar/0/
      data iverb/1/

      call whatido("compute mean/standard deviation, normalise",iverb)
      nmaxx=ican("l",nx)
      nexcl=ican("x",0)
      jcol=ican("c",0)
      if(lopt("a",1).eq.1) lmean=1
      if(lopt("v",1).eq.1) lvar=1
      isout=igetout(fout,iverb)
      if(iv_io(iverb).eq.1) write(istderr(),*) 
     .   "mean / standard deviation / smallest / largest"

      do 10 ifi=1,nstrings()
         call nthstring(ifi,file)
         nmax=nmaxx
         call readfile(nmax,x,nexcl,jcol,file,iverb)
         if(file.eq."-") file="stdin"
         call rms(nmax,x,sc,sd)
         call minmax(nmax,x,xmin,xmax)
         if(lvar.eq.1.or.lmean.eq.1) then
            if(iv_io(iverb).eq.1) write(istderr(),*) 
     .         sc, sd, xmin, xmax, "   ", file(1:index(file," ")-1)
            if(lvar.eq.1) then
               if(isout.eq.1) call addsuff(fout,file,"_v")
               call normal1(nmax,x,sc,sd)
            else 
               if(isout.eq.1) call addsuff(fout,file,"_a")
               call normal(nmax,x,sc,sd)
            endif
            call writefile(nmax,x,fout,iverb)
         else
            write(*,*) 
     .         sc, sd, xmin, xmax, "   ", file(1:index(file," ")-1)
         endif
 10      continue
      end

      subroutine usage()
c usage message

      call whatineed(
     .   "[-a -v -o outfile -l# -x# -c# -V# -h] file(s)")
      call popt("a","subtract average")
      call popt("v","subtract mean, normalise to unit variance")
      call popt("l","number of values to be read (all)")
      call popt("x","number of values to be skipped (0)")
      call popt("c","column to be read (1 or file,#)")
      call pout("file_a (if -a), file_v (if -v)")
      call pall()
      stop
      end


