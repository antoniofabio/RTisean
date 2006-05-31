c spike train autocorrelation function
c Copyright (C) T. Schreiber (1997)

      parameter(nx=1000000, nhist=100000)
      dimension x(nx), lx(nx), ihist(nhist)
      character*72 file, fout
      data iverb/1/

      call whatido("spike train autocorrelation function",iverb)
      bin=fmust("d")
      totbin=fmust("D")
      nbin=int(totbin/bin)+1
      nmaxx=ican("l",nx)
      nexcl=ican("x",0)
      jcol=ican("c",0)
      inter=lopt("i",1)
      isout=igetout(fout,iverb)

      do 10 ifi=1,nstrings()
         call nthstring(ifi,file)
         nmax=nmaxx
         call readfile(nmax,x,nexcl,jcol,file,iverb)
         if(inter.eq.0) goto 1
         do 20 n=2,nmax
 20         x(n)=x(n)+x(n-1)
 1       call sort(nmax,x,lx)
         do 30 i=1,nbin
 30         ihist(i)=0
         do 40 n1=1,nmax
            do 50 n2=n1+1,nmax
               il=int((x(n2)-x(n1))/bin)+1
               if(il.gt.nbin) goto 40
 50            ihist(il)=ihist(il)+1
 40         continue
         if(file.eq."-") file="stdin"
         if(isout.eq.1) call addsuff(fout,file,"_sco")
         call outfile(fout,iunit,iverb)
         do 60 i=1,nbin
 60         write(iunit,*) (i-0.5)*bin, ihist(i)
 10      if(iunit.ne.istdout()) close(iunit)
      end

      subroutine usage()
c usage message

      call whatineed(
     .   "-d# -D# [-i -o outfile -l# -x# -c# -V# -h] file(s)")
      call popt("d","time span of one bin")
      call popt("D","total time spanned")
      call popt("i","expect intervals rather than times")
      call popt("l","number of values to be read (all)")
      call popt("x","number of values to be skipped (0)")
      call popt("c","column to be read (1 or file,#)")
      call pout("file_sco")
      call pall()
      stop
      end



