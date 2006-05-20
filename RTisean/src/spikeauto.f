c spike train autocorrelation function
c Copyright (C) T. Schreiber (1997)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine spikeauto(options_filename)
        character*30 options_filename
      parameter(nx=100000, nhist=100000)
      dimension x(nx), lx(nx), ihist(nhist)
      character*72 file, fout
      data iverb/1/
	character*255 str	
	call getparam(str,options_filename)	

      call whatido("spike train autocorrelation function",iverb,str)
      bin=fmust("d",str)
      totbin=fmust("D",str)
      nbin=int(totbin/bin)+1
      nmaxx=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      inter=lopt("i",1,str)
      isout=igetout(fout,iverb,str)

      do 10 ifi=1,nstrings()
         call nthstring(ifi,file,str)
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
      return
	end

     

