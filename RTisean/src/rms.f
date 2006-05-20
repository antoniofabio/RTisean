c subtract mean, normalise to unit variance, or
c print mean, standard deviation, and range of series in file(s)
c Copyright (C) T. Schreiber (1997)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine myrms(options_filename)
        character*30 options_filename
      parameter(nx=100000)
      dimension x(nx)
      character*72 file, fout
c     data lmean/0/, lvar/0/
	integer lmean,lvar
      data iverb/1/
	character*255 str

	lmean=0
	lvar=0
		
	call getparam(str,options_filename)

c     call whatido("compute mean/standard deviation, normalise",iverb,str)
	call whatido("compute mean/standard deviation, normalise",iverb,str)

      nmaxx=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      if(lopt("a",1,str).eq.1) lmean=1
      if(lopt("v",1,str).eq.1) lvar=1
      isout=igetout(fout,iverb,str)
      if(iv_io(iverb).eq.1) write(istderr(),*) 
     .   "mean / standard deviation / smallest / largest"

      do 10 ifi=1,nstrings()
         call nthstring(ifi,file,str)
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
      return
	end

      
