c get command line arguments 
c Copyright (C) T. Schreiber (1997)

c modified by Alexei Grigoriev, 27.4.2006 

      subroutine argdel(i,str)
      parameter(margs=1000)
      character*255 str
      dimension largs(margs)
      common /args/ nargs, largs

      if(i.eq.0) then
         nargs=min(margs,myiargc(str))
         do 10 n=1,nargs
 10         largs(n)=1
      else
         if(i.gt.myiargc(str)) return
         if(largs(i).eq.0) return
         largs(i)=0
         nargs=nargs-1
      endif
      end

      function nstrings()
      parameter(margs=1000)
      dimension largs(margs)
      common /args/ nargs, largs

      nstrings=max(nargs,1)
      end

      subroutine nthstring(n,string,str)
      parameter(margs=1000)
      dimension largs(margs)
      common /args/ nargs, largs
      character*(*) string
      character*255 str
      
      iv=0
      do 10 i=1,myiargc(str)
         if(largs(i).eq.1) iv=iv+1
 10      if(iv.eq.n) goto 1
      string="-"
      return
 1    call mygetarg(i,string,str)
      end

      function imust(c,str)
c get mandatory integer argument, call usage statement if missing
      character c
	character*255 str

      imust=iopt(c,1,ierr,str)
      if(ierr.ne.0) call usage()
      end

      function fmust(c,str)
c get mandatory real argument, call usage statement if missing
      character c
      character*255 str

      fmust=fopt(c,1,ierr,str)
      if(ierr.ne.0) call usage()
      end

      subroutine smust(c,string,str)
c get mandatory string argument, call usage statement if missing
      character c
      character*(*) string
	character*255 str

      call sopt(c,1,string,ierr,str)
      if(ierr.ne.0) call usage()
      end


      function ican(c,idef,str)
c get optional integer argument, provide default if missing
      character c
	character*255 str

      ican=iopt(c,1,ierr,str)
      if(ierr.ne.0) ican=idef
      end

      
      function fcan(c,fdef,str)
c get optional real argument, provide default if missing
      character c
      character*255 str

      fcan=fopt(c,1,ierr,str)
      if(ierr.ne.0) fcan=fdef
      end

      subroutine stcan(c,string,dstring,str)
c get optional string argument, provide default if missing
      character c
      character*(*) string, dstring
	character*255 str


      call sopt(c,1,string,ierr,str)
      if(ierr.ne.0) string=dstring
      end

      function igetout(fout,iverb,str)
c gets alternate output file name, default " "
c return 1 if fout must be determined from input file name
      character*(*) fout
      character*255 str

      igetout=0
      call stcan("o",fout," ",str)
      if(fout.ne." ".and.nstrings().gt.1.and.iv_io(iverb).ne.0) 
     .   write(istderr(),*) '*** single output file for multiple'//
     .   ' input files - results may be overwritten'
      if(fout.ne." ") return
      igetout=lopt("o",1,str)
      end

      subroutine imcan(c,mmax,mc,ilist,str)
c get optional integer argument with multiple comma separated values
      character c
      character*72 string
      dimension ilist(*)
	character*255 str

      call stcan(c,string," ",str)
      string(index(string," "):index(string," "))=","
      do 10 m=1,mmax
         if(index(string,",").le.1) goto 1
         read(string(1:index(string,",")-1),*,err=1,end=1) ilist(m)
 10      string=string(index(string,",")+1:72)
 1    mc=m-1
      end

      subroutine fmcan(c,mmax,mc,flist,str)
c get optional real argument with multiple comma separated values
      character c
      character*72 string
      dimension flist(*)
      character*255 str

      call stcan(c,string," ",str)
      string(index(string," "):index(string," "))=","
      do 10 m=1,mmax
         if(index(string,",").le.1) goto 1
         read(string(1:index(string,",")-1),*,err=1,end=1) flist(m)
 10      string=string(index(string,",")+1:72)
 1    mc=m-1
      end


      subroutine usage()
      return
      end

	subroutine getparam(str,options_filename)
	character*255 str
        character*30 options_filename
c   assigning number 3 to the parameter file (parameters.$$$) handle
	integer pfilehandle
	pfilehandle = 3
	open(pfilehandle, file=options_filename,status='UNKNOWN')	
	read (pfilehandle,10) str
 10   format(A255)
	close(pfilehandle)
	return
	end


