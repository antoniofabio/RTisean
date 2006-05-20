c iterate AR model, e.g. as fitted by ar-model (Dresden)
c Copyright (C) T. Schreiber (1999)

c modified by Alexei Grigoriev, 27.4.2006 
	subroutine ar_run(options_filename)
        character*30 options_filename
      parameter(npmax=100)
      character*72 file, fout, fline
      dimension x(-npmax:npmax), a(npmax)
      external rand
c     data np/npmax/, ntrans/10000/, iuni/0/
	integer np,ntrans,iuni
      data iverb/1/
	character*255 str

c   initializing defaults
	np=nmax
	ntrans=10000
	iuni=0
	
	call getparam(str,options_filename)

	call whatido("iterate AR model, e.g. as fitted by ar-model",iverb,str)

      np=ican("p",np,str)
      if(np.gt.npmax) goto 999
c      if(np.gt.npmax) stop "ar-run: make npmax larger."
      nmax=imust('l',str)
      ntrans=ican("x",ntrans,str)
      if(lopt("u",1,str).eq.1) iuni=1
      r=rand(sqrt(abs(fcan("I",0.0,str))))
      isout=igetout(fout,iverb,str)

      do 10 n=1,npmax
         x(-n)=0.
 10      x(n)=0.
      call nthstring(1,file,str)
      call infile(file,iunit,iverb)
      read(iunit,'(a)') fline
      if(fline(1:1).eq."#") then
         read(fline(18:72),'(f20.0)',err=999) var
         do 20 j=1,np
            read(iunit,'(a1,f20.0)',err=999) fline(1:1), a(j)
 20         if(fline(1:1).ne."#") goto 1
      else
         read(fline(1:72),'(f20.0)',err=999) var
         do 30 j=1,np
 30         read(iunit,'(f20.0)',err=999,end=1) a(j)
      endif
 1    np=j-1
      if(iv_echo(iverb).eq.1) then
         write(istderr(),*) 'coefficients:      ', (a(i),i=1,np)
         write(istderr(),*) 'driving amplitude: ', var
      endif
      if(isout.eq.1) fout="ar.dat"
      call outfile(fout,iunit,iverb)
      n=-ntrans
 2    n=n+1
      nn=mod(n+ntrans,np)+1
      xx=rgauss(0.0,var)
      do 40 j=1,np
 40      xx=xx+a(j)*x(nn-j)
      x(nn)=xx
      x(nn-np)=xx
      if(n.lt.1) goto 2
      write(iunit,*) xx
      if(nmax.eq.0.or.n.lt.nmax) goto 2
 99   close(iunit)
	return

 999  return
c	write(istderr(),'(a)') "wrong input format! try:"
c     write(istderr(),'(a)') "(rms of increments)"
c     write(istderr(),'(a)') "a(1)"
c     write(istderr(),'(a)') "a(2)"
c     write(istderr(),'(a)') "..."
      
	end


