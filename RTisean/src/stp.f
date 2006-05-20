c space time separation plot
c see  H. Kantz, T. Schreiber, Nonlinear Time Series Analysis, Cambridge
c      University Press (1997)
c Copyright (C) T. Schreiber (1997)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine mystp(options_filename)
        character*30 options_filename
      parameter(nx=100000,mdt=500,mfrac=100)
      dimension x(nx), stp(mfrac,mdt)
      character*72 file, fout
c     data idt/1/, perc/0.05/, ndt/100/
	real perc
	integer idt,ndt
      data iverb/1/
	character*255 str	

	idt=1
	perc=0.05
	ndt=100

	call getparam(str,options_filename)	

      call whatido("space-time separation plot",iverb,str)
      id=imust("d",str)
      m=imust("m",str)
      idt=ican("#",idt,str)
      ndt=min(ican("t",ndt,str),mdt)
      perc=fcan("%",perc,str)
      nfrac=min(mfrac,int(1/perc))
      perc=1./real(nfrac)
      nmax=ican("l",nx,str)
      nexcl=ican("x",0,str)
      jcol=ican("c",0,str)
      isout=igetout(fout,iverb,str)
      if(iv_io(iverb).eq.1) write(istderr(),*) "computing ", nfrac, 
     .   " levels at fractions ", perc, 2*perc, "..."

      call nthstring(1,file,str)
      call readfile(nmax,x,nexcl,jcol,file,iverb)
      call minmax(nmax,x,xmin,xmax)
      call stplot(nmax,x,id,m,xmax-xmin,stp,nfrac,ndt,idt)
      if(isout.eq.1) call addsuff(fout,file,"_stp")
      call outfile(fout,iunit,iverb)
      do 10 iper=1,mfrac
         do 20 it=1,ndt
 20         write(iunit,*) it*idt, stp(iper,it)
 10      write(iunit,'()')
	close(iunit)
	return
      end

      
      subroutine stplot(nmax,y,id,m,epsmax,stp,nfrac,mdt,idt)
      parameter(meps=1000,mfrac=100)
      dimension y(nmax),stp(mfrac,mdt),ihist(meps)

      do 10 it=1,mdt
         do 20 ieps=1,meps
 20         ihist(ieps)=0
         do 30 n=it*idt+(m-1)*id+1,nmax
            dis=0                            ! compute distance in m dimensions
            do 40 me=0,m-1
 40            dis=max(dis,abs(y(n-me*id)-y(n-me*id-it*idt)))
            ih=min(int(meps*dis/epsmax)+1,meps)
 30         ihist(ih)=ihist(ih)+1
         do 10 ifrac=1,nfrac
            need=(nmax-it*idt-(m-1)*id)*ifrac/real(nfrac)
            is=0
            do 50 ieps=1,meps
               is=is+ihist(ieps)
 50            if(is.ge.need) goto 1
 1          stp(ifrac,it)=ieps*epsmax/meps
 10      continue
      end






