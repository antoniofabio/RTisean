c iterate Henon map
c Copyright (C) Thomas Schreiber (1998)

c modified by Alexei Grigoriev, 27.4.2006 
      
      subroutine henon(options_filename)
      character*30 options_filename
      double precision xo, yo, xn, yn, a, b
      character*72 fout
      data iverb/1/
      character*255 str      

      a=1.4
      b=0.3
      ntrans=10000
      xo=0.68587
      yo=0.65876
      
      call getparam(str,options_filename)
      call whatido("Henon map",iverb,str)
      nmax=imust('l',str)
      ntrans=ican('x',ntrans,str)
      a=fcan('A',real(a),str)
      b=fcan('B',real(b),str)
      xo=fcan('X',real(xo),str)
      yo=fcan('Y',real(yo),str)
      isout=igetout(fout,iverb,str)

      if(isout.eq.1) fout="henon.dat"
      call outfile(fout,iunit,iverb)
      n=-ntrans
 1    n=n+1
      xn=1.-a*xo**2+b*yo
      yn=xo
      xo=xn
      yo=yn
      if(n.lt.1) goto 1
      write(iunit,*) real(xn), real(yn)
      if(nmax.eq.0.or.n.lt.nmax) goto 1
      end


