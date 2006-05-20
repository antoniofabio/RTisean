c Utilities for usage message
c Copyright (C) T. Schreiber (1998)

c modified by Alexei Grigoriev, 27.4.2006 

      subroutine whatido(text,iverb,str)
      character*72 progname
      character*(*) text
      character*255 str
	progname = ''

c     call getarg(0,progname,str)
c the following argdel initializes parameters 'parser'
      call argdel(0,str)
      iverb=igetv(iverb,str)
      if(iv_io(iverb).eq.1) then
c  to avoid R run-time errors the following is commented  
c         write(istderr(),'()')  
c         write(istderr(),'(a)') 
c     .      "TISEAN 2.1 (C) R. Hegger, H. Kantz, T. Schreiber"
c         write(istderr(),'()')  
c         write(istderr(),'(a,a,a)') 
c     .      progname(1:index(progname," ")-1), ": ", text
      endif
      if(lopt("h",1,str).eq.1) call usage()
      end

      subroutine whatineed(text)
      character*72 progname
      character*(*) text
c	progname=''	

c     call getarg(0,progname)
c     write(istderr(),'()') 
c     write(istderr(),'(a,a,x,a)') 
c    .   "Usage: ", progname(1:index(progname," ")-1),  text
      end

      subroutine popt(c,text)
      character*(*) c,text

c     write(istderr(),'(5h    -,a,x,1h<,a,1h>)') c, text
      end

      subroutine ptext(text)
      character*(*) text

c     write(istderr(),'(3x,a)') text
      end

      subroutine pout(text)
      character*(*) text

c     write(istderr(),'(8h    -o <,a,a,1h>)') 
c    .   "output file name, just -o means ", text
      end

      subroutine pall()

      call popt("V","verbosity level (0 = only fatal errors)")
      call popt("h","show this message")
      write(istderr(),'()')
      end


