c get command line options
c Copyright (C) T. Schreiber (1998)

c modified by Alexei Grigoriev, 27.4.2006 

      function iopt(c,ith,ierr,str)
c get ith occurence of switch -c as integer
      character*255 str
	character*72 argv
      character c

      iopt=0
      ifound=0
      do 10 i=1,myiargc(str)
         call mygetarg(i,argv,str)
         if(argv(1:2).eq.'-'//c) then
            ifound=ifound+1
            if(ifound.eq.ith) then
               call argdel(i,str)
               if(argv(3:72).ne.' ') then
                  iopt=i_s(argv(3:72),ierr)
               else if(i+1.le.myiargc(str)) then
                  call mygetarg(i+1,argv,str)
                  iopt=i_s(argv,ierr)
                  if(ierr.eq.0) call argdel(i+1,str)
               else
                  ierr=1
               endif
               return
            endif
         endif
 10      continue
      ierr=1
      end

      function fopt(c,ith,ierr,str)
c get ith occurence of switch -c as real
	character*255 str
      character*72 argv
      character c

      fopt=0
      ifound=0
      do 10 i=1,myiargc(str)
         call mygetarg(i,argv,str)
         if(argv(1:2).eq.'-'//c) then
            ifound=ifound+1
            if(ifound.eq.ith) then
               call argdel(i,str)
               if(argv(3:72).ne.' ') then
                  fopt=f_s(argv(3:72),ierr)
               else if(i+1.le.myiargc(str)) then
                  call mygetarg(i+1,argv,str)
                  fopt=f_s(argv,ierr)
                  if(ierr.eq.0) call argdel(i+1,str)
               else
                  ierr=1
               endif
               return
            endif
         endif
 10      continue
      ierr=1
      end

      subroutine sopt(c,ith,string,ierr,str)
c get ith occurence of switch -c as string
	character*255 str
      character*(*) string
      character c

      ifound=0
      do 10 i=1,myiargc(str)
         call mygetarg(i,string,str)
         if(string(1:2).eq.'-'//c) then
            ifound=ifound+1
            if(ifound.eq.ith) then
               call argdel(i,str)
               if(string(3:).ne.' ') then
                  string=string(3:)
                  ierr=0
               else if(i+1.le.myiargc(str)) then
                  call mygetarg(i+1,string,str)
                  if(string(1:1).eq."-") then
                     ierr=1
                     return
                  endif
                  call argdel(i+1,str)
                  ierr=0
               else
                  ierr=1
               endif
               return
            endif
         endif
 10      continue
      ierr=1
      end

      function lopt(c,ith,str)
c test if ith occurence of switch -c is present
      character*72 argv
      character c
	character*255 str

      lopt=0
      ifound=0
      do 10 i=1,myiargc(str)
         call mygetarg(i,argv,str)
         if(argv(1:2).eq.'-'//c) then
            ifound=ifound+1
            if(ifound.eq.ith) then
               lopt=1
               call argdel(i,str)
               return
            endif
         endif
 10      continue
      end

      function iget(inum,str)
c get inum'th argument as integer
      character*72 argv
	character*255 str      

      iget=0
      call mygetarg(inum,argv,str)
      if(argv.eq.' ') 
     .   write(istderr(),'(a,i10)') "iget: missing integer argument",inum
      iget=i_s(argv,ierr)
      if(ierr.ne.0) 
     .   write(istderr(),'(a,i10)') "iget: integer argument expected:",inum
      end

      function fget(inum,str)
c get inum'th argument as real
      character*72 argv
	character*255 str
      
      fget=0
      call mygetarg(inum,argv,str)
      if(argv.eq.' ') 
     .   write(istderr(),'(a)') "fget: missing real argument",inum
      fget=f_s(argv,ierr)
      if(ierr.ne.0) 
     .   write(istderr(),'(a)') "fget: real argument expected:;",inum
      end

	
	subroutine mygetarg(i,argv,str)
	character*72 argv
	character*255 str
c	getarg applied to the string str
	integer i
        integer j
        integer ln
        integer pos,posl,posr
        
        ln=myiargc(str)
        if (i.gt.ln) then
            argv=''
            return
        endif
	
        pos=0
        
        do 10 j=1,i
            call getnextword(pos,posl,posr,str)
            pos=posr
  10    continue
        
        argv = str(posl:posr)
	return
	end
	

	integer function myiargc(str)
c 	iargc applied to the string str
        character*255 str
        integer i,posl,posr,pos
        i=0
        posl=1
        pos=0
  10    if (posl.ne.0) then
            call getnextword(pos,posl,posr,str)
            pos=posr
            if (posl.ne.0) then
                i=i+1
            endif
            goto 10
        endif
        myiargc = i
	return
	end

        
	
	subroutine getnextword(pos,posl,posr,str)
c  starting from position pos, searches for the next word (str(posl:posr)), delimited by spaces	
c  pos must be strictly smaller than the actual position where the next word starts
c  pos=0 is allowed
	character*255 str
	integer pos,posl,posr
	integer i 
	integer spacefound,nonspacefound 
	i=pos
	spacefound = 0
	nonspacefound = 0	

	if (i.gt.len(str)) then
		posl=0
		posr=0
		return
	endif
	
        if (pos.gt.0) then
	i=pos
  10	if ((spacefound.eq.0).AND.(i.le.len(str))) then
		if (str(i:i).eq.' ') then 
			spacefound=1
		else
			i = i+1
		endif
		goto 10
	endif
	if (i.gt.len(str)) then
		posl=0
		posr=0
		return
	endif
        else
        i=1
        endif
        
	
  20	if ((nonspacefound.eq.0).AND.(i.le.len(str))) then
		if (str(i:i).ne.' ') then 
			nonspacefound=1
		else
			i = i+1
		endif
		goto 20
	endif

	if (i.gt.len(str)) then
		posl=0
		posr=0
		return
	endif

	posl = i

	spacefound = 0
	
  30	if ((spacefound.eq.0).AND.(i.le.len(str))) then
		if (str(i:i).eq.' ') then 
			spacefound=1
		else
			i = i+1
		endif
		goto 30
	endif

	if (i.gt.len(str)) then
		posr=len(str)
		return
	endif
	
	posr = i-1
	
	return
	end


