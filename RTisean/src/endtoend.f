c Determine end-to-end mismatch before making surrogate data
c Copyright (C) T. Schreiber (1999)

c modified by Alexei Grigoriev, 27.4.2006 

	subroutine endtoend(options_filename)
        character*30 options_filename

      parameter(nx=100000,mx=20)
      dimension x(nx,mx), icol(mx)
      character*72 file, fout
      data iverb/15/
	character*255 str	
	call getparam(str,options_filename)	

      call whatido("Determine end-to-end mismatch",iverb,str)
      nmax=ican("l",nx,str)
      nexcl=ican("x",0,str)
      wjump=fcan("j",0.5,str)
      mcmax=ican("m",0,str)
      call columns(mc,mx,icol,str)
      if(mcmax.eq.0) mcmax=max(1,mc)
      isout=igetout(fout,iverb,str)

      call nthstring(1,file,str)
      call xreadfile(nmax,mcmax,nx,x,nexcl,icol,file,iverb)
      if(file.eq."-") file="stdin"
      if(isout.eq.1) call addsuff(fout,file,"_end")
      call outfile(fout,iunit,iverb)
      nmaxp=nmax
      etotm=mcmax
 1    nmaxp=nless(nmaxp)
      call jump(nmax,nmaxp,nx,x,mcmax,wjump,etot,ejump,eslip,njump)
      if(etot.lt.etotm) then
         etotm=etot
         write(iunit,'(a,i7,a,i7,a,f5.1,a)')
     .      "length:", nmaxp, 
     .      "  offset: ", nexcl+njump,
     .      "  lost: ", real(nmax-nmaxp)/real(nmax)*100, " %"
         write(iunit,*) "      jump: ", ejump*100, " %"
         write(iunit,*) "      slip: ", eslip*100, " %" 
         write(iunit,*) "  weighted: ", etot*100, " %"
         write(iunit,'()')
      endif
      if(etot.lt.1e-5) goto 2
      nmaxp=nmaxp-1
      if(nmaxp.gt.2) goto 1
 2	close(iunit)
      return
	end

      subroutine jump(nmax,nmaxp,nx,x,mcmax,wjump,etot,ejump,eslip,
     .   njump)
c loop through time ofsets to minimize jump effect
      dimension x(nx,*)

      etot=mcmax
      do 10 nj=0,nmax-nmaxp
         xj=0
         sj=0
         do 20 m=1,mcmax
            xj=xj+xjump(nmaxp,x(1+nj,m))
 20         sj=sj+sjump(nmaxp,x(1+nj,m))
         if(wjump*xj+(1-wjump)*sj.ge.etot) goto 10
         etot=wjump*xj+(1-wjump)*sj
         ejump=xj
         eslip=sj
         njump=nj
 10      continue
      end

      function xjump(nmax,x)
c contribution of end effect to 1st derivative
      dimension x(*)

      call rms(nmax,x,sc,sd)
      xjump=0
      if(sd.eq.0.) return
      xjump=(x(1)-x(nmax))**2/(nmax*sd**2)
      end

      function sjump(nmax,x)
c contribution of end effect to 2nd derivative
      dimension x(*)

      call rms(nmax,x,sc,sd)
      sjump=0
      if(sd.eq.0.) return
      sjump=((x(nmax)-x(nmax-1))-(x(2)-x(1)))**2 / (nmax*sd**2)
      end


