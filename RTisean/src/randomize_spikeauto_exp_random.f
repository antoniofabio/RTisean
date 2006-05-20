c constrained randomization
c Copyright (C) T. Schreiber (1999)

c modified by Alexei Grigoriev, 27.4.2006 
	subroutine randomize_spikeauto_exp_random(options_filename)
        character*30 options_filename
      parameter(nx=10000,mx=20) 
      double precision time
      dimension x(nx,mx), y(nx,mx), xx(nx,mx), icol(mx)
      character*72 file, fout, comment
      common nmax,cost,temp,cmin,rate,x
      external rand
c     data wr/0.9/, nsur/1/
	real wr
	integer nsur
      data iverb/15/
	character*255 str

	wr=0.9
	nsur=1

	call getparam(str,options_filename)

      call whatido("constrained randomization",iverb)
      
      rr=rand(ican("I",0)/real(2**22))
      nsur=min(999,ican("n",nsur))
      nmax=ican("l",nx)
      nexcl=ican("x",0)
      mcmax=ican("m",0)
      call columns(mc,mx,icol)
      if(mcmax.eq.0) mcmax=max(1,mc)
      wr=fcan("u",wr)
      call opts_cost_spikeauto(mcmax)
      call opts_cool_exp()
      call opts_permute_random()
      isout=igetout(fout,iverb)

      call nthstring(1,file)
      call xreadfile(nmax,mcmax,nx,xx,nexcl,icol,file,iverb)
      call cost_transform_spikeauto(nmax,mcmax,nx,xx)
      if(file.eq."-") file="stdin"
      if(isout.eq.1) call addsuff(fout,file,"_rnd")
      if(nsur.gt.1) call suffix(fout,"_000")

      do 10 isur=1,nsur
         do 20 n=1,nmax
            do 20 m=1,mcmax
 20            x(n,m)=xx(n,m)
         rate=1
         cost=r1mach(2)
         cmin=cost
         if(nsur.gt.1) write(fout(index(fout," ")-3:72),'(i3.3)') isur
         temp=cool_init_exp()
         call cost_init_spikeauto()
         call permute_init_random()
         call cost_full_spikeauto(iv_vcost(iverb))
         cmin=cost
         time=0
 1       time=time+1.
         call permute_random(n1,n2)
         cmax=cost-temp*log(rand(0.0))    ! maximal acceptable cost
         call cost_update_spikeauto(n1,n2,cmax,iaccept,iv_vmatch(iverb))
         tnew=cool_exp(iaccept,iend,iv_cool(iverb))
         if(tnew.ne.temp.or.cost.lt.cmin*wr) then
            cc=cost
            call cost_full_spikeauto(iv_vcost(iverb))
            if(iv_match(iverb).eq.1) write(istderr(),*) 
     .         "cost function mismatch: ", abs((cc-cost)/cost)
         endif
         temp=tnew
         if(cost.lt.cmin*wr) then
            if(iv_cost(iverb).eq.1) write(istderr(),*) 
     .         "after ",real(time)," steps at T=",temp," cost: ",cost
            cmin=cost
            call cost_inverse_spikeauto(nmax,mcmax,nx,x,y)
            write(comment,'(8h# cost: ,g15.5)') cost
            call xwritecfile(nmax,mcmax,nx,y,fout,iverb,comment)
         endif
         if(iend.ne.1) goto 1
         write(comment,'(8h# cost: ,g15.5)') cost
         call writecfile(nmax,mcmax,nx,y,fout,iverb,comment)
 10      continue
      end

      

