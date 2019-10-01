
procedure razdsms(x,k,p1,gamma,srx,ssx,s1)
Local i,deltp,deltgam,lnpopt,gamopt,p1opt,s2,stnd1,stnd2,lnpx,r,rr,h
*Разделение смеси по Х
srx=0
ssx=0
for i:=1 to k
  srx=srx+x[i]
  ssx=ssx+x[i]*x[i]
next
srx=srx/k
ssx=(ssx-srx*srx*k)/(k-1)
deltp=1.0000/k
deltp=iif(deltp<0.05,0.05,deltp)
deltgam=1
p1=1-deltp

lnpopt=0
for i:=1 to k
  r=(x[i]-srx)*(x[i]-srx)
  lnpopt=lnpopt-r/(ssx+ssx)
next
lnpopt=lnpopt-0.5*k*LOG(ssx)
gamopt=10000000000
s1opt=ssx
p1opt=1
?STR(lnpopt,15,8)
inkey(0)
  
do while p1>0
  gamma=9
  do while gamma<=40
    h=p1+(1-p1)*gamma
    s1=ssx/h
    stnd1=SQRT(s1)
    s2=gamma*s1
    stnd2=SQRT(s2)
    lnpx=k*LOG((1-p1)/stnd2)
    for i:=1 to k
      r=-(x[i]-srx)*(x[i]-srx)/2
      lnpx=lnpx+r/s2
      rr=r*(1/s1-1/s2)
      if rr>-20 
        lnpx=lnpx+LOG(p1*stnd2*EXP(rr)/(stnd1*(1-p1))+1)
      endif
    next
@5,1 say "p1="+STR(p1,6,3)+"  gamma="+STR(gamma,5,1)+"  ф.правд.="+;
    STR(lnpx,15,5)
    if lnpx>lnpopt
      p1opt=p1
      gamopt=gamma
      lnpopt=lnpx
      s1opt=s1
      @7,1 say "popt="+STR(p1opt,6,3)+"  gamopt="+STR(gamopt,5,1)+;
         "   ф.правд.="+STR(lnpopt,15,5)
    endif
    gamma=gamma+deltgam
  enddo
  p1=p1-deltp
enddo
p1=p1opt
gamma=gamopt
s1=s1opt
return

