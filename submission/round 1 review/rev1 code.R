#R CODE computing share of .04s to .05s with 90% power 
#Critical t-values for "strong reanalaysis"
t.03875=abs(qt(.03875/2,df=38))
t.04    =abs(qt(.04/2,   df=38))
t.04875=abs(qt(.04875/2,df=38))
t.05    =abs(qt(.05/2,   df=38))

#Power of true studies
power=.9

#Effect size giving 90% power
library(pwr)
d=pwr.t.test(n=20,power=power)$d

#Noncentrality parameter for desired power
ncp=d*sqrt(10)

#Expected ratio of (.03875-.04) vs (.04875-.05)
#Probability that p<.04
prop1=1-pt(t.04,df=38,ncp=ncp)
#probability that p<.03875
prop2=1-pt(t.03875,df=38,ncp=ncp)


#Probability that p<.05
prop3=1-pt(t.05,df=38,ncp=ncp)
#probability that p<.04875
prop4=1-pt(t.04875,df=38,ncp=ncp)

#Share of p-values in bin .03875-.04
bin.04=prop1-prop2
bin.05=prop3-prop4

#Ratio of .04 to .05
bin.04/bin.05
