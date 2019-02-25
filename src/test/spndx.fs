s(s1,s2,n1,n2) {return log(s1/s2)/log(n1/n2);}
s1=0.915pm0.071
s2=2.215pm0.166
n1=610
n2=240
t=log(s1/s2)/log(n1/n2);
t=sqrt((s1.rms/s1.val)^2 + (s2.rms/s2.val)^2)/log(n1/n2)
print "Spectal index = ",s(s1,s2,n1,n2),"\nComputed error by hand = ",t,"\n";
quit

