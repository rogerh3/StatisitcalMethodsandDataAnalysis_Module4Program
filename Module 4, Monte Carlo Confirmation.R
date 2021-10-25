#Roger H Hayden III
#9/6/2021
#Statistical Methods and Data Analysis
#Monte Carlo Confirmation - Module 4

#We will confirm your results in the HW for problems 3.10.4 and 3.10.6.

#To do this, you can use the Inverse Transform Sampling Method for random number generation
#from an arbitrary continuous distribution having an inverse. For a continuous random variable,
#X, having distribution F_x(x) it is possible to generate random deviates of X by forming
#F^(-1)_x(u),where u is a random number from a uniform distribution on [0,1].

#MC 3.10.4

#Find the cdf for the density in problem 3.10.4. Use that to randomly generate a sample of size
#5, call it y. Sort the sample and determine if the interval Y’1 to Y’5 contains 0.60. The interval is
#y[1] to y[5]. Repeat for 1000 trials and compare with your theoretical result.

reps = 1000
c=array(dim=1)

for(i in 1:reps){
  u=runif(5,0,1)
  x=sort(sqrt(u))
  if(x[1]<0.6 && x[5]>0.6){
   c[i]=1
  }else{
    c[i]=0
  }
}
x = mean(c)
x

difference = abs(0.887 - x)
difference

#MC 3.10.6

#After determining n analytically from problem 3.10.6, sample n values repeatedly from the
#distribution of Y. You will use the approach above, first finding the distribution function and
#then using the inverse distribution evaluated at a random uniform value on [0,1] to generate a
#random deviate for Y. For each of the repetitions of n values, sort the y values and identify the
#minimum y[1] for comparison to to 0.2. Compute the sample proportion in 1000 trials of n
#values to see if the P(Ymin<0.2)>0.9. If your computed n is correct, the probability should be very
#close to 0.9.


n<-12
trials<-1000

u<-runif(n*R)
y<- -log(1-u)
y

x<-matrix(y,nrow=R)
x

ymin<-apply(y,1,min)
ymin

prob<-sum(ymin<0.2)/R
prob

difference2 = 0.9 - prob
difference2