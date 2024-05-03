pop<-read.csv("https://raw.githubusercontent.com/EdaGKocyigit/simulationcode/main/datasets/big51.65.csv")  #Read the population
colnames(pop)=c("x","y") #Describe the variables
Sim<-1  #Number of simulation
w1<-0.5 # Weight for EWMA
w2<-0.1 # Weight for HEWMA
n<-10  # Sample size
t<-2  # Number of past and current samples

mu0<-numeric(Sim);mu1<-numeric(Sim);mu2<-numeric(Sim);mu3<-numeric(Sim);mu4t<-numeric(Sim);mu5t<-numeric(Sim);mu6t<-numeric(Sim);mu7t<-numeric(Sim) # Define estimators
rss0<-numeric(Sim);rss1<-numeric(Sim);rss2<-numeric(Sim);rss3<-numeric(Sim);rss4t<-numeric(Sim);rss5t<-numeric(Sim);rss6t<-numeric(Sim);rss7t<-numeric(Sim) # Define Mean Square Errors

for (si in 1:Sim) {
  ewmay<-numeric(t);ewmax<-numeric(t);hewmay<-numeric(t);hewmax<-numeric(t)
  for(ti in 1:t) {
    sample <- pop[sample(c(1:nrow(pop)),size = n,replace = F),]
    ewmay[ti] <-  ifelse(ti <= 1,((mean(sample$y))),((w1*mean(sample$y))+((1-w1)*ewmay[ti-1])))
    hewmay[ti]<-  ifelse(ti <= 1,((ewmay[ti])),((w2*mean(sample$y))+((1-w2)*hewmay[ti-1])))
    ewmax[ti] <-  ifelse(ti <= 1,((mean(sample$x))),((w1*mean(sample$x))+((1-w1)*ewmax[ti-1])))
    hewmax[ti]<-  ifelse(ti <= 1,((ewmax[ti])),((w2*mean(sample$x))+((1-w2)*hewmax[ti-1])))
  } 
    cy<-sqrt(var(sample$y))/mean(sample$y);  cx<-sqrt(var(sample$x))/mean(sample$x) # Coefficients for Chhaparwal and Kumar estimator (2022)
    b<-cor(pop$y,pop$x)*sqrt(var(sample$y))/sqrt(var(sample$x)) #Slope coefficient for regression type estimator 
  mu0<-mean(sample$y) #SRS basic mean estimator
  mu1<-(mean(sample$y)*mean(pop$x))/mean(sample$x)  #SRS ratio estimator
  mu2<-(mean(sample$y))*exp((mean(pop$x)-mean(sample$x))/(mean(pop$x)+mean(sample$x)))  #SRS exponential ratio type estimator Bahl Tuteja (1991)
  mu3<-mean(sample$y)+(b*(mean(pop$x)-mean(sample$x))) #SRS  regression estimator
  mu4t<-(hewmay[t]/hewmax[t])*mean(pop$x) #SRS HEWMA estimator Noor ul Amin (2021)
  mu5t<-hewmay[t]*(mean(pop$x)+(cx))/(hewmax[t]+cx)  #Chhaparwal and Kumar estimator (2022)
  mu6t<-hewmay[t]*exp((mean(pop$x)-hewmax[t])/(mean(pop$x)+hewmax[t]))  #Proposed exponential ratio type HEWMA estimator 
  mu7t<-hewmay[t]+(b*(mean(pop$x)-hewmax[t]))  #Proposed HEWMA regression-type estimator 

rss0[si]<-((mu0-mean(pop$y))^2);rss1[si]<-((mu1-mean(pop$y))^2);rss2[si]<-((mu2-mean(pop$y))^2);rss3[si]<-((mu3-mean(pop$y))^2);rss4t[si]<-((mu4t-mean(pop$y))^2);rss5t[si]<-((mu5t-mean(pop$y))^2);rss6t[si]<-((mu6t-mean(pop$y))^2);rss7t[si]<-((mu7t-mean(pop$y))^2)
  
}
re1<-mean(rss0)/mean(rss1);re2<-mean(rss0)/mean(rss2);re3<-mean(rss0)/mean(rss3);re4t<-mean(rss0)/mean(rss4t);re5t<-mean(rss0)/mean(rss5t);re6t<-mean(rss0)/mean(rss6t);re7t<-mean(rss0)/mean(rss7t) # Calculate Relative Efficiency
re1;re2;re3;re4t;re5t;re6t;re7t # RE results
