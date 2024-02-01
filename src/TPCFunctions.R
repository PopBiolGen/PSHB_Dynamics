# TPC function from Martin and Huey 2008 with example parameter values
TPC<-function(Tb, rmax=10, beta=6, Trmax=29, alpha=0.2){
  rmax*exp(-(exp(beta*(Tb-Trmax)-8)+alpha*(Tb-Trmax)^2))
}

#negative Log-Likelihood function - calculates negative log-likelihood of data given model
TPC.LL<-function(pars, t.data, w.data){
  E.w<-TPC(Tb=t.data, rmax=pars[1], beta=pars[2], Trmax=pars[3], alpha=pars[4])
  D.w<-w.data-E.w
  D.w<-dnorm(D.w, mean=0, sd=sd(D.w), log=TRUE) 
  -sum(D.w)
}


TPC.fit<-function(Tw.matrix, in.beta=2, in.alpha=0.04, ...){
	in.rmax=max(Tw.matrix[,2])
	in.Trmax=Tw.matrix[Tw.matrix[,2]==in.rmax,1]
	fit<-optim(fn=TPC.LL, par=c(rmax=in.rmax, beta=in.beta, Trmax=in.Trmax, alpha=in.alpha), t.data=Tw.matrix[,1], w.data=Tw.matrix[,2], method="L-BFGS-B",
		lower=c(0, 0, 10, 0), upper=c(1.5*in.rmax, 10, 45, 1), ...)
	fit
}

# A simple TPC based on the meeting of two Gaussian functions
TPC.q<-function(Tb, rmax=10, Trmax=28, acc=9, dec.prop=0.5){
	lhs<-rmax*exp(-(Tb-Trmax)^2/(2*acc^2))
	rhs<-rmax*exp(-(Tb-Trmax)^2/(2*(acc*dec.prop)^2))
	ifelse(Tb<Trmax, lhs, rhs )
}

#negative Log-Likelihood function - calculates negative log-likelihood of data given model
TPC.q.LL<-function(pars, t.data, w.data){
  E.w<-TPC.q(Tb=t.data, rmax=pars[1], Trmax=pars[2], acc=pars[3], dec.prop=pars[4])
  D.w<-w.data-E.w
  D.w<-dnorm(D.w, mean=0, sd=sd(D.w), log=TRUE) 
  -sum(D.w)
}

TPC.q.fit<-function(Tw.matrix, in.acc=10, in.dec.prop=0.5, ...){
	in.rmax=max(Tw.matrix[,2])
	in.Trmax=Tw.matrix[Tw.matrix[,2]==in.rmax,1]
	fit<-optim(fn=TPC.q.LL, par=c(rmax=in.rmax, Trmax=in.Trmax, acc=in.acc, dec.prop=in.dec.prop),
		t.data=Tw.matrix[,1], w.data=Tw.matrix[,2], method="L-BFGS-B",
		lower=c(0, 10, 1, 0.001), upper=c(1.5*in.rmax, 45, 80, 1), ...)
	fit
}

# The breadth at 80% performance for TPC.q
breadth.q<-function(rmax=10, Trmax=25, acc=10, dec.prop=0.5, perc=80){acc*(1+dec.prop)*sqrt(-2*log(perc/100))}

# returns the optimal temp range (defined by a % of max performance)
opt.range.q<-function(rmax=10, Trmax=25, acc=10, dec.prop=0.5, perc=80){
	lower<-Trmax-acc*sqrt(-2*log(perc/100))
	upper<-Trmax+acc*dec.prop*sqrt(-2*log(perc/100))
	out<-c(lower, Trmax, upper)
	names(out)<-c("lower", "mid", "upper")
	out
}

# Calculates mean fitness with variance in Tb
fitness<-function(Tpref, sd.Tpref, rmax, beta, Trmax, alpha, bins=1000){
  w<-c()
  #browser()
  for (ii in 1:length(Tpref)){
    upper=Tpref[ii]+3*sd.Tpref
    lower=Tpref[ii]-3*sd.Tpref
    X<-seq(lower, upper, length.out=bins)
    weights<-dnorm(x=X, mean=Tpref[ii], sd=sd.Tpref)
    weights<-weights/sum(weights)
    Y<-TPC(Tb=X, rmax=rmax, beta=beta, Trmax=Trmax, alpha=alpha)
    w<-c(w, sum(Y*weights))
  }
  w
}

#finds approximate optimal Tpref for a given TPC curve and sd.Tpref
fitness.opt<-function(sd.Tpref, rmax, beta, Trmax, alpha, interval, ...){
  tprange<-seq(interval[1], interval[2], 0.1)
  w<-fitness(Tpref=tprange, sd.Tpref, rmax, beta, Trmax, alpha)
  tprange[w==max(w)]
}

# the breadth function for the G*G curve
breadth<-function(percentile, rmax, beta, Trmax, alpha){
  num.min<-function(Tb, percentile, rmax, beta, Trmax, alpha){
    (TPC(Tb, rmax, beta, Trmax, alpha)-percentile*rmax)^2
  }
  lower<-optimize(f=num.min, lower=0, upper=Trmax, percentile=percentile, rmax=rmax, beta=beta, Trmax=Trmax, alpha=alpha)$minimum
  # note: the upper part of the curve fools the optimizer unless the upper bound of the search is very tight.
  # the fix below has not been robustly tested, so if weird results are forthcoming, this is why.
  upper<-optimize(f=num.min, lower=Trmax, upper=2*Trmax-lower, percentile=percentile, rmax=rmax, beta=beta, Trmax=Trmax, alpha=alpha)$minimum
  breadth<-upper-lower
  breadth
}

plot.tpc<-function(x, y, pars, pars.q, ...){
	plot(y~x, ...)
	lx<-seq(0, 45, 0.2)
	ly<-TPC.q(lx, pars.q[1], pars.q[2], pars.q[3], pars.q[4])
	lines(lx, ly, col="green")
	if (!is.null(pars)) {
		ly2<-TPC(lx, pars[1], pars[2], pars[3], pars[4])
		lines(lx, ly2, col="red")
	}
	
}

