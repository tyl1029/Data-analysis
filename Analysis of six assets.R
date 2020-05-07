asset.names = c("vfinx","veurx","veiex","vbltx","vbisx","vpacx")
start.date = "2014-01-01"
end.date = "2019-01-31"

vfinx.prices = get.hist.quote(instrument="vfinx", start=start.date,
                              end=end.date, quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")    
veurx.prices = get.hist.quote(instrument="veurx", start=start.date,
                              end=end.date, quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")
veiex.prices = get.hist.quote(instrument="veiex", start=start.date,
                              end=end.date, quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")
vbltx.prices = get.hist.quote(instrument="vbltx", start=start.date,
                              end=end.date, quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")
vbisx.prices = get.hist.quote(instrument="vbisx", start=start.date,
                              end=end.date, quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")
vpacx.prices = get.hist.quote(instrument="vpacx", start=start.date,
                              end=end.date, quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")

index(vfinx.prices) = as.yearmon(index(vfinx.prices))
index(veurx.prices) = as.yearmon(index(veurx.prices))
index(veiex.prices) = as.yearmon(index(veiex.prices))
index(vbltx.prices) = as.yearmon(index(vbltx.prices))
index(vbisx.prices) = as.yearmon(index(vbisx.prices))
index(vpacx.prices) = as.yearmon(index(vpacx.prices))

projectPrices.z = merge(vfinx.prices,veurx.prices,veiex.prices,vbltx.prices,
                        vbisx.prices,vpacx.prices)
colnames(projectPrices.z) = asset.names
projectPrices.df = coredata(projectPrices.z)
rownames(projectPrices.df) = as.character(index(projectPrices.z))

# 1
# compute cc and simple returns
projectReturns.z = diff(log(projectPrices.z))   
projectReturnsSimple.z = exp(projectReturns.z) - 1
projectReturns.df = as.data.frame(coredata(projectReturns.z))
ret.mat = coredata(projectReturns.z)
rownames(projectReturns.df) = as.character(index(projectReturns.z))
projectReturnsSimple.df = as.data.frame(coredata(projectReturnsSimple.z))
rownames(projectReturnsSimple.df) = as.character(index(projectReturnsSimple.z))

# time plot
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
plot(projectPrices.z, col="blue", lwd=2)
plot(projectReturns.z, panel=my.panel, col="blue", lwd=2)
chart.CumReturns(projectReturnsSimple.z, wealth.index=TRUE, legend.loc="topleft", 
                 lwd=2, main="growth of $1") 
#four panel, boxplot
fourPanelPlot(projectReturns.z[, "vfinx", drop=FALSE])
fourPanelPlot(projectReturns.z[, "veurx", drop=FALSE])
fourPanelPlot(projectReturns.z[, "veiex", drop=FALSE])
fourPanelPlot(projectReturns.z[, "vbltx", drop=FALSE])
fourPanelPlot(projectReturns.z[, "vbisx", drop=FALSE])
fourPanelPlot(projectReturns.z[, "vpacx", drop=FALSE])
boxplot(ret.mat, main="Vanguard Returns", col="cornflowerblue")

# univariate
muhat.vals = colMeans(projectReturns.z)
sd.vals = apply(projectReturns.z, 2, sd)
skew.vals = apply(projectReturns.z, 2, skewness)
ekurt.vals = apply(projectReturns.z, 2, kurtosis)
cov.mat = var(projectReturns.z)
cor.mat = cov2cor(cov.mat)
covhat.vals = cov.mat[lower.tri(cov.mat)]
rhohat.vals = cor.mat[lower.tri(cor.mat)]
names(covhat.vals) <- names(rhohat.vals) <- 
  c("vfinx,veurx","vfinx,veiex","vfinx,vbltx", "vfinx,vbisx", "vfinx,vpacx",
    "veurx,veiex", "veurx,vbltx", "veurx,vbisx", "veurx,vpacx",
    "veiex,vbltx", "veiex,vbisx", "veiex,vpacx",
    "vbltx,vbisx", "vbltx,vpacx",
    "vbisx,vpacx") 
q.vals = apply(projectReturns.z, 2, quantile, prob=c(0.01,0.05))
stats.mat = rbind(muhat.vals, 
                  sd.vals,
                  skew.vals,
                  ekurt.vals,
                  q.vals)
rownames(stats.mat) = c("Mean", "Std Dev", "Skewness", 
                        "Excess Kurtosis", "1% Quantile", 
                        "5% Quantile")
stats.mat
cov.mat
# annualize
12*muhat.vals
sqrt(12)*sd.vals

# Sharpe Ratio
rf = 0.005/12
plot(sd.vals, muhat.vals, xlim=c(0, 0.06), ylim=c(0, 0.013),
     ylab="Expected Return", xlab="Standard Deviation",
     cex=2, pch=16, col="cornflowerblue")
text(sd.vals, muhat.vals, labels=colnames(projectReturns.z),
     pos=3)
SharpeRatios = (muhat.vals - rf)/sd.vals
SharpeRatios
sharpeRatio.boot = function(x, idx, risk.free) {
  muhat = mean(x[idx])
  sigmahat = sd(x[idx])
  sharpeRatio = (muhat - risk.free)/sigmahat
  sharpeRatio
}
sharpe.vfinx.boot = boot(ret.mat[, "vfinx"], 
                         statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.vfinx.boot
sharpe.veurx.boot = boot(ret.mat[, "veurx"], 
                         statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.veurx.boot
sharpe.veiex.boot = boot(ret.mat[, "veiex"], 
                         statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.veiex.boot
sharpe.vbltx.boot = boot(ret.mat[, "vbltx"], 
                         statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.vbltx.boot
sharpe.vbisx.boot = boot(ret.mat[, "vbisx"], 
                         statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.vbisx.boot
sharpe.vpacx.boot = boot(ret.mat[, "vpacx"], 
                         statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.vpacx.boot

# mean/sd CI
nobs = nrow(ret.mat)
se.muhat = sd.vals/sqrt(nobs)
rbind(muhat.vals,se.muhat)
mu.lower = muhat.vals - 2*se.muhat
mu.upper = muhat.vals + 2*se.muhat
cbind(mu.lower,mu.upper)

se.sd = sd.vals/sqrt(2*nobs)
rbind(sd.vals,se.sd)
sd.lower = sd.vals - 2*se.sd
sd.upper = sd.vals + 2*se.sd
cbind(sd.lower,sd.upper)

#annualize Sharpe Ratio
rf_a = 0.005
muhat.vals_a = 12*muhat.vals
sd.vals_a = sqrt(12)*sd.vals
12*muhat.vals
sqrt(12)*sd.vals
SharpeRatios_a = (muhat.vals_a - rf_a)/sd.vals_a
SharpeRatios_a
FV_5year = exp(muhat.vals_a*5)
FV_5year

# pair-wise
pairs(ret.mat, col="blue", cex = 0.5)

#correlation
cor.mat
pairs(ret.mat, col="blue",cex = 0.5)
corrplot(cor.mat, method="ellipse")

# 2
# VaR
Value.at.Risk = function(x, p=0.05, w=100000, method=c("normal", "empirical")) {
  method=method[1]
  x = as.matrix(x)
  if (method == "normal") {
    q = apply(x, 2, mean) + apply(x, 2, sd)*qnorm(p)
  } else {    
    q = apply(x, 2, quantile, p)
  }
  VaR = (exp(q) - 1)*w
  VaR
}
# compute 5% and 1% normal VaR for all assets
VaR.normal.05 = Value.at.Risk(ret.mat, p=0.05, method="normal")
VaR.normal.05
VaR.normal.01 = Value.at.Risk(ret.mat, p=0.01)
VaR.normal.01
# empirical VaR
VaR.empirical.05 = Value.at.Risk(ret.mat, p=0.05, method="empirical")
VaR.empirical.05
VaR.empirical.01 = Value.at.Risk(ret.mat, p=0.01, method="empirical")
VaR.empirical.01
# bootstrap vfinx
ValueAtRisk.boot = function(x, idx, p=0.01, w=100000) {
  
  q = mean(x[idx]) + sd(x[idx])*qnorm(p)
  VaR = (exp(q) - 1)*w
  VaR
}
VaR.05.boot.vfinx = boot(ret.mat[, "vfinx"], 
                         statistic=ValueAtRisk.boot, R=999)
VaR.05.boot.vfinx
VaR.05.boot.veurx = boot(ret.mat[, "veurx"], 
                         statistic=ValueAtRisk.boot, R=999)
VaR.05.boot.veurx
VaR.05.boot.veiex = boot(ret.mat[, "veiex"], 
                         statistic=ValueAtRisk.boot, R=999)
VaR.05.boot.veiex
VaR.05.boot.vbltx = boot(ret.mat[, "vbltx"], 
                         statistic=ValueAtRisk.boot, R=999)
VaR.05.boot.vbltx
VaR.05.boot.vbisx = boot(ret.mat[, "vbisx"], 
                         statistic=ValueAtRisk.boot, R=999)
VaR.05.boot.vbisx
VaR.05.boot.vpacx = boot(ret.mat[, "vpacx"], 
                         statistic=ValueAtRisk.boot, R=999)
VaR.05.boot.vpacx

VaR.01.boot.vfinx = boot(ret.mat[, "vfinx"], 
                         statistic=ValueAtRisk.boot, R=999)
VaR.01.boot.vfinx
VaR.01.boot.veurx = boot(ret.mat[, "veurx"], 
                         statistic=ValueAtRisk.boot, R=999)
VaR.01.boot.veurx
VaR.01.boot.veiex = boot(ret.mat[, "veiex"], 
                         statistic=ValueAtRisk.boot, R=999)
VaR.01.boot.veiex
VaR.01.boot.vbltx = boot(ret.mat[, "vbltx"], 
                         statistic=ValueAtRisk.boot, R=999)
VaR.01.boot.vbltx
VaR.01.boot.vbisx = boot(ret.mat[, "vbisx"], 
                         statistic=ValueAtRisk.boot, R=999)
VaR.01.boot.vbisx
VaR.01.boot.vpacx = boot(ret.mat[, "vpacx"], 
                         statistic=ValueAtRisk.boot, R=999)
VaR.01.boot.vpacx

boot.ci(VaR.05.boot.vfinx, conf = 0.95, type = c("norm","perc"))
boot.ci(VaR.05.boot.veurx, conf = 0.95, type = c("norm","perc"))
boot.ci(VaR.05.boot.veiex, conf = 0.95, type = c("norm","perc"))
boot.ci(VaR.05.boot.vbltx, conf = 0.95, type = c("norm","perc"))
boot.ci(VaR.05.boot.vbisx, conf = 0.95, type = c("norm","perc"))
boot.ci(VaR.05.boot.vpacx, conf = 0.95, type = c("norm","perc"))
boot.ci(VaR.01.boot.vfinx, conf = 0.95, type = c("norm","perc"))
boot.ci(VaR.01.boot.veurx, conf = 0.95, type = c("norm","perc"))
boot.ci(VaR.01.boot.veiex, conf = 0.95, type = c("norm","perc"))
boot.ci(VaR.01.boot.vbltx, conf = 0.95, type = c("norm","perc"))
boot.ci(VaR.01.boot.vbisx, conf = 0.95, type = c("norm","perc"))
boot.ci(VaR.01.boot.vpacx, conf = 0.95, type = c("norm","perc"))
plot(VaR.05.boot.vfinx)

# year VaR
Value.at.Risk.year = function(mu, sd,p=0.05,w=100000) {
  q = mu + sd*qnorm(p)
  VaR = (exp(q) - 1)*w
  VaR
}
mu = c(0.102534633,	0.016714559,	0.042753991,	0.047448791,	0.011392787,	0.050095101)
sd = c(0.115755579,	0.137246491,	0.155430255,	0.077862135,	0.013545242,	0.127712992)
Value.at.Risk.year(mu,sd,p=0.01)
Value.at.Risk.year(mu,sd,p=0.05)

# 3
# global min
gmin.port <- globalMin.portfolio(muhat.vals, cov.mat)
attributes(gmin.port)
print(gmin.port)
summary(gmin.port, risk.free=rf)
plot(gmin.port)

Value.at.Risk.port = function(mean, sd, p, w=100000) {
  q = mean + sd*qnorm(p)
  VaR = (exp(q) - 1)*w
  VaR
}
VaR.0.1.gmin = Value.at.Risk.port(gmin.port$er,gmin.port$sd,p = 0.01)
VaR.0.1.gmin
VaR.0.5.gmin = Value.at.Risk.port(gmin.port$er,gmin.port$sd,p = 0.05)
VaR.0.5.gmin

#global min no short sale
gmin.port.ns <- globalMin.portfolio(muhat.vals, cov.mat, shorts=FALSE)
attributes(gmin.port.ns)
print(gmin.port.ns)
summary(gmin.port.ns, risk.free=rf)
plot(gmin.port.ns)
VaR.0.1.gmin.ns = Value.at.Risk.port(gmin.port.ns$er,gmin.port.ns$sd,p = 0.01)
VaR.0.5.gmin.ns = Value.at.Risk.port(gmin.port.ns$er,gmin.port.ns$sd,p = 0.05)
VaR.0.1.gmin.ns
VaR.0.5.gmin.ns

#efficient portfolio
target.return <- max(muhat.vals)
e.port.max<- efficient.portfolio(muhat.vals, cov.mat, target.return)
e.port.max
summary(e.port.max, risk.free=rf)
plot(e.port.max)

#tangency portfolio
tan.port <- tangency.portfolio(muhat.vals, cov.mat, rf)
tan.port
summary(tan.port, risk.free=rf)
plot(tan.port)

## compute efficient frontier
ef <- efficient.frontier(muhat.vals, cov.mat, alpha.min=-1, 
                         alpha.max=1.5, nport=20)
plot(ef, plot.assets=TRUE, col="blue", lwd=2)
points(gmin.port$sd, gmin.port$er, col="orange", lwd=2)
points(tan.port$sd, tan.port$er, col="red", lwd=2)
text(tan.port$sd, tan.port$er, labels="tangency", pos=4)
sr.tan = (tan.port$er - rf)/tan.port$sd
abline(a=rf, b=sr.tan, col="green", lwd=2)
abline(v=0, h=0)
points(0, rf, col="green", lwd=2)
text(0, rf, labels="rf", pos=4)

## compute efficient frontier not allowing short sales
ef.ns <- efficient.frontier(muhat.vals, cov.mat, alpha.min=0, 
                            alpha.max=1, nport=20, shorts=FALSE)
ef.ns
plot(ef, plot.assets=TRUE, col="blue", lwd=2)
points(ef.ns$sd, ef.ns$er, type="b", col="red", lwd=2)
abline(h=0, v=0)
points(0, rf, col="green", lwd=2)
text(0, rf, labels="rf", pos=4)

# tangency
tan.port.ns <- tangency.portfolio(muhat.vals, cov.mat, rf, shorts=FALSE)
tan.port.ns
summary(tan.port.ns, risk.free=rf)
plot(tan.port.ns)

## asset allocation
# target return
target.return <- 0.005
e.port.exp<- efficient.portfolio(muhat.vals, cov.mat, target.return, shorts=FALSE)
summary(e.port.exp, risk.free=rf)
plot(e.port.exp)
target.return <- 0.01
e.port.exp<- efficient.portfolio(muhat.vals, cov.mat, target.return, shorts=FALSE)
# VaR
Value.at.Risk.port(e.port.exp$er,e.port.exp$sd,p = 0.01)
Value.at.Risk.port(e.port.exp$er,e.port.exp$sd,p = 0.05)
# random portfolio
set.seed(123)
x.vfinx = runif(900000, min = 0, max = 1)
x.veurx = runif(900000, min = 0, max = 1)
x.veiex = runif(900000, min = 0, max = 1)
x.vbltx = runif(900000, min = 0, max = 1)
x.vbisx = runif(900000, min = 0, max = 1)
x.vpacx = 1-x.vfinx-x.veurx-x.veiex-x.vbltx-x.vbisx
long.only = which(x.vfinx>0 & x.vpacx>0 & x.veurx>0 & x.veiex>0 & x.vbltx>0 & x.vbisx>0)
x.vfinx = x.vfinx[long.only]
x.veurx = x.veurx[long.only]
x.veiex = x.veiex[long.only]
x.vbltx = x.vbltx[long.only]
x.vbisx = x.vbisx[long.only]
x.vpacx = x.vpacx[long.only]
weight = cbind(x.vfinx,x.veurx,x.veiex,x.vbltx,x.vbisx,x.vpacx)
mean.r = numeric(length(long.only))
sd.r = numeric(length(long.only))
for (i in 1:length(long.only)) {
  mean.r[i] = weight[i,] %*% muhat.vals
  sd.r[i] = sqrt(weight[i,]%*%cov.mat%*%weight[i,])
}
plot(sd.vals,muhat.vals, col = "red", pch = 16, cex = 1.5,xlab = "standard deviation", ylab = "mean")
points(sd.r, mean.r, col = "grey", pch = 16)
points(sd.vals,muhat.vals, col = "red", pch = 16, cex = 1.5)
text(sd.vals,muhat.vals,labels = c("          vfinx","           veurx","veiex","          vbltx","          vbisx","            vpacx"))