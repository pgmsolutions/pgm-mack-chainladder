
lastYear <- as.integer(format(Sys.Date(), "%Y")) - 1L
Years <- (lastYear-n+1):lastYear

rownames(C) <- Years

f_ <- function(C)
{
    f <- numeric(n-1L)
    for(j in 1L:(n-1L))
        f[j] <- sum(C[1L:(n-j), j + 1L])/sum(C[1L:(n-j), j])
    return(f)
}

f <- f_(C)

png("f.png", width = 480*2, height = 480*2)
plot(1L:(n-1L), f, type = "b", pch = 16, lwd = 2, cex = 2, cex.lab = 2, cex.axis = 2)
dev.off()

s2_ <- function(C, f)
{
    s2 <- numeric(n-1L)
    for(j in 1L:(n-2L))
        s2[j] <- sum( C[1L:(n-j), j] * (C[1L:(n-j), j + 1L]/C[1L:(n-j), j] - f[j])^2 )/(n - j - 1L)
    s2[n-1L] <- min(s2[n-2L]^2/s2[n-3L], s2[n-3L], s2[n-2L])
    return(s2)
}

s2 <- s2_(C, f)
s <- sqrt(s2)

r_ <- function(C, f, s)
{
    r <- numeric(0)
    for(j in 1:(n-1))
        r <- c(r, (C[1:(n-j), j+1] - f[j]*C[1:(n-j), j])/(s[j]*sqrt(C[1:(n-j), j])))
    return(r)
}

r <- r_(C, f, s) 

hatC_ <- function(C, f)
{
    for(i in 2L:n)
        C[i, (n-i+2L):n] <- cumprod(f[(n-i+1L):(n-1L)])*C[i, n - i + 1L]
    return(C)
}

hatC <- hatC_(C, f)

C_bootstrap_before_ <- function(C, f, s, r)
{
    C_bootstrap <- C
    for(j in 1:(n-1))
    {
        rm <- sample(r, n-j, replace = TRUE)
        C_bootstrap[1:(n-j), j+1] = C_bootstrap[1:(n-j), j]*f[j] + s[j]*sqrt(C_bootstrap[1:(n-j), j])*rm
    }
    return(C_bootstrap)
}

C_bootstrap_after_ <- function(C, fm, sm, r)
{
    C_bootstrap <- C
    for(j in 1:(n-1))
    {
        rm <- sample(r, j, replace = TRUE)
        C_bootstrap[(n-j+1):n, j+1] = C_bootstrap[(n-j+1):n, j]*fm[j] + sm[j]*sqrt(C_bootstrap[(n-j+1):n, j])*rm
    }

    return(C_bootstrap)
}

Rall_ <- function(C)
    return(C[, n] - rev(C[row(C) + col(C) == n + 1L]))


R_ <- function(C)
    return(sum(Rall_(C)))


Rall <- Rall_(hatC)

R <- R_(hatC)

##risk MACK
C0 <- C; C0[row(C) + col(C) >= n+1L] <- 0
eqm_R <- numeric(n)
for(i in 2L:n)
{
    J <- (n+1-i):(n-1)
    eqm_R[i] <- hatC[i, n]^2*sum( (s2[J]/f[J]^2) * (1/hatC[i,J] + 1/colSums(C0[, J, drop = FALSE])) )
}

eqm <- eqm_R[n]
for(i in 2L:(n-1L))
{
    J <- (n+1L-i):(n-1L)
    eqm <- eqm + eqm_R[i] + hatC[i, n]*sum(hatC[(i+1):n, n])*sum((2*s2[J]/f[J]^2)/(colSums(C0[, J, drop = FALSE])))
}



####Graphics####

layout <- list(title = 'Reserve and Ultimate by year', xaxis = list(title = "Year", tickfont = list(size = 14, color = 'rgb(107, 107, 107)')), yaxis = list(title = "EUR", titlefont = list(size = 16, 
color = 'rgb(107, 107, 107)'), tickfont = list(size = 14, color='rgb(107, 107, 107)')), legend = list(x = 1., y = 1., bgcolor = 'rgba(255, 255, 255, 0)',
bordercolor = 'rgba(255, 255, 255, 0)'), barmode = "group", bargap = 0.15, bargroupgap = 0.1)

trace1 = list(
  x= Years,
  y= Rall,
  name = 'Reserve',
  marker = list(color= 'rgb(55, 83, 109)'),
  type= 'bar'
)

trace2 = list(
  x= Years,
  y= hatC[, n],
  name = 'Ultimate',
  marker = list(color= 'rgb(26, 118, 255)'),
  type= 'bar'
)

plotreserve <- list(traces = list(trace1, trace2), layout = layout)

trace1 = list(
  y= rev(Years),
  x= rev(Rall),
  name = 'Reserve',
  marker = list(color= 'rgb(55, 83, 109)'),
  type= 'bar',
  orientation= 'h'
)


trace2 = list(
  y= rev(Years),
  x= rev(hatC[, n]),
  name = 'Ultimate',
  marker = list(color= 'rgb(26, 118, 255)'),
  type= 'bar',
  orientation= 'h'
)

plotreserve2 <- list(traces = list(trace2, trace1), layout = layout, list(displayModeBar = TRUE))

#Factors
trace = list(
  x= 1L:(n-1L),
  y= f,
  mode= 'lines+markers',
  type= 'scatter',
   line= list(
      color= "#2980b9",
      width= 1
   )
)

layout <- list(title = 'ChainLadder factors', xaxis = list(title = "Year", tickfont = list(size = 14, color = 'rgb(107, 107, 107)')), yaxis = list(title = "f[j]", titlefont = list(size = 16, 
color = 'rgb(107, 107, 107)'), tickfont = list(size = 14, color='rgb(107, 107, 107)')), plot_bgcolor = "#ecf0f1")


plotf <- list(traces = trace, layout = layout)

###Plus de statistiques
fij <- as.matrix(C[-1L]/C[-n])
trace <- list()

col <- interpCol(1:(n-1), "#2c3e50", "#3498db")

for(i in 1L:(n-1L))
{
    trace[[i]] <- list(
  x= 1L:(n-i),
  y= fij[i, 1L:(n-i)], #Mise en vecteur car sinon data.frame...
  name = Years[i],
  mode= 'lines+markers',
  type= 'scatter',
     line= list(
      color= col[i],
      width= 1
   )
)
}

layout <- list(title = 'ChainLadder factors', xaxis = list(title = "Year", tickfont = list(size = 14, color = 'rgb(107, 107, 107)')), yaxis = list(title = "f[i, j]", titlefont = list(size = 16, 
color = 'rgb(107, 107, 107)'), tickfont = list(size = 14, color='rgb(107, 107, 107)')), plot_bgcolor = "#ecf0f1")
plotfij <- list(traces = trace, layout = layout)


######Approx loi
#Normal
mu_norm <- R
sd_norm <- sqrt(eqm)

x <- seq(mu_norm - 4*sd_norm, mu_norm + 4*sd_norm, length.out = 1001)
tracenorm <- list(x = x, y = dnorm(x, mu_norm, sd_norm), mode = "lines", line = list(color = "black"), name = "Normal")

sd_lnorm <- sqrt(log(1+sd_norm^2/mu_norm^2))
mu_lnorm <- log(mu_norm) - 0.5*sd_lnorm^2

tracelnorm <- list(x = x, y = dlnorm(x, mu_lnorm, sd_lnorm), mode = "lines", line = list(color = "#c0392b"), name = "Log-Normal")

alpha <- mu_norm^2/sd_norm^2
beta <- mu_norm/sd_norm^2

tracegamma <- list(x = x, y = dgamma(x, alpha, beta), mode = "lines", line = list(color = "#8e44ad"), name = "Gamma")

layout <- list(title = "Distribution of the Reserve")
plodistribution <- list(traces = list(tracenorm, tracelnorm, tracegamma), layout = layout)

distr <- "norm"
quantileR <- function(qalpha = 0.995)
{
    if(distr == "norm")
        return(qnorm(qalpha, mu_norm, sd_norm))
    if(distr == "lnorm")
        return(qlnorm(qalpha, mu_lnorm, sd_lnorm))
    if(distr == "gamma")
        return(qgamma(qalpha, alpha, beta))
}

