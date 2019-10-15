
#bootstrap
gui.hide(rpgm.step("main", "risk"), "bootprogress")
bootstrap <- function(N)
{
    if(N <= 0L)
    {
        gui.showMessage("this", "N_boostrap", "error", "The number of simulations must be positive.")
        return(numeric(0))
    }
    gui.hideMessage("this", "N_boostrap")
    gui.disable("this", "bootbutton")
    gui.setProperties("this", "bootbutton", list(buttondesign = "dark", value = "Computing..."))
    gui.show("this", "bootprogress")
    gui.setProperties("this", "bootprogress", list(value = 0L, progresscolor = "rgb(0, 128, 255)"))
    R <- numeric(N)
    for(m in 1L:N)
    {
        if(N >= 100L)
        {
            if(m %% (N%/%100L) == 0L)
                gui.setValue("this", "bootprogress", (100L*m)%/%N)
        }
        else
        {
            gui.setValue("this", "bootprogress", as.integer(100L*m/N))
        }
        C_bootstrap_before <- C_bootstrap_before_(C, f, s, r)
        fm <- f_(C_bootstrap_before)
        sm <- sqrt(s2_(C_bootstrap_before, fm))
        C_bootstrap_after <- C_bootstrap_after_(C, fm, sm, r)
        R[m] <- R_(C_bootstrap_after)
    }
    gui.setProperties("this", "bootprogress", list(value = 100L, progresscolor = "#27ae60"))
    gui.setProperties("this", "bootbutton", list(value = "Simulate", buttondesign ="primary"))
    gui.enable("this", "bootbutton")

    #Update graphique
    plotboot <- updateGraphBoot(density(R))
    gui.setValue("this", "labelboot", paste0("The quantile at 99.5% is: <strong style = \"color:#c0392b;\">", formatC(round(quantile(R, 0.995), 0), digits = 10L, big.mark = " "), "</strong>."))
    gui.setValue("this", "graphboot", plotboot)

    rpgm.notification("info", "Bootstrap finished!")
    return(R)
}

updateGraphBoot <- function(densityR)
{
    #Factors
trace = list(
  x= densityR$x,
  y= densityR$y,
  mode= 'lines',
   line= list(
      color= "#2980b9",
      width= 1
   )
)

layout <- list(title = 'ChainLadder factors', xaxis = list(title = "Year", tickfont = list(size = 14, color = 'rgb(107, 107, 107)')), yaxis = list(title = "f[j]", titlefont = list(size = 16, 
color = 'rgb(107, 107, 107)'), tickfont = list(size = 14, color='rgb(107, 107, 107)')), plot_bgcolor = "#ecf0f1")


plotlist <- list(traces = trace, layout = layout)
return(plotlist)
}

gui.hide(rpgm.step("main", "risk"), "graphboot")