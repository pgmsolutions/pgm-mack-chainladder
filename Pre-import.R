datapath <- ""
C <- try(stop(), silent = TRUE)

gui.hide(rpgm.step("main", "Import"), "data")
gui.disable(rpgm.step("main", "Import"), "data")
importData <- function()
{
    C <<- try(read.table(datapath, header = as.logical(header), sep = sep, colClasses = "double"), silent = TRUE)
    if(class(C) == "try-error")
    {
        gui.setProperty("this", "buttonImportFile", "buttondesign", "danger")
        gui.showMessage("this", "buttonImportFile", "error", C)
        gui.hide("this", "data")
    }
    else
    {
        gui.hideMessage("this", "ImportCol")
        n <<- ncol(C)
        colnames(C) <<- paste0("C[i,", 1L:n, "]");
        gui.setProperty("this", "buttonImportFile", "buttondesign", "success")
        gui.hideMessage("this", "buttonImportFile")
        gui.update("this", "data")
        gui.show("this", "data")
    }
}

ShowFileState <- FALSE 
gui.hide(rpgm.step("main", "Import"), "textShowFile")

#Définition fonctions
toggleFile <- function()
{
    if(ShowFileState)
    {
        gui.hide("this", "textShowFile")
        gui.setProperty("this", "buttonShowFile", "value", "Show File")
    }
    else
    {
        gui.update("this", "textShowFile")
        gui.show("this", "textShowFile")
        gui.setProperty("this", "buttonShowFile", "value", "Hide File")
    }
    ShowFileState <<- !ShowFileState
}

