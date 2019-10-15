

gui.setProperty(rpgm.step("main", "Import"), "buttonImportFile", "buttondesign", "danger")

if(class(C) == "try-error")
{
    gui.showMessage(rpgm.step("main", "Import"), "ImportCol", "error", "Data have not been imported with <strong>Import File</strong>")
} else
{
    gui.showMessage(rpgm.step("main", "Import"), "ImportCol", "error", paste0("Data have been imported but there is <strong>", n, " year</strong>, please check."))
}

