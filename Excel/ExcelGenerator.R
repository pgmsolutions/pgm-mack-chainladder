
#For the Excel button
gui.setProperty(rpgm.step("main", "results"), "buttonexcel", "labeltext", paste0("<img src=\"", rpgm.pgmFilePath("images/excel.png"), "\" style=\"width:20px;\">")) 

#Create the Excel file

xlsx.open(rpgm.pgmFilePath("Excel/Results_template.xlsx"))
xlsx.selectWorksheet("Inputs")
xlsx.setName("X_input", data)

xlsx.selectWorksheet("Results")
xlsx.setName("fj", t(f))
xlsx.setName("reserve", R)

xlsx.selectWorksheet("Developed")
xlsx.setName("tild.X", hatC)

xlsx.selectWorksheet("Main")

xlsx.saveAs("Results.xlsx")

