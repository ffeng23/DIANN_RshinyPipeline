# section 1: about creating "clickable" file for running R shiny app.
This only works on Windows.
There are two scripts, "activate_diannShiny.bat" and "runAnalysis_test.bat". The second one is the clickable entry point. It calls on the first to evoke the running of the R shiny. 
The second could be anywhere in the system. It will called the first one, which is in the conda bin folder. I modified it to call r shinny and it is originally to activate conda.
To deploy this, we need to make accordiingly changes. Also we have the copy (.backup) in this corrent folder.