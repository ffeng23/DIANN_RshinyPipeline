::### command code conda for installation of necessary dependencies.
::#  Developed by Feng @ Sensei Bio
::#				===== version 1.0 
::#Prerequisite are conda and mamba and Rtools

::#Installing conda and mamba manually

::#activate conda environment.
echo Entering conda environment.....
call conda activate Rv40  

::#installation libraries
echo Installing dependencies......
call mamba install -c conda-forge r-shinyfiles r-fs r-shiny r-ggplot2 r-tidyverse r-devtools
call mamba install -c r rtools
R -e "library(devtools); install_github('https://github.com/vdemichev/diann-rpackage')"

echo Done with installation.