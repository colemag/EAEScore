# EAEScore
This shiny-based application was designed to help graph and analyze Experimental Autoimmune Encephalomyelitis (EAE) data. EAE is the most commonly used animal model of Multiple Sclerosis, and can be complex due to its longitiduinal design. To help researchers we built this tool which is also hosted free-to-use at eaescore.net
I recommend using the web-hosted version as it requires no set-up. However, should you want to run the app locally this is the source code for the application. You will need to install all of the dependencies the app uses as listed below:

shiny,
shinydashboard,
ggplot2,
ggpubr,
dplyr,
tidyr,
DT,
tibble,
lmerTest,
pheatmap,
gtools,
cowplot,
MASS,
AER


Once you have installed all of the packages, I recommend you use the following approach:

library("shiny")

runGitHub( "EAEScore", "colemag")

This will pull the files needed from this github and begin to run the application. If you get an error, it is because you don't have one of the dependency packages installed. The advantage to using this approach instead of downloading the files is that you will always have the most up-to-date code.
