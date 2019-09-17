##################################################################################
#                             First File in R Club Git Repo                      #
#                                                                                #
# Run Lines 12-21 all at once to install & load beepr, install & load Rcade,      #
# and hear the Mario sound when it has finished. Then line 26 to get            #
# Pacman started                                                                 #
##################################################################################


### Install beepr ###

install.packages(beepr)
library(beepr)

### Install devtools and rcade ###

install.packages("devtools")
devtools::install_github('RLesur/Rcade')

### Play Mario sound when done ###
beep(8)



#### Play Pacman ####

Rcade::games$'Pacman'