# store_packages.R
#
# stores a list of your currently installed packages
# source: http://hlplab.wordpress.com/2012/06/01/transferring-installed-packages-between-different-installations-of-r/

tmp <- installed.packages()
installedpackages <- as.vector(tmp[is.na(tmp[, "Priority"]), 1])
save(installedpackages, file = paste0(getwd(), "/SAVES/SAVED.PACKAGES/installed_packages.rda"))