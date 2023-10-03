# restore_packages.R
#
# installs each package from the stored list of packages
# source: http://hlplab.wordpress.com/2012/06/01/transferring-installed-packages-between-different-installations-of-r/

load("~/installed_packages.rda")

for (count in seq_along(installedpackages)) {
    install.packages(installedpackages[count])
}