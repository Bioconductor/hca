# Bioconductor Work

## Prerequisites
- The Bioconductor team works with the development branch of R and the most up-to-date version of Bioconductor
    - The devel branch of R can be installed from [here](https://mac.r-project.org/)
- Prerequisite tools for compiling R can be found [here](https://mac.r-project.org/tools/)
    - Needed XCode and GNU Fortran compiler installed and properly configured on host
- Additionally, I used my admin account to install [`brew`](https://brew.sh/) and gave my regular account sudo access
- Next, I generated the `.Renviron` and `.Rprofile` files in my home directory, which set different environment variables and other configurations to be used by R
    - In `.Rprofile` we added `options(pkgType="source")` for installing packages from source when needed
    - In `.Renviron` we added the following for indicating where to install packages given the current R and Bioconductor versions, and setting the `PATH` used to search for software packages
    ```
    R_LIBS_USER="~/Library/R/4.1.0/Bioc/3.13/library"
    PATH="/usr/local/opt:/usr/local/bin:/Library/PostgreSQL/11/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin:/opt/X11/bin:/usr/local/gfortran/bin"
    ```
    - More on using `.Renviron` and `.Rprofile` [here](https://support.rstudio.com/hc/en-us/articles/360047157094-Managing-R-with-Rprofile-Renviron-Rprofile-site-Renviron-site-rsession-conf-and-repos-conf)
- Because of conflict between `anaconda` installed packages and those I installed with `brew` specifically for R, I had to remove references to `anaconda` from `PATH`, and pre-append the location of the desired `brew` installed packages
    - So delete any `anaconda` paths from `PATH`
    - Then pre-append like so: `PATH="/usr/local/opt:/usr/local/bin:/Library/PostgreSQL/11/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin:/opt/X11/bin:/usr/local/gfortran/bin"`
- To find out location of R installation: `which R`
    - should be the newly downloaded development branch
- Update RStudio by checking for updates in the UI, and replacing the existing version if need be
- Create a library folder for packages to this version of R and Bioconductor
```
~/Library/R/4.1.0/Bioc/3.13/library
```
- RStudio alias (start RStudio from command line):
```
alias rstudio='RSTUDIO_WHICH_R=/usr/local/bin/R /Applications/RStudio.app/Contents/MacOS/RStudio'
```
- Once in RStudio:
check configuration is correct
```
.libPaths()
Sys.getenv("PATH")
```
install Bioconductor packages
```
install.packages("BiocManager", repos = "https://cran.r-project.org")
BiocManager::install(version="devel")
BiocManager::version()
BiocManager::valid()
BiocManager::install(c("devtools", "remotes"))
```
- individual installs of packages that failed to install may need to be installed using `brew` first

---
Todo > see Trello
