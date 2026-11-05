# Software environment

JoonHo Lee ([jlee296@ua.edu](mailto:jlee296@ua.edu))

The recorded environment uses R 4.6.0, CmdStan 2.38.0, cmdstanr 0.8.0, posterior 1.7.0, postrank 0.9.0, and the HiGHS R interface 1.12.0.3. Exact R package versions and sources are in [renv.lock](../renv.lock); [config/software.csv](../config/software.csv) separates ordinary analysis, maps, fitting, and original-workbook parsing dependencies.

## R packages

Install R and the three bootstrap packages first. They allow the setup script to read configuration, verify the local dependency archive, and restore the remaining environment.

```r
install.packages(c("renv", "jsonlite", "digest"), repos = "https://cloud.r-project.org")
```

Then run from the compendium root:

```sh
Rscript scripts/00_setup.R --restore
Rscript scripts/00_setup.R --check
```

Restoration is explicit and requires network access. The library is placed under the selected external cache, rather than inside the repository. Pass the same `--cache=/absolute/path` to subsequent commands to use it. The lockfile pins postrank to the included source archive; its implementation and upstream notices are described in [vendor/README.md](../vendor/README.md).

Add the roles needed for your work:

```sh
Rscript scripts/00_setup.R --restore --maps --fits --sources
Rscript scripts/00_setup.R --check --maps --fits --sources
```

`--maps` adds sf and geographic helpers. `--fits` adds CmdStanR; it does not install CmdStan. `--sources` adds readxl for rebuilding the ADECA workbook extract. Reading the supplied application facts needs no spreadsheet software. Compilation from source can require platform development tools and system libraries; the installation output identifies missing components. On the macOS host used for the isolated restore, source builds required CMake, pkg-config, gettext headers and OpenSSL headers. These system components sit outside the R lockfile. Ensure the tools are on PATH and the headers/libraries are visible to R's compiler and pkg-config. A separate Makevars file selected with `R_MAKEVARS_USER` can supply local include/library paths without changing the repository or global R configuration. GIS source builds also need GDAL (including `gdal-config`), GEOS and PROJ development libraries. See the [sf installation guide](https://r-spatial.github.io/sf/#installing) for platform prerequisites, while retaining this compendium's pinned R package versions.

## CmdStan and computation

Full refitting requires a working C++ toolchain and CmdStan 2.38.0. In an R session using the restored library:

```r
source("R/io.R")
rep_init(".") # Add cache= for the same custom cache used by the scripts.
cmdstanr::install_cmdstan(version = "2.38.0", cores = 2)
cmdstanr::cmdstan_version()
```

The ordinary CmdStanR installation location is discovered automatically. For a nondefault installation, set `CMDSTAN` in the environment that launches the analysis scripts; for example, in a POSIX shell:

```sh
export CMDSTAN="/absolute/path/to/cmdstan-2.38.0"
```

The interactive `cmdstanr::set_cmdstan_path()` changes only that R session, so it does not configure a later shell-launched `Rscript`. Model compilation and executables remain in the external cache. The fit runner checks the CmdStan version and uses four chains, at most two concurrent chains, and one thread per chain. The report solver uses one thread. A shared cache lock prevents overlapping heavy runs using the same cache; inspect a stale lock's owner before removing it.

The fit settings record per-fit seeds, sampling lengths, adaptation controls and numerical precision. Do not concatenate the original and extended A pools. The supplied archive contains the adopted A computation for each fit.

## PDF and geographic tools

Statistical figures use a functioning `grDevices::cairo_pdf` device. On macOS, the script can use the Quartz PDF device when Cairo's system libraries are unavailable. It reports the selected device and checks that a nonempty PDF was written. Both paths preserve vector geometries and cells. Helvetica is the requested family; the installed font system determines its available implementation. Optional PNG companions use ragg.

Figure 1 uses `pdflatex` and the packages listed in [fig_src/fig1_workflow.tex](../fig_src/fig1_workflow.tex). The other figures require no TeX. Geographic packages load only for map figures. Their native geometry sources and coordinate reference systems are documented in [the data guide](data.md).

The macOS validation used R 4.6.0 and covered an isolated package restore, archived-output replay, all 14 full measurement fits, report reselection, and table/figure generation. Linux and Windows were reviewed for portable paths and device selection but were not executed. Different font or projection libraries can change PDF bytes or geometry serialization; assess data keys, numerical values, panel meaning and documented Monte Carlo tolerances, rather than demanding identical files across platforms.
