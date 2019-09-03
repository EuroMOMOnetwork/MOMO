# MOMOpack for R
[![Build Status](https://travis-ci.org/raubreywhite/MOMOpack-for-R.svg?branch=master)](https://travis-ci.org/raubreywhite/MOMOpack-for-R)
[![codecov](https://codecov.io/gh/raubreywhite/MOMOpack-for-R/branch/master/graph/badge.svg)](https://codecov.io/gh/raubreywhite/MOMOpack-for-R)

#
This is a full port of A-MOMOpack (the code used to analyze mortality data for the [EuroMOMO project](www.euroMOMO.eu)) from Stata to R.

# News

## MOMO 2018.08.24

* There are now more options for extracting data from inside your MOMO run. Previously only `MOMO::dataExport$toSave` was available, however, now there is also `MOMO::dataExport$aggr`, `MOMO::dataExport$aggr_fullDelay`, and `MOMO::dataExport$aggr_delay`
    
## MOMO 2017.12.14

* This is a full port of A-MOMOpack (the code used to analyze mortality data for the [EuroMOMO project](www.euroMOMO.eu)) from Stata to R.

# How to install

Please use my [drat](https://github.com/eddelbuettel/drat) repository to install this package from, as the drat repository only contains package versions that have passed [testing](http://travis-ci.org/euromomonetwork/MOMO).

```
install.packages("MOMO", repos="https://euromomonetwork.github.io/drat/")
```

# Basic example

```
SetOpts(
    DoA=as.Date("2013-12-31"),
    DoPR=as.Date("2008-1-1"),
    WStart=1,
    WEnd=52,
    country = "Denmark",
    source = "SSI",
    MFILE = "DoD_DoR.txt",
    HFILE = "holidays.txt",
    INPUTDIR = system.file("testdata",package="MOMO"),
    WDIR = tempdir(),
    back = 3,
    WWW = 290,
    Ysum = 2013,
    Wsum = 40,
    USEglm2 = TRUE,
    useAUTOMN = TRUE,
    datesISO = FALSE,
    plotGraphs = TRUE,
    delayVersion = "2017-12",
    MOMOgroups = list(
      "0to4" =  "age >= 0 & age <=4",
      "5to14" = "age >= 5 & age <=14",
      "15to64" = "age >= 15 & age <=64",
      "65P" = "age >= 65 | is.na(age)",
      "Total" = "age >= 0 | is.na(age)"
    ),
    MOMOmodels = c(
      "0to4" = "LINE",
      "5to14" = "LINE",
      "15to64" = "LINE_SIN",
      "65P" = "LINE_SIN",
      "Total" = "LINE_SIN"
    ))

RunMOMO()
```

# Another example using MDATA and HDATA instead of MFILE and HFILE

```
MDATA <- as.data.frame(data.table::fread(file.path(system.file("testdata",package="MOMO"),"DoD_DoR.txt")))
HDATA <- as.data.frame(data.table::fread(file.path(system.file("testdata",package="MOMO"),"holidays.txt")))

SetOpts(
    DoA=as.Date("2013-12-31"),
    DoPR=as.Date("2008-1-1"),
    WStart=1,
    WEnd=52,
    country = "Denmark",
    source = "SSI",
    MDATA = MDATA,
    HDATA = HDATA,
    WDIR = tempdir(),
    back = 3,
    WWW = 290,
    Ysum = 2013,
    Wsum = 40,
    USEglm2 = TRUE,
    useAUTOMN = TRUE,
    datesISO = FALSE,
    plotGraphs = TRUE,
    delayVersion = "2017-12",
    MOMOgroups = list(
      "0to4" =  "age >= 0 & age <=4",
      "5to14" = "age >= 5 & age <=14",
      "15to64" = "age >= 15 & age <=64",
      "65P" = "age >= 65 | is.na(age)",
      "Total" = "age >= 0 | is.na(age)"
    ),
    MOMOmodels = c(
      "0to4" = "LINE",
      "5to14" = "LINE",
      "15to64" = "LINE_SIN",
      "65P" = "LINE_SIN",
      "Total" = "LINE_SIN"
    ))

RunMOMO()
```

# Delay corrections available

| Delay correction | Description |
| --- | --- |
| original | ??? |
| sincos | original+sin52+cos52 |
| richard | Interacts WR with categorical YoDi, includes closed, sin52, and cos52nb2 ~ WRXX*as.factor(YoDi) + closed + sin52 + cos52 |

## original

???

## sincos

???

## richard

`nb2 ~ WRXX*as.factor(YoDi) + closed + sin52 + cos52`

