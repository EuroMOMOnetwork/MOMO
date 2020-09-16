# MOMOpack for R

# News

## MOMO 2020.9.3

Spring weeks of 2020, COVID-19, permanently excluded from baseline estimation

## MOMO 2020.4.17

Ydrop and Wdrop as parameters
glmnet removed from list of dependencies

## MOMO 2018.8.24

* There are now more options for extracting data from inside your MoMo run. Previously only `MOMO::dataExport$toSave` was available, however, now there is also `MOMO::dataExport$aggr`, `MOMO::dataExport$aggr_fullDelay`, and `MOMO::dataExport$aggr_delay`

## MOMO 2018.8.23

* The new delay correction that Jens developed in December 2017 is now available. This can be chosen by `delayVersion="2017-12"`, or the original delay correction can be chosen by `delayVersion="original"` (default).
    
## Initial: MOMO 2017.12.14

* This is a full port of the Stata A-MOMO package to R. The code used to analyze mortality data for the [EuroMOMO project](www.euromomo.eu).


# How to install

Please use the [drat](https://github.com/eddelbuettel/drat) repository to install this package from, as the drat repository only contains package versions that have passed [testing](http://travis-ci.org/euromomonetwork/MOMO).

```
install.packages("MOMO", repos = c("https://euromomonetwork.github.io/drat", "https://cran.rstudio.com"))
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
  INPUTDIR = system.file("testdata", package="MOMO"),
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
  MOMOgroups = list(`0to14` = "0 <= age & age <= 14",
                    `15to44` = "15 <= age & age <= 44",
                    `45to64` = "45 <= age & age <= 64",
                    `65P` = "age >= 65 & !is.na(age)",
                    `65to74` = "65 <= age & age <= 74",
                    `75to84` = "75 <= age & age <= 84",
                    `85P` = "age >= 85 & !is.na(age)",
                    `Total` = "age >= 0 | is.na(age)"
  ),
  MOMOmodels = c(`0to14` = "LINE",
                 `15to44` = "LINE_SIN",
                 `45to64` = "LINE_SIN",
                 `65P` = "LINE_SIN",
                 `65to74` = "LINE_SIN",
                 `75to84` = "LINE_SIN",
                 `85P` = "LINE_SIN",
                 `Total` = "LINE_SIN"
  ),
  Ydrop = 9999,
  Wdrop = 99,
  verbose = TRUE
)

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
  MOMOgroups = list(`0to14` = "0 <= age & age <= 14",
                    `15to44` = "15 <= age & age <= 44",
                    `45to64` = "45 <= age & age <= 64",
                    `65P` = "age >= 65 & !is.na(age)",
                    `65to74` = "65 <= age & age <= 74",
                    `75to84` = "75 <= age & age <= 84",
                    `85P` = "age >= 85 & !is.na(age)",
                    `Total` = "age >= 0 | is.na(age)"
  ),
  MOMOmodels = c(`0to14` = "LINE",
                 `15to44` = "LINE_SIN",
                 `45to64` = "LINE_SIN",
                 `65P` = "LINE_SIN",
                 `65to74` = "LINE_SIN",
                 `75to84` = "LINE_SIN",
                 `85P` = "LINE_SIN",
                 `Total` = "LINE_SIN"
  ),
  Ydrop = 9999,
  Wdrop = 99,
  verbose = TRUE
)

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

