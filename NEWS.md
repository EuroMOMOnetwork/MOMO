## MOMO 2018.08.24

* There are now more options for extracting data from inside your MoMo run. Previously only `MOMO::dataExport$toSave` was available, however, now there is also `MOMO::dataExport$aggr`, `MOMO::dataExport$aggr_fullDelay`, and `MOMO::dataExport$aggr_delay`

## MOMO 2018.08.23

* The new delay correction that Jens developed in December 2017 is now available. This can be chosen by `delayVersion="2017-12"`, or the original delay correction can be chosen by `delayVersion="original"` (default).
    
## MOMO 2017.12.14

* This is a full port of A-MOMOpack (the code used to analyze mortality data for the [EuroMOMO project](www.euromomo.eu)) from Stata to R.
