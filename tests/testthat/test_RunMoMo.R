context("RunMoMo base functionality")

# delay="original"
test_that("Check Complete (with MFILE/HFILE), delay='original'", {
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
    plotGraphs = FALSE,
    delayVersion = "original")

  RunMoMo()

  res <- data.table::fread(file.path(opts$WDIR,"MOMOv4-3-Denmark-2013-52","EUROMOMO-COMPLETE-Denmark-2013-52","EUROMOMOv4-3-COMPLETE-Denmark-2013-52.txt"))
  expectedRes <- data.table::fread(system.file("testdata", "original_EUROMOMOv4-3-COMPLETE-Denmark-2013-52.txt", package = "MOMO"))
  expectedRes <- expectedRes[wk2 != max(wk2)]
  expect_equal(res,expectedRes)
})

# delay="original"
test_that("Check Complete (with MDATA/HDATA), delay='original'", {
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
    plotGraphs = FALSE,
    delayVersion = "original")

  RunMoMo()

  res <- data.table::fread(file.path(opts$WDIR,"MOMOv4-3-Denmark-2013-52","EUROMOMO-COMPLETE-Denmark-2013-52","EUROMOMOv4-3-COMPLETE-Denmark-2013-52.txt"))
  expectedRes <- data.table::fread(system.file("testdata", "original_EUROMOMOv4-3-COMPLETE-Denmark-2013-52.txt", package = "MOMO"))
  expectedRes <- expectedRes[wk2 != max(wk2)]
  expect_equal(res,expectedRes)
})

# delay="original
test_that("Check restricted, delay='original'", {
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
    plotGraphs = FALSE,
    delayVersion = "original")

  RunMoMo()

  res <- data.table::fread(file.path(opts$WDIR,"MOMOv4-3-Denmark-2013-52","EUROMOMO-RESTRICTED-Denmark-2013-52","EUROMOMOv4-3-RESTRICTED-Denmark-2013-52.txt"))
  expectedRes <- data.table::fread(system.file("testdata", "original_EUROMOMOv4-3-RESTRICTED-Denmark-2013-52.txt", package = "MOMO"))
  expectedRes <- expectedRes[wk2 != max(wk2)]
  expect_equal(res,expectedRes)
})
#
# # delay="original"
# test_that("Check Complete (with MDATA/HDATA) is the same as momo::dataExport$toSave, delay='original'", {
#   MDATA <- as.data.frame(data.table::fread(file.path(system.file("testdata",package="MOMO"),"DoD_DoR.txt")))
#   HDATA <- as.data.frame(data.table::fread(file.path(system.file("testdata",package="MOMO"),"holidays.txt")))
#
#   SetOpts(
#     DoA=as.Date("2013-12-31"),
#     DoPR=as.Date("2008-1-1"),
#     WStart=1,
#     WEnd=52,
#     country = "Denmark",
#     source = "SSI",
#     MDATA = MDATA,
#     HDATA = HDATA,
#     WDIR = tempdir(),
#     back = 3,
#     WWW = 290,
#     Ysum = 2013,
#     Wsum = 40,
#     USEglm2 = TRUE,
#     useAUTOMN = TRUE,
#     datesISO = FALSE,
#     plotGraphs = FALSE,
#     delayVersion = "original")
#
#   RunMoMo()
#
#   res <- data.table::rbindlist(dataExport$toSave,fill=T)
#
#   res <- data.table::fread(file.path(opts$WDIR,"MOMOv4-3-Denmark-2013-52","EUROMOMO-COMPLETE-Denmark-2013-52","EUROMOMOv4-3-COMPLETE-Denmark-2013-52.txt"))
#   expectedRes <- data.table::fread(system.file("testdata", "original_EUROMOMOv4-3-COMPLETE-Denmark-2013-52.txt", package = "MOMO"))
#   expectedRes <- expectedRes[wk2 != max(wk2)]
#   expect_equal(res,expectedRes)
# })
