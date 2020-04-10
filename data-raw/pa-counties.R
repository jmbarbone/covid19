
# ---------------------------------------------------------------------------------------------
# County level data for Pennsylvania
# ---------------------------------------------------------------------------------------------

options(tidyverse.quiet = TRUE)
library(tidyverse)
library(janitor)

read_text_table <- function(text, sep = "") {
  text %>% 
    textConnection() %>% 
    read.table(sep = sep) %>% 
    mutate(V1 = str_trim(V1),
           V1 = recode(V1, "Mckean" = "McKean")) %>% 
    pivot_wider(names_from = V1,
                values_from = V2) %>% 
    mutate_all(as.double) ## will convert everything back to integers
}

# Total count as per: https://www.health.pa.gov/topics/disease/coronavirus/Pages/Cases.aspx
# Archived here: https://www.health.pa.gov/topics/disease/coronavirus/Pages/Archives.aspx

pa_county_confirmed <- list( 
  "2020-03-06" = tibble( 
    Delaware = 1,
    Wayne = 1,
  ),
  "2020-03-07" = tibble( 
    Delaware= 1,
    Montgomery = 2,
    Wayne = 1,
  ),
  "2020-03-08" = tibble( 
    Delaware = 1,
    Montgomery = 4,
    Wayne = 1,
  ),
  "2020-03-09" = tibble( 
    Delaware = 1,
    Monroe = 1,
    Montgomery = 7,
    Wayne = 1,
  ),
  "2020-03-10" = tibble( 
    Delaware = 1,
    Monroe = 1,
    Montgomery = 8,
    Philadelphia = 1,
    Wayne = 1,
  ),
  "2020-03-11" = tibble( 
    Bucks = 2,
    Delaware = 1,
    Monroe = 2,
    Montgomery = 9,
    Philadelphia = 1,
    Wayne = 1,
  ),
  "2020-03-12" = tibble( 
    Bucks = 2,
    Delaware = 1,
    Monroe = 2,
    Montgomery = 13,
    Northampton = 1,
    Philadelphia = 1,
    Pike = 1,
    Wayne = 1,
  ),
  "2020-03-13" = tibble( 
    Bucks = 3,
    Chester = 1,
    Cumberland = 3,
    Delaware = 6,
    Monroe = 3,
    Montgomery = 18,
    Northampton = 1,
    Philadelphia = 3,
    Pike = 1,
    Washington = 1,
    Wayne = 1,
  ),
  ## Archive lists as 15 March
  "2020-03-14" = tibble( 
    Allegheny = 2, 
    Bucks = 3,
    Chester = 2,
    Cumberland = 3,
    Delaware = 6,
    Monroe = 3,
    Montgomery = 20,
    Northampton = 1,
    Philadelphia = 4,
    Pike = 1,
    Washington = 1,
    Wayne = 1,
  ),
  "2020-03-15" = tibble( 
    Allegheny = 3, 
    Bucks = 4,
    Chester = 2,
    Cumberland = 5,
    Delaware = 7,
    Lehigh = 1,
    Luzerne = 1,
    Monroe = 6,
    Montgomery = 24,
    Northampton = 1,
    Philadelphia = 6,
    Pike = 1,
    Washington = 1,
    Wayne = 1,
  ),
  "2020-03-16" = read_text_table(
    "Allegheny 5
    Bucks 5
    Chester 2
    Cumberland 5
    Delaware 7
    Lehigh 1
    Luzerne 1
    Monroe 8
    Montgomery 30
    Northampton 1
    Philadelphia 8
    Pike 1
    Wayne 1
    Washington 1"
  ),
  "2020-03-17" = read_text_table(
    "Allegheny 7
    Beaver 1
    Bucks 8
    Chester 4
    Cumberland 10
    Delaware 9
    Lehigh 1
    Luzerne 1
    Monroe 8
    Montgomery 32
    Northampton 1
    Philadelphia 10
    Pike 1
    Washington 2
    Wayne 1"
  ),
  "2020-03-18" = read_text_table( ## deaths removes
    'Allegheny 11
    Beaver 2
    Berks 1
    Bucks 9
    Chester 9
    Cumberland 10
    Delaware 14
    Lackawanna 1
    Lehigh 1
    Luzerne 1
    Monroe 7
    Montgomery 42
    Northampton 1
    Philadelphia 17
    Pike 2
    Washington 2
    Wayne 1
    York 2'
  ),
  "2020-03-19" = read_text_table(
    "Adams  1
    Allegheny 16
    Beaver 2
    Berks 1
    Bucks 12
    Chester 10
    Cumberland 11
    Delaware 14
    Lackawanna 2
    Lancaster 2
    Lebanon 1
    Lehigh 1
    Luzerne 1
    Monroe 15
    Montgomery 47
    Northampton 5
    Philadelphia 33
    Pike 3
    Washington 3
    Wayne 1
    Westmoreland 2
    York 2"
  ),
  "2020-03-20" = read_text_table(
    "Adams 4
    Allegheny 28
    Beaver 3
    Berks 5
    Bucks	16
    Centre 1
    Chester	17
    Cumberland	11
    Delaware 23
    Erie 1
    Franklin 1
    Lackawanna 4
    Lancaster 2
    Lebanon 1
    Lehigh 2
    Luzerne 2
    Monroe 19
    Montgomery 59
    Northampton	10
    Philadelphia 42
    Pike 2
    Potter 1
    Washington 3
    Wayne 1
    Westmoreland 4
    York 6"
  ),
  "2020-03-21" = read_text_table(
    'Adams 4
    Allegheny 31
    Beaver 3
    Berks 7
    Bucks 24
    Butler 1
    Centre 1
    Chester 19
    Cumberland 11
    Delaware 33
    Erie 1
    Franklin 1
    Lackawanna 5
    Lancaster 4
    Lebanon 2
    Lehigh 13
    Luzerne 6
    Monroe 25
    Montgomery 71
    Montour 1
    Northampton 17
    Philadelphia 69
    Pike 2
    Potter 1
    Washington 5
    Wayne 1
    Westmoreland 4
    York	9'
  ),
  "2020-03-22" = read_text_table(
    'Adams 5
    Allegheny 40
    Beaver 3
    Berks 13
    Bucks 32
    Butler 1
    Centre 1
    Chester 23
    Columbia 1
    Cumberland 11
    Dauphin 1 
    Delaware	43
    Erie 2
    Fayette 1
    Franklin 1
    Lackawanna 6
    Lancaster 6
    Lebanon 3
    Lehigh 19
    Luzerne 7
    Mercer 1
    Monroe 31
    Montgomery 87
    Montour 1
    Northampton 21
    Philadelphia 91
    Pike 3
    Potter 1
    Schuylkill 1
    Washington 7
    Wayne 2
    Westmoreland 4
    York 10'
  ),
  "2020-03-23" = read_text_table(
    'Adams 6
    Allegheny 48
    Beaver 3
    Berks 14
    Bucks 43
    Butler 5
    Cambria 1
    Centre 3
    Chester 40
    Columbia 1
    Cumberland 12
    Dauphin 1
    Delaware 54
    Erie 3
    Fayette 1
    Franklin 1
    Lackawanna 7
    Lancaster 5
    Lebanon 3
    Lehigh 25
    Luzerne 10
    Mercer 1
    Monroe 43
    Montgomery 129
    Montour 1
    Northampton 23
    Philadelphia 128
    Pike 3
    Potter 1
    Schuylkill 3
    Washington 7
    Wayne 3
    Westmoreland 6
    York 10'
  ),
  "2020-03-24" = read_text_table(
    'Adams 6
    Allegheny 58
    Armstrong 1
    Beaver 3
    Berks 16
    Bradford 1 
    Bucks 65
    Butler 6
    Cambria 1
    Carbon 1
    Centre 7 
    Chester 40
    Clearfield 1
    Columbia 1 
    Cumberland 13
    Dauphin 4 
    Delaware 84
    Erie 4
    Fayette 2
    Franklin 3
    Juniata 1
    Lackawanna 15
    Lancaster 10
    Lebanon 3 
    Lehigh 27 
    Luzerne 21
    Mercer 2 
    Monroe 45
    Montgomery 144
    Montour 3 
    Northampton 33
    Philadelphia 177
    Pike 4
    Potter 1
    Schuylkill 5
    Somerset 1
    Washington 9
    Wayne 4
    Westmoreland 11
    York 18'
  ),
  "2020-03-25" = read_text_table(
    'Adams 6
    Allegheny 88
    Armstrong 1
    Beaver 7
    Berks 20
    Bradford 1
    Bucks 86
    Butler 12
    Cambria 1
    Carbon 1
    Centre 8
    Chester 54
    Clearfield 2
    Columbia 1
    Cumberland 13
    Dauphin 10
    Delaware 101
    Erie 4
    Fayette 4
    Franklin 5
    Greene 2
    Juniata 1
    Lackawanna 18
    Lancaster 12
    Lawrence 1
    Lebanon 3
    Lehigh 38
    Luzerne 27
    Lycoming 1
    Mercer 2
    Monroe 51
    Montgomery 172
    Montour 4
    Northampton 44
    Philadelphia 257
    Pike 9
    Potter 1
    Schuylkill 6
    Somerset 2
    Warren 1
    Washington 10
    Wayne 4
    Westmoreland 16
    York 20'
  ),
  "2020-03-26" = read_text_table(
    'Adams	7
    Allegheny	133
    Armstrong	1
    Beaver	13
    Berks	36
    Blair	1
    Bradford	2
    Bucks	107
    Butler	19
    Cambria	1
    Carbon	2
    Centre	9
    Chester	84
    Clearfield	2
    Columbia	3
    Crawford	1
    Cumberland	15
    Dauphin	13
    Delaware	156
    Erie	4
    Fayette	8
    Franklin	5
    Greene	3
    Indiana	1
    Juniata	1
    Lackawanna	28
    Lancaster	21
    Lawrence	1
    Lebanon	4
    Lehigh	63
    Luzerne	36
    Lycoming	1
    Mercer	3
    Monroe	67
    Montgomery	282
    Montour	4
    Northampton	56
    Philadelphia	402
    Pike	15
    Potter	1
    Schuylkill	9
    Somerset	2
    Susquehanna	1
    Warren	1
    Washington	12
    Wayne	6
    Westmoreland	24
    York 	21', sep = "\t"
  ),
  "2020-03-27" = read_text_table(
    'Adams	8
    Allegheny	158
    Armstrong	1
    Beaver	14
    Berks	65
    Blair	1
    Bradford	2
    Bucks	124
    Butler	26
    Cambria	1
    Carbon	2
    Centre	11
    Chester	107
    Clearfield	2
    Columbia	3
    Crawford	1
    Cumberland	16
    Dauphin	18
    Delaware	185
    Erie	7
    Fayette	9
    Franklin	5
    Greene	4
    Indiana	2
    Juniata	1
    Lackawanna	35
    Lancaster	33
    Lawrence	4
    Lebanon	12
    Lehigh	93
    Luzerne	55
    Lycoming	2
    Mercer	4
    Monroe	98
    Montgomery	374
    Montour	4
    Northampton	79
    Northumberland	1
    Philadelphia	530
    Pike	23
    Potter	1
    Schuylkill	13
    Somerset	2
    Susquehanna	1
    Union	1
    Warren	1
    Washington	14
    Wayne	6
    Westmoreland	30
    York	29', sep = "\t"
  ),
  "2020-03-28" = read_text_table(
    'Adams	8
    Allegheny	219
    Armstrong	2
    Beaver	22
    Berks	65
    Blair	2
    Bradford	3
    Bucks	152
    Butler	41
    Cambria	1
    Cameron	1
    Carbon	3
    Centre	15
    Chester	116
    Clarion	1
    Clearfield	2
    Columbia	4
    Crawford	2
    Cumberland	22
    Dauphin	23
    Delaware	226
    Erie	7
    Fayette	10
    Franklin	7
    Greene	6
    Huntingdon	1
    Indiana	2
    Juniata	1
    Lackawanna	51
    Lancaster	45
    Lawrence	8
    Lebanon	15
    Lehigh	109
    Luzerne	65
    Lycoming	2
    Mckean	1
    Mercer	6
    Monroe	106
    Montgomery	411
    Montour	5
    Northampton	94
    Northumberland	1
    Perry	1
    Philadelphia	709
    Pike	27
    Potter	2
    Schuylkill	16
    Snyder	1
    Somerset	2
    Susquehanna	1
    Tioga	1
    Warren	1
    Washington	23
    Wayne	6
    Westmoreland	41
    York	37', sep = "\t"
  ),
  "2020-03-29" = read_text_table(
    'Adams	8
    Allegheny	265
    Armstrong	3
    Beaver	28
    Berks	68
    Blair	3
    Bradford	3
    Bucks	203
    Butler	47
    Cambria	1
    Cameron	1
    Carbon	9
    Centre	22
    Chester	137
    Clarion	1
    Clearfield	2
    Columbia	6
    Crawford	3
    Cumberland	22
    Dauphin	35
    Delaware	276
    Erie	7
    Fayette	10
    Franklin	11
    Greene	6
    Huntingdon	1
    Indiana	2
    Juniata	1
    Lackawanna	56
    Lancaster	67
    Lawrence	8
    Lebanon	19
    Lehigh	151
    Luzerne	94
    Lycoming	3
    Mckean	1
    Mercer	7
    Mifflin	2
    Monroe	135
    Montgomery	488
    Montour	4
    Northampton	126
    Northumberland	1
    Perry	1
    Philadelphia	865
    Pike	33
    Potter	2
    Schuylkill	21
    Snyder	2
    Somerset	2
    Susquehanna	1
    Tioga	1
    Venango	1
    Warren	1
    Washington	24
    Wayne	7
    Westmoreland	47
    York	43', sep = "\t"
  ),
  "2020-03-30" = read_text_table(
    'Adams	8
    Allegheny	290
    Armstrong	3
    Beaver	44
    Berks	82
    Blair	6
    Bradford	3
    Bucks	246
    Butler	49
    Cambria	2
    Cameron	1
    Carbon	13
    Centre	24
    Chester	146
    Clarion	1
    Clearfield	4
    Columbia	6
    Crawford	4
    Cumberland	24
    Dauphin	36
    Delaware	303
    Erie	13
    Fayette	11
    Franklin	12
    Greene	7
    Huntingdon	1
    Indiana	2
    Juniata	3
    Lackawanna	62
    Lancaster	97
    Lawrence	10
    Lebanon	27
    Lehigh	231
    Luzerne	150
    Lycoming	4
    Mckean	1
    Mercer	7
    Mifflin	1
    Monroe	182
    Montgomery	540
    Montour	10
    Northampton	184
    Northumberland	1
    Perry	1
    Philadelphia	1007
    Pike	39
    Potter	2
    Schuylkill	30
    Snyder	2
    Somerset	2
    Susquehanna	1
    Tioga	1
    Union	4
    Venango	1
    Warren	1
    Washington	26
    Wayne	10
    Westmoreland	55
    York	54', sep = "\t"
  ),
  "2020-03-31" = read_text_table(
    'Adams	9
    Allegheny	325
    Armstrong	5
    Beaver	52
    Bedford	2
    Berks	110
    Blair	4
    Bradford	7
    Bucks	286
    Butler	60
    Cambria	2
    Cameron	1
    Carbon	17
    Centre	26
    Chester	159
    Clarion	3
    Clearfield	4
    Columbia	7
    Crawford	4
    Cumberland	36
    Dauphin	45
    Delaware	338
    Erie	14
    Fayette	14
    Franklin	19
    Greene	9
    Huntingdon	1
    Indiana	6
    Juniata	3
    Lackawanna	78
    Lancaster	123
    Lawrence	13
    Lebanon	28
    Lehigh	272
    Luzerne	212
    Lycoming	6
    Mckean	1
    Mercer	8
    Mifflin	2
    Monroe	236
    Montgomery	570
    Montour	9
    Northampton	245
    Northumberland	1
    Perry	1
    Philadelphia	1197
    Pike	48
    Potter	2
    Schuylkill	38
    Snyder	2
    Somerset	2
    Susquehanna	1
    Tioga	2
    Union	4
    Venango	3
    Warren	1
    Washington	33
    Wayne	10
    Westmoreland	61
    York	66', sep = "\t"
  ),
  "2020-04-01" = read_text_table(
    'Adams	12
    Allegheny	356
    Armstrong	5
    Beaver	54
    Bedford	3
    Berks	151
    Blair	4
    Bradford	8
    Bucks	312
    Butler	64
    Cambria	3
    Cameron	1
    Carbon	23
    Centre	27
    Chester	183
    Clarion	4
    Clearfield	4
    Columbia	8
    Crawford	5
    Cumberland	38
    Dauphin	59
    Delaware	390
    Erie	15
    Fayette	14
    Franklin	21
    Greene	9
    Huntingdon	1
    Indiana	6
    Juniata	2
    Lackawanna	85
    Lancaster	157
    Lawrence	13
    Lebanon	36
    Lehigh	374
    Luzerne	282
    Lycoming	7
    Mckean	1
    Mercer	8
    Mifflin	1
    Monroe	278
    Montgomery	649
    Montour	13
    Northampton	312
    Northumberland	6
    Perry	1
    Philadelphia	1478
    Pike	57
    Potter	2
    Schuylkill	47
    Snyder	3
    Somerset	3
    Susquehanna	2
    Tioga	2
    Union	2
    Venango	3
    Warren	1
    Washington	35
    Wayne	14
    Westmoreland	72
    York	79', sep = "\t"
  ),
  "2020-04-02" = read_text_table(
    'Adams	18
    Allegheny	419
    Armstrong	7
    Beaver	55
    Bedford	3
    Berks	168
    Blair	4
    Bradford	8
    Bucks	370
    Butler	69
    Cambria	4
    Cameron	1
    Carbon	26
    Centre	28
    Chester	210
    Clarion	4
    Clearfield	4
    Columbia	11
    Crawford	5
    Cumberland	41
    Dauphin	67
    Delaware	470
    Erie	16
    Fayette	15
    Forest	1
    Franklin	23
    Greene	11
    Huntingdon	2
    Indiana	6
    Juniata	2
    Lackawanna	108
    Lancaster	203
    Lawrence	16
    Lebanon	45
    Lehigh	479
    Luzerne	384
    Lycoming	7
    Mckean	1
    Mercer	9
    Mifflin	1
    Monroe	321
    Montgomery	735
    Montour	15
    Northampton	378
    Northumberland	8
    Perry	3
    Philadelphia	1852
    Pike	68
    Potter	2
    Schuylkill	54
    Snyder	3
    Somerset	3
    Susquehanna	3
    Tioga	2
    Union	2
    Venango	3
    Warren	1
    Washington	38
    Wayne	17
    Westmoreland	84
    Wyoming	1
    York	102', sep = "\t"
  ),
  "2020-04-03" = read_text_table(
    'Adams	19
    Allegheny	476
    Armstrong	11
    Beaver	65
    Bedford	3
    Berks	201
    Blair	4
    Bradford	9
    Bucks	446
    Butler	75
    Cambria	4
    Cameron	1
    Carbon	34
    Centre	32
    Chester	226
    Clarion	4
    Clearfield	5
    Clinton	1
    Columbia	15
    Crawford	5
    Cumberland	45
    Dauphin	79
    Delaware	542
    Erie	17
    Fayette	20
    Forest	1
    Franklin	26
    Greene	11
    Huntingdon	3
    Indiana	7
    Juniata	5
    Lackawanna	119
    Lancaster	232
    Lawrence	19
    Lebanon	54
    Lehigh	584
    Luzerne	484
    Lycoming	8
    Mckean	1
    Mercer	10
    Mifflin	2
    Monroe	397
    Montgomery	875
    Montour	16
    Northampton	466
    Northumberland	8
    Perry	4
    Philadelphia	2284
    Pike	83
    Potter	2
    Schuylkill	63
    Snyder	4
    Somerset	3
    Susquehanna	4
    Tioga	3
    Union	3
    Venango	3
    Warren	1
    Washington	40
    Wayne	23
    Westmoreland	110
    Wyoming	2
    York	121', sep = "\t"
  ),
  "2020-04-04" = read_text_table(
    'Adams	21
    Allegheny	552
    Armstrong	12
    Beaver	69
    Bedford	4
    Berks	235
    Blair	5
    Bradford	10
    Bucks	488
    Butler	84
    Cambria	6
    Cameron	1
    Carbon	46
    Centre	39
    Chester	250
    Clarion	4
    Clearfield	7
    Clinton	1
    Columbia	20
    Crawford	5
    Cumberland	54
    Dauphin	99
    Delaware	616
    Erie	19
    Fayette	23
    Forest	2
    Franklin	27
    Greene	12
    Huntingdon	4
    Indiana	9
    Juniata	7
    Lackawanna	146
    Lancaster	291
    Lawrence	22
    Lebanon	87
    Lehigh	804
    Luzerne	648
    Lycoming	10
    McKean	1
    Mercer	14
    Mifflin	4
    Monroe	484
    Montgomery	982
    Montour	19
    Northampton	588
    Northumberland	9
    Perry	5
    Philadelphia	2610
    Pike	97
    Potter	3
    Schuylkill	77
    Snyder	6
    Somerset	3
    Sullivan	1
    Susquehanna	5
    Tioga	3
    Union	5
    Venango	3
    Warren	1
    Washington	46
    Wayne	28
    Westmoreland	135
    Wyoming	5
    York	144', sep = "\t"
  ),
  "2020-04-05" = read_text_table(
    'Adams	22
    Allegheny	605
    Armstrong	12
    Beaver	84
    Bedford	4
    Berks	276
    Blair	5
    Bradford	10
    Bucks	555
    Butler	87
    Cambria	7
    Cameron	1
    Carbon	50
    Centre	43
    Chester	269
    Clarion	5
    Clearfield	7
    Clinton	1
    Columbia	22
    Crawford	7
    Cumberland	58
    Dauphin	118
    Delaware	708
    Erie	19
    Fayette	27
    Forest	3
    Franklin	30
    Fulton	1
    Greene	12
    Huntingdon	4
    Indiana	13
    Juniata	7
    Lackawanna	172
    Lancaster	371
    Lawrence	23
    Lebanon	106
    Lehigh	877
    Luzerne	741
    Lycoming	9
    McKean	1
    Mercer	18
    Mifflin	9
    Monroe	528
    Montgomery	1111
    Montour	37
    Northampton	636
    Northumberland	14
    Perry	5
    Philadelphia	3135
    Pike	114
    Potter	3
    Schuylkill	90
    Snyder	8
    Somerset	4
    Sullivan	1
    Susquehanna	6
    Tioga	3
    Union	6
    Venango	3
    Warren	1
    Washington	50
    Wayne	33
    Westmoreland	147
    Wyoming	5
    York	171', sep = "\t"
  ),
  "2020-04-06" = read_text_table(
    'Adams	25
    Allegheny	642
    Armstrong	13
    Beaver	96
    Bedford	4
    Berks	326
    Blair	5
    Bradford	10
    Bucks	619
    Butler	91
    Cambria	7
    Cameron	1
    Carbon	59
    Centre	44
    Chester	307
    Clarion	6
    Clearfield	7
    Clinton	3
    Columbia	26
    Crawford	9
    Cumberland	68
    Dauphin	132
    Delaware	822
    Erie	20
    Fayette	29
    Forest	4
    Franklin	32
    Fulton	1
    Greene	12
    Huntingdon	4
    Indiana	17
    Juniata	11
    Lackawanna	190
    Lancaster	408
    Lawrence	24
    Lebanon	124
    Lehigh	1006
    Luzerne	849
    Lycoming	10
    McKean	1
    Mercer	18
    Mifflin	5
    Monroe	572
    Montgomery	1230
    Montour	33
    Northampton	716
    Northumberland	15
    Perry	5
    Philadelphia	3611
    Pike	125
    Potter	3
    Schuylkill	103
    Snyder	8
    Somerset	6
    Sullivan	1
    Susquehanna	6
    Tioga	8
    Union	6
    Venango	5
    Warren	1
    Washington	53
    Wayne	35
    Westmoreland	157
    Wyoming	5
    York	189', sep = "\t"
  ),
  "2020-04-07" = read_text_table(
    'Adams	28
    Allegheny	689
    Armstrong	18
    Beaver	116
    Bedford	4
    Berks	369
    Blair	6
    Bradford	14
    Bucks	690
    Butler	107
    Cambria	7
    Cameron	1
    Carbon	67
    Centre	55
    Chester	335
    Clarion	9
    Clearfield	7
    Clinton	3
    Columbia	42
    Crawford	8
    Cumberland	77
    Dauphin	155
    Delaware	898
    Elk	2
    Erie	28
    Fayette	32
    Forest	5
    Franklin	39
    Fulton	1
    Greene	15
    Huntingdon	5
    Indiana	21
    Jefferson	1
    Juniata	14
    Lackawanna	235
    Lancaster	490
    Lawrence	29
    Lebanon	145
    Lehigh	1146
    Luzerne	982
    Lycoming	12
    McKean	1
    Mercer	26
    Mifflin	6
    Monroe	610
    Montgomery	1359
    Montour	27
    Northampton	774
    Northumberland	22
    Perry	9
    Philadelphia	4012
    Pike	136
    Potter	3
    Schuylkill	119
    Snyder	9
    Somerset	7
    Sullivan	1
    Susquehanna	11
    Tioga	10
    Union	7
    Venango	5
    Warren	1
    Washington	57
    Wayne	40
    Westmoreland	177
    Wyoming	5
    York	218', sep = "\t"
  ),
  "2020-04-08" = read_text_table(
    'Adams	33
    Allegheny	720
    Armstrong	19
    Beaver	128
    Bedford	4
    Berks	416
    Blair	6
    Bradford	15
    Bucks	756
    Butler	113
    Cambria	9
    Cameron	1
    Carbon	76
    Centre	57
    Chester	373
    Clarion	8
    Clearfield	7
    Clinton	3
    Columbia	54
    Crawford	8
    Cumberland	84
    Dauphin	168
    Delaware	1034
    Elk	2
    Erie	29
    Fayette	35
    Forest	5
    Franklin	43
    Fulton	1
    Greene	17
    Huntingdon	6
    Indiana	21
    Jefferson	1
    Juniata	18
    Lackawanna	266
    Lancaster	561
    Lawrence	32
    Lebanon	169
    Lehigh	1319
    Luzerne	1134
    Lycoming	15
    McKean	1
    Mercer	27
    Mifflin	10
    Monroe	671
    Montgomery	1521
    Montour	26
    Northampton	857
    Northumberland	22
    Perry	13
    Philadelphia	4456
    Pike	148
    Potter	3
    Schuylkill	136
    Snyder	9
    Somerset	7
    Sullivan	1
    Susquehanna	14
    Tioga	10
    Union	8
    Venango	5
    Warren	1
    Washington	59
    Wayne	47
    Westmoreland	183
    Wyoming	5
    York	233', sep = "\t"
  ),
  "2020-04-09" = read_text_table(
    'Adams	38
    Allegheny	759
    Armstrong	20
    Beaver	129
    Bedford	4
    Berks	616
    Blair	6
    Bradford	15
    Bucks	871
    Butler	113
    Cambria	10
    Cameron	1
    Carbon	83
    Centre	59
    Chester	425
    Clarion	8
    Clearfield	7
    Clinton	4
    Columbia	65
    Crawford	13
    Cumberland	88
    Dauphin	180
    Delaware	1222
    Elk	2
    Erie	32
    Fayette	45
    Forest	5
    Franklin	52
    Fulton	1
    Greene	21
    Huntingdon	8
    Indiana	21
    Jefferson	1
    Juniata	23
    Lackawanna	312
    Lancaster	596
    Lawrence	37
    Lebanon	187
    Lehigh	1466
    Luzerne	1241
    Lycoming	17
    McKean	1
    Mercer	30
    Mifflin	10
    Monroe	716
    Montgomery	1693
    Montour	25
    Northampton	949
    Northumberland	24
    Perry	15
    Philadelphia	5029
    Pike	163
    Potter	3
    Schuylkill	149
    Snyder	9
    Somerset	7
    Sullivan	1
    Susquehanna	16
    Tioga	10
    Union	11
    Venango	5
    Warren	1
    Washington	63
    Wayne	49
    Westmoreland	190
    Wyoming	6
    York	250', sep = "\t"
  ),
  "2020-04-10" = read_text_table(
    'Adams	43
    Allegheny	788
    Armstrong	22
    Beaver	139
    Bedford	3
    Berks	720
    Blair	9
    Bradford	16
    Bucks	958
    Butler	123
    Cambria	11
    Cameron	1
    Carbon	93
    Centre	61
    Chester	485
    Clarion	10
    Clearfield	8
    Clinton	6
    Columbia	81
    Crawford	13
    Cumberland	96
    Dauphin	199
    Delaware	1377
    Elk	2
    Erie	36
    Fayette	49
    Forest	5
    Franklin	57
    Fulton	1
    Greene	21
    Huntingdon	8
    Indiana	26
    Jefferson	1
    Juniata	30
    Lackawanna	346
    Lancaster	648
    Lawrence	45
    Lebanon	218
    Lehigh	1562
    Luzerne	1325
    Lycoming	18
    McKean	1
    Mercer	36
    Mifflin	11
    Monroe	752
    Montgomery	1889
    Montour	29
    Northampton	994
    Northumberland	29
    Perry	16
    Philadelphia	5521
    Pike	190
    Potter	3
    Schuylkill	164
    Snyder	12
    Somerset	10
    Sullivan	1
    Susquehanna	22
    Tioga	11
    Union	12
    Venango	5
    Warren	1
    Washington	66
    Wayne	53
    Westmoreland	202
    Wyoming	6
    York	283', sep = "\t"
  )
) %>% 
  map(mutate_all, as.integer) %>% 
  enframe("date", "confirmed") %>% 
  unnest("confirmed") %>% 
  map_dfr(replace_na, 0L) %>%
  mutate_at("date", as.Date)


# County populations --------------------------------------------------------------------------

file <- "docs/PEP_2018_PEPANNRES_with_ann.csv"

text <- read_csv(file, col_names = FALSE)[1:2,]
temp <- read_csv(file, skip = 2, col_names = FALSE)
names(temp) <- text[1,] %>% unlist(use.names = TRUE) %>% make_clean_names()
Hmisc::label(temp) <- text[2, ]

pa_county_population <- temp %>% 
  select(county = geo_display_label,
         estimate_population = respop72018) %>% 
  mutate(county = str_remove(county, "\\sCounty,\\sPennsylvania$"))

library(usethis)

use_data(pa_county_confirmed, overwrite = TRUE)
use_data(pa_county_population, overwrite = TRUE)

