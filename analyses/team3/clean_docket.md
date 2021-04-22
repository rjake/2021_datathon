clean and quick summary docket data
================
Chun

# load packages

``` r
library(tidyverse)
library(lubridate)
```

# original data glance

``` r
ddt = read_csv("https://storage.googleapis.com/jat-rladies-2021-datathon/defendant_docket_details.csv")
```

Original data constitute 370,313 rows and 19 columns, with column names
listed as

``` r
colnames(ddt)
```

    ##  [1] "docket_id"                                        
    ##  [2] "gender"                                           
    ##  [3] "race"                                             
    ##  [4] "date_of_birth"                                    
    ##  [5] "arrest_date"                                      
    ##  [6] "complaint_date"                                   
    ##  [7] "disposition_date"                                 
    ##  [8] "filing_date"                                      
    ##  [9] "initiation_date"                                  
    ## [10] "status_name"                                      
    ## [11] "court_office__court__display_name"                
    ## [12] "current_processing_status__processing_status"     
    ## [13] "current_processing_status__status_change_datetime"
    ## [14] "municipality__name"                               
    ## [15] "municipality__county__name"                       
    ## [16] "judicial_districts"                               
    ## [17] "court_office_types"                               
    ## [18] "court_types"                                      
    ## [19] "representation_type"

Each row has unique docket\_id (thus 370,313 ids), which should be used
to key for data split and table merging.

Quick description of data

``` r
Hmisc::describe(ddt)
```

    ## ddt 
    ## 
    ##  19  Variables      370313  Observations
    ## --------------------------------------------------------------------------------
    ## docket_id 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##   370313        0   370313        1   185158   123439    18517    37033 
    ##      .25      .50      .75      .90      .95 
    ##    92580   185158   277737   333284   351799 
    ## 
    ## lowest :      1      2      3      4      5, highest: 370311 370312 370313 370314 370315
    ## --------------------------------------------------------------------------------
    ## gender 
    ##        n  missing distinct 
    ##   370083      230        2 
    ##                         
    ## Value      Female   Male
    ## Frequency   59729 310354
    ## Proportion  0.161  0.839
    ## --------------------------------------------------------------------------------
    ## race 
    ##        n  missing distinct 
    ##   361380     8933        7 
    ## 
    ## lowest : Asian                          Asian/Pacific Islander         Bi-Racial                      Black                          Native American/Alaskan Native
    ## highest: Bi-Racial                      Black                          Native American/Alaskan Native Unknown/Unreported             White                         
    ## 
    ## Asian (77, 0.000), Asian/Pacific Islander (5076, 0.014), Bi-Racial (89, 0.000),
    ## Black (220624, 0.611), Native American/Alaskan Native (261, 0.001),
    ## Unknown/Unreported (4760, 0.013), White (130493, 0.361)
    ## --------------------------------------------------------------------------------
    ## date_of_birth 
    ##          n    missing   distinct       Info       Mean        Gmd        .05 
    ##     370094        219         87      0.999 1981-01-14       4830 1959-01-01 
    ##        .10        .25        .50        .75        .90        .95 
    ## 1963-01-01 1973-01-01 1984-01-01 1990-01-01 1994-01-01 1996-01-01 
    ## 
    ## lowest : 1900-01-01 1901-01-01 1902-01-01 1903-01-01 1924-01-01
    ## highest: 2003-01-01 2004-01-01 2005-01-01 2011-01-01 2012-01-01
    ## --------------------------------------------------------------------------------
    ## arrest_date 
    ##          n    missing   distinct       Info       Mean        Gmd        .05 
    ##     370236         77        270          1 2014-10-12       1299 2010-05-01 
    ##        .10        .25        .50        .75        .90        .95 
    ## 2010-10-01 2012-03-01 2014-07-01 2017-06-01 2019-05-01 2019-10-01 
    ## 
    ## lowest : 1962-09-01 1986-11-01 1988-07-01 1988-08-01 1990-04-01
    ## highest: 2020-02-01 2020-03-01 2020-04-01 2020-05-01 2020-06-01
    ## --------------------------------------------------------------------------------
    ## complaint_date 
    ##          n    missing   distinct       Info       Mean        Gmd        .05 
    ##     175245     195068        265          1 2012-10-05       1014 2010-02-01 
    ##        .10        .25        .50        .75        .90        .95 
    ## 2010-04-01 2010-12-01 2012-01-01 2013-02-01 2017-08-01 2019-03-01 
    ## 
    ## lowest : 1988-07-01 1990-04-01 1991-02-01 1991-12-01 1992-09-01
    ## highest: 2020-02-01 2020-03-01 2020-04-01 2020-05-01 2020-06-01
    ## --------------------------------------------------------------------------------
    ## disposition_date 
    ##          n    missing   distinct       Info       Mean        Gmd        .05 
    ##     321488      48825        126          1 2014-11-05       1174 2010-10-01 
    ##        .10        .25        .50        .75        .90        .95 
    ## 2011-03-01 2012-07-01 2014-08-01 2017-02-01 2019-01-01 2019-08-01 
    ## 
    ## lowest : 2010-01-01 2010-02-01 2010-03-01 2010-04-01 2010-05-01
    ## highest: 2020-02-01 2020-03-01 2020-04-01 2020-05-01 2020-06-01
    ## --------------------------------------------------------------------------------
    ## filing_date 
    ##          n    missing   distinct       Info       Mean        Gmd        .05 
    ##     370313          0        126          1 2014-11-11       1286 2010-06-01 
    ##        .10        .25        .50        .75        .90        .95 
    ## 2010-11-01 2012-04-01 2014-08-01 2017-07-01 2019-06-01 2019-11-01 
    ## 
    ## lowest : 2010-01-01 2010-02-01 2010-03-01 2010-04-01 2010-05-01
    ## highest: 2020-02-01 2020-03-01 2020-04-01 2020-05-01 2020-06-01
    ## --------------------------------------------------------------------------------
    ## initiation_date 
    ##          n    missing   distinct       Info       Mean        Gmd        .05 
    ##     370313          0        164          1 2014-11-09       1286 2010-06-01 
    ##        .10        .25        .50        .75        .90        .95 
    ## 2010-11-01 2012-04-01 2014-08-01 2017-07-01 2019-05-01 2019-11-01 
    ## 
    ## lowest : 1962-09-01 1988-08-01 2001-03-01 2001-07-01 2001-10-01
    ## highest: 2020-02-01 2020-03-01 2020-04-01 2020-05-01 2020-06-01
    ## --------------------------------------------------------------------------------
    ## status_name 
    ##        n  missing distinct 
    ##   370313        0        4 
    ##                                                           
    ## Value           Active Adjudicated      Closed    Inactive
    ## Frequency        27080        9904      312171       21158
    ## Proportion       0.073       0.027       0.843       0.057
    ## --------------------------------------------------------------------------------
    ## court_office__court__display_name 
    ##        n  missing distinct 
    ##   370313        0        2 
    ##                                                     
    ## Value          Municipal Court - Philadelphia County
    ## Frequency                                     261237
    ## Proportion                                     0.705
    ##                                                     
    ## Value      Philadelphia County Court of Common Pleas
    ## Frequency                                     109076
    ## Proportion                                     0.295
    ## --------------------------------------------------------------------------------
    ## current_processing_status__status_change_datetime 
    ##          n    missing   distinct       Info       Mean        Gmd        .05 
    ##     370313          0        127          1 1948-09-22      48659 2010-06-01 
    ##        .10        .25        .50        .75        .90        .95 
    ## 2011-04-01 2013-01-01 2015-11-01 2018-11-01 2019-12-01 2020-02-01 
    ## 
    ## lowest : 0001-01-01 2010-01-01 2010-02-01 2010-03-01 2010-04-01
    ## highest: 2020-02-01 2020-03-01 2020-04-01 2020-05-01 2020-06-01
    ##                                            
    ## Value      -002-09-16 1997-05-19 2024-10-04
    ## Frequency       12351      20283     337679
    ## Proportion      0.033      0.055      0.912
    ## 
    ## For the frequency table, variable is rounded to the nearest 10000
    ## --------------------------------------------------------------------------------
    ## municipality__name 
    ##        n  missing distinct 
    ##   356825    13488        5 
    ## 
    ## lowest : Bensalem Township    E Goshen Township    Philadelphia City    Upper Darby Township Warminster Township 
    ## highest: Bensalem Township    E Goshen Township    Philadelphia City    Upper Darby Township Warminster Township 
    ##                                                                          
    ## Value         Bensalem Township    E Goshen Township    Philadelphia City
    ## Frequency                     1                    1               356817
    ## Proportion                    0                    0                    1
    ##                                                     
    ## Value      Upper Darby Township  Warminster Township
    ## Frequency                     5                    1
    ## Proportion                    0                    0
    ## --------------------------------------------------------------------------------
    ## municipality__county__name 
    ##        n  missing distinct 
    ##   356825    13488        4 
    ##                                                               
    ## Value             Bucks      Chester     Delaware Philadelphia
    ## Frequency             2            1            5       356817
    ## Proportion            0            0            0            1
    ## --------------------------------------------------------------------------------
    ## judicial_districts 
    ##        n  missing distinct 
    ##   370313        0       88 
    ## 
    ## lowest : Adams, Philadelphia                    Beaver, Philadelphia                   Bedford, Philadelphia                  Berks, Chester, Delaware, Philadelphia Berks, Dauphin, Philadelphia          
    ## highest: Tioga, Philadelphia                    Washington, Philadelphia               Wayne, Philadelphia                    Westmoreland, Philadelphia             York, Philadelphia                    
    ## --------------------------------------------------------------------------------
    ## court_office_types 
    ##        n  missing distinct 
    ##   370313        0       41 
    ## 
    ## lowest : Commonwealth, Criminal, Municipal           Commonwealth, Criminal, Superior, Municipal Commonwealth, Municipal                     Commonwealth, Municipal, Criminal           Commonwealth, Superior, Criminal, Municipal
    ## highest: Supreme, Municipal                          Supreme, Superior, Criminal                 Supreme, Superior, Criminal, Municipal      Supreme, Superior, Municipal                Supreme, Superior, Municipal, Criminal     
    ## --------------------------------------------------------------------------------
    ## court_types 
    ##        n  missing distinct 
    ##   370313        0       11 
    ## 
    ## lowest : CP          CP, MC      CP, PAC     CP, PAC, MC MC         
    ## highest: MC, PAC     PAC, CP     PAC, CP, MC PAC, MC     PAC, MC, CP
    ## 
    ## CP (1395, 0.004), CP, MC (186072, 0.502), CP, PAC (2, 0.000), CP, PAC, MC (99,
    ## 0.000), MC (152411, 0.412), MC, CP (18922, 0.051), MC, PAC (5, 0.000), PAC, CP
    ## (53, 0.000), PAC, CP, MC (10907, 0.029), PAC, MC (219, 0.001), PAC, MC, CP
    ## (228, 0.001)
    ## --------------------------------------------------------------------------------
    ## representation_type 
    ##        n  missing distinct 
    ##   370310        3       97 
    ## 
    ## lowest : Attorney General                                     Co-Counsel, Prosecutor                               Co-Counsel, Prosecutor, Court Appointed              Co-Counsel, Prosecutor, Public Defender              Court Appointed                                     
    ## highest: Prosecutor, Standby - Court Appointed                Public Defender                                      Public Defender, Attorney General                    Public Defender, Attorney General, District Attorney Public Defender, District Attorney                  
    ## --------------------------------------------------------------------------------
    ## 
    ## Variables with all observations missing:
    ## 
    ## [1] current_processing_status__processing_status

# clean data

## defendent info

Defendent information were stored in first 4 columns (`docket_id`,
`gender`, `race`, `date_of_birth`). The age of defendent can be added by
subtraction between `date_of_birth` and `arrest_date`. The reason to
choose `arrest_date` over `complaint_date` is missing value number.
`arrest_age` was calculated just using year of `arrest_date`.

``` r
defendent_info = ddt %>% select(docket_id, gender, race, date_of_birth, arrest_date) %>% 
  mutate(arrest_age_by_year=year(arrest_date) - year(date_of_birth))
```

## docket dates

There are several docket relevant dates included in docket data.
`arrest_date`, `complaint_date`, `disposition_date`, `filing_date`,
`initiation_date` and
`current_processing_status__status_change_datetime`. Since “day is set
to the first of the month to provide some de-identification”, we can use
this date to figure the order of arrest, complaint, disposition, filing
and initiation.

By common sense, I assumed the date order should be complaint -\>
initiation -\> filing -\> arrest -\> disposition. However, 52.6% of
complaint is missing, thus we used `initiation_date` as reference point.

``` r
ddt_dates = ddt %>% 
  select(docket_id, arrest_date, filing_date, initiation_date, disposition_date, filing_date, complaint_date, current_processing_status__status_change_datetime) %>% 
  mutate(
    complaint_by_month=month(complaint_date) - month(initiation_date),
    filing_by_month=month(filing_date) - month(initiation_date), 
    arrest_by_month=month(arrest_date) - month(initiation_date),
    disposition_by_month=month(disposition_date) - month(initiation_date),
  )

# fix current_processing_status__status_change_datetime date
ddt_dates = ddt_dates %>% 
  mutate(current_processing_status__status_change_datetime = ifelse(current_processing_status__status_change_datetime==ymd("0001-01-01"), NA, current_processing_status__status_change_datetime))

ddt_dates %>% select(docket_id, contains("by_month"))
```

    ## # A tibble: 370,313 x 5
    ##    docket_id complaint_by_mon… filing_by_month arrest_by_month disposition_by_m…
    ##        <dbl>             <dbl>           <dbl>           <dbl>             <dbl>
    ##  1    354343                NA               0               0                NA
    ##  2    347123                NA               0               0                NA
    ##  3    355476                NA               0               0                NA
    ##  4    366646                NA               0               0                NA
    ##  5    346083                NA               0               0                NA
    ##  6    350351                NA               0               0                NA
    ##  7    352784                NA               0               0                NA
    ##  8    340746                NA               0               0                NA
    ##  9    365568                NA               0               0                NA
    ## 10    363272                NA               0              -3                NA
    ## # … with 370,303 more rows

In comparison between complaint and initiation, 4.4% dockets initiated
before the complaints

``` r
nrow(ddt_dates %>% filter(!is.na(complaint_by_month), complaint_by_month > 0)) / nrow(ddt_dates)
```

    ## [1] 0.04437867

In comparison between filing and initiation, 0.5% dockets filed before
the initiation. Can it be an error?

``` r
nrow(ddt_dates %>% filter(!is.na(filing_by_month), filing_by_month < 0)) / nrow(ddt_dates)
```

    ## [1] 0.00483915

In comparison between arrest and initiation, 20% dockets defendent was
arrested before the initiation. Can it be an error?

``` r
nrow(ddt_dates %>% filter(!is.na(arrest_by_month), arrest_by_month < 0)) / nrow(ddt_dates)
```

    ## [1] 0.2014134

In comparison between arrest and filing, among 20% dockets, defendent
was arrested before the docket was filed. Can it be an error?

``` r
nrow(ddt_dates %>% filter(arrest_by_month -  filing_by_month < 0)) / nrow(ddt_dates)
```

    ## [1] 0.2076946

In comparison between disposition and initiation, 25.3% dockets was
under disposition before the initiation. Can it be an error?

``` r
nrow(ddt_dates %>% filter(!is.na(disposition_by_month), disposition_by_month < 0)) / nrow(ddt_dates)
```

    ## [1] 0.2533938

## deal with aggregation values

4 values are aggregated - `judicial_districts`, `court_office_types`,
`court_types`, `representation_type`.

### judicial\_districts

There are 44 unique judicial\_districts. All docket cases contains
“Philadephia”.

``` r
ddt_judicial_districts = ddt %>% select(docket_id, judicial_districts) %>% 
  separate_rows(judicial_districts, sep=", ")
```

``` r
ddt_judicial_districts %>% 
  group_by(judicial_districts) %>% 
  summarise(docket_n = n_distinct(docket_id)) %>% 
  ungroup() %>% 
  arrange(desc(docket_n)) %>% 
  mutate(docket_ratio = scales::percent(docket_n /nrow(ddt))) %>% 
  rmarkdown::paged_table()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["judicial_districts"],"name":[1],"type":["chr"],"align":["left"]},{"label":["docket_n"],"name":[2],"type":["int"],"align":["right"]},{"label":["docket_ratio"],"name":[3],"type":["chr"],"align":["left"]}],"data":[{"1":"Philadelphia","2":"370313","3":"100.00000%"},{"1":"Delaware","2":"1010","3":"0.27274%"},{"1":"Bucks","2":"689","3":"0.18606%"},{"1":"Montgomery","2":"610","3":"0.16473%"},{"1":"Chester","2":"224","3":"0.06049%"},{"1":"Lehigh","2":"94","3":"0.02538%"},{"1":"Lancaster","2":"91","3":"0.02457%"},{"1":"Berks","2":"84","3":"0.02268%"},{"1":"York","2":"43","3":"0.01161%"},{"1":"Luzerne","2":"38","3":"0.01026%"},{"1":"Dauphin","2":"26","3":"0.00702%"},{"1":"Cambria","2":"23","3":"0.00621%"},{"1":"Schuylkill","2":"23","3":"0.00621%"},{"1":"Northampton","2":"19","3":"0.00513%"},{"1":"Lebanon","2":"14","3":"0.00378%"},{"1":"Monroe","2":"13","3":"0.00351%"},{"1":"Mifflin","2":"11","3":"0.00297%"},{"1":"Carbon","2":"10","3":"0.00270%"},{"1":"Northumberland","2":"9","3":"0.00243%"},{"1":"Wayne","2":"7","3":"0.00189%"},{"1":"Bedford","2":"5","3":"0.00135%"},{"1":"Blair","2":"5","3":"0.00135%"},{"1":"Cumberland","2":"5","3":"0.00135%"},{"1":"Lackawanna","2":"5","3":"0.00135%"},{"1":"McKean","2":"5","3":"0.00135%"},{"1":"Bradford","2":"4","3":"0.00108%"},{"1":"Erie","2":"4","3":"0.00108%"},{"1":"Huntingdon","2":"4","3":"0.00108%"},{"1":"Washington","2":"4","3":"0.00108%"},{"1":"Indiana","2":"3","3":"0.00081%"},{"1":"Tioga","2":"3","3":"0.00081%"},{"1":"Westmoreland","2":"3","3":"0.00081%"},{"1":"Adams","2":"2","3":"0.00054%"},{"1":"Beaver","2":"2","3":"0.00054%"},{"1":"Centre","2":"2","3":"0.00054%"},{"1":"Clearfield","2":"2","3":"0.00054%"},{"1":"Columbia/Montour","2":"2","3":"0.00054%"},{"1":"Franklin/Fulton","2":"2","3":"0.00054%"},{"1":"Sullivan/Wyoming","2":"2","3":"0.00054%"},{"1":"Susquehanna","2":"2","3":"0.00054%"},{"1":"Clinton","2":"1","3":"0.00027%"},{"1":"Crawford","2":"1","3":"0.00027%"},{"1":"Lawrence","2":"1","3":"0.00027%"},{"1":"Somerset","2":"1","3":"0.00027%"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

add all other columns into table `ddt_judicial_districts`

``` r
ddt_judicial_districts = ddt %>% 
  select(docket_id, status_name, current_processing_status__processing_status, court_office__court__display_name, municipality__name, municipality__county__name) %>% 
left_join(
  ddt_judicial_districts
)
```

### court\_office\_types and court\_types

``` r
ddt_court_type = ddt %>% select(docket_id, court_office_types, court_types)
```

``` r
ddt_court_type %>% separate_rows(court_office_types) %>% 
  group_by(court_office_types) %>% 
  summarise(docket_n = n_distinct(docket_id)) %>% 
  ungroup() %>% 
  arrange(desc(docket_n)) %>% 
  mutate(docket_ratio = scales::percent(docket_n /nrow(ddt))) %>% 
  rmarkdown::paged_table()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["court_office_types"],"name":[1],"type":["chr"],"align":["left"]},{"label":["docket_n"],"name":[2],"type":["int"],"align":["right"]},{"label":["docket_ratio"],"name":[3],"type":["chr"],"align":["left"]}],"data":[{"1":"Municipal","2":"368863","3":"99.61%"},{"1":"Criminal","2":"217678","3":"58.78%"},{"1":"Superior","2":"11238","3":"3.03%"},{"1":"Supreme","2":"3992","3":"1.08%"},{"1":"Commonwealth","2":"363","3":"0.10%"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

[3 court
types](https://github.com/rjake/2021_datathon/blob/meredith-defendant-clean/data/data_description.md#court-type)
- PAC: Pennsylvania Appellate Court (Superior, Commonwealth, Supreme) -
CP: Court of Common Pleas - MC: Municipal Court

``` r
ddt_court_type %>% separate_rows(court_types) %>% 
  group_by(court_types) %>% 
  summarise(docket_n = n_distinct(docket_id)) %>% 
  ungroup() %>% 
  arrange(desc(docket_n)) %>% 
  mutate(docket_ratio = scales::percent(docket_n /nrow(ddt))) %>% 
  rmarkdown::paged_table()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["court_types"],"name":[1],"type":["chr"],"align":["left"]},{"label":["docket_n"],"name":[2],"type":["int"],"align":["right"]},{"label":["docket_ratio"],"name":[3],"type":["chr"],"align":["left"]}],"data":[{"1":"MC","2":"368863","3":"100%"},{"1":"CP","2":"217678","3":"59%"},{"1":"PAC","2":"11513","3":"3%"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
ddt_court_type = ddt_court_type %>% 
  separate_rows(court_office_types) %>% 
  mutate(court_types = case_when(
    court_office_types %in% c("Superior", "Supreme", "Commonwealth") ~ "PAC",
    court_office_types=="Municipal" ~ "MC",
    court_office_types=="Criminal" ~ "CP",
    TRUE ~ court_types
  ))
```

### representation\_type

There are 17 unique representation types, and 67.3% dockets used “Public
Defender” (compared to 23.7% used private)

``` r
ddt_representation_type = ddt %>% select(docket_id, representation_type) %>% 
  separate_rows(representation_type, sep=", ")
```

``` r
ddt_representation_type %>% 
  group_by(representation_type) %>% 
  summarise(docket_n = n_distinct(docket_id)) %>% 
  ungroup() %>% 
  arrange(desc(docket_n)) %>% 
  mutate(docket_ratio=scales::percent(docket_n/nrow(ddt))) %>% 
  rmarkdown::paged_table()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["representation_type"],"name":[1],"type":["chr"],"align":["left"]},{"label":["docket_n"],"name":[2],"type":["int"],"align":["right"]},{"label":["docket_ratio"],"name":[3],"type":["chr"],"align":["left"]}],"data":[{"1":"Prosecutor","2":"369863","3":"99.87848%"},{"1":"Public Defender","2":"249126","3":"67.27444%"},{"1":"Private","2":"87934","3":"23.74586%"},{"1":"Court Appointed","2":"57344","3":"15.48528%"},{"1":"Court Appointed - Private","2":"766","3":"0.20685%"},{"1":"District Attorney","2":"223","3":"0.06022%"},{"1":"Attorney General","2":"167","3":"0.04510%"},{"1":"Court Appointed - Public Defender","2":"165","3":"0.04456%"},{"1":"Standby - Court Appointed","2":"143","3":"0.03862%"},{"1":"Federal Defender","2":"60","3":"0.01620%"},{"1":"Co-Counsel","2":"57","3":"0.01539%"},{"1":"Court Appointed - Co-Counsel","2":"54","3":"0.01458%"},{"1":"Complainant's Attorney","2":"18","3":"0.00486%"},{"1":"Court Appointed - Pending","2":"14","3":"0.00378%"},{"1":"Court Appointed - Vendor","2":"3","3":"0.00081%"},{"1":"NA","2":"3","3":"0.00081%"},{"1":"PCRA Counsel","2":"1","3":"0.00027%"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

# save all files

``` r
save(defendent_info, ddt_dates, ddt_judicial_districts, ddt_court_type, ddt_representation_type, file="ddt_tables.Rdata")
# write.csv(defendent_info, file=file.path("../../data/_clean","docket_defendent.csv"), row.names=F)
# write.csv(ddt_dates, file=file.path("../../data/_clean","docket_dates.csv"), row.names=F)
# write.csv(ddt_judicial_districts, file=file.path("../../data/_clean","docket_judicialDistricts.csv"), row.names=F)
# write.csv(ddt_court_type, file=file.path("../../data/_clean","docket_courtType.csv"), row.names=F)
# write.csv(ddt_representation_type, file=file.path("../../data/_clean","docket_representationType.csv"), row.names=F)
```

# session info

``` r
sessionInfo()
```

    ## R version 4.0.3 (2020-10-10)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Big Sur 10.16
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] lubridate_1.7.9 forcats_0.5.0   stringr_1.4.0   dplyr_1.0.2    
    ##  [5] purrr_0.3.4     readr_1.4.0     tidyr_1.1.2     tibble_3.0.4   
    ##  [9] ggplot2_3.3.2   tidyverse_1.3.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] httr_1.4.2          jsonlite_1.7.2      splines_4.0.3      
    ##  [4] modelr_0.1.8        Formula_1.2-4       assertthat_0.2.1   
    ##  [7] latticeExtra_0.6-29 blob_1.2.1          cellranger_1.1.0   
    ## [10] yaml_2.2.1          pillar_1.4.6        backports_1.1.10   
    ## [13] lattice_0.20-41     glue_1.4.2          digest_0.6.27      
    ## [16] RColorBrewer_1.1-2  checkmate_2.0.0     rvest_0.3.6        
    ## [19] colorspace_1.4-1    htmltools_0.5.0     Matrix_1.2-18      
    ## [22] pkgconfig_2.0.3     broom_0.7.2         haven_2.3.1        
    ## [25] scales_1.1.1        jpeg_0.1-8.1        htmlTable_2.1.0    
    ## [28] generics_0.1.0      ellipsis_0.3.1      withr_2.3.0        
    ## [31] nnet_7.3-14         cli_2.1.0           survival_3.2-7     
    ## [34] magrittr_1.5        crayon_1.3.4        readxl_1.3.1       
    ## [37] evaluate_0.14       fs_1.5.0            fansi_0.4.1        
    ## [40] xml2_1.3.2          foreign_0.8-80      tools_4.0.3        
    ## [43] data.table_1.13.6   hms_0.5.3           lifecycle_0.2.0    
    ## [46] munsell_0.5.0       reprex_0.3.0        cluster_2.1.0      
    ## [49] compiler_4.0.3      rlang_0.4.8         grid_4.0.3         
    ## [52] rstudioapi_0.11     htmlwidgets_1.5.3   base64enc_0.1-3    
    ## [55] rmarkdown_2.5       gtable_0.3.0        DBI_1.1.0          
    ## [58] curl_4.3            R6_2.5.0            gridExtra_2.3      
    ## [61] knitr_1.30          utf8_1.1.4          Hmisc_4.5-0        
    ## [64] stringi_1.5.3       Rcpp_1.0.5          vctrs_0.3.4        
    ## [67] rpart_4.1-15        png_0.1-7           dbplyr_1.4.4       
    ## [70] tidyselect_1.1.0    xfun_0.19
