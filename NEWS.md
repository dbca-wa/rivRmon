# rivRmon 1.1.0

* `oxy_wranglR` function (GitHub #1, @Bartesto) has been added. This was 
previously a stand alone function called `oxy_kpi4`.

* `phyto_plotR` is not restricted to finding data 7 days prior to user entered
date for subsetting data for the prior plot. Issue arose when public holidays
delay a sampling run. Function now checks that the user entered date is 
available and that there is data for a prior plot in the summary data and then 
selects the closest preceding date out of all available. (GitHub #2, @Bartesto).


# rivRmon 1.0.0

* Initial release of rivRmon.
* Functions from two other development packages have been amalgamated into 
rivRmon (monitoR and reportR) along with 3 phyto functions.
* CKAN functions are in development. Please do not use.



