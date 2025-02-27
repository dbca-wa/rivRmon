# rivRmon 3.0.1

### Minor update to internal data

* Sampling site KEND50 addition caused problems with over-plotting at Kent Street 
weir. Site was artificially moved a little downstream so that the weir appearance did not 
change in the long running surfer plots. Bottom bathymetry was slightly adjusted 
to account for sampling depth indicators on the plot.

# rivRmon 3.0.0

### Minor update to functions

* `swan_sufR` has been updated:
    + to replace the `swan_sufR_alt` function which has been removed.
    + to include minor adjustments to legend titles and legend bars to improve 
    aesthetics.
    + site SAND is now SANDBR in plots and it is expected to be called this in 
    the sonde data.
    + centred axis labels and added minor padding to the x axis to avoid 
    truncating labels.
    + top bin of colour scales in now a greater than ('>') category to accommodate 
    temperatures that exceed the existing scale.
    + response sites can now be included in the data and will be used as extra data 
    for interpolation, will appear in the plots in the water column but will not 
    be labelled (due to room limitations).
    
* `canning_sufR` has been updated:
    + to include minor adjustments to legend titles and legend bars to improve 
    aesthetics.
    + centred axis labels and added minor padding to the x axis to avoid 
    truncating labels.
    + top bin of colour scales in now a greater than ('>') category to accommodate 
    temperatures that exceed the existing scale.
    + response sites can now be included in the data and will be used as extra data 
    for interpolation, will appear in the plots in the water column but will not 
    be labelled.
    
### Update to internal data

* `sysdata`, which is only accessible to the package functions, has been updated:
    + to create bathymetry profile from newly updated origin shape files that now 
    contains site historical response sites.
    + to have new data breaks and labels to incorporate changes to the surfer plot 
    legends where the top bins are now a greater than ('>') category.
    
### A new function!

* `plot_metric` is a new function:
    + it draws on data from an edited excel workbook sheet that has BOTH sonde 
    data files appended and produces a single surfer style plot of any metric in the data.
    + the user will be prompted by the code for the metric they wish to plot.
    + the plots are intended as ad-hoc/quick looks at the data.

# rivRmon 2.0.2

### Minor update to functions

* `canning_surfR` has been updated:
    + to incorporate a new site, NIC-IN.
    + to remove some plot aesthetics relating to downgraded sampling sites (KENU300,
    BACD300, BACU300, GRE, MASD50, NICD200, MACD50).
    + to add plot aesthetics for site KS7.
    + to remove PDF plot output.
    
* `swan_sufR` has been updated:
    + to remove PDF plot output.
    
* `phyto_plotR` has been updated:
    + to add plot aesthetics if/when site KS7 is used for phytoplankton surveys. 
    
### Update to internal data

* `sysdata`, which is only accessible to the package functions, has been updated:
    + to create bathymetry profile from newly updated origin shape files that now 
    contains site NIC-IN.
    + to add new fixed depths for downgraded sampling sites (KENU300,
    BACD300, BACU300, GRE, MASD50, NICD200, MACD50).

# rivRmon 2.0.1

### Minor updates to functions

* `swan_surfR` has been updated:
     + to incorporate new names for sites BWR10 and Mulberry Farm, 
     to SAND and MULB respectively.
     
* `phyto_groupR` has been updated:
     + to function with a change of multiple project names to a single one with the 
     change of lab. Outputs now named by "river".
     
* `sysdata`, which is only accessible to the package functions, has been updated:
     + to create bathymetry profile from newly updated origin shape files.
     + to create applicable filters for masking missing data.

# rivRmon 2.0.0

### Updates to functions

* `oxy_wranglR` has been updated:
     + to fix unwanted inclusion of gaps in plotted lines where data 
     points were not one week apart (i.e. when public holidays delayed sampling)
     + to output all weekly and seasonal statistics.
     + names outputs in stats to sensible names.
     
* `phyto_finder` (internal function) has been updated:
     + to search for data beginning with at least 6 digits. There is a naming 
     discrepancy between original data as output from the PEU and how RE archive
     this data. This ensures it can use either as source data.
     
* `phyto_groupR` has been updated:
     + to address new PEU data delivery format (GitHub #9, @Bartesto).
     + to only focus on a sheet with "Routine" in the name (seems to be most 
     unchangeable element in the raw data).
     + "bacillariophyta" now included in Diatom group.
     + to now take date from file name rather than from that reported inside the
     workbook. This addresses weird intermittent date format issues on reading of 
     the input data.
     
* `phyto_plotR` has been updated:
     + export plots as PNG format so to aid inclusion in Microsoft products 
     (GitHub#6, @Bartesto).
     + can now include site "SHELL" if needed.
     
* `swan_surfR` and `canning_surfR` have been updated:
     + removed titles (they are titled within the report headers).
     + removed oxygen plant status legend (had been too cramped and small and 
     info appears in the report).
     + ability to add in the site "SHELL" if needed in the Canning River plots.
     + exports plots in PNG format for easier inclusion without resizing to 
     reports, removed margins and made any transparent.
     + river bottom at site locations now adjusts to the data (+ 20cm).
     + updated colour ramps to fix white "blob" issue (GitHub#4).
     
### New functions
     
* `hab_groupR` is a new function! It reads in the same PEU data and summarises 
the data according to HAB reporting species and outputs a weekly summary to CSV 
file. Also coded to include site "SHELL" if needed.

* `hab_tablR` is a new function! It takes the weekly summary HAB data and 
creates a conditionally formatted table as a PPTX output according to HAB 
reportable triggers.

* `lims_parsR` is a new function! It takes 3 interim weekly chlorophyll a result 
workbooks as supplied from the Chem Centre, summarises and outputs them to CSV.

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
* CKAN functions are in development. Please do not use them.
