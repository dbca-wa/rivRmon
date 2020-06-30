#' Management Response Triggers
#' 
#' This data set is an example of the format required for creating a table for 
#' ingestion to the (\code{\link{hab_groupR}}) and (\code{\link{hab_tablR}}) 
#' functions. Values in this dataset determine which species are summarised and 
#' what values constitute differing levels of management action.
#' 
#' Although this is an example of the current triggers (as at time of writing) it 
#' is intended that this be used as a template to guide the creation of trigger 
#' data that would be saved out as a csv for use with the HAB functions. Note 
#' that values falling between surveillance and notification are classified as 
#' investigate.
#' 
#' @format a data.frame with 5 variables:
#' \describe{
#' \item{table_ord}{numeric index of the desired order of reportable groups}
#' \item{table_name}{character representing the reportable group name as it is intended for the output}
#' \item{spp_name}{character representing the exact species name/s that compromise the reportable group}
#' \item{surveillance}{numeric values below this are categorised as surveillance}
#' \item{notification}{numeric values equal to or above are categorised as notification}
#' }
#' @examples 
#' mngt_response_triggers
"mngt_response_triggers"