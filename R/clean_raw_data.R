
#' clean_ncc_sheet
#'
#' @param path path to the raw excel file
#' @param sheet name of the sheet
#'
#' @return a tbl
#' @export
#'
#' @examples
#' 
clean_ncc_sheet <- function(path, sheet) {
  readxl::read_excel(
    path = path, 
    sheet = sheet, 
    skip = 2, 
    col_types = c(rep("text", 20), "date")
  ) %>% 
 tricky::set_standard_names() %>%
 dplyr::filter(is.na(numero) == FALSE)  
  }
