#' Title
#'
#' @return
#' @export
#'
#' @examples
#'
initialize_project <- function(){

  dir_name <- c("insight_matrix_set", "recommend_result", "error_log", "raw_corpus")

  for (i in dir_name) {
    if (!dir.exists(i)) {
      dir.create(i)
    } else {
      message(paste0(i, " has been created."))
    }
  }

  message("initialize completed!")

}
