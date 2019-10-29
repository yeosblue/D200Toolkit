#' @title
#' Initialize the working directory.
#'
#' @description
#'  `initialize_project()` is a side effect function. Must use it at the
#'  beginning of the whole project, it will auto create three folder in your
#'  working directory.
#'
#' @details
#'  The purpose of each folder:
#'  - insight_matrix_set : The place store insight_matrix_set's rds file.
#'  - recommend_result : The place store the result list of recommendation.
#'  - stopword : The place tore the list of stop word.
#'
#'  If each folder has been created in the previous project, the function
#'   will also notify the folder already existed.
#' @export
#' @md

initialize_project <- function(){

  dir_name <- c("insight_matrix_set", "recommend_result", "stopword")

  for (i in dir_name) {
    if (!dir.exists(i)) {
      dir.create(i)
    } else {
      message(paste0("\n ",i , " has been created."))
    }
  }

  message("initialize completed!")

}
