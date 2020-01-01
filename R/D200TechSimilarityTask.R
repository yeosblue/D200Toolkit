#' Create a list of corpus similarity based recommendations.
#'
#' @description
#' Imagine this object is a lab. You can use two methods in this lab
#'  easy to make a recommendation list.
#'
#' @import dplyr
#' @import rlang
#' @import text2vec
#' @import stringr
#' @import progress
#' @importFrom R6 R6Class
#' @importFrom purrr map_chr
#' @importFrom tidyr gather
#' @importFrom tibble as_tibble
#' @importFrom jiebaR worker
#' @importFrom readr write_rds
#' @importFrom readr write_excel_csv
#' @importFrom tibble rowid_to_column
#' @export
#' @md

D200TechSimilarityTask <- R6::R6Class(
  classname = "D200TechSimilarityTask",
  public = list(

    #' @field raw_corpus Preprocessed corpus after initialize.
    raw_corpus = NULL,

    #' @field corpus_matrix The output of the private method
    #' `get_vectorize_matrix`.
    corpus_matrix = NULL,

    #' @field insight_matrix_set The output of the public method
    #' `insight_matrix_set` ( the raw result of similarity between
    #' `main_item` and `ref_item` ).
    insight_matrix_set = NULL,

    #' @description
    #' Create a new D200TechSimilarityTask object. You
    #' can image this step is to create or enter into the lab you can
    #' easy to make a recommendation list.
    #' This initializing step also do the following work :
    #' 1. Check column name.
    #' 2. Check data type (tibble).
    #' 3. If the data type is wrong, it will auto convert
    #' to the right data type.
    #'
    #' @param raw_corpus The raw corpus you want to analysis
    #' from external place(.csv or .xlsx file). It must be noted that
    #' the corpus column name must be named according to the
    #' following sample :
    #' - text_item_name : the name of the article/text.
    #' - text_content : the content of the article/text.
    #' - text_category : the category/source of the article/text.
    #'
    #' @return A new `D200TechSimilarityTask` object.
    initialize = function(raw_corpus) {

      if (is.null(raw_corpus)) {
        message(" \nThe task create successfully!")
        return()
      }

      tmp_raw_corpus <- raw_corpus
      df_class <- map_chr(tmp_raw_corpus, class)
      fct_col <- names(which(df_class == "factor"))
      col_name <- colnames(tmp_raw_corpus)
      col_diff <- setdiff(col_name, c("text_item_name", "text_content", "text_category"))

      if (!identical(names(which(df_class == "factor")), character(0))) {
        for (i in fct_col) {
          tmp_raw_corpus[[i]] <- as.character(tmp_raw_corpus[[i]])
          message(paste0(" \ndrop factor class from column - ", i))
        }
      }

      if (!inherits(tmp_raw_corpus, "tbl_df")){
        tmp_raw_corpus <- as_tibble(tmp_raw_corpus)
      }

      if (!identical(col_diff, character(0))) {
        abort("Please check whether the column name is correct.")
      }

      self$raw_corpus <- tmp_raw_corpus

      message(" \nThe task create successfully!")


    },


    #' @description
    #' Create the insight matrix set
    #'
    #'
    #' @param corpus_matrix Like above description, `corpus_matrix` is the output
    #' of the private method `get_vectorize_matrix`.
    #'
    #' @param set_cat the `list` object contain two following character vector
    #' from `text_category` field from raw corpus :
    #' - main_cat : which types of corpus you want to base on.
    #' - ref_cat : which types of corpus you want to be recommended.
    #'
    #'
    #' @return
    #' - a matrix object store in `R6_object$insight_matrix_set`.
    #' - auto create .rds file in insight_matrix_set folder in your
    #' working directory.
    #'
    get_insight_matrix = function(corpus_matrix = self$corpus_matrix,
                                  set_cat = list(main_cat = "main_cat_name",
                                                 ref_cat = "ref_cat_name")){
      self$raw_corpus <- self$raw_corpus %>%
        arrange(match(text_category, c(set_cat[["ref_cat"]], set_cat[["main_cat"]]))) %>%
        rowid_to_column()

      private$get_vectorize_matrix()

      text_category_all <- unique(self$raw_corpus$text_category)

      if (!(set_cat[["main_cat"]] %in% text_category_all
            && set_cat[["ref_cat"]] %in% text_category_all)) {
        abort("Please check the categories you choose are actually within raw_corpus.")
      }

      main_cat_id <- self$raw_corpus %>%
        filter(text_category %in% set_cat[["main_cat"]]) %>%
        pull(rowid)

      ref_cat_id <- self$raw_corpus %>%
        filter(text_category %in% set_cat[["ref_cat"]]) %>%
        pull(rowid)

      insight_marix_set <- list(
        insight_matrix = sim2(corpus_matrix[main_cat_id,],
                              corpus_matrix[ref_cat_id,],
                              method = "cosine",
                              norm = "l2"),
        set_cat = list(
          "main_cat_id" = main_cat_id,
          "ref_cat_id" = ref_cat_id
        )
      )

      #if (!dir.exists("insight_matrix_set")) {
      #  abort("folder-insight_matrix_set not found! Please initialize the project/check the working directory whether correct")
      #}

      #tmp_file_name <- Sys.time() %>% format(format = "%Y-%m-%d-%H-%M-%S")
      #write_rds(insight_marix_set, paste0("insight_matrix_set//",tmp_file_name,"_ins_matrix.RDS"))

      #message(" \ninsight_matrix_set has been stored in insight_matrix_set folder successfully!")

      self$insight_matrix_set <- insight_marix_set

      message(" \ninsight_matrix_set is stored in R6_Object_name$insight_matrix_set!")
    },

    #' @description
    #' Create the recommendation list.
    #'
    #' @param insight_matrix_set Like above description, corpus_matrix is the output
    #' of the public method `get_insight_matrix`. The default value of this
    #' argument is `insight_matrix_set` make by latest excutation. You also can use
    #' different version `insight_matrix_set` stored in insight_matrix_set folder in
    #' your working directory.
    #'
    #' @param topn n most relevant item in ref_cat you choice.
    #'
    #' @return The recommendation list will export in recommend_result folder in
    #' your working directory.

    get_recommend_result = function(insight_matrix_set = self$insight_matrix_set, topn){

      tmp_matrix_set <- insight_matrix_set
      tmp_matrix <- tmp_matrix_set$insight_matrix

      if (is.null(self$raw_corpus)) {
        tmp_raw_corpus <- insight_matrix_set$raw_corpus_in_set
      } else {
        tmp_raw_corpus <- self$raw_corpus
      }

      tmp_tbl <- matrix(ceiling(order(row(tmp_matrix), tmp_matrix, decreasing = TRUE) / nrow(tmp_matrix)),
                        nrow(tmp_matrix),
                        byrow = TRUE) %>%
        `[`(nrow(tmp_matrix):1, ) %>%
        as_tibble() %>%
        select(1:topn)

      colnames(tmp_tbl) <- colnames(tmp_matrix)[1:topn]

      tmp_tbl <- tmp_tbl %>%
        mutate(main_id = row.names(tmp_matrix)) %>%
        gather(rank, ref_id, colnames(tmp_tbl)) %>%
        mutate(main_id = as.numeric(main_id),
               ref_id = as.numeric(ref_id)) %>%
        arrange(main_id) %>%
        left_join(tmp_raw_corpus %>% select(rowid, text_item_name, text_category),
                  by = c("main_id" = "rowid")) %>%
        left_join(tmp_raw_corpus %>% select(rowid, text_item_name, text_category),
                  by = c("ref_id"= "rowid")) %>%
        rename("main_item" = "text_item_name.x",
               "ref_item" = "text_item_name.y",
               "main_item_category" = "text_category.x",
               "ref_item_category" = "text_category.y") %>%
        select(rank, ref_item, ref_item_category, main_item, main_item_category)

      # tmp_file_name <- Sys.time() %>% format(format = "%Y-%m-%d-%H-%M-%S")
      # write_excel_csv(tmp_tbl,  paste0("recommend_result//",tmp_file_name,"_recom_result.csv"))
      # message(" \n Export recommendation list successfully! Please check recommend_result folder in your working directory.")

      return(tmp_tbl)
    },

    get_connection = function(id1, id2) {

      if (is.null(self$corpus_matrix)) {
        abort("corpus_matrix not found !")
      }

      tmp_connection <- normalize(self$corpus_matrix[as.character(id1),,drop = FALSE], "l2") *
        normalize(self$corpus_matrix[as.character(id2),,drop = FALSE], "l2")

      connection_tbl <- tibble(term = tmp_connection@Dimnames[[2]][which(as.matrix(tmp_connection)>0)],
                               connect_percentage = tmp_connection@x) %>%
        mutate(connect_percentage = connect_percentage/sum(connect_percentage)) %>%
        arrange(desc(connect_percentage))

      connection_tbl %>% View()

    }

  ),

  private = list(
    get_vectorize_matrix = function(corpus_data = self$raw_corpus) {

      if (file.exists("stopword/stop_word.txt")) {
        cutter <- jiebaR::worker(stop_word = "stopword/stop_word.txt")
      } else {
        message(" \nCan't find stopword/stop_word.txt, So current tokenizer isn't set stop word.")
        cutter <- jiebaR::worker()
      }

      pb <- progress_estimated(length(corpus_data$text_content))

      message(" \nTokenizing ...")

      token_func <- function(x){
        pb$tick()$print()
        cutter[x] %>% str_c(collapse = " ")
      }

      t2v_token_set <- corpus_data %>%
        mutate(token = map_chr(corpus_data$text_content, token_func))
      it <- itoken(t2v_token_set$token, progressbar = FALSE)
      self$raw_corpus <- t2v_token_set
      message(" \nStep(1/5):Tokenize corpus successfully!")

      v <- create_vocabulary(it)
      message(" \nStep(2/5):Create vocabulary successfully!")

      vectorizer <- vocab_vectorizer(v)
      message(" \nStep(3/5):Vectorize successfully!")

      dtm <- create_dtm(it, vectorizer)
      message(" \nStep(4/5):Create DTM successfully!")

      tfidf <- TfIdf$new()
      dtm_tfidf <- fit_transform(dtm, tfidf)
      message(" \nStep(5/5):TF-IDF transform successfully!")

      self$corpus_matrix <- dtm_tfidf
      message(" \nThe Matrix is stored in R6_Object_name$corpus_matrix!")
    }

  )

)


