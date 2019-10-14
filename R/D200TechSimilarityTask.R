D200TechSimilarityTask <- R6Class(
  classname = "D200TechSimilarityTask",
  public = list(
    raw_corpus = NULL,
    corpus_matrix = NULL,
    insight_matrix_set = NULL,
    initialize = function(raw_corpus) {

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

      tmp_raw_corpus <- tmp_raw_corpus %>%
        rowid_to_column()

      self$raw_corpus <- tmp_raw_corpus

      message(" \nThe task created successfully!")

      private$get_vectorize_matrix()
    },

    get_insight_matrix = function(corpus_matrix = self$corpus_matrix,
                                  set_cat = list(main_cat = "",
                                                 ref_cat = "")){

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

      if (!dir.exists("insight_matrix_set")) {
        abort("folder-insight_matrix_set not found! Please initialize the project/check the working directory whether correct")
      }

      tmp_file_name <- Sys.time() %>% format(format = "%Y-%m-%d-%H-%M-%S")
      write_rds(insight_marix_set, paste0("insight_matrix_set//",tmp_file_name,"_ins_matrix.RDS"))

      message(" \ninsight_matrix_set has been stored in insight_matrix_set folder successfully!")

      self$insight_matrix_set <- insight_marix_set

      message(" \ninsight_matrix_set is stored in R6_Object_name$insight_matrix_set!")
    },

    get_recommend_result = function(insight_matrix_set = self$insight_matrix_set, topn){

      tmp_matrix_set <- insight_matrix_set

      tmp_list <- vector("list", length(tmp_matrix_set$set_cat$main_cat_id))
      names(tmp_list) <- tmp_matrix_set$set_cat$main_cat_id

      pb <- progress_bar$new(
        total = length(tmp_list),
        format = "  exporting recommended list [:bar] :current/:total (:percent) eta: :eta"
      )
      for (i in 1:length(tmp_list)) {
        pb$tick()
        tmp_list[[i]] <-
          tmp_matrix_set$insight_matrix[i,
                                        tmp_matrix_set$insight_matrix[i, ] %>% order(decreasing=TRUE)] %>%
          names() %>%
          `[`(1:topn)
      }

      tmp_tbl <- tmp_list %>%
        as_tibble() %>%
        gather(main_id, ref_id, names(tmp_list)) %>%
        mutate(main_id = as.numeric(main_id),
               ref_id = as.numeric(ref_id)) %>%
        left_join(test$raw_corpus %>% select(rowid, text_item_name, text_category),
                  by = c("main_id" = "rowid")) %>%
        left_join(test$raw_corpus %>% select(rowid, text_item_name, text_category),
                  by = c("ref_id"= "rowid")) %>%
        mutate(rank = rep(1:topn, length(tmp_list))) %>%
        rename("main_item" = "text_item_name.x",
               "ref_item" = "text_item_name.y",
               "main_item_category" = "text_category.x",
               "ref_item_category" = "text_category.y") %>%
        select(rank, ref_item, ref_item_category, main_item, main_item_category)

      tmp_file_name <- Sys.time() %>% format(format = "%Y-%m-%d-%H-%M-%S")
      write.csv(tmp_tbl,  paste0("recommend_result//",tmp_file_name,"_recom_result.csv"), row.names = FALSE)
      message(" \nExport recommended list successfully!")

    }

  ),

  private = list(
    cutter = jiebaR::worker(stop_word = "stopword/stop_word.txt"),
    get_vectorize_matrix = function(corpus_data = self$raw_corpus) {

      pb <- progress_estimated(length(self$raw_corpus$text_content))

      message(" \nTokenizing ...")

      token_func <- function(x){
        pb$tick()$print()
        private$cutter[x] %>% str_c(collapse = " ")
      }

      t2v_token_set <- self$raw_corpus %>%
        mutate(token = map_chr(self$raw_corpus$text_content, token_func))
      it <- itoken(t2v_token_set$token, progressbar = FALSE)
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
