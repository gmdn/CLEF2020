find_labels_tfidf_only_stem_codiesp <- function(ith_doc) { #ith_doc <- 2
  
  ith_doc_id <- one_line_docs_test$doc_id[ith_doc]
  
  tfidf_ot <- tfidf_codiesp_D_only_stem[ith_doc, ]
  
  output_diag <- one_line_codiesp_D[which(tfidf_ot > 0), ]
  
  output_diag <- output_diag %>%
    mutate(tfidf = tfidf_ot[tfidf_ot > 0]) 
  
  # output_diag <- class_for_ith_doc %>%
  #   filter(class == "DIAGNOSTICO")
  
  if (nrow(output_diag) > 0) {
    
    ith_class <- output_diag %>%
      mutate(case_id = ith_doc_id) %>% 
      rename(code = doc_id) %>%
      select(case_id, code, tfidf) %>%
      group_by(case_id, code) %>%
      summarize(sum_tfidf = sum(tfidf)) %>%
      ungroup() %>%
      arrange(desc(sum_tfidf)) %>%
      top_n(19, sum_tfidf)
    
    test_D <<- test_D %>%
      bind_rows(ith_class)
    
  }  
  
  tfidf_ot <- tfidf_codiesp_P_only_stem[ith_doc, ]
  
  output_diag <- one_line_codiesp_P[which(tfidf_ot > 0), ]
  
  output_diag <- output_diag %>%
    mutate(tfidf = tfidf_ot[tfidf_ot > 0]) 
  
  if (nrow(output_diag) > 0) {
    
    ith_class <- output_diag %>%
      mutate(case_id = ith_doc_id) %>% 
      rename(code = doc_id) %>%
      select(case_id, code, tfidf) %>%
      group_by(case_id, code) %>%
      summarize(sum_tfidf = sum(tfidf)) %>%
      ungroup() %>%
      arrange(desc(sum_tfidf)) %>%
      top_n(6, sum_tfidf)
    
    test_P <<- test_P %>%
      bind_rows(ith_class)
    
  }  
  
  #print(test_D)
  #print(test_P)
  
}