find_labels_tfifd_only_token <- function(ith_doc) { #ith_doc <- 1
  
  ith_doc_id <- one_line_docs_test$doc_id[ith_doc]
  
  tfidf_ot <- tfidf_only_token[ith_doc, ]
  
  class_for_ith_doc <- one_line_data_x[which(tfidf_ot > 0), ]
  
  class_for_ith_doc <- class_for_ith_doc %>%
    mutate(tfidf = tfidf_ot[tfidf_ot > 0]) 
  
  output_diag <- class_for_ith_doc %>%
    filter(class == "DIAGNOSTICO")
  
  if (nrow(output_diag) > 0) {
    
    ith_class <- output_diag %>%
      mutate(case_id = ith_doc_id) %>% 
      rename(code = class_id) %>%
      select(case_id, code, tfidf) %>%
      group_by(case_id, code) %>%
      summarize(sum_tfidf = sum(tfidf)) %>%
      ungroup() %>%
      arrange(desc(sum_tfidf))
    
    test_D <<- test_D %>%
      bind_rows(ith_class)
    
  }  
  
  output_proc <- class_for_ith_doc %>%
    filter(class == "PROCEDIMIENTO")
  
  if (nrow(output_proc) > 0) {
    
    ith_class <- output_proc %>%
      mutate(case_id = ith_doc_id) %>% 
      rename(code = class_id) %>%
      select(case_id, code, tfidf) %>%
      group_by(case_id, code) %>%
      summarize(sum_tfidf = sum(tfidf)) %>%
      ungroup() %>%
      arrange(desc(sum_tfidf))
    
    test_P <<- test_P %>%
      bind_rows(ith_class)
    
  }  
  
  #print(test_D)
  #print(test_P)
  
}