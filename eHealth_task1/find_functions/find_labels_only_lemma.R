find_labels_only_lemma <- function(ith_doc) { #ith_doc <- 1
  
  ith_doc_id <- one_line_docs_test$doc_id[ith_doc]
  
  test_only_lemma <- str_detect(one_line_docs_test$only_lemma[ith_doc], 
                                one_line_data_x$one_line_lemma)
  
  class_for_ith_doc <- one_line_data_x[which(test_only_lemma), ]
  
  output_diag <- class_for_ith_doc %>%
    filter(class == "DIAGNOSTICO")
  
  if (nrow(output_diag) > 0) {
    
    ith_class <- output_diag %>%
      mutate(case_id = ith_doc_id) %>% rename(code = class_id) %>%
      select(case_id, code) %>%
      group_by(case_id, code) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      arrange(desc(count))
    
    test_D <<- test_D %>%
      bind_rows(ith_class)
    
  }  
  
  output_proc <- class_for_ith_doc %>%
    filter(class == "PROCEDIMIENTO")
  
  if (nrow(output_proc) > 0) {
    
    ith_class <- output_proc %>%
      mutate(case_id = ith_doc_id) %>% rename(code = class_id) %>%
      select(case_id, code) %>%
      group_by(case_id, code) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      arrange(desc(count))
    
    test_P <<- test_P %>%
      bind_rows(ith_class)
    
  }  

  #print(test_D)
  #print(test_P)
  
}