find_labels_only_token_codiesp <- function(ith_doc) { #ith_doc <- 1000
  
  ith_doc_id <- one_line_docs_test$doc_id[ith_doc]
  
  test_only_token <- str_detect(one_line_docs_test$only_token[ith_doc], 
                                one_line_codiesp_D$only_token)
  
  output_diag <- one_line_codiesp_D[which(test_only_token), ]
  
  # output_diag <- class_for_ith_doc %>%
  #   filter(class == "DIAGNOSTICO")
  
  if (nrow(output_diag) > 0) {
    
    ith_class <- output_diag %>%
      mutate(case_id = ith_doc_id) %>% 
      rename(code = doc_id) %>%
      select(case_id, code) %>%
      group_by(case_id, code) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      arrange(desc(count))
    
    test_D <<- test_D %>%
      bind_rows(ith_class)
    
  }  
  
  test_only_token <- str_detect(one_line_docs_test$only_token[ith_doc], 
                                one_line_codiesp_P$only_token)
  
  output_proc <- one_line_codiesp_P[which(test_only_token), ]
  
  if (nrow(output_proc) > 0) {
    
    ith_class <- output_proc %>%
      mutate(case_id = ith_doc_id) %>% 
      rename(code = doc_id) %>%
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