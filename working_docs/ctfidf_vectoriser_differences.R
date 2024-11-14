# initial data setup ----
library(tidyverse)
data <- read_rds("~/Google Drive/My Drive/data_science_project_work/microsoft/project_work/706_word_excel_pp_peaks_pits/data/topic_modelling/word/word_peaks_df_9Jan2023.rds")

bt <- reticulate::import("bertopic")
# model <- bt$BERTopic$load("~/Google Drive/My Drive/data_science_project_work/microsoft/project_work/706_word_excel_pp_peaks_pits/data/topic_modelling/word/word_peaks_model.bt")


data %>% nrow()
  # colnames()
data <- data %>% select(message, message_gpt, message_clean)

embedder <- bt_make_embedder_st("all-miniLM-L6-v2")
embeddings <- bt_do_embedding(embedder, data$message_gpt,
                              accelerator = "mps")

reducer <- bt_make_reducer_umap(metric = "euclidean", n_components = 5)
reduced_embeddings <- bt_do_reducing(reducer, embeddings)

clusterer <- bt_make_clusterer_hdbscan(cluster_selection_method = "leaf")

# BERTopic model with reduced embeddings ----
empty_embedder <- bt$backend$BaseEmbedder()
empty_reducer <- bt$dimensionality$BaseDimensionalityReduction()
model_BERT <- bt$BERTopic(embedding_model = empty_embedder,
                          umap_model = empty_reducer,
                          hdbscan_model = clusterer)
model_BERT$fit(data$message_gpt, reduced_embeddings)
model_BERT$get_topic_info() %>%
  select(-Representative_Docs) %>% DT::datatable()

# BertopicR model with reduced embeddings ----
model_bertr <- bt_compile_model(embedding_model = bt_empty_embedder(),
                                reduction_model = bt_empty_reducer(),
                                clustering_model = clusterer)
bt_fit_model(model_bertr, data$message_gpt, reduced_embeddings)
model_bertr$get_topic_info() %>%
  select(-Representative_Docs) %>% DT::datatable()

# Compare outliers ----
outliers_BERT <- model_BERT$reduce_outliers(documents = data$message_gpt,
                           topics = model_BERT$topics_,
                           threshold = 0.1)
outliers_BERT <- data.frame(current_topics = model_BERT$topics_,
             new_topics = unlist(outliers_BERT))

outliers_bertr <- bt_outliers_ctfidf(fitted_model = model_bertr,
                                     documents = data$message_gpt,
                                     topics = model_bertr$topics_,
                                     threshold = 0.1)

outliers_BERT %>%
  filter(current_topics == -1, new_topics != -1) %>% nrow()
outliers_bertr %>%
  filter(current_topics == -1, new_topics != -1) %>% nrow()

dim(model_BERT$c_tf_idf_) == dim(model_bertr$c_tf_idf_)

# these are different and so is self.c_tf_idf_ - investigate ----
docs_per_topic_BERT <- data.frame(Document = data$message_gpt,
                             Topic = model_BERT$topics_)
docs_per_topic_BERT <- docs_per_topic_BERT %>%
  group_by(Topic) %>%
  summarise(Document = paste(Document, collapse = " "))

docs_per_topic_bertr <- data.frame(Document = data$message_gpt,
                                  Topic = model_bertr$topics_)
docs_per_topic_bertr <- docs_per_topic_bertr %>%
  group_by(Topic) %>%
  summarise(Document = paste(Document, collapse = " "))


ctfidf_BERT <- reticulate::py_eval("r.model_BERT._c_tf_idf(r.docs_per_topic_BERT)")
ctfidf_bertr <- reticulate::py_eval("r.model_bertr._c_tf_idf(r.docs_per_topic_bertr)")

dim(ctfidf_BERT[[1]]) == dim(model_BERT$c_tf_idf_)
dim(ctfidf_bertr[[1]]) == dim(model_bertr$c_tf_idf_)

# Investigate why output dimensions are different ----
documents_BERT <- reticulate::py_eval("r.model_BERT._preprocess_text(r.docs_per_topic_BERT.Document.values)")

X_BERT <- model_BERT$vectorizer_model$transform(documents_BERT)

documents_bertr <- reticulate::py_eval("r.model_bertr._preprocess_text(r.docs_per_topic_bertr.Document.values)")

X_bertr <- model_bertr$vectorizer_model$transform(documents_bertr)

dim(X_BERT)
dim(X_bertr)

model_BERT$vectorizer_model
model_bertr$vectorizer_model

# what if I use the base vectoriser for bertopicr ----
sklearn <- reticulate::import("sklearn")
base_vectorisor <- sklearn$feature_extraction$text$CountVectorizer(ngram_range=reticulate::tuple(1L,1L))

model_bertr_rev2 <- bt_compile_model(embedding_model = bt_empty_embedder(),
                                reduction_model = bt_empty_reducer(),
                                clustering_model = clusterer,
                                vectoriser_model = base_vectorisor)
bt_fit_model(model_bertr_rev2, data$message_gpt, reduced_embeddings)
model_bertr_rev2$get_topic_info() %>%
  select(-Representative_Docs) %>% DT::datatable()


outliers_bertr_rev2 <- bt_outliers_ctfidf(fitted_model = model_bertr_rev2,
                                     documents = data$message_gpt,
                                     topics = model_bertr_rev2$topics_,
                                     threshold = 0.1)
outliers_bertr_rev2 %>%
  filter(current_topics == -1, new_topics != -1) %>% nrow()

dim(model_bertr_rev2$c_tf_idf_) == dim(model_BERT$c_tf_idf_)
all(model_bertr_rev2$c_tf_idf_ == model_BERT$c_tf_idf_)
# What's different about model_bertr_rev2 and model_BERT ----
docs_per_topic_BERT <- data.frame(Document = data$message_gpt,
                                  Topic = model_BERT$topics_)
docs_per_topic_BERT <- docs_per_topic_BERT %>%
  group_by(Topic) %>%
  summarise(Document = paste(Document, collapse = " "))

docs_per_topic_bertr_rev2 <- data.frame(Document = data$message_gpt,
                                   Topic = model_bertr_rev2$topics_)
docs_per_topic_bertr_rev2 <- docs_per_topic_bertr_rev2 %>%
  group_by(Topic) %>%
  summarise(Document = paste(Document, collapse = " "))


ctfidf_BERT <- reticulate::py_eval("r.model_BERT._c_tf_idf(r.docs_per_topic_BERT)")
ctfidf_bertr_rev2 <- reticulate::py_eval("r.model_bertr_rev2._c_tf_idf(r.docs_per_topic_bertr_rev2)")

dim(ctfidf_BERT[[1]]) == dim(model_BERT$c_tf_idf_)
dim(ctfidf_bertr_rev2[[1]]) == dim(model_bertr_rev2$c_tf_idf_)

model_BERT$ctfidf_model$get_params
model_BERT_rev2$ctfidf_model$

# if I make bertr and BERT with same ctfidf ----
ctfidf_rev1 <- bt_make_ctfidf(reduce_frequent_words = FALSE)
model_bertr_rev3 <- bt_compile_model(embedding_model = bt_empty_embedder(),
                                     reduction_model = bt_empty_reducer(),
                                     clustering_model = clusterer,
                                     vectoriser_model = base_vectorisor,
                                     ctfidf_model = ctfidf_rev1)
bt_fit_model(model_bertr_rev3, data$message_gpt, reduced_embeddings)
model_bertr_rev3$get_topic_info() %>%
  select(-Representative_Docs) %>% DT::datatable()


outliers_bertr_rev3<- bt_outliers_ctfidf(fitted_model = model_bertr_rev3,
                                          documents = data$message_gpt,
                                          topics = model_bertr_rev3$topics_,
                                          threshold = 0.1)
outliers_bertr_rev3 %>%
  filter(current_topics == -1, new_topics != -1) %>% nrow()

outliers_BERT %>%
  filter(current_topics == -1, new_topics != -1) %>% nrow()

docs_per_topic_BERT <- data.frame(Document = data$message_gpt,
                                  Topic = model_BERT$topics_)
docs_per_topic_BERT <- docs_per_topic_BERT %>%
  group_by(Topic) %>%
  summarise(Document = paste(Document, collapse = " "))

docs_per_topic_bertr_rev3 <- data.frame(Document = data$message_gpt,
                                        Topic = model_bertr_rev3$topics_)
docs_per_topic_bertr_rev3 <- docs_per_topic_bertr_rev3 %>%
  group_by(Topic) %>%
  summarise(Document = paste(Document, collapse = " "))


ctfidf_BERT <- reticulate::py_eval("r.model_BERT._c_tf_idf(r.docs_per_topic_BERT)")
ctfidf_bertr_rev3 <- reticulate::py_eval("r.model_bertr_rev3._c_tf_idf(r.docs_per_topic_bertr_rev3)")

dim(ctfidf_BERT[[1]]) == dim(model_BERT$c_tf_idf_)
dim(ctfidf_bertr_rev3[[1]]) == dim(model_bertr_rev3$c_tf_idf_)
dim(ctfidf_bertr_rev3[[1]]) == dim(ctfidf_BERT[[1]]) 

all(model_BERT$c_tf_idf_ == model_bertr_rev3$c_tf_idf_)
# if I make a BERTopic model with my vectoriser ----
bertr_vectoriser <- bt_make_vectoriser()
model_BERT_rev2 <- bt$BERTopic(embedding_model = empty_embedder,
                              umap_model = empty_reducer,
                              hdbscan_model = clusterer,
                              vectorizer_model = bertr_vectoriser)
model_BERT_rev2$fit(data$message_gpt, reduced_embeddings)
model_BERT_rev2$get_topic_info() %>%
  select(-Representative_Docs) %>% DT::datatable()

outliers_BERT_rev2 <- model_BERT_rev2$reduce_outliers(documents = data$message_gpt,
                                            topics = model_BERT_rev2$topics_,
                                            threshold = 0.1)
outliers_BERT_rev2 <- data.frame(current_topics = model_BERT_rev2$topics_,
                            new_topics = unlist(outliers_BERT_rev2))

outliers_BERT_rev2 %>%
  filter(current_topics == -1, new_topics != -1) %>% nrow()
