########################################################
#' Build a topic model from scratch based on user values from an interactive console.
#' @description Creates directories inside a user determined base directory and names files with identifiable file name relevant to the selected parameters.  Using this function does not provide access to all of Mallet's parameters.
#' @export
########################################################

build_model_interactively <- function(){
  base_directory <-  readline("Enter a path to a base directory. If it does not already exist, R will create a new sub-directory titled MODEL inside this base directory. If you wish to use your current working directory, simply hit return:")
  if(base_directory == ""){
    base_directory <- getwd()
  }
  model_name <- readline("Enter a name for your model: ")
  is_dir <- dir.exists(file.path(base_directory, model_name, "MODEL"))
  if(is_dir){
    continue<- readline("A MODEL directory already exists in this location. Enter Y to continue using this directory or N to cancel: ")
    if (toupper(continue) == "N") {
      stop("You have aborted this build.")
    }
  } else {
    dir.create(file.path(base_directory, model_name))
    dir.create(file.path(base_directory, model_name, "MODEL"))
  }

  input_directory <-  readline("Enter a path to a directory of files to be modeled: ")
  num_topics <- readline("Enter the number of topics you wish to model, e.g. 20: ")
  num_iterations <- readline("Enter the number of iterations you wish to perform, e.g. 200: ")
  MALLET_PATH <- readline("Enter the path to your installation of Mallet, e.g. /Applications/Mallet-Dev/: ")
  stops <- readline("Enter/paste space seperated list of extra stopwords or leave blank to skip: ")
  output_dir <- file.path(base_directory, model_name, "MODEL")

  if(stops == ""){
    stoplist_file <- ""
    write(paste(stoplist_file, collapse = " "), file = file.path(output_dir, "stoplist_file.txt"))
  } else {
    stoplist_file <- gsub("\\s+", "\r", stops)
    write(paste(sort(unique(stoplist_file)), collapse = " "), file = file.path(output_dir, "stoplist_file.txt"))
  }

  cat(
    "Here is a summary of your selections:",
    "\nBase Directory:",
    base_directory,
    "\nFile directory: ",
    input_directory,
    "\nNumber of topics: ",
    num_topics,
    "\nNumber of iterations: ",
    num_iterations,
    "\nStop words: ",
    stops,
    "\nPath to Mallet: ",
    MALLET_PATH,
    "\nModel build location: ",
    file.path(base_directory, model_name, "MODEL")
    )
  proceed <- readline("That's it! Hit return to start the build process or type 'CANCEL' to abort the build: ")
  if(proceed == 'CANCEL') stop("Your job has been aborted.  Have a nice day:-)")
  cat("Creating instance list from files in", input_directory, "\n")

  input_training_file <- import_dir(
    MALLET_PATH,
    input_directory,
    output_dir,
    keep_sequences = TRUE,
    remove_stops = TRUE,
    stoplist_file = file.path(output_dir, "stoplist_file.txt")
  )

  cat("Building model from instance list, smoke 'em if you got 'em, this can take a while....\n")

  model_out <- train_topics(
    MALLET_PATH,
    input_training_file,
    output_dir,
    num_topics = num_topics,
    num_iterations = num_iterations,
    num_top_words = 20,
    topic_word_weights_file = file.path(output_dir, paste(num_topics, num_iterations, "word_weights_file.txt", sep = "_")),
    word_topic_counts_file = file.path(output_dir, paste(num_topics, num_iterations, "word_counts_file.txt", sep = "_")),
    num_threads = 2)

  cat("Hey, that was fun!\nLook for your new files inside: ", output_dir)

  return(model_out)
}


########################################################
#' Run MALLET import-dir function.
#' @description load the contents of a directory into mallet instances (one per file)
#' @param MALLET_PATH path to local mallet directory
#' @param import_dir path to directory of files for importing
#' @param output_dir path to output directory
#' @param keep_sequences TRUE|FALSE
#' @param remove_stops TRUE|FALSE
#' @param stoplist_file path to file with newline-separated words default = NULL
#' @export
########################################################
import_dir <- function(
  MALLET_PATH,
  import_dir,
  output_dir,
  keep_sequences = TRUE,
  remove_stops = FALSE,
  stoplist_file = NULL
  ){
  output_file_path <- file.path(output_dir, "instances.mallet")
  cmd <- paste("cd", MALLET_PATH,  "&& ")
  cmd <- paste(
    cmd,
    "bin/mallet import-dir",
    "--input",
    import_dir,
    "--output",
    output_file_path,
    "--keep-sequence",
    keep_sequences,
    "--remove-stopwords",
    remove_stops,
    "--stoplist-file",
    stoplist_file
  )
  system(cmd)
  return(output_file_path)
}

########################################################
#' Run MALLET import-file function.
#' @description load a single file into mallet instances (one per line)
#' @param MALLET_PATH path to local mallet directory
#' @param MODEL_PATH path to an existing Topic Model
#' @param input_file path to a file for import
#' @param output_file path to output file
#' @param keep_sequences TRUE|FALSE
#' @param remove_stops TRUE|FALSE
#' @param extra_stopwords path to file with whitespace-separated words default = NULL
#' @export
########################################################
import_file <- function(
  MALLET_PATH,
  MODEL_PATH,
  input_file,
  output_file,
  keep_sequences = TRUE,
  remove_stops = FALSE,
  extra_stopwords = NULL
  ){
  cmd <- paste("cd", MALLET_PATH,  "&& ")
  cmd <- paste(
    cmd,
    "bin/mallet import-file",
    "--input",
    input_file,
    "--output",
    output_file,
    "--keep-sequence",
    keep_sequences,
    "--remove-stopwords",
    remove_stops,
    "--extra-stopwords",
    extra_stopwords,
    "--use-pipe-from-without-rewrite",
    file.path(MODEL_PATH, "doc_chunks_instances")
  )
  system(cmd)
}

########################################################
#' Run MALLET train-topics function.
#' @description train a topic model from Mallet data files
#' @param MALLET_PATH path to local mallet directory
#' @param input_file path to file output from calling import_dir or import_file
#' @param output_dir path to output directory
#' @param num_topics the number of topics to collect
#' @param num_iterations The number of iterations of Gibbs sampling.
#' @param random_seed useful to make process repeatable
#' @param num_top_words the number of top words to show in console and keys file
#' @param topic_word_weights_file a file path
#' @param word_topic_counts_file a file path
#' @param num_threads number of threads to use.
#' @export
########################################################
train_topics <- function(
  MALLET_PATH,
  input_file,
  output_dir,
  num_topics,
  num_iterations = 1000,
  random_seed = 0,
  num_top_words = 20,
  topic_word_weights_file = NULL,
  word_topic_counts_file = NULL,
  num_threads = 1
  ){
  output_state_path <- paste(output_dir, paste("state", "gz", sep="."), sep="/")
  output_keys_path <- paste(output_dir, paste("keys", "txt", sep="."), sep="/")
  output_doc_topics_path <- paste(output_dir, paste("doc_topics", "txt", sep="."), sep="/")
  output_model_path <- paste(output_dir, paste("model",  "mallet", sep="."), sep="/")
  inferencer_path <- paste(output_dir, paste("model", "inferencer", sep="."), sep="/")
  parameters_path <- paste(output_dir, paste("model", "parameters", "csv", sep="."), sep="/")
  x <- tibble::tibble(num_topics = num_topics, num_iterations = num_iterations, num_top_words = num_top_words)
  readr::write_csv(x, file = parameters_path)
  topic_word_weights_path <- topic_word_weights_file
  word_topic_counts_path <- word_topic_counts_file
  cmd <- paste("cd", MALLET_PATH,  "&& ")
  cmd <- paste(
    cmd,
    "bin/mallet train-topics",
    "--input",
    input_file,
    "--num-topics",
    num_topics,
    "--output-state",
    output_state_path,
    "--output-topic-keys",
    output_keys_path,
    "--output-doc-topics",
    output_doc_topics_path,
    "--output-model",
    output_model_path,
    "--num-iterations",
    num_iterations,
    "--inferencer-filename",
    inferencer_path,
    "--random-seed",
    random_seed,
    "--num-top-words",
    num_top_words,
    "--topic-word-weights-file",
    topic_word_weights_file,
    "--word-topic-counts-file",
    word_topic_counts_file,
    "--num-threads",
    num_threads
  )
  system(cmd)
  return(
    list(
      keys = output_keys_path,
      doc_topics = output_doc_topics_path,
      state = output_state_path,
      model = output_model_path,
      inferencer = inferencer_path,
      topic_word_weights = topic_word_weights_path,
      word_topic_counts = word_topic_counts_path,
      parameters = parameters_path
    )
  )
}

########################################################
#' Run MALLET infer-topics function.
#' @description use a trained topic model to infer topics for new documents
#' @param MALLET_PATH path to local mallet directory
#' @param bookId id of document for which topics are being inferred
#' @param input_string a string of words to infer on
#' @param pipe_from_path path to instances file created during import_dir
#' @param inferencer_path path to inferencer file created during train_topics
#' @param remove_id_and_name_row Set TRUE to remove the first two rows of the output table.  These rows contain the doc ID and doc title.
#' @importFrom utils "read.table"
#' @export
########################################################
infer_topics <- function(
  MALLET_PATH,
  bookId,
  input_string,
  pipe_from_path,
  inferencer_path,
  remove_id_and_name_row = FALSE
  ){
  temp_file <- tempfile(paste(bookId, "temp_text.txt", sep = "_"))
  clean_str <- gsub("\\s+|\\r+", " ", paste(bookId, input_string, collapse = " "))
  write(clean_str, file = temp_file)
  temp_instances <- tempfile(paste(bookId, "inferred_instances.mallet", sep="_"))
  temp_topics <- tempfile(paste(bookId, "inferred_doc_topics_file.txt", sep = "_"))
  cmd <- paste("cd", MALLET_PATH,  "&& ")
  cmd <- paste(
    cmd,
    "bin/mallet import-file",
    "--input",
    temp_file,
    "--output",
    temp_instances,
    "--use-pipe-from-without-rewrite",
    pipe_from_path
  )
  system(cmd)
  cmd <- paste("cd", MALLET_PATH,  "&& ")
  cmd <- paste(
    cmd,
    "bin/mallet infer-topics",
    "--inferencer",
    inferencer_path,
    "--input",
    temp_instances,
    "--output-doc-topics",
    temp_topics
  )
  system(cmd)
  data_out <- read.table(
    temp_topics,
    header = FALSE,
    skip = 1,
    stringsAsFactors = FALSE,
    col.names = "Prop"
  )
  if(remove_id_and_name_row){
    data_out <- slice(data_out, c(-1,-2)) %>%
      mutate(Prop = as.numeric(Prop))
  }
  file.remove(temp_instances)
  file.remove(temp_topics)
  file.remove(temp_file)
  return(data_out)
}

########################################################
#' Get top documents for given topic.
#' @description use a trained topic model to infer topics for new documents
#' @param doc_topics_path location of doc_topics output from calling train_topics
#' @param topic_index The index of the target topic of interest: e.g. 1:n. Note that topics are reindexed from one to be more R-like.  e.g. Mallet topic 0 is now topic 1.
#' @param rows The number of documents to return.
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
########################################################
get_top_docs <- function(
  doc_topics_path,
  topic_index,
  rows = 5
  ){
  doc_topic_data <- readr::read_tsv(file = doc_topics_path, col_names = F)
  col_index <- topic_index + 2 # to skip the first two columns
  result <- doc_topic_data[, c(2, col_index)]
  colnames(result) <- c("Filename", "Topic")
  dplyr::arrange(result, dplyr::desc(.data$Topic)) %>%
    dplyr::top_n(rows)
}

########################################################
#' Get the topic keys (i.e. top words) for a given topic.
#' @description use a trained topic model to infer topics for new documents
#' @param topic_keys_path location of keys output from calling train_topics
#' @param topic_index The index of the target topic of interest: e.g. 1:n. Note that topics are re-indexed from 1 to n so as to be more R-like.  e.g. Mallet topic 0 is now topic 1.
#' @importFrom rlang .data
#' @export
########################################################
get_topic_keys <- function(
  topic_keys_path,
  topic_index
  ){
  topic_keys_data <- readr::read_tsv(file = topic_keys_path, col_names = F)
  result <- topic_keys_data[topic_index,]
  colnames(result) <- c("Topic", "Proportion", "Keys")
  dplyr::mutate(result, Topic = .data$Topic + 1)
}

########################################################
#' Get the topic word weights
#' @description Extract the word weights file.
#' @param word_weights_path location of topic word weights file output from calling train_topics
#' @export
########################################################
get_word_weights <- function(word_weights_path){
  word_weights <- readr::read_tsv(file = word_weights_path, col_names = c("Topic", "Word", "Weight"))
}

########################################################
#' Get the word counts per topic
#' @description Extract the word counts for each topic
#' @param word_counts_path location of topic word weights file output from calling train_topics
#' @export
########################################################
get_word_counts <- function(word_counts_path){
  word_counts <- readr::read_tsv(file = word_counts_path, col_names = F)
}

########################################################
#' Get information about instances.
#' @description A tool for printing information about instance lists of feature vectors.
#' @param MALLET_PATH path to local mallet directory
#' @param input_file Read the instance list from this file; Using - indicates stdin
#' @param print_instances Print labels and contents for all instances. TRUE|FALSE
#' @param print_infogain Print top N words by information gain, sorted. Default is 0
#' @param print_labels Print class labels known to instance list, one per line. TRUE|FALSE
#' @param print_features Print the data alphabet, one feature per line. TRUE|FALSE
#' @param print_feature_counts Print feature names, feature counts (i.e. term frequency), and feature index counts (i.e. document frequency). TRUE|FALSE
#' @param print_matrix Print word/document matrix in the specified format (a|s)(b|i)(n|w|c|e), for (all vs. sparse), (binary vs. integer), (number vs. word vs. combined vs. empty) Defaut is sic
#' @export
########################################################
get_info <- function(
  MALLET_PATH,
  input_file = "-",
  print_instances = FALSE,
  print_infogain = 0,
  print_labels = FALSE,
  print_features = FALSE,
  print_feature_counts = FALSE,
  print_matrix = "sic"
){
  cmd <- paste("cd", MALLET_PATH,  "&& ")
  cmd <- paste(
    cmd,
    "bin/mallet info",
    "--input",
    input_file,
    "--print-instances",
    print_instances,
    "--print-infogain",
    print_infogain,
    "--print-labels",
    print_labels,
    "--print-features",
    print_features,
    "--print-feature-counts",
    print_feature_counts,
    "--print-matrix",
    print_matrix
  )
  system(cmd)
}

########################################################
#' Prune vocabulary
#' @description Prune removes features based on frequency or information gain.  This is an impoverished version of the core function in Mallet that allows for more options.
#' @param input path to mallet.instances file
#' @param output path to write pruned instances file
#' @param prune_count Reduce features to those that occur more than N times.
#' @param prune_document_freq Reduce features to those that occur in more than N contexts
#' @param min_idf Remove frequent features using inverse document frequency less than this value.
#' @param max_idf Remove rare features with inverse document frequency greater than this value.
#' @return path to the resulting pruned instances file.
#' @export
########################################################
prune <- function(
  input = "-",
  output = "-",
  prune_count = 0,
  prune_document_freq = 0,
  min_idf = 0.0,
  max_idf = "Infinity"
){
  cmd <- paste("cd", MALLET_PATH,  "&& ")
  cmd <- paste(
    cmd,
    "bin/mallet prune",
    "--input",
    input,
    "--output",
    output,
    "--prune-count",
    prune_count,
    "--prune-document-freq",
    prune_document_freq,
    "--min-idf",
    min_idf,
    "--max-idf",
    max_idf
  )
  system(cmd)
  return(output)
}


# OTHER FUNCTIONS TO BE IMPLEMENTED....???

# import-svmlight    load SVMLight format data files into Mallet instances
# import_svmlight <- function(){
# }
#
# train-classifier   train a classifier from Mallet data files
# train_classifier <- function(){
# }
#
# classify-dir       classify the contents of a directory with a saved classifier
# classify_dir <- function(){
# }
#
# classify-file       data from a single file with a saved classifier
# classify_file <- function(){
# }
#
# classify-svmlight  classify data from a single file in SVMLight format
# classify_svmlight <- function(){
# }
#
# evaluate-topics    estimate the probability of new documents under a trained model
# evaluate_topics <- function(){
# }
#
# split              divide data into testing, training, and validation portions
# split <- function(){
# }
#
# bulk-load          for big input files, efficiently prune vocabulary and import docs
# bulk_load <- function(){
# }

