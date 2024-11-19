#' Pivot Summary Files
#'
#' This function reads summary files from a specified directory, processes the data, and returns a combined data frame with pivoted data.
#'
#' @param base_dir A string specifying the base directory containing the 'qc_checks' folder.
#' @return A data frame with pivoted summary data.
#' @examples
#' result <- pivot_summary_files("/path/to/your/folder/")
#' #df = pivot_summary_files('~/Library/CloudStorage/OneDrive-Illumina,Inc./Documents/Applied Bioinformatics/modules/next_generation_sequencing_informatics/assignment/')


pivot_summary_files = function(base_dir) {
  qc_filepath = base_dir
  dir_names = list.dirs(path = qc_filepath, recursive = FALSE)
  all_summary = list()
  sample_names = c()
  
  for (dir in dir_names) {
    file_path = file.path(dir, 'summary.txt')
    if (file.exists(file_path)) {
      data = read.table(
        file_path, header = FALSE,
        sep = '\t',
        col.names = c('flag', 'metric', 'sample'))
      sample_name = unique(data$sample)
      sample_name = gsub('_[12].fq.gz', '', sample_name)
      sample_names = append(sample_names, sample_name)
      all_summary = append(all_summary, list(data))
    } else {
      warning(paste('File not found:', file_path))
    }
  }
  
  names(all_summary) = sample_names
  pivoted_list = list()
  
  for (sample_summary in all_summary) {
    pvt = pivot_wider(sample_summary, names_from = metric, values_from = flag)
    pivoted_list = append(pivoted_list, list(pvt))
  }
  
  df = bind_rows(pivoted_list)
  return(df)
}




                    
                    
                    
                    