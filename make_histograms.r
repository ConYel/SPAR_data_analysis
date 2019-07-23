library(tidyverse)
setwd("/home")
files_hist <- dir(full.names = TRUE,
            recursive = TRUE,
            pattern = 'mature_seqs_all.xls')

#pdf_res <- str_remove(files_hist,"mature_seqs_all.xls")
#read the matrix
seq_mat <- 
  sapply(files_hist, function(x)
  read_tsv(x,
                    col_types = cols_only(
                      peakID = col_character(),
                      Length = col_integer(),
                      Sequence = col_character()
                    )) %>%
  arrange(Length,Sequence) %>% 
  distinct(Sequence,.keep_all = TRUE) %>% 
  ggplot(aes(x = Length )) +
  geom_histogram(
    binwidth = 0.5,
    color = "red",
    fill = "red",
    alpha = .2) +
  theme_minimal() +
  labs(title="Mature Reads Length Distribution",
       y = "sequences count")+
  scale_x_continuous(limits = c(13,45), breaks = seq(13,45,by = 1)) + 
  ggsave(filename = "mature_read_length_distribution.pdf",
         device = "pdf",
         dpi = "retina",
         path = str_replace(x,"/results/mature_seqs_all.xls","/figures"))
  )
