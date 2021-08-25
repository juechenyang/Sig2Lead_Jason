library(tidyverse)
file_dir = "./Precision_20targets/"
out_dir = "./outputs/"
total_df = paste(file_dir, list.files(pattern = "*.csv", path = file_dir), sep = "/") %>% map_df(~read_csv(.))
#get the real probability of precision
total_df$precision = total_df$precision/100
#checkout content for merged dataframe
head(total_df,30)
#remove  RFs, NNs, and cML100
total_df = total_df[!grepl("RF|NN|cML100", total_df$group),]
total_df = data.frame(total_df)

#import median data
median_df = read.csv("median_prec.csv", header = T)
median_df = median_df[!grepl("RF|NN|cML100", median_df$group),]
median_df$median_prec = median_df$median_prec/100

median_df_0.1_5 = median_df[median_df$size<=5, ]
median_df_0.1_1 = median_df[median_df$size<=1, ]

#remove redundant column for both df
total_df = total_df[,-1]
median_df = median_df[,-1]



kld_test_pairwise = function(v, df, precision_var){
  x = df[df$group==v[1],precision_var]
  y = df[df$group==v[2],precision_var]
  hx = hist(x, breaks = seq(0,1,0.05))
  hx$density = hx$counts/sum(hx$counts)
  hy = hist(y, breaks = seq(0,1,0.05))
  hy$density = hy$counts/sum(hy$counts)
  res.KLD = LaplacesDemon::KLD(hx$density,hy$density)
  return(c("comparison"=paste0(v[1],"-", v[2]), "KLD" = res.KLD$intrinsic.discrepancy))
}

ks_test_pairwise = function(v, df, precision_var, alternative){
  x = df[df$group==v[1],precision_var]
  y = df[df$group==v[2],precision_var]
  hx = hist(x, breaks = seq(0,1,0.05))
  hx$density = hx$counts/sum(hx$counts)
  hy = hist(y, breaks = seq(0,1,0.05))
  hy$density = hy$counts/sum(hy$counts)
  res.ks = ks.test(hx$density, hy$density, alternative = alternative)
  return(c("comparison"=paste0(v[1],"-", v[2]), res.ks$statistic, "p.value"=res.ks$p.value))
}

lib_size_repo = unique(total_df$size)
for(i in seq(1, length(lib_size_repo))){
  sub_total_df = total_df[total_df$size==lib_size_repo[i], ]
  combinations = combn(unique(sub_total_df$group),2)
  res.ks.pw = invisible(apply(combinations, 2, kld_test_pairwise, sub_total_df, "precision"))
  res.ks.pw = data.frame(res.ks.pw)
  names(res.ks.pw) = res.ks.pw[1,]
  res.ks.pw = t(res.ks.pw[-1,])
  write.csv(res.ks.pw, paste0(out_dir, "full_kld_test_result_normalized", "-", lib_size_repo[i], ".csv"))
}