Corrected_qqplot <- function(input_pvalues,exact=1)
{
  nullprop <-null_estimation(input_pvalues)
  pnull1 <- adjust_quantile(nullprop$alpha00,nullprop$alpha01,nullprop$alpha10,
                                     nullprop$alpha1,nullprop$alpha2,input_pvalues,exact=exact)
  pmax <- apply(input_pvalues,1,max)
  correct_qqplot(pmax, pnull1)
}