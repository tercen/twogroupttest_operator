library(tercen)
library(dplyr)

do.twocompare = function(df, reverse, ...) {
  p_value = NaN
  result = try(t.test(.y ~ .x, data = arrange(df, .x), ...), silent = TRUE)
  if (!inherits(result, 'try-error'))
  {
    p_value = result$p.value
  }
  
  df_diff <- df %>% group_by(.x) %>% summarise(mean = mean(.y))
  
  mean_first    <- df_diff[1, 2, drop = TRUE]
  mean_second   <- df_diff[2, 2, drop = TRUE]
  
  fc   = ifelse(reverse, mean_second / mean_first, mean_first / mean_second)
  diff = ifelse(reverse, mean_second - mean_first, mean_first - mean_second)
  
  lfc = log2(fc)
  minus_log10_p = -log10(p_value)
  
  return (
    data.frame(
      .ri = df$.ri[1],
      .ci = df$.ci[1],
      p_value     = c(p_value),
      diff        = c(diff),
      fc          = c(fc),
      lfc         = c(lfc),
      minus_log10_p = c(minus_log10_p)
    )
  )
}

ctx = tercenCtx()

if (as.logical(ctx$op.value('paired')) && (length(ctx$colors) < 1))
  stop("A color factor is required for pairing.")

if (!ctx$hasXAxis)
  stop("An x-axis is required.")

ctx %>% select(.ci, .ri, .y, .x, .colorLevels) %>%
  group_by(.ci, .ri) %>%
  do(
    do.twocompare(
      .,
      reverse = as.logical(ctx$op.value('reverse')),
      paired = as.logical(ctx$op.value('paired')),
      alternative = ctx$op.value('alternative'),
      mu = as.double(ctx$op.value('mu')),
      var.equal = as.logical(ctx$op.value('var.equal')),
      conf.level = as.double(ctx$op.value('conf.level'))
    )
  ) %>%
  ctx$addNamespace() %>%
  ctx$save()
