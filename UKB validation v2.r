# ============================================================================
# 0. Configuration
# ============================================================================
CFG <- list(
  seed        = 2026,        # identical to GDPH
  # collapse these multi-level variables to 0 = reference level ("never") vs 1 = any other level ("ever")
  binarize    = list(Smoking = "0", Drinking = "0"),
  files       = c(HC   = "Z121_only_UKB_controls_for_R.xlsx",
                  CVNP = "I6_withoutG_controls_for_R.xlsx",
                  VaP  = "67 UKB patients for R.xlsx"),
  out_dir     = "Output_UKB",
  fig_dir     = "Figures_UKB",
  prefix      = "UKB_",       # prefix of figure file names
  run_mdr     = TRUE,         # same as GDPH (Figure 4)
  # force the reference (0) / alternative (1) level of a binary variable, e.g.
  # coding_override = list(Sex = list(ref = "1", alt = "0"))  # if 1 = Male, 0 = Female
  coding_override = list(),
  cran_mirror = "https://mirrors.ustc.edu.cn/CRAN/",
  alpha       = 0.05,
  fdr_method  = "BH",
  # missing data
  max_missing_impute = 0.20,  # impute within a group only if <= 20 % missing
  struct_missing     = 0.50,  # variable unusable for a comparison if >= 50 % missing in any group
  # which variables are drawn in the figures: "fdr" | "p" | "all"
  select_box    = "fdr",
  select_forest = "p",
  # regression
  min_n_logit = 20,
  min_epv     = 10,
  # mediation
  med_boot  = TRUE,           # as GDPH; FALSE = quasi-Bayesian (much faster)
  med_sims  = 5000,           # simulations per mediation model (slow; scales with n_cores)
  med_min_n = 30,
  med_top   = 8,              # max rows per panel in the mediation figure
  n_cores   = max(1L, parallel::detectCores() - 1L),
  # metadeconfoundR
  mdr_q = 0.1, mdr_adjust_level = 1,
  # figures (mm, 8-pt type; double column = 180 mm)
  font = "sans", font_size = 7, fig_width = 180, box_ncol = 4, box_panel_h = 52
)

# ---- packages --------------------------------------------------------------
ensure_pkgs <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      try(install.packages(pkg, repos = CFG$cran_mirror), silent = TRUE)
    }
    if (!requireNamespace(pkg, quietly = TRUE) && pkg == "metadeconfoundR") {
      if (!requireNamespace("remotes", quietly = TRUE))
        install.packages("remotes", repos = CFG$cran_mirror)
      remotes::install_github("TillBirkner/metadeconfoundR")
    }
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Package '", pkg, "' could not be installed.", call. = FALSE)
  }
}
ensure_pkgs(c("openxlsx", "dplyr", "ggplot2", "patchwork", "rstatix",
              "car", "mediation", "metadeconfoundR"))
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(patchwork)
})

RNGkind("L'Ecuyer-CMRG"); set.seed(CFG$seed)
options(warn = 1)
for (d in c(CFG$out_dir, CFG$fig_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

NOTES <- character()
note <- function(...) {
  m <- paste0(...)
  NOTES <<- c(NOTES, m)
  message("[note] ", m)
}

# ============================================================================
# 1. Palette, themes, figure export
# ============================================================================
# Okabe-Ito colour-blind-safe palette
PAL <- c("HC" = "#56B4E9", "CVNP" = "#E69F00", "VaP" = "#D55E00",
         "VaP-1" = "#009E73", "VaP-2" = "#CC79A7", "VaP-3" = "#0072B2")
get_cols <- function(grps) {
  out <- PAL[grps]
  if (anyNA(out)) {
    fb <- c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00")
    out[is.na(out)] <- fb[seq_len(sum(is.na(out)))]
    names(out) <- grps
  }
  out
}

theme_pub <- function(base = CFG$font_size) {
  theme_classic(base_size = base, base_family = CFG$font) +
    theme(
      axis.text        = element_text(colour = "black"),
      axis.title       = element_text(colour = "black"),
      axis.line        = element_line(linewidth = 0.3),
      axis.ticks       = element_line(linewidth = 0.3),
      plot.title       = element_text(face = "bold", hjust = 0.5, size = base + 1),
      plot.subtitle    = element_text(hjust = 0.5, size = base - 1, colour = "grey25"),
      plot.tag         = element_text(face = "bold", size = base + 3),
      plot.caption     = element_text(hjust = 0, size = base - 1.5, colour = "grey25"),
      legend.key.size  = unit(3, "mm"),
      legend.text      = element_text(size = base - 1),
      legend.title     = element_text(size = base - 1),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", size = base),
      panel.grid       = element_blank()
    )
}

# vector PDF (editable text) + 600 dpi LZW TIFF; sizes in mm
save_fig <- function(p, stem, width = CFG$fig_width, height) {
  if (is.null(p)) return(invisible(NULL))
  if (height > 240) note(basename(stem), ": height ", round(height),
                         " mm exceeds a typical 240 mm page; consider select_* = \"fdr\".")
  f <- file.path(CFG$fig_dir, paste0(CFG$prefix, stem))
  pdf_dev <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else grDevices::pdf
  ggsave(paste0(f, ".pdf"), p, device = pdf_dev, width = width, height = height,
         units = "mm", bg = "white", limitsize = FALSE)
  ggsave(paste0(f, ".tiff"), p, device = "tiff", dpi = 600, width = width,
         height = height, units = "mm", bg = "white", compression = "lzw",
         limitsize = FALSE)
  message("[OK] ", f, ".pdf / .tiff  (", width, " x ", round(height), " mm)")
  invisible(f)
}

wrap_cap <- function(x, w = 150) paste(strwrap(x, width = w), collapse = "\n")
cap_theme <- theme(plot.caption = element_text(hjust = 0, size = CFG$font_size - 1.5,
                                               colour = "grey25", family = CFG$font))

make_tags <- function(n) head(c(LETTERS, paste0("A", LETTERS), paste0("B", LETTERS)), n)

# ============================================================================
# 2. Generic helpers
# ============================================================================
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
fmt_p <- function(p, digits = 3) {
  ifelse(is.na(p), "NA",
         ifelse(p < 10^-digits, paste0("<", format(10^-digits, scientific = FALSE)),
                formatC(p, digits = digits, format = "f")))
}
pretty_var <- function(v) gsub("\\.", " ", v)

to_num <- function(x, v = "") {
  if (is.numeric(x)) return(x)
  z <- suppressWarnings(as.numeric(as.character(x)))
  bad <- sum(is.na(z) & !is.na(x))
  if (bad > 0) note(bad, " non-numeric entries in '", v, "' were set to NA.")
  z
}

read_clean <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  df <- tryCatch(openxlsx::read.xlsx(path, rowNames = TRUE, colNames = TRUE),
                 error = function(e) openxlsx::read.xlsx(path, colNames = TRUE))
  names(df) <- trimws(names(df))
  df[, !names(df) %in% c("row.names", "Row.names", "X1", "X"), drop = FALSE]
}

# ---- binary / categorical coding (deterministic, independent of row order) ----
make_coding <- function(x, override = NULL) {
  z <- trimws(as.character(x))
  lev <- sort(unique(z[!is.na(z) & z != ""]))
  if (!is.null(override)) {
    if (length(lev) == 2 && all(c(override$ref, override$alt) %in% lev))
      return(list(type = "binary", ref = override$ref, alt = override$alt))
    note("coding_override ignored (levels found: ", paste(lev, collapse = ", "), ").")
  }
  if (length(lev) < 2) return(list(type = "constant", levels = lev))
  if (length(lev) > 2) return(list(type = "multi", levels = lev))
  neg <- c("0", "No", "NO", "no", "N", "Male", "male", "M", "False", "FALSE")
  pos <- c("1", "Yes", "YES", "yes", "Y", "Female", "female", "F", "True", "TRUE")
  num <- suppressWarnings(as.numeric(lev))
  ref <- if (!anyNA(num)) lev[which.min(num)]
         else if (lev[1] %in% neg || lev[2] %in% pos) lev[1]
         else if (lev[1] %in% pos || lev[2] %in% neg) lev[2]
         else lev[1]
  list(type = "binary", ref = ref, alt = setdiff(lev, ref))
}
apply_coding <- function(x, cd) {
  z <- trimws(as.character(x)); z[z == ""] <- NA
  if (cd$type == "binary") return(ifelse(is.na(z), NA_integer_, as.integer(z == cd$alt)))
  factor(z, levels = cd$levels)
}

# ---- marker transformation: log if right-skewed (skew > 1, all > 0), then z ----
make_tf <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NULL)
  sk <- mean((x - mean(x))^3) / stats::sd(x)^3
  lg <- is.finite(sk) && sk > 1 && min(x) > 0
  y <- if (lg) log(x) else x
  list(log = lg, mu = mean(y), sd = stats::sd(y))
}
apply_tf <- function(x, tf) {
  if (is.null(tf) || !is.finite(tf$sd) || tf$sd == 0) return(x)
  ((if (tf$log) log(x) else x) - tf$mu) / tf$sd
}

# ---- missing data ----------------------------------------------------------
missing_report <- function(df, vars, group = "Group") {
  out <- list()
  for (v in intersect(vars, names(df))) for (g in levels(df[[group]])) {
    x <- df[[v]][df[[group]] == g]
    out[[length(out) + 1]] <- data.frame(
      Variable = v, Group = g, N = length(x), N_missing = sum(is.na(x)),
      Pct_missing = round(100 * mean(is.na(x)), 1), stringsAsFactors = FALSE)
  }
  dplyr::bind_rows(out)
}

# median (continuous) / mode (categorical) imputation WITHIN group; never for
# groups where the variable is (almost) entirely absent
impute_by_group <- function(df, cont_vars, cat_vars, group = "Group") {
  log <- list()
  for (v in c(cont_vars, cat_vars)) {
    if (!v %in% names(df)) next
    is_cont <- v %in% cont_vars
    for (g in levels(df[[group]])) {
      idx  <- which(df[[group]] == g)
      x    <- df[[v]][idx]
      miss <- is.na(x)
      if (!any(miss)) next
      frac <- mean(miss)
      action <- if (all(miss)) "not imputed (absent in group)"
                else if (frac > CFG$max_missing_impute)
                  sprintf("not imputed (>%.0f%% missing)", 100 * CFG$max_missing_impute)
                else "imputed"
      val <- NA_character_
      if (action == "imputed") {
        if (is_cont) {
          m <- stats::median(x, na.rm = TRUE); x[miss] <- m; val <- sprintf("%.4f", m)
        } else {
          tb <- table(as.character(x)); m <- names(tb)[which.max(tb)]
          x <- as.character(x); x[miss] <- m; val <- m
        }
        df[[v]][idx] <- x
      }
      log[[length(log) + 1]] <- data.frame(
        Variable = v, Group = g, N_missing = sum(miss),
        Pct_missing = round(100 * frac, 1), Action = action, Value = val,
        stringsAsFactors = FALSE)
    }
  }
  list(data = df, log = dplyr::bind_rows(log))
}

# variables usable for a given comparison: observed in every group present and
# varying within the analysed sample
drop_structural <- function(d, vars, label = "", group_col = "Group") {
  vars <- intersect(vars, names(d))
  keep <- vapply(vars, function(v) {
    fr <- tapply(is.na(d[[v]]), droplevels(factor(d[[group_col]])), mean)
    ok <- all(fr < CFG$struct_missing)
    ok && length(unique(d[[v]][!is.na(d[[v]])])) >= 2
  }, logical(1))
  if (any(!keep))
    note(label, ": excluded (missing in a group or constant): ",
         paste(vars[!keep], collapse = ", "))
  vars[keep]
}

# ---- multiplicity: BH within variable family (and within `by`) --------------
FAMILIES <- list(
  demographic  = c("Age", "BMI", "Sex", "Smoking", "Drinking", "Hypertension",
                   "Diabetes", "Medications"),
  imaging      = c("PVWM", "DWM", "Fazekas.Score", "NIHSS.Score",
                   "Fazekas.Classification"),
  inflam_cell  = c("Leucocyte", "Neutrophil", "Lymphocyte", "Monocyte",
                   "Platelet", "Eosinophil", "Basophil", "NLR", "PLR", "MLR",
                   "SII", "SIRI", "AISI"),
  inflam_lipid = c("Total.cholesterol", "Triglyceride", "HDL", "LDL", "AIP",
                   "RC", "MHR")
)
# dedupe = TRUE: rows sharing the same variable (e.g. levels of one categorical
# variable) share one p-value and are counted once
adjust_fdr_family <- function(df, p_col = "P_value", var_col = "Variable",
                              by = NULL, dedupe = TRUE) {
  if (is.null(df) || nrow(df) == 0) return(df)
  fam_of <- function(v) {
    f <- names(FAMILIES)[vapply(FAMILIES, function(z) v %in% z, logical(1))]
    if (length(f)) f[1] else "other"
  }
  df$Family <- vapply(as.character(df[[var_col]]), fam_of, character(1))
  grp <- if (is.null(by)) rep("all", nrow(df)) else
    as.character(interaction(df[by], drop = TRUE))
  key  <- paste(grp, df$Family, sep = "|")
  full <- paste(key, df[[var_col]], sep = "|")
  first <- !is.na(df[[p_col]])
  if (dedupe) first <- first & !duplicated(full)
  adj <- rep(NA_real_, nrow(df))
  for (k in unique(key)) {
    idx <- which(key == k & first)
    if (length(idx)) adj[idx] <- stats::p.adjust(df[[p_col]][idx], CFG$fdr_method)
  }
  if (dedupe) {
    src <- which(first)
    adj <- adj[src][match(full, full[src])]
  }
  df$P_FDR <- adj
  df
}

# ============================================================================
# 3. Group comparison (Table 1) with matching post-hoc tests
# ============================================================================
compare_cont <- function(d) {                 # d: data.frame(y, g)
  out <- list(p = NA_real_, method = NA_character_, stat = NA_real_,
              parametric = FALSE, posthoc = NULL)
  d <- d[!is.na(d$y) & !is.na(d$g), , drop = FALSE]
  d$g <- droplevels(factor(d$g))
  tab <- table(d$g)
  d <- d[d$g %in% names(tab)[tab >= 3], , drop = FALSE]
  d$g <- droplevels(d$g)
  k <- nlevels(d$g)
  if (k < 2) return(out)
  shap <- vapply(levels(d$g), function(l) {
    tryCatch(stats::shapiro.test(d$y[d$g == l])$p.value, error = function(e) NA_real_)
  }, numeric(1))
  normal <- all(shap > 0.05, na.rm = TRUE)
  lev_p  <- tryCatch(car::leveneTest(y ~ g, data = d)$`Pr(>F)`[1],
                     error = function(e) NA_real_)
  eq_var <- !is.na(lev_p) && lev_p > 0.05
  if (k == 2) {
    if (normal) {
      r <- stats::t.test(y ~ g, data = d, var.equal = eq_var)
      return(modifyList(out, list(p = r$p.value, stat = unname(r$statistic),
                                  parametric = TRUE,
                                  method = if (eq_var) "Student t-test" else "Welch t-test")))
    }
    r <- stats::wilcox.test(y ~ g, data = d, exact = FALSE)
    return(modifyList(out, list(p = r$p.value, stat = unname(r$statistic),
                                method = "Mann-Whitney U")))
  }
  ph_fun <- NULL
  if (normal && eq_var) {
    s <- summary(stats::aov(y ~ g, data = d))[[1]]
    out <- modifyList(out, list(p = s[["Pr(>F)"]][1], stat = s[["F value"]][1],
                                parametric = TRUE, method = "One-way ANOVA"))
    ph_fun <- function() rstatix::tukey_hsd(d, y ~ g)
  } else if (normal) {
    r <- stats::oneway.test(y ~ g, data = d, var.equal = FALSE)
    out <- modifyList(out, list(p = r$p.value, stat = unname(r$statistic),
                                parametric = TRUE, method = "Welch ANOVA"))
    ph_fun <- function() rstatix::games_howell_test(d, y ~ g)
  } else {
    r <- stats::kruskal.test(y ~ g, data = d)
    out <- modifyList(out, list(p = r$p.value, stat = unname(r$statistic),
                                method = "Kruskal-Wallis"))
    ph_fun <- function() rstatix::dunn_test(d, y ~ g, p.adjust.method = "BH")
  }
  ph <- tryCatch(as.data.frame(ph_fun()), error = function(e) NULL)
  if (!is.null(ph) && all(c("group1", "group2", "p.adj") %in% names(ph)))
    out$posthoc <- ph[, c("group1", "group2", "p.adj")]
  out
}

compare_cat <- function(x, g) {
  d <- data.frame(x = factor(x), g = droplevels(factor(g)))
  d <- d[stats::complete.cases(d), , drop = FALSE]
  tab <- table(d$x, d$g)
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (nrow(tab) < 2 || ncol(tab) < 2) return(NULL)
  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  if (any(chi$expected < 5)) {
    p <- tryCatch(stats::fisher.test(tab)$p.value,
                  error = function(e) stats::fisher.test(tab, simulate.p.value = TRUE,
                                                          B = 1e5)$p.value)
    return(list(p = p, method = "Fisher exact", stat = NA_real_, tab = tab))
  }
  list(p = chi$p.value, method = "Chi-square", stat = unname(chi$statistic), tab = tab)
}

fmt_meansd <- function(x) sprintf("%.2f (%.2f)", mean(x), stats::sd(x))
fmt_iqr    <- function(x) {
  q <- stats::quantile(x, c(.25, .5, .75), names = FALSE)
  sprintf("%.2f (%.2f-%.2f)", q[2], q[1], q[3])
}

build_table1 <- function(df, cont_vars, cat_vars, group = "Group") {
  df <- df[!is.na(df[[group]]), , drop = FALSE]
  df[[group]] <- droplevels(factor(df[[group]]))
  glev <- levels(df[[group]])
  rows <- list(); post <- list()
  for (v in intersect(cont_vars, names(df))) {
    d <- data.frame(y = to_num(df[[v]], v), g = df[[group]])
    d <- d[!is.na(d$y), , drop = FALSE]
    if (nrow(d) == 0) next
    cmp <- compare_cont(d)
    f <- if (cmp$parametric) fmt_meansd else fmt_iqr
    r <- data.frame(Variable = v, Base_variable = v, Type = "Continuous",
                    Summary = if (cmp$parametric) "Mean (SD)" else "Median (IQR)",
                    stringsAsFactors = FALSE)
    for (g in glev) {
      x <- d$y[d$g == g]
      r[[g]] <- if (length(x)) f(x) else NA_character_
    }
    r$N_total <- nrow(d); r$Method <- cmp$method
    r$Statistic <- cmp$stat; r$P_value <- cmp$p
    rows[[length(rows) + 1]] <- r
    if (!is.null(cmp$posthoc)) post[[v]] <- cmp$posthoc
  }
  for (v in intersect(cat_vars, names(df))) {
    cmp <- compare_cat(df[[v]], df[[group]])
    if (is.null(cmp)) next
    tab <- cmp$tab
    for (lv in rownames(tab)) {
      r <- data.frame(Variable = paste0(v, " = ", lv), Base_variable = v,
                      Type = "Categorical", Summary = "n (%)",
                      stringsAsFactors = FALSE)
      for (g in glev) {
        r[[g]] <- if (g %in% colnames(tab))
          sprintf("%d (%.1f%%)", tab[lv, g], 100 * tab[lv, g] / sum(tab[, g]))
        else NA_character_
      }
      r$N_total <- sum(tab); r$Method <- cmp$method
      r$Statistic <- cmp$stat; r$P_value <- cmp$p
      rows[[length(rows) + 1]] <- r
    }
  }
  if (!length(rows)) return(data.frame())
  out <- dplyr::bind_rows(rows)
  out <- adjust_fdr_family(out, "P_value", "Base_variable", dedupe = TRUE)
  attr(out, "posthoc") <- post
  out
}

# pairwise categorical comparisons (Fisher / chi-square, BH within variable)
pairwise_cat <- function(df, vars, group = "Group") {
  out <- list()
  g_all <- levels(droplevels(factor(df[[group]])))
  if (length(g_all) < 2) return(data.frame())
  for (v in intersect(vars, names(df))) {
    res <- list()
    for (p in utils::combn(g_all, 2, simplify = FALSE)) {
      s <- df[df[[group]] %in% p, , drop = FALSE]
      cmp <- compare_cat(s[[v]], s[[group]])
      if (is.null(cmp)) next
      res[[length(res) + 1]] <- data.frame(Variable = v, group1 = p[1], group2 = p[2],
                                           Method = cmp$method, p = cmp$p,
                                           stringsAsFactors = FALSE)
    }
    if (length(res)) {
      r <- dplyr::bind_rows(res); r$p.adj <- stats::p.adjust(r$p, "BH")
      out[[v]] <- r
    }
  }
  dplyr::bind_rows(out)
}

# ============================================================================
# 4. Regression helpers
# ============================================================================
fit_logit <- function(d, outcome, predictor, covs, label) {
  covs <- setdiff(covs, predictor)
  need <- unique(c(outcome, predictor, covs))
  if (!all(need %in% names(d))) return(NULL)
  d <- d[stats::complete.cases(d[, need, drop = FALSE]), need, drop = FALSE]
  if (nrow(d) < CFG$min_n_logit) return(NULL)
  d[[outcome]] <- as.integer(d[[outcome]])
  if (length(unique(d[[outcome]])) < 2 || length(unique(d[[predictor]])) < 2) return(NULL)
  covs <- covs[vapply(covs, function(v) length(unique(d[[v]])) >= 2, logical(1))]
  fml  <- stats::as.formula(paste(outcome, "~", paste(c(predictor, covs), collapse = " + ")))
  warn <- character()
  fit <- tryCatch(
    withCallingHandlers(stats::glm(fml, data = d, family = stats::binomial()),
                        warning = function(w) {
                          warn <<- c(warn, conditionMessage(w))
                          invokeRestart("muffleWarning")
                        }),
    error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  cf <- summary(fit)$coefficients
  if (!predictor %in% rownames(cf)) return(NULL)
  b <- cf[predictor, 1]; se <- cf[predictor, 2]
  n1 <- sum(d[[outcome]] == 1); n0 <- sum(d[[outcome]] == 0)
  epv <- min(n1, n0) / max(length(stats::coef(fit)) - 1, 1)
  flag <- character()
  if (!isTRUE(fit$converged)) flag <- c(flag, "non-convergence")
  if (any(grepl("numerically 0 or 1", warn))) flag <- c(flag, "separation")
  if (!is.finite(se) || se > 10) flag <- c(flag, "unstable SE")
  if (epv < CFG$min_epv) flag <- c(flag, sprintf("low EPV (%.1f)", epv))
  data.frame(Variable = predictor, Outcome = label,
             OR = exp(b), OR_lo = exp(b - stats::qnorm(0.975) * se),
             OR_hi = exp(b + stats::qnorm(0.975) * se),
             P_value = cf[predictor, 4], N = nrow(d), N_events = n1,
             EPV = round(epv, 1), Flag = paste(flag, collapse = "; "),
             stringsAsFactors = FALSE)
}

# primary model on (group-wise) imputed data + complete-case sensitivity
run_logit_set <- function(d_imp, d_obs, outcome, predictors, covs, label) {
  predictors <- drop_structural(d_imp, predictors, label)
  covs       <- drop_structural(d_imp, covs, label)
  res <- lapply(predictors, function(v) {
    r <- fit_logit(d_imp, outcome, v, covs, label)
    if (is.null(r)) return(NULL)
    rc <- fit_logit(d_obs, outcome, v, covs, label)
    r$OR_cc <- if (is.null(rc)) NA_real_ else rc$OR
    r$P_cc  <- if (is.null(rc)) NA_real_ else rc$P_value
    r$N_cc  <- if (is.null(rc)) NA_integer_ else rc$N
    r
  })
  res <- res[!vapply(res, is.null, logical(1))]
  if (!length(res)) return(data.frame())
  out <- adjust_fdr_family(dplyr::bind_rows(res), "P_value", "Variable")
  out[order(out$P_value), , drop = FALSE]
}

pair_data <- function(df, g_ref, g_alt, group_col = "Group") {
  d <- df[df[[group_col]] %in% c(g_ref, g_alt), , drop = FALSE]
  d$Group   <- factor(as.character(d[[group_col]]), levels = c(g_ref, g_alt))
  d$Outcome <- as.integer(d$Group == g_alt)
  d
}

# ============================================================================
# 5. metadeconfoundR
# ============================================================================
make_meta <- function(d, outcome, covs) {
  meta <- data.frame(Outcome = as.numeric(d[[outcome]]))
  for (v in covs) {
    x <- d[[v]]
    if (is.numeric(x)) meta[[v]] <- x
    else {
      f <- factor(x)
      for (lv in levels(f)[-1]) meta[[paste0(v, "_", make.names(lv))]] <- as.numeric(f == lv)
    }
  }
  meta
}

run_mdr <- function(d, outcome, markers, covs, label) {
  markers <- drop_structural(d, markers, label)
  covs    <- drop_structural(d, covs, label)
  if (length(markers) < 2) return(NULL)
  meta <- make_meta(d, outcome, covs)
  feat <- d[, markers, drop = FALSE]
  ok   <- stats::complete.cases(feat) & stats::complete.cases(meta)
  feat <- as.matrix(feat[ok, , drop = FALSE]); storage.mode(feat) <- "double"
  meta <- meta[ok, , drop = FALSE]
  meta <- meta[, vapply(meta, function(x) length(unique(x)) >= 2, logical(1)), drop = FALSE]
  if (nrow(feat) < 20 || !"Outcome" %in% names(meta)) return(NULL)
  ids <- paste0("S", seq_len(nrow(feat)))
  rownames(feat) <- ids; rownames(meta) <- ids
  res <- tryCatch(
    metadeconfoundR::MetaDeconfound(
      featureMat = feat, metaMat = meta, nnodes = 1, adjustMethod = "fdr",
      adjustLevel = CFG$mdr_adjust_level, QCutoff = CFG$mdr_q, DCutoff = 0,
      logLevel = "ERROR", returnLong = TRUE),
    error = function(e) { note(label, ": metadeconfoundR failed - ", conditionMessage(e)); NULL })
  if (is.null(res)) return(NULL)
  res <- as.data.frame(res)
  res$Comparison <- label
  res
}

# ============================================================================
# 6. Mediation (self-contained worker -> safe for parallel back-ends)
# ============================================================================
med_worker <- function(tk) {
  dd <- tk$data
  rhs_m <- paste(c(tk$expo, tk$covs), collapse = " + ")
  rhs_o <- paste(c(tk$expo, tk$med, tk$covs), collapse = " + ")
  f_med <- stats::as.formula(paste(tk$med, "~", rhs_m))
  f_out <- stats::as.formula(paste(tk$outcome, "~", rhs_o))
  tryCatch({
    set.seed(tk$seed)
    # models are built through do.call so the data are inlined in the call
    # (mediate() re-evaluates the call; avoids scoping errors)
    mfit <- do.call("lm",  list(formula = f_med, data = dd))
    ofit <- suppressWarnings(do.call("glm", list(formula = f_out, data = dd,
                                                 family = stats::binomial())))
    mo <- mediation::mediate(mfit, ofit, treat = tk$expo, mediator = tk$med,
                             control.value = tk$ctrl, treat.value = tk$trt,
                             boot = tk$boot, sims = tk$sims, boot.ci.type = "perc")
    s <- suppressWarnings(summary(mo))
    data.frame(Comparison = tk$comparison, Exposure = tk$expo, Mediator = tk$med,
               N = nrow(dd), Contrast = sprintf("%.3g -> %.3g", tk$ctrl, tk$trt),
               ACME = s$d.avg, ACME_lo = s$d.avg.ci[1], ACME_hi = s$d.avg.ci[2], ACME_p = s$d.avg.p,
               ADE = s$z.avg, ADE_lo = s$z.avg.ci[1], ADE_hi = s$z.avg.ci[2], ADE_p = s$z.avg.p,
               Total = s$tau.coef, Total_lo = s$tau.ci[1], Total_hi = s$tau.ci[2], Total_p = s$tau.p,
               PropMed = s$n.avg, PropMed_lo = s$n.avg.ci[1], PropMed_hi = s$n.avg.ci[2],
               stringsAsFactors = FALSE)
  }, error = function(e) paste0(tk$expo, " -> ", tk$med, ": ", conditionMessage(e)))
}

par_map <- function(X, FUN) {
  nc <- min(CFG$n_cores, length(X))
  if (nc <= 1) return(lapply(X, FUN))
  if (.Platform$OS.type == "windows") {
    cl <- parallel::makeCluster(nc)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::parLapplyLB(cl, X, FUN)
  } else {
    parallel::mclapply(X, FUN, mc.cores = nc, mc.preschedule = FALSE)
  }
}

build_med_tasks <- function(d, outcome, comparison, exposures, mediators, covs) {
  tasks <- list()
  for (ex in exposures) for (md in mediators) {
    if (ex == md) next
    cv   <- setdiff(covs, ex)
    need <- unique(c(outcome, ex, md, cv))
    dd   <- d[stats::complete.cases(d[, need, drop = FALSE]), need, drop = FALSE]
    if (nrow(dd) < CFG$med_min_n || !is.numeric(dd[[ex]])) next
    if (any(vapply(c(outcome, ex, md), function(v) length(unique(dd[[v]])) < 2, logical(1)))) next
    cv <- cv[vapply(cv, function(v) length(unique(dd[[v]])) >= 2, logical(1))]
    if (length(unique(dd[[ex]])) == 2) {
      ctrl <- min(dd[[ex]]); trt <- max(dd[[ex]])
    } else {                                   # continuous exposure: Q1 -> Q3
      q <- unname(stats::quantile(dd[[ex]], c(.25, .75)))
      ctrl <- q[1]; trt <- q[2]
      if (ctrl == trt) next
    }
    tasks[[length(tasks) + 1]] <- list(
      data = dd, expo = ex, med = md, outcome = outcome, covs = cv, ctrl = ctrl,
      trt = trt, boot = CFG$med_boot, sims = CFG$med_sims,
      comparison = comparison, seed = CFG$seed + length(tasks) + 1L)
  }
  tasks
}

run_mediation <- function(tasks) {
  if (!length(tasks)) return(data.frame())
  res <- par_map(tasks, med_worker)
  err <- res[vapply(res, is.character, logical(1))]
  if (length(err)) note(length(err), " mediation model(s) failed, e.g. ", err[[1]])
  ok <- res[vapply(res, is.data.frame, logical(1))]
  if (!length(ok)) return(data.frame())
  out <- dplyr::bind_rows(ok)
  out <- adjust_fdr_family(out, "ACME_p", "Mediator", by = "Comparison", dedupe = FALSE)
  names(out)[names(out) == "P_FDR"] <- "ACME_FDR"
  out[order(out$Comparison, out$ACME_p), , drop = FALSE]
}

# ============================================================================
# 7. Plot builders
# ============================================================================
# ---- 7.1 box plots ---------------------------------------------------------
plot_box <- function(df, var, group_col, p_global = NA_real_, method = NA_character_,
                     posthoc = NULL, tag = NULL) {
  d <- data.frame(y = to_num(df[[var]], var), g = factor(df[[group_col]]))
  d <- d[!is.na(d$y) & !is.na(d$g), , drop = FALSE]
  d$g <- droplevels(d$g)
  if (nrow(d) < 5 || nlevels(d$g) < 2) return(NULL)
  grps <- levels(d$g); n_tab <- table(d$g)[grps]
  sub <- if (!is.na(p_global))
    sprintf("%s, P %s", if (is.na(method)) "" else method, ifelse(p_global < 0.001, "< 0.001",
                                                sprintf("= %.3f", p_global))) else NULL
  p <- ggplot(d, aes(x = g, y = y, fill = g)) +
    geom_boxplot(width = 0.6, outlier.shape = NA, colour = "black",
                 linewidth = 0.3, alpha = 0.85) +
    geom_point(position = position_jitter(width = 0.12, height = 0, seed = CFG$seed),
               size = 0.5, alpha = 0.45, colour = "grey15") +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 1.8,
                 fill = "white", colour = "black", stroke = 0.35) +
    scale_fill_manual(values = get_cols(grps), guide = "none") +
    scale_x_discrete(labels = paste0(grps, "\n(n=", as.integer(n_tab), ")")) +
    labs(x = NULL, y = pretty_var(var), subtitle = sub, tag = tag) +
    theme_pub()
  nbr <- 0
  if (!is.null(posthoc) && nrow(posthoc) > 0) {
    pw <- posthoc[!is.na(posthoc$p.adj) & posthoc$p.adj < CFG$alpha, , drop = FALSE]
    pw$x1 <- match(as.character(pw$group1), grps)
    pw$x2 <- match(as.character(pw$group2), grps)
    pw <- pw[!is.na(pw$x1) & !is.na(pw$x2), , drop = FALSE]
    nbr <- nrow(pw)
    if (nbr > 0) {
      yr  <- diff(range(d$y)); if (!is.finite(yr) || yr <= 0) yr <- 1
      pw$ypos <- max(d$y) + 0.09 * yr * seq_len(nbr)
      pw$lab  <- ifelse(pw$p.adj < 0.001, "***", ifelse(pw$p.adj < 0.01, "**", "*"))
      tip <- 0.02 * yr
      p <- p +
        geom_segment(data = pw, inherit.aes = FALSE,
                     aes(x = x1, xend = x2, y = ypos, yend = ypos), linewidth = 0.3) +
        geom_segment(data = pw, inherit.aes = FALSE,
                     aes(x = x1, xend = x1, y = ypos, yend = ypos - tip), linewidth = 0.3) +
        geom_segment(data = pw, inherit.aes = FALSE,
                     aes(x = x2, xend = x2, y = ypos, yend = ypos - tip), linewidth = 0.3) +
        geom_text(data = pw, inherit.aes = FALSE,
                  aes(x = (x1 + x2) / 2, y = ypos + tip * 0.3, label = lab),
                  size = 2.6, vjust = 0.3)
    }
  }
  p + scale_y_continuous(expand = expansion(mult = c(0.04, 0.10 + 0.09 * nbr)))
}

select_vars <- function(tb, mode) {
  if (is.null(tb) || nrow(tb) == 0) return(character())
  tb <- tb[tb$Type == "Continuous" & !is.na(tb$P_value), , drop = FALSE]
  keep <- switch(mode,
                 fdr = !is.na(tb$P_FDR) & tb$P_FDR < CFG$alpha,
                 p   = tb$P_value < CFG$alpha,
                 rep(TRUE, nrow(tb)))
  tb$Variable[keep][order(tb$P_value[keep])]
}

box_block <- function(df, tb, group_col, tags) {
  vars <- select_vars(tb, CFG$select_box)
  ph   <- attr(tb, "posthoc")
  plots <- list()
  for (i in seq_along(vars)) {
    v <- vars[i]; r <- tb[tb$Variable == v, ][1, ]
    p <- plot_box(df, v, group_col, r$P_value, r$Method, ph[[v]], tags[i])
    if (!is.null(p)) plots[[length(plots) + 1]] <- p
  }
  plots
}

assemble_box_figure <- function(blocks, ncol = CFG$box_ncol) {
  parts <- list(); hts <- numeric()
  for (b in blocks) {
    if (!length(b$plots)) next
    n <- length(b$plots); nr <- ceiling(n / ncol)
    pl <- c(b$plots, rep(list(patchwork::plot_spacer()), nr * ncol - n))
    head <- ggplot() + theme_void() +
      annotate("text", x = 0, y = 0, label = b$title, hjust = 0, fontface = "bold",
               size = 3.2, family = CFG$font) +
      scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
      scale_y_continuous(limits = c(-1, 1), expand = c(0, 0))
    parts <- c(parts, list(head, patchwork::wrap_plots(pl, ncol = ncol)))
    hts   <- c(hts, 7, nr * CFG$box_panel_h)
  }
  if (!length(parts)) return(NULL)
  list(plot = patchwork::wrap_plots(parts, ncol = 1, heights = hts),
       height = sum(hts) + 8)
}

# ---- 7.2 shared row layout for forest-style figures -------------------------
layout_rows <- function(df, panel_col = "Panel") {
  df[[panel_col]] <- droplevels(factor(df[[panel_col]]))
  y <- 0; rows <- list(); hdr <- list()
  for (pn in levels(df[[panel_col]])) {
    d <- df[df[[panel_col]] == pn, , drop = FALSE]
    if (!nrow(d)) next
    y <- y + 1
    hdr[[length(hdr) + 1]] <- data.frame(Panel = pn, ypos = y, stringsAsFactors = FALSE)
    d$ypos <- y + seq_len(nrow(d))
    y <- y + nrow(d) + 0.6
    rows[[length(rows) + 1]] <- d
  }
  rows <- do.call(rbind, rows); hdr <- do.call(rbind, hdr)
  top <- max(rows$ypos) + 1
  rows$y <- top - rows$ypos; hdr$y <- top - hdr$ypos
  list(rows = rows, hdr = hdr, y_head = max(hdr$y) + 1.1, y_min = min(rows$y) - 0.8)
}

# ---- 7.3 logistic forest (all comparisons in one figure) ---------------------
plot_forest_logit <- function(df, caption) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df <- df[is.finite(df$OR) & is.finite(df$OR_lo) & is.finite(df$OR_hi), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  df$Family <- factor(df$Family, levels = c("demographic", "inflam_cell", "inflam_lipid",
                                            "imaging", "other"))
  df <- df[order(df$Panel, df$Family, df$OR), , drop = FALSE]
  lay <- layout_rows(df); d <- lay$rows; h <- lay$hdr
  xl <- c(max(min(d$OR_lo), 0.02), min(max(d$OR_hi), 50))
  d$lo_c <- pmax(d$OR_lo, xl[1]); d$hi_c <- pmin(d$OR_hi, xl[2])
  d$sig  <- factor(ifelse(!is.na(d$P_FDR) & d$P_FDR < CFG$alpha,
                          "FDR < 0.05", "FDR ≥ 0.05"),
                   levels = c("FDR < 0.05", "FDR ≥ 0.05"))
  d$lab  <- pretty_var(d$Variable)
  d$est_txt <- sprintf("%.2f (%.2f, %.2f)", d$OR, d$OR_lo, d$OR_hi)
  d$p_txt   <- fmt_p(d$P_value); d$q_txt <- fmt_p(d$P_FDR)
  xs <- c(0, 1.55, 2.85, 3.5); sz <- CFG$font_size * 0.3
  p_txt <- ggplot() +
    geom_text(data = h, aes(x = 0, y = y, label = Panel), hjust = 0, fontface = "bold",
              size = sz, family = CFG$font) +
    geom_text(data = d, aes(x = xs[1], y = y, label = lab), hjust = 0, size = sz, family = CFG$font) +
    geom_text(data = d, aes(x = xs[2], y = y, label = est_txt), hjust = 0, size = sz, family = CFG$font) +
    geom_text(data = d, aes(x = xs[3], y = y, label = p_txt), hjust = 0, size = sz, family = CFG$font) +
    geom_text(data = d, aes(x = xs[4], y = y, label = q_txt), hjust = 0, size = sz, family = CFG$font) +
    annotate("text", x = xs, y = lay$y_head, hjust = 0, fontface = "bold", size = sz,
             family = CFG$font, label = c("Variable", "aOR (95% CI)", "P", "FDR")) +
    scale_x_continuous(limits = c(0, 4.1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(lay$y_min, lay$y_head + 0.5), expand = c(0, 0)) +
    theme_void(base_family = CFG$font) + theme(plot.margin = margin(2, 0, 2, 2))
  p_for <- ggplot(d, aes(y = y)) +
    geom_hline(data = h, aes(yintercept = y), colour = "grey85", linewidth = 0.3) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey35", linewidth = 0.3) +
    geom_segment(aes(x = lo_c, xend = hi_c, yend = y), linewidth = 0.4) +
    geom_point(aes(x = OR, shape = sig), size = 1.7, fill = "white", stroke = 0.5) +
    scale_shape_manual(values = c(16, 1), name = NULL, drop = FALSE) +
    scale_x_log10() +
    coord_cartesian(xlim = xl) +
    scale_y_continuous(limits = c(lay$y_min, lay$y_head + 0.5), expand = c(0, 0)) +
    labs(x = "Adjusted odds ratio (log scale)", y = NULL) +
    theme_pub() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.line.y = element_blank(), legend.position = "bottom",
          plot.margin = margin(2, 4, 2, 0))
  fig <- p_txt + p_for + patchwork::plot_layout(widths = c(1.5, 1)) +
    patchwork::plot_annotation(caption = wrap_cap(caption), theme = cap_theme)
  list(plot = fig, height = max(80, (nrow(d) + nrow(h)) * 4.3 + 30))
}

# ---- 7.4 mediation forest ----------------------------------------------------
plot_forest_mediation <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df <- df[is.finite(df$ACME) & is.finite(df$ACME_lo) & is.finite(df$ACME_hi) &
             !is.na(df$ACME_p), , drop = FALSE]
  all_panels <- unique(df$Comparison)
  df <- df[df$ACME_p < CFG$alpha, , drop = FALSE]
  empty <- setdiff(all_panels, unique(df$Comparison))
  if (!nrow(df)) { note("No mediation effect with ACME P < 0.05; figure skipped."); return(NULL) }
  df <- df[order(df$Comparison, ifelse(is.na(df$ACME_FDR), Inf, df$ACME_FDR), df$ACME_p), ]
  df <- do.call(rbind, lapply(split(df, df$Comparison), head, CFG$med_top))
  df$Panel <- factor(df$Comparison, levels = all_panels)
  lay <- layout_rows(df); d <- lay$rows; h <- lay$hdr
  d$lab   <- paste0(pretty_var(d$Exposure), " → ", pretty_var(d$Mediator))
  d$est_txt <- sprintf("%.4f (%.4f, %.4f)", d$ACME, d$ACME_lo, d$ACME_hi)
  d$p_txt <- fmt_p(d$ACME_p); d$q_txt <- fmt_p(d$ACME_FDR)
  d$fdr_sig <- factor(ifelse(!is.na(d$ACME_FDR) & d$ACME_FDR < CFG$alpha,
                             "FDR < 0.05", "FDR ≥ 0.05"),
                      levels = c("FDR < 0.05", "FDR ≥ 0.05"))
  # proportion mediated is only interpretable when the total effect is significant
  show_pm <- is.finite(d$PropMed_lo) & is.finite(d$PropMed_hi) & !is.na(d$Total_p) &
    d$Total_p < CFG$alpha
  d$pm <- ifelse(show_pm, d$PropMed, NA_real_)
  d$pm_lo <- ifelse(show_pm, pmax(d$PropMed_lo, -1), NA_real_)
  d$pm_hi <- ifelse(show_pm, pmin(d$PropMed_hi, 2), NA_real_)
  xs <- c(0, 3.5, 6.9, 8.4); sz <- CFG$font_size * 0.3
  yl <- scale_y_continuous(limits = c(lay$y_min, lay$y_head + 0.5), expand = c(0, 0))
  p_txt <- ggplot() +
    geom_text(data = h, aes(x = 0, y = y, label = Panel), hjust = 0, fontface = "bold",
              size = sz, family = CFG$font) +
    geom_text(data = d, aes(x = xs[1], y = y, label = lab), hjust = 0, size = sz, family = CFG$font) +
    geom_text(data = d, aes(x = xs[2], y = y, label = est_txt), hjust = 0, size = sz, family = CFG$font) +
    geom_text(data = d, aes(x = xs[3], y = y, label = p_txt), hjust = 0, size = sz, family = CFG$font) +
    geom_text(data = d, aes(x = xs[4], y = y, label = q_txt), hjust = 0, size = sz, family = CFG$font) +
    annotate("text", x = xs, y = lay$y_head, hjust = 0, fontface = "bold", size = sz,
             family = CFG$font, label = c("Exposure → mediator", "ACME (95% CI)", "P", "FDR")) +
    scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) + yl +
    theme_void(base_family = CFG$font) + theme(plot.margin = margin(2, 0, 2, 2))
  base_for <- function() list(
    geom_hline(data = h, aes(yintercept = y), colour = "grey85", linewidth = 0.3),
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey35", linewidth = 0.3),
    yl, theme_pub(),
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.line.y = element_blank(), plot.margin = margin(2, 4, 2, 0)))
  p_acme <- ggplot(d, aes(y = y)) + base_for() +
    geom_segment(aes(x = ACME_lo, xend = ACME_hi, yend = y), linewidth = 0.4) +
    geom_point(aes(x = ACME, shape = fdr_sig), size = 1.7, fill = "white", stroke = 0.5) +
    scale_shape_manual(values = c(16, 1), name = NULL, drop = FALSE) +
    labs(x = "ACME (indirect effect)", y = NULL) +
    theme(legend.position = "bottom")
  p_pm <- ggplot(d, aes(y = y)) + base_for() +
    geom_segment(aes(x = pm_lo, xend = pm_hi, yend = y), linewidth = 0.4, na.rm = TRUE) +
    geom_point(aes(x = pm), size = 1.7, na.rm = TRUE) +
    labs(x = "Proportion mediated", y = NULL)
  cap <- paste0("ACME: average causal mediation effect (",
                if (isTRUE(CFG$med_boot)) "bootstrap percentile CI, " else "quasi-Bayesian CI, ",
                CFG$med_sims, " simulations); continuous exposures contrasted Q1 vs Q3. ",
                "Proportion mediated is shown only where the total effect P < 0.05 ",
                "(axis truncated to [-1, 2]). Only ACME P < 0.05 shown (top ", CFG$med_top,
                " per panel by FDR).",
                if (length(empty)) paste0(" No ACME P < 0.05: ", paste(empty, collapse = "; "), "."))
  fig <- p_txt + p_acme + p_pm + patchwork::plot_layout(widths = c(2.3, 1, 1)) +
    patchwork::plot_annotation(caption = wrap_cap(cap), theme = cap_theme)
  list(plot = fig, height = max(80, (nrow(d) + nrow(h)) * 4.3 + 35))
}

# ---- 7.5 metadeconfoundR heat maps (one figure, one facet per comparison) -----
plot_mdr_heatmap <- function(long) {
  if (is.null(long) || nrow(long) == 0) return(NULL)
  nm <- names(long)
  pick <- function(cands) intersect(cands, nm)[1]
  cf <- pick(c("feature", "Feature")); cm <- pick(c("metaVariable", "MetaVariable", "meta"))
  cd <- pick(c("Ds", "D")); cq <- pick(c("Qs", "Q"))
  if (anyNA(c(cf, cm, cd, cq))) {
    note("metadeconfoundR long output has unexpected columns (", paste(nm, collapse = ", "),
         "); heat map skipped - see Output4_MDR.xlsx.")
    return(NULL)
  }
  h <- data.frame(Comparison = long$Comparison, feature = as.character(long[[cf]]),
                  meta = as.character(long[[cm]]), D = long[[cd]], Q = long[[cq]],
                  stringsAsFactors = FALSE)
  h$star <- ifelse(!is.na(h$Q) & h$Q < CFG$mdr_q, "*", "")
  h$Comparison <- factor(h$Comparison, levels = unique(long$Comparison))
  h$meta <- factor(h$meta, levels = c("Outcome", setdiff(unique(h$meta), "Outcome")))
  h$feature <- factor(h$feature, levels = rev(unique(h$feature)))
  nfeat <- nlevels(h$feature); npan <- nlevels(h$Comparison); nr <- ceiling(npan / 3)
  p <- ggplot(h, aes(x = meta, y = feature, fill = D)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(aes(label = star), size = 3, vjust = 0.75) +
    scale_fill_gradient2(low = "#0072B2", mid = "white", high = "#D55E00", midpoint = 0,
                         limits = c(-1, 1), na.value = "grey92",
                         name = "Effect size\n(Cliff's δ / Spearman ρ)") +
    facet_wrap(~Comparison, scales = "free_x", ncol = 3) +
    labs(x = NULL, y = NULL,
         caption = sprintf(paste0("Outcome column: effect size of the group contrast on each marker; ",
                                  "other columns: effect of covariates. * FDR q < %s."), CFG$mdr_q)) +
    theme_pub() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.line = element_blank(),
          axis.ticks = element_blank(), legend.position = "bottom",
          legend.key.width = unit(10, "mm"))
  list(plot = p, height = nr * (nfeat * 3.6 + 22) + 25)
}

# ============================================================================
# 8. Data loading and preparation
# ============================================================================
message("\n===== Data loading =====")
raw_list <- lapply(CFG$files, read_clean)
for (g in names(raw_list)) message(sprintf("%-5s n = %d", g, nrow(raw_list[[g]])))

vars_cont_demo <- c("Age", "BMI")
vars_cat_demo  <- c("Sex", "Smoking", "Drinking", "Hypertension", "Diabetes")
vars_inflam    <- c("Total.cholesterol", "Triglyceride", "HDL", "LDL", "Leucocyte",
                    "Platelet", "Neutrophil", "Lymphocyte", "Monocyte", "NLR", "PLR",
                    "MLR", "MHR", "SII", "SIRI", "AISI", "AIP", "RC")
vars_img_cont  <- c("PVWM", "DWM", "Fazekas.Score", "NIHSS.Score")
var_img_cat    <- "Fazekas.Classification"
var_med        <- "Medications"
req_common <- c(vars_cont_demo, vars_cat_demo)   # biomarkers: whatever is available
for (g in names(raw_list)) {
  miss <- setdiff(req_common, names(raw_list[[g]]))
  if (length(miss)) stop(g, " is missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
}

std_df <- function(df, cols, grp) {
  for (m in setdiff(cols, names(df))) df[[m]] <- NA
  out <- df[, cols, drop = FALSE]
  out[] <- lapply(out, function(x) {                 # type-safe across the 3 sheets
    x <- trimws(as.character(x)); x[x %in% c("", "NA", "N/A")] <- NA; x
  })
  out$Group <- grp
  out
}
all_cols <- Reduce(union, lapply(raw_list, names))
vars_inflam <- intersect(vars_inflam, all_cols)
if (!length(vars_inflam)) stop("No biomarker variables found in the UKB files.", call. = FALSE)
note("UKB validation: Medications, Subtype, Fazekas score/grade and NIHSS are not available; ",
     "models are adjusted for the 7 risk factors only; no subtype analyses.")
raw_all <- dplyr::bind_rows(lapply(names(raw_list), function(g) std_df(raw_list[[g]], all_cols, g)))
raw_all$Group <- factor(raw_all$Group, levels = c("HC", "CVNP", "VaP"))
num_vars <- intersect(c(vars_cont_demo, vars_inflam, vars_img_cont), names(raw_all))
for (v in num_vars) raw_all[[v]] <- to_num(raw_all[[v]], v)
cat_vars_all <- intersect(c(vars_cat_demo, var_med, var_img_cat), names(raw_all))
cat("\nSample size by group:\n"); print(table(raw_all$Group))

# ---- v2: collapse multi-level smoking/drinking to ever (1) vs never (0) for modelling ----
# In v1 these 3-level factors (i) made 313 of 327 bootstrap mediation models fail
# ("factor Drinking has new levels") and (ii) silently dropped smoking/drinking from
# the risk-factor model. Table 1 keeps the original levels (raw_all).
raw_mod <- raw_all
for (v in intersect(names(CFG$binarize), names(raw_mod))) {
  x <- trimws(as.character(raw_mod[[v]]))
  raw_mod[[v]] <- ifelse(is.na(x) | x == "", NA_character_,
                         ifelse(x == CFG$binarize[[v]], "0", "1"))
  note(v, ": collapsed for modelling (", CFG$binarize[[v]], " = 0, all other levels = 1) - verify coding.")
}

# ---- missingness, imputation, coding, transformation -------------------------
df_missing_before <- missing_report(raw_all, c(num_vars, cat_vars_all))
imp <- impute_by_group(raw_mod, cont_vars = num_vars, cat_vars = cat_vars_all)
imp_all <- imp$data
df_impute_log <- imp$log
cat("\nVariables with missing data (before imputation):\n")
print(df_missing_before[df_missing_before$N_missing > 0, ], row.names = FALSE)

cat_model_vars <- intersect(c(vars_cat_demo, var_med), names(raw_all))
codings <- lapply(setNames(cat_model_vars, cat_model_vars), function(v) make_coding(raw_mod[[v]], CFG$coding_override[[v]]))
df_coding <- dplyr::bind_rows(lapply(names(codings), function(v) {
  cd <- codings[[v]]
  data.frame(Variable = v, Type = cd$type,
             Coding = if (cd$type == "binary") sprintf("0 = %s; 1 = %s", cd$ref, cd$alt)
                      else paste(cd$levels, collapse = " | "), stringsAsFactors = FALSE)
}))
cat("\nCoding used in models (odds ratios refer to level 1):\n"); print(df_coding, row.names = FALSE)
for (v in names(codings)) {
  cd <- codings[[v]]
  if (cd$type != "binary")
    note("VERIFY ", v, ": coded as '", cd$type, "' (levels: ", paste(cd$levels, collapse = ", "),
         "). GDPH uses a binary variable; recode (e.g. current vs not) for comparability, otherwise ",
         "it is dropped from the risk-factor and mediation exposure analyses.")
  else if (v == "Sex")
    note("VERIFY Sex coding: reference (0) = ", cd$ref, ", level 1 = ", cd$alt,
         ". GDPH reports Female vs Male (Female = 1); use CFG$coding_override if UKB differs.")
}

markers_all <- intersect(c(vars_inflam, vars_img_cont), names(raw_all))
tf <- lapply(setNames(markers_all, markers_all), function(v) make_tf(raw_all[[v]]))
df_tf <- dplyr::bind_rows(lapply(names(tf), function(v) {
  if (is.null(tf[[v]])) return(NULL)
  data.frame(Variable = v, Log_transformed = tf[[v]]$log, Mean = tf[[v]]$mu, SD = tf[[v]]$sd)
}))

encode_for_models <- function(df) {
  for (v in names(codings)) df[[v]] <- apply_coding(df[[v]], codings[[v]])
  for (v in names(tf)) df[[v]] <- apply_tf(df[[v]], tf[[v]])
  # v2: composite monocyte-related inflammatory score = standardised mean of the
  # z-scores of the four monocyte-containing indices (one test for the whole cluster)
  mono <- intersect(c("Monocyte", "MLR", "MHR", "SIRI"), names(df))
  if (length(mono) >= 2)
    df$MonoScore <- as.numeric(scale(rowMeans(as.matrix(df[, mono, drop = FALSE]))))
  df
}
mod_imp <- encode_for_models(imp_all)   # primary analysis
mod_obs <- encode_for_models(raw_mod)   # complete-case sensitivity
markers_all <- c(markers_all, "MonoScore")   # v2: composite monocyte score

cov_demo <- c("Age", "BMI", "Sex", "Smoking", "Drinking", "Hypertension", "Diabetes")
cov_full <- c(cov_demo, intersect(var_med, names(mod_imp)))

# ============================================================================
# 9. Table 1 (observed data)
# ============================================================================
message("\n===== Table 1 =====")
t1_cont <- intersect(c(vars_cont_demo, vars_inflam, vars_img_cont), names(raw_all))
t1_cat  <- intersect(c(vars_cat_demo, var_med, var_img_cat), names(raw_all))
table1  <- build_table1(raw_all, t1_cont, t1_cat, "Group")

# ============================================================================
# 10. Adjusted logistic regression
# ============================================================================
message("\n===== Logistic regression =====")
comparisons <- list(
  list(label = "VaP vs HC",   ref = "HC",   alt = "VaP"),
  list(label = "VaP vs CVNP", ref = "CVNP", alt = "VaP")
)
logit_tabs <- list(); mdr_tabs <- list(); med_tasks <- list()
exposures <- c("Age", "Sex", "BMI", "Smoking", "Drinking")

# main contrasts
for (cp in comparisons) {
  di <- pair_data(mod_imp, cp$ref, cp$alt); do <- pair_data(mod_obs, cp$ref, cp$alt)
  if (cp$ref == "HC") {                        # risk factors (mutually adjusted)
    r <- run_logit_set(di, do, "Outcome", cov_demo, cov_full, paste0(cp$label, ": risk factors"))
    if (nrow(r)) { r$Panel <- paste0(cp$label, " – risk factors"); r$Family <- "demographic"
                   logit_tabs[[length(logit_tabs) + 1]] <- r }
  }
  r <- run_logit_set(di, do, "Outcome", markers_all, cov_full, paste0(cp$label, ": markers"))
  if (nrow(r)) { r$Panel <- paste0(cp$label, " – markers")
                 logit_tabs[[length(logit_tabs) + 1]] <- r }
  if (isTRUE(CFG$run_mdr)) {
    m <- run_mdr(di, "Outcome", markers_all, cov_full, cp$label)
    if (!is.null(m)) mdr_tabs[[cp$label]] <- m
  }
  md_mark <- drop_structural(di, markers_all, cp$label)
  md_cov  <- drop_structural(di, cov_full, cp$label)
  med_tasks <- c(med_tasks, build_med_tasks(di, "Outcome", cp$label, exposures, md_mark, md_cov))
}

df_logit <- if (length(logit_tabs)) dplyr::bind_rows(logit_tabs) else data.frame()
if (nrow(df_logit)) df_logit$Panel <- factor(df_logit$Panel, levels = unique(df_logit$Panel))

# ============================================================================
# 11. Mediation (parallel, bootstrap)
# ============================================================================
message("\n===== Mediation: ", length(med_tasks), " models, ", CFG$n_cores,
        " core(s), sims = ", CFG$med_sims, " =====")
t0 <- Sys.time()
df_med <- run_mediation(med_tasks)
message("Mediation finished in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

df_mdr <- if (length(mdr_tabs)) dplyr::bind_rows(mdr_tabs) else data.frame()

# ============================================================================
# 12. Figures (3 in total; 4 if run_mdr = TRUE)
# ============================================================================
message("\n===== Figures =====")
# --- Figure 1: box plots (baseline) --------------------------------------------
tags1 <- make_tags(80)
b1 <- box_block(raw_all, table1, "Group", tags1)
fig1 <- assemble_box_figure(list(
  list(title = sprintf("UKB baseline continuous variables: HC vs CVNP vs VaP (%s < 0.05)",
                       if (CFG$select_box == "fdr") "FDR" else "P"), plots = b1)))
if (!is.null(fig1)) save_fig(fig1$plot, "Figure1_Boxplots", height = fig1$height)

# --- Figure 2: forest plots (all logistic analyses) ---------------------------
if (nrow(df_logit)) {
  keep <- switch(CFG$select_forest,
                 fdr = !is.na(df_logit$P_FDR) & df_logit$P_FDR < CFG$alpha,
                 p   = !is.na(df_logit$P_value) & df_logit$P_value < CFG$alpha,
                 rep(TRUE, nrow(df_logit)))
  df_fp <- df_logit[keep, , drop = FALSE]
  fig2 <- plot_forest_logit(df_fp, paste0(
    "Adjusted for ", paste(tolower(pretty_var(cov_full)), collapse = ", "),
    " (covariates unavailable in a group were dropped; see Output0). ",
    "Odds ratios (alt vs ref group) are per 1 SD for markers (log-transformed if right-skewed), ",
    "per unit for age and BMI, and level 1 vs 0 for binary variables. ",
    "Filled symbol: FDR < 0.05 (BH within variable family). Shown: ",
    if (CFG$select_forest == "all") "all variables." else
      paste0(if (CFG$select_forest == "fdr") "FDR" else "nominal P", " < 0.05.")))
  if (!is.null(fig2)) save_fig(fig2$plot, "Figure2_Forest_Logistic", height = fig2$height)
}

# --- Figure 3: mediation ------------------------------------------------------
fig3 <- plot_forest_mediation(df_med)
if (!is.null(fig3)) save_fig(fig3$plot, "Figure3_Mediation", height = fig3$height)

# --- Figure 4: metadeconfoundR heat maps ---------------------------------------
fig4 <- plot_mdr_heatmap(df_mdr)
if (!is.null(fig4)) save_fig(fig4$plot, "Figure4_MDR_Heatmaps", height = fig4$height)

# ============================================================================
# 13. Tables
# ============================================================================
message("\n===== Writing tables =====")
w <- function(lst, f) openxlsx::write.xlsx(lst, file.path(CFG$out_dir, f), overwrite = TRUE)
w(list(Notes = data.frame(Note = NOTES), Coding = df_coding,
       Marker_transform = df_tf, Missing_before = df_missing_before,
       Imputation_log = df_impute_log), "Output0_Methods_Log.xlsx")
w(list(Baseline = table1, Sample_size = as.data.frame(table(raw_all$Group))),
  "Output1_Table1_Baseline.xlsx")
w(list(Logistic = df_logit), "Output2_Logistic.xlsx")
w(list(Mediation = df_med), "Output3_Mediation.xlsx")
if (nrow(df_mdr)) w(list(metadeconfoundR = df_mdr), "Output4_MDR.xlsx")
writeLines(utils::capture.output(utils::sessionInfo()),
           file.path(CFG$out_dir, "sessionInfo.txt"))

cat("\n", strrep("=", 70), "\nAnalysis completed.\n", strrep("=", 70), "\n", sep = "")
cat("Figures (Figures_UKB/): UKB_Figure1_Boxplots, UKB_Figure2_Forest_Logistic, UKB_Figure3_Mediation",
    if (isTRUE(CFG$run_mdr)) ", UKB_Figure4_MDR_Heatmaps", "\n", sep = "")
cat("Notes recorded (", length(NOTES), ") -> Output_UKB/Output0_Methods_Log.xlsx\n", sep = "")
cat(R.version.string, "\n")
