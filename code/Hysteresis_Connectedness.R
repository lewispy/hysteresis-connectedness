# ============================================================
# Replication codes for Onodje, P. (2026). Hysteresis between 
# Bitcoin, Gold, Oil, and the S&P 500 index: Evidence from a 
# Multi-threshold Connectedness Approach. Finance Research Open, 100091. 
# ============================================================

req_pkgs <- c("ConnectednessApproach", "zoo", "readxl", "openxlsx",
              "ggplot2", "reshape2")

install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(missing) > 0) install.packages(missing, dependencies = TRUE)
  invisible(TRUE)
}

load_pkgs <- function(pkgs) {
  for (p in pkgs) suppressPackageStartupMessages(library(p, character.only = TRUE))
  invisible(TRUE)
}

install_if_missing(req_pkgs)
load_pkgs(req_pkgs)

# ------------------------------------------------------------
# 0) Robust Date parser (for Date, POSIXct, Excel numeric, character)
# ------------------------------------------------------------
parse_any_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  
  # Excel numeric dates commonly appear from readxl depending on cell formatting
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  
  # Character
  if (is.character(x)) return(as.Date(x))
  
  # Fallback
  as.Date(x)
}

# ------------------------------------------------------------
# 1) Read regimes workbook (3 sheets) into zoo + keep reference dates
# ------------------------------------------------------------
read_regimes_xlsx_as_zoo <- function(regimes_xlsx,
                                     sheets = c("increasing", "stable", "decreasing")) {
  
  read_one <- function(sh) {
    df <- readxl::read_excel(regimes_xlsx, sheet = sh)
    date_col <- names(df)[1]
    dts <- parse_any_date(df[[date_col]])
    x <- as.data.frame(df[, -1])
    zoo::zoo(x, order.by = dts)
  }
  
  inc <- read_one(sheets[1])
  sta <- read_one(sheets[2])
  dec <- read_one(sheets[3])
  
  list(
    inc = inc, sta = sta, dec = dec,
    dates_ref = zoo::index(inc)  # master reference dates
  )
}

# ------------------------------------------------------------
# 2) Run DCA for one regime zoo object (TVP-VAR)
# ------------------------------------------------------------
run_dca_tvpvar <- function(z,
                           nlag = 1,
                           nfore = 12,
                           window.size = 200,
                           kappa1 = 0.99,
                           kappa2 = 0.96,
                           prior = "BayesPrior") {
  
  ConnectednessApproach::ConnectednessApproach(
    z,
    nlag = nlag,
    nfore = nfore,
    window.size = window.size,
    model = "TVP-VAR",
    connectedness = "Time",
    VAR_config = list(TVPVAR = list(kappa1 = kappa1, kappa2 = kappa2, prior = prior))
  )
}

# ------------------------------------------------------------
# 3) FORCE correct dates onto DCA output
# ------------------------------------------------------------
attach_dates_to_dca <- function(dca, dates_ref) {
  dates_ref <- parse_any_date(dates_ref)
  
  # Use TCI length as the canonical length
  L <- NROW(dca[["TCI"]])
  if (length(dates_ref) < L) stop("dates_ref is shorter than DCA output length. Check your input dates.")
  d <- tail(dates_ref, L)
  
  # Rebuild zoo objects with correct Date index
  for (nm in c("FROM", "TO", "NET")) {
    x <- dca[[nm]]
    dca[[nm]] <- zoo::zoo(zoo::coredata(x), order.by = d)
    colnames(dca[[nm]]) <- colnames(x)
  }
  
  # Enforce TCI to be a zoo object with dates
  tci_vals <- as.numeric(zoo::coredata(dca[["TCI"]]))
  dca[["TCI"]] <- zoo::zoo(tci_vals, order.by = d)
  
  # Optional: ensure PCI/NPDC time dimnames match (protects pairwise dates too)
  for (nm in c("PCI", "NPDC")) {
    arr <- dca[[nm]]
    if (!is.null(arr) && length(dim(arr)) == 3 && dim(arr)[3] == length(d)) {
      dn <- dimnames(arr)
      dn[[3]] <- as.character(d)
      dimnames(arr) <- dn
      dca[[nm]] <- arr
    }
  }
  
  dca
}

# ------------------------------------------------------------
# 4) Table 2 exporter
# ------------------------------------------------------------
table2_df <- function(dca) {
  tab <- dca[["TABLE"]]
  df <- as.data.frame(tab)
  df <- cbind(RowNames = rownames(df), df)
  rownames(df) <- NULL
  df
}

export_table2_excel <- function(dca_inc, dca_sta, dca_dec, file = "Table2.xlsx") {
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "Increasing")
  openxlsx::writeData(wb, "Increasing", table2_df(dca_inc))
  
  openxlsx::addWorksheet(wb, "Stable")
  openxlsx::writeData(wb, "Stable", table2_df(dca_sta))
  
  openxlsx::addWorksheet(wb, "Decreasing")
  openxlsx::writeData(wb, "Decreasing", table2_df(dca_dec))
  
  headerStyle <- openxlsx::createStyle(textDecoration = "bold")
  for (sh in c("Increasing", "Stable", "Decreasing")) {
    openxlsx::addStyle(wb, sh, headerStyle, rows = 1, cols = 1:100, gridExpand = TRUE)
    openxlsx::setColWidths(wb, sh, cols = 1:100, widths = "auto")
  }
  
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

# ------------------------------------------------------------
# 5) Table 3 helpers (means, t-tests, stars, hysteresis)
# ------------------------------------------------------------
stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

ttest_stat_p <- function(x, y, paired = TRUE) {
  x <- as.numeric(x); y <- as.numeric(y)
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(list(t = NA_real_, p = NA_real_))
  res <- stats::t.test(x[ok], y[ok], paired = paired)
  list(t = unname(res$statistic), p = res$p.value)
}

align_3 <- function(z_inc, z_sta, z_dec) {
  m <- zoo::merge.zoo(z_inc, z_sta, z_dec, all = FALSE)
  list(
    inc = zoo::coredata(m[, 1]),
    sta = zoo::coredata(m[, 2]),
    dec = zoo::coredata(m[, 3])
  )
}

pair_series_from_arr3d <- function(arr3d, To, From) {
  dts <- parse_any_date(dimnames(arr3d)[[3]])
  vals <- vapply(seq_along(dts), function(k) arr3d[From, To, k], numeric(1))
  zoo::zoo(vals, order.by = dts)
}

one_row <- function(panel, item, z_inc, z_sta, z_dec,
                    paired = TRUE, hyst_alpha = 0.05, digits = 3) {
  a <- align_3(z_inc, z_sta, z_dec)
  
  mu_inc <- mean(a$inc, na.rm = TRUE)
  mu_sta <- mean(a$sta, na.rm = TRUE)
  mu_dec <- mean(a$dec, na.rm = TRUE)
  
  ts1 <- ttest_stat_p(a$inc, a$sta, paired = paired)
  ts2 <- ttest_stat_p(a$dec, a$sta, paired = paired)
  
  t1_str <- ifelse(is.na(ts1$t), NA_character_,
                   sprintf(paste0("%.", digits, "f%s"), ts1$t, stars(ts1$p)))
  t2_str <- ifelse(is.na(ts2$t), NA_character_,
                   sprintf(paste0("%.", digits, "f%s"), ts2$t, stars(ts2$p)))
  
  hyst <- ifelse(!is.na(ts1$p) && !is.na(ts2$p) &&
                   (ts1$p < hyst_alpha) && (ts2$p < hyst_alpha), "S", "NS")
  
  data.frame(
    Panel = panel,
    Item = item,
    `Increasing regime` = round(mu_inc, digits),
    `Stable regime`     = round(mu_sta, digits),
    `Decreasing regime` = round(mu_dec, digits),
    `Increasing vs stable` = t1_str,
    `Decreasing vs stable` = t2_str,
    Hysteresis = hyst,
    stringsAsFactors = FALSE
  )
}

build_table3 <- function(dca_inc, dca_sta, dca_dec,
                         assets_map = c(bitcoin = "Bitcoin",
                                        gold    = "Gold",
                                        oil     = "Oil",
                                        sp500   = "SP500"),
                         paired = TRUE,
                         hyst_alpha = 0.05,
                         digits = 3) {
  
  FROM_inc <- dca_inc[["FROM"]]; FROM_sta <- dca_sta[["FROM"]]; FROM_dec <- dca_dec[["FROM"]]
  TO_inc   <- dca_inc[["TO"]]  ; TO_sta   <- dca_sta[["TO"]]  ; TO_dec   <- dca_dec[["TO"]]
  NET_inc  <- dca_inc[["NET"]] ; NET_sta  <- dca_sta[["NET"]] ; NET_dec  <- dca_dec[["NET"]]
  TCI_inc  <- dca_inc[["TCI"]] ; TCI_sta  <- dca_sta[["TCI"]] ; TCI_dec  <- dca_dec[["TCI"]]
  
  PCI_inc  <- dca_inc[["PCI"]] ; PCI_sta  <- dca_sta[["PCI"]] ; PCI_dec  <- dca_dec[["PCI"]]
  NPDC_inc <- dca_inc[["NPDC"]]; NPDC_sta <- dca_sta[["NPDC"]]; NPDC_dec <- dca_dec[["NPDC"]]
  
  asset_keys <- names(assets_map)
  out <- list()
  
  for (k in asset_keys) out[[length(out) + 1]] <- one_row("From others", assets_map[[k]],
                                                          FROM_inc[, k], FROM_sta[, k], FROM_dec[, k],
                                                          paired, hyst_alpha, digits)
  
  for (k in asset_keys) out[[length(out) + 1]] <- one_row("To others", assets_map[[k]],
                                                          TO_inc[, k], TO_sta[, k], TO_dec[, k],
                                                          paired, hyst_alpha, digits)
  
  for (k in asset_keys) out[[length(out) + 1]] <- one_row("Net total", assets_map[[k]],
                                                          NET_inc[, k], NET_sta[, k], NET_dec[, k],
                                                          paired, hyst_alpha, digits)
  
  pci_pairs <- list(
    list(To = "gold",  From = "bitcoin", label = "Gold and Bitcoin"),
    list(To = "oil",   From = "bitcoin", label = "Oil and Bitcoin"),
    list(To = "oil",   From = "gold",    label = "Oil and Gold"),
    list(To = "sp500", From = "bitcoin", label = "SP500 and Bitcoin"),
    list(To = "sp500", From = "gold",    label = "SP500 and Gold"),
    list(To = "sp500", From = "oil",     label = "SP500 and Oil")
  )
  
  for (p in pci_pairs) {
    out[[length(out) + 1]] <- one_row(
      "Pairwise", p$label,
      pair_series_from_arr3d(PCI_inc,  p$To, p$From),
      pair_series_from_arr3d(PCI_sta,  p$To, p$From),
      pair_series_from_arr3d(PCI_dec,  p$To, p$From),
      paired, hyst_alpha, digits
    )
  }
  
  npdc_pairs <- list(
    list(To = "gold",  From = "bitcoin", label = "Gold to Bitcoin"),
    list(To = "oil",   From = "bitcoin", label = "Oil to Bitcoin"),
    list(To = "oil",   From = "gold",    label = "Oil to Gold"),
    list(To = "sp500", From = "bitcoin", label = "SP500 to Bitcoin"),
    list(To = "sp500", From = "gold",    label = "SP500 to Gold"),
    list(To = "sp500", From = "oil",     label = "SP500 to Oil")
  )
  
  for (p in npdc_pairs) {
    out[[length(out) + 1]] <- one_row(
      "Net Pairwise", p$label,
      pair_series_from_arr3d(NPDC_inc, p$To, p$From),
      pair_series_from_arr3d(NPDC_sta, p$To, p$From),
      pair_series_from_arr3d(NPDC_dec, p$To, p$From),
      paired, hyst_alpha, digits
    )
  }
  
  out[[length(out) + 1]] <- one_row("Total", "TCI", TCI_inc, TCI_sta, TCI_dec,
                                    paired, hyst_alpha, digits)
  
  do.call(rbind, out)
}

export_table3_excel <- function(table3_df, file = "Table3.xlsx") {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Table3")
  openxlsx::writeData(wb, "Table3", table3_df)
  
  headerStyle <- openxlsx::createStyle(textDecoration = "bold")
  openxlsx::addStyle(wb, "Table3", headerStyle, rows = 1, cols = 1:ncol(table3_df), gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Table3", cols = 1:ncol(table3_df), widths = "auto")
  
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

# ----------------------------------------------------------------
# 6) Plotting Figures 3 to 8 (stable fill only, your exact colours)
# ----------------------------------------------------------------
COL_DEC_LINE <- "#3188BC"
COL_INC_LINE <- "#D45700"
COL_STA_LINE <- "#050505"
COL_STA_FILL <- "#363636"
ALPHA_STA_FILL <- 0.8

make_asset_df <- function(z_inc, z_sta, z_dec, label) {
  df <- data.frame(
    Date       = parse_any_date(zoo::index(z_inc)),
    Increasing = as.numeric(zoo::coredata(z_inc)),
    Stable     = as.numeric(zoo::coredata(z_sta)),
    Decreasing = as.numeric(zoo::coredata(z_dec)),
    Label      = label,
    stringsAsFactors = FALSE
  )
  df$ymin_sta <- pmin(0, df$Stable)
  df$ymax_sta <- pmax(0, df$Stable)
  df
}

plot_assets_figure <- function(Z_inc, Z_sta, Z_dec, assets_map, ylab, out_file, ncol = 2) {
  keys <- names(assets_map)
  labels <- unname(assets_map)
  
  df <- do.call(rbind, lapply(seq_along(keys), function(i) {
    k <- keys[i]
    make_asset_df(Z_inc[, k], Z_sta[, k], Z_dec[, k], labels[i])
  }))
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Date)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ymin_sta, ymax = ymax_sta),
                         fill = COL_STA_FILL, alpha = ALPHA_STA_FILL) +
    ggplot2::geom_line(ggplot2::aes(y = Decreasing, colour = "Decreasing regime"), linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = Increasing, colour = "Increasing regime"), linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = Stable,     colour = "Stable regime"),     linewidth = 0.7) +
    ggplot2::scale_colour_manual(values = c(
      "Decreasing regime" = COL_DEC_LINE,
      "Increasing regime" = COL_INC_LINE,
      "Stable regime"     = COL_STA_LINE
    )) +
    ggplot2::facet_wrap(~Label, ncol = ncol, scales = "free_y") +
    ggplot2::labs(y = ylab, x = NULL, colour = NULL) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 14, face = "bold"),
      strip.text = ggplot2::element_text(size = 14, face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  ggplot2::ggsave(out_file, plot = p, width = 12, height = 7, dpi = 300)
  invisible(p)
}

plot_pairs_figure <- function(arr_inc, arr_sta, arr_dec, pairs, ylab, out_file, ncol = 3) {
  
  build_pair_df <- function(arr3d, To, From, label, regime) {
    dts <- parse_any_date(dimnames(arr3d)[[3]])
    vals <- vapply(seq_along(dts), function(k) arr3d[From, To, k], numeric(1))
    data.frame(Date = dts, Value = vals, Pair = label, Regime = regime, stringsAsFactors = FALSE)
  }
  
  df_long <- do.call(rbind, lapply(pairs, function(p) {
    rbind(
      build_pair_df(arr_inc, p$To, p$From, p$label, "Increasing regime"),
      build_pair_df(arr_sta, p$To, p$From, p$label, "Stable regime"),
      build_pair_df(arr_dec, p$To, p$From, p$label, "Decreasing regime")
    )
  }))
  
  # stable fill per pair
  df_sta <- subset(df_long, Regime == "Stable regime")
  df_sta$ymin_sta <- pmin(0, df_sta$Value)
  df_sta$ymax_sta <- pmax(0, df_sta$Value)
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = df_sta,
      ggplot2::aes(x = Date, ymin = ymin_sta, ymax = ymax_sta),
      fill = COL_STA_FILL, alpha = ALPHA_STA_FILL
    ) +
    ggplot2::geom_line(
      data = df_long,
      ggplot2::aes(x = Date, y = Value, colour = Regime),
      linewidth = 0.7
    ) +
    ggplot2::scale_colour_manual(values = c(
      "Decreasing regime" = COL_DEC_LINE,
      "Increasing regime" = COL_INC_LINE,
      "Stable regime"     = COL_STA_LINE
    )) +
    ggplot2::facet_wrap(~Pair, ncol = ncol, scales = "free_y") +
    ggplot2::labs(y = ylab, x = NULL, colour = NULL) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 14, face = "bold"),
      strip.text = ggplot2::element_text(size = 14, face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  ggplot2::ggsave(out_file, plot = p, width = 14, height = 7, dpi = 300)
  invisible(p)
}

plot_tci_figure <- function(tci_inc, tci_sta, tci_dec, out_file) {
  df <- data.frame(
    Date       = parse_any_date(zoo::index(tci_inc)),
    Increasing = as.numeric(zoo::coredata(tci_inc)),
    Stable     = as.numeric(zoo::coredata(tci_sta)),
    Decreasing = as.numeric(zoo::coredata(tci_dec))
  )
  df$ymin_sta <- pmin(0, df$Stable)
  df$ymax_sta <- pmax(0, df$Stable)
  
  df_long <- reshape2::melt(df[, c("Date", "Increasing", "Stable", "Decreasing")], id.vars = "Date")
  names(df_long) <- c("Date", "Regime", "Value")
  
  # map to legend labels
  df_long$Regime <- factor(df_long$Regime,
                           levels = c("Increasing", "Stable", "Decreasing"),
                           labels = c("Increasing regime", "Stable regime", "Decreasing regime"))
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = df,
      ggplot2::aes(x = Date, ymin = ymin_sta, ymax = ymax_sta),
      fill = COL_STA_FILL, alpha = ALPHA_STA_FILL
    ) +
    ggplot2::geom_line(
      data = df_long,
      ggplot2::aes(x = Date, y = Value, colour = Regime),
      linewidth = 0.8
    ) +
    ggplot2::scale_colour_manual(values = c(
      "Decreasing regime" = COL_DEC_LINE,
      "Increasing regime" = COL_INC_LINE,
      "Stable regime"     = COL_STA_LINE
    )) +
    ggplot2::labs(y = "TCI", x = NULL, colour = NULL) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 14, face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  ggplot2::ggsave(out_file, plot = p, width = 12, height = 4, dpi = 300)
  invisible(p)
}

plot_figures_3_8 <- function(dca_inc, dca_sta, dca_dec, out_dir,
                             assets_map = c(bitcoin = "Bitcoin",
                                            gold    = "Gold",
                                            oil     = "Oil",
                                            sp500   = "SP500")) {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  plot_assets_figure(dca_inc[["FROM"]], dca_sta[["FROM"]], dca_dec[["FROM"]],
                     assets_map, ylab = "FROM",
                     out_file = file.path(out_dir, "Figure_3_FROM.png"), ncol = 2)
  
  plot_assets_figure(dca_inc[["TO"]], dca_sta[["TO"]], dca_dec[["TO"]],
                     assets_map, ylab = "TO",
                     out_file = file.path(out_dir, "Figure_4_TO.png"), ncol = 2)
  
  plot_assets_figure(dca_inc[["NET"]], dca_sta[["NET"]], dca_dec[["NET"]],
                     assets_map, ylab = "NET",
                     out_file = file.path(out_dir, "Figure_5_NET.png"), ncol = 2)
  
  pci_pairs <- list(
    list(To = "gold",  From = "bitcoin", label = "Gold and Bitcoin"),
    list(To = "oil",   From = "bitcoin", label = "Oil and Bitcoin"),
    list(To = "oil",   From = "gold",    label = "Oil and Gold"),
    list(To = "sp500", From = "bitcoin", label = "SP500 and Bitcoin"),
    list(To = "sp500", From = "gold",    label = "SP500 and Gold"),
    list(To = "sp500", From = "oil",     label = "SP500 and Oil")
  )
  
  plot_pairs_figure(dca_inc[["PCI"]], dca_sta[["PCI"]], dca_dec[["PCI"]],
                    pairs = pci_pairs, ylab = "PCI",
                    out_file = file.path(out_dir, "Figure_6_PAIRWISE.png"), ncol = 3)
  
  npdc_pairs <- list(
    list(To = "gold",  From = "bitcoin", label = "Gold to Bitcoin"),
    list(To = "oil",   From = "bitcoin", label = "Oil to Bitcoin"),
    list(To = "oil",   From = "gold",    label = "Oil to Gold"),
    list(To = "sp500", From = "bitcoin", label = "SP500 to Bitcoin"),
    list(To = "sp500", From = "gold",    label = "SP500 to Gold"),
    list(To = "sp500", From = "oil",     label = "SP500 to Oil")
  )
  
  plot_pairs_figure(dca_inc[["NPDC"]], dca_sta[["NPDC"]], dca_dec[["NPDC"]],
                    pairs = npdc_pairs, ylab = "NPDC",
                    out_file = file.path(out_dir, "Figure_7_NET_PAIRWISE.png"), ncol = 3)
  
  plot_tci_figure(dca_inc[["TCI"]], dca_sta[["TCI"]], dca_dec[["TCI"]],
                  out_file = file.path(out_dir, "Figure_8_TCI.png"))
  
  invisible(TRUE)
}

# ------------------------------------------------------------
# 7) End-to-end runner (tables + figures). This DOES plot Figures 3 to 8.
# ------------------------------------------------------------
run_export_and_plot <- function(regimes_xlsx,
                                out_dir = ".",
                                sheets = c("increasing", "stable", "decreasing"),
                                nlag = 1,
                                nfore = 12,
                                window.size = 200,
                                kappa1 = 0.99,
                                kappa2 = 0.96,
                                prior = "BayesPrior",
                                assets_map = c(bitcoin = "Bitcoin", gold = "Gold", oil = "Oil", sp500 = "SP500")) {
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  Z <- read_regimes_xlsx_as_zoo(regimes_xlsx, sheets = sheets)
  
  dca_inc <- run_dca_tvpvar(Z$inc, nlag, nfore, window.size, kappa1, kappa2, prior)
  dca_sta <- run_dca_tvpvar(Z$sta, nlag, nfore, window.size, kappa1, kappa2, prior)
  dca_dec <- run_dca_tvpvar(Z$dec, nlag, nfore, window.size, kappa1, kappa2, prior)
  
  # Critical: to reattach correct dates before any tables/plots that merge/plot time series
  dca_inc <- attach_dates_to_dca(dca_inc, Z$dates_ref)
  dca_sta <- attach_dates_to_dca(dca_sta, Z$dates_ref)
  dca_dec <- attach_dates_to_dca(dca_dec, Z$dates_ref)
  
  export_table2_excel(dca_inc, dca_sta, dca_dec, file = file.path(out_dir, "Table2.xlsx"))
  t3 <- build_table3(dca_inc, dca_sta, dca_dec, assets_map = assets_map, paired = TRUE, hyst_alpha = 0.05, digits = 3)
  export_table3_excel(t3, file = file.path(out_dir, "Table3.xlsx"))
  
  plot_figures_3_8(dca_inc, dca_sta, dca_dec, out_dir = out_dir, assets_map = assets_map)
  
  invisible(list(dca_inc = dca_inc, dca_sta = dca_sta, dca_dec = dca_dec, table3 = t3))
}

# ------------------------------------------------------------
# 8) Robustness loop
# ------------------------------------------------------------
runs <- list(
  list(tag = "q30_70", file = "regimes_30_70.xlsx"),
  list(tag = "q25_75", file = "regimes_25_75.xlsx"),
  list(tag = "q20_80", file = "regimes_20_80.xlsx")
)

base_out <- "robustness_outputs_files"
dir.create(base_out, showWarnings = FALSE, recursive = TRUE)

for (r in runs) {
  out_dir <- file.path(base_out, r$tag)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  run_export_and_plot(
    regimes_xlsx = r$file,
    out_dir = out_dir,
    sheets = c("increasing", "stable", "decreasing"),
    nlag = 1,
    nfore = 12,
    window.size = 200,
    kappa1 = 0.99,
    kappa2 = 0.96,
    prior = "BayesPrior",
    assets_map = c(bitcoin = "Bitcoin", gold = "Gold", oil = "Oil", sp500 = "SP500")
  )
}
