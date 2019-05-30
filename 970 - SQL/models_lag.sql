/****** Script for SelectTopNRows command from SSMS  ******/
SELECT 'Lex_afinn' AS method,lag,[sentiment_raw],[sentiment_pos],[sentiment_neg],[sentiment_pos_amp],[sentiment_neg_amp],[sentiment_npr],[sentiment_npr_amp],[sentiment_npnr],[sentiment_sum_pos_neg],[sentiment_npr_alt],[diff_sentiment_raw],[diff_sentiment_pos],[diff_sentiment_neg],[diff_sentiment_pos_amp],[diff_sentiment_neg_amp],[diff_sentiment_npr],[diff_sentiment_npr_amp],[diff_sentiment_npnr],[diff_sentiment_sum_pos_neg],[diff_sentiment_npr_alt]
  FROM [BitcoinTwts].[dbo].Final_Lex_afinn_res
UNION ALL
SELECT 'Lex_bing' AS method,lag,[sentiment_raw],[sentiment_pos],[sentiment_neg],[sentiment_pos_amp],[sentiment_neg_amp],[sentiment_npr],[sentiment_npr_amp],[sentiment_npnr],[sentiment_sum_pos_neg],[sentiment_npr_alt],[diff_sentiment_raw],[diff_sentiment_pos],[diff_sentiment_neg],[diff_sentiment_pos_amp],[diff_sentiment_neg_amp],[diff_sentiment_npr],[diff_sentiment_npr_amp],[diff_sentiment_npnr],[diff_sentiment_sum_pos_neg],[diff_sentiment_npr_alt]
  FROM [BitcoinTwts].[dbo].Final_Lex_bing_res
UNION ALL
SELECT 'Lex_huliu' AS method,lag,[sentiment_raw],[sentiment_pos],[sentiment_neg],[sentiment_pos_amp],[sentiment_neg_amp],[sentiment_npr],[sentiment_npr_amp],[sentiment_npnr],[sentiment_sum_pos_neg],[sentiment_npr_alt],[diff_sentiment_raw],[diff_sentiment_pos],[diff_sentiment_neg],[diff_sentiment_pos_amp],[diff_sentiment_neg_amp],[diff_sentiment_npr],[diff_sentiment_npr_amp],[diff_sentiment_npnr],[diff_sentiment_sum_pos_neg],[diff_sentiment_npr_alt]
  FROM [BitcoinTwts].[dbo].Final_Lex_huliu_res
  UNION ALL
SELECT 'Lex_jockers' AS method,lag,[sentiment_raw],[sentiment_pos],[sentiment_neg],[sentiment_pos_amp],[sentiment_neg_amp],[sentiment_npr],[sentiment_npr_amp],[sentiment_npnr],[sentiment_sum_pos_neg],[sentiment_npr_alt],[diff_sentiment_raw],[diff_sentiment_pos],[diff_sentiment_neg],[diff_sentiment_pos_amp],[diff_sentiment_neg_amp],[diff_sentiment_npr],[diff_sentiment_npr_amp],[diff_sentiment_npnr],[diff_sentiment_sum_pos_neg],[diff_sentiment_npr_alt]
  FROM [BitcoinTwts].[dbo].Final_Lex_jockers_res
  UNION ALL
SELECT 'Lex_jockers_rinker' AS method,lag,[sentiment_raw],[sentiment_pos],[sentiment_neg],[sentiment_pos_amp],[sentiment_neg_amp],[sentiment_npr],[sentiment_npr_amp],[sentiment_npnr],[sentiment_sum_pos_neg],[sentiment_npr_alt],[diff_sentiment_raw],[diff_sentiment_pos],[diff_sentiment_neg],[diff_sentiment_pos_amp],[diff_sentiment_neg_amp],[diff_sentiment_npr],[diff_sentiment_npr_amp],[diff_sentiment_npnr],[diff_sentiment_sum_pos_neg],[diff_sentiment_npr_alt]
  FROM [BitcoinTwts].[dbo].Final_Lex_jockers_rinker_res
  UNION ALL
SELECT 'Lex_loughran_mcdonald' AS method,lag,[sentiment_raw],[sentiment_pos],[sentiment_neg],[sentiment_pos_amp],[sentiment_neg_amp],[sentiment_npr],[sentiment_npr_amp],[sentiment_npnr],[sentiment_sum_pos_neg],[sentiment_npr_alt],[diff_sentiment_raw],[diff_sentiment_pos],[diff_sentiment_neg],[diff_sentiment_pos_amp],[diff_sentiment_neg_amp],[diff_sentiment_npr],[diff_sentiment_npr_amp],[diff_sentiment_npnr],[diff_sentiment_sum_pos_neg],[diff_sentiment_npr_alt]
  FROM [BitcoinTwts].[dbo].Final_Lex_loughran_mcdonald_res
  UNION ALL
SELECT 'Lex_nrc_res' AS method,lag,[sentiment_raw],[sentiment_pos],[sentiment_neg],[sentiment_pos_amp],[sentiment_neg_amp],[sentiment_npr],[sentiment_npr_amp],[sentiment_npnr],[sentiment_sum_pos_neg],[sentiment_npr_alt],[diff_sentiment_raw],[diff_sentiment_pos],[diff_sentiment_neg],[diff_sentiment_pos_amp],[diff_sentiment_neg_amp],[diff_sentiment_npr],[diff_sentiment_npr_amp],[diff_sentiment_npnr],[diff_sentiment_sum_pos_neg],[diff_sentiment_npr_alt]
  FROM [BitcoinTwts].[dbo].Final_Lex_nrc_res
  UNION ALL
SELECT 'Lex_senticnet_res' AS method,lag,[sentiment_raw],[sentiment_pos],[sentiment_neg],[sentiment_pos_amp],[sentiment_neg_amp],[sentiment_npr],[sentiment_npr_amp],[sentiment_npnr],[sentiment_sum_pos_neg],[sentiment_npr_alt],[diff_sentiment_raw],[diff_sentiment_pos],[diff_sentiment_neg],[diff_sentiment_pos_amp],[diff_sentiment_neg_amp],[diff_sentiment_npr],[diff_sentiment_npr_amp],[diff_sentiment_npnr],[diff_sentiment_sum_pos_neg],[diff_sentiment_npr_alt]
  FROM [BitcoinTwts].[dbo].Final_Lex_senticnet_res
  UNION ALL
SELECT 'Lex_sentiword_res' AS method,lag,[sentiment_raw],[sentiment_pos],[sentiment_neg],[sentiment_pos_amp],[sentiment_neg_amp],[sentiment_npr],[sentiment_npr_amp],[sentiment_npnr],[sentiment_sum_pos_neg],[sentiment_npr_alt],[diff_sentiment_raw],[diff_sentiment_pos],[diff_sentiment_neg],[diff_sentiment_pos_amp],[diff_sentiment_neg_amp],[diff_sentiment_npr],[diff_sentiment_npr_amp],[diff_sentiment_npnr],[diff_sentiment_sum_pos_neg],[diff_sentiment_npr_alt]
  FROM [BitcoinTwts].[dbo].Final_Lex_sentiword_res
  UNION ALL
SELECT 'Lex_slangsd_res' AS method,lag,[sentiment_raw],[sentiment_pos],[sentiment_neg],[sentiment_pos_amp],[sentiment_neg_amp],[sentiment_npr],[sentiment_npr_amp],[sentiment_npnr],[sentiment_sum_pos_neg],[sentiment_npr_alt],[diff_sentiment_raw],[diff_sentiment_pos],[diff_sentiment_neg],[diff_sentiment_pos_amp],[diff_sentiment_neg_amp],[diff_sentiment_npr],[diff_sentiment_npr_amp],[diff_sentiment_npnr],[diff_sentiment_sum_pos_neg],[diff_sentiment_npr_alt]
  FROM [BitcoinTwts].[dbo].Final_Lex_slangsd_res
  UNION ALL
SELECT 'Lex_socal_google_res' AS method,lag,[sentiment_raw],[sentiment_pos],[sentiment_neg],[sentiment_pos_amp],[sentiment_neg_amp],[sentiment_npr],[sentiment_npr_amp],[sentiment_npnr],[sentiment_sum_pos_neg],[sentiment_npr_alt],[diff_sentiment_raw],[diff_sentiment_pos],[diff_sentiment_neg],[diff_sentiment_pos_amp],[diff_sentiment_neg_amp],[diff_sentiment_npr],[diff_sentiment_npr_amp],[diff_sentiment_npnr],[diff_sentiment_sum_pos_neg],[diff_sentiment_npr_alt]
  FROM [BitcoinTwts].[dbo].Final_Lex_socal_google_res
  UNION ALL
SELECT 'Lex_syuzhet_dict_res' AS method,lag,[sentiment_raw],[sentiment_pos],[sentiment_neg],[sentiment_pos_amp],[sentiment_neg_amp],[sentiment_npr],[sentiment_npr_amp],[sentiment_npnr],[sentiment_sum_pos_neg],[sentiment_npr_alt],[diff_sentiment_raw],[diff_sentiment_pos],[diff_sentiment_neg],[diff_sentiment_pos_amp],[diff_sentiment_neg_amp],[diff_sentiment_npr],[diff_sentiment_npr_amp],[diff_sentiment_npnr],[diff_sentiment_sum_pos_neg],[diff_sentiment_npr_alt]
  FROM [BitcoinTwts].[dbo].Final_Lex_syuzhet_dict_res
  UNION ALL
SELECT 'RF_sentiment_res' AS method,lag,[sentiment_raw],[sentiment_pos],[sentiment_neg],[sentiment_pos_amp],[sentiment_neg_amp],[sentiment_npr],[sentiment_npr_amp],[sentiment_npnr],[sentiment_sum_pos_neg],[sentiment_npr_alt],[diff_sentiment_raw],[diff_sentiment_pos],[diff_sentiment_neg],[diff_sentiment_pos_amp],[diff_sentiment_neg_amp],[diff_sentiment_npr],[diff_sentiment_npr_amp],[diff_sentiment_npnr],[diff_sentiment_sum_pos_neg],[diff_sentiment_npr_alt]
  FROM [BitcoinTwts].[dbo].Final_RF_sentiment_res
  UNION ALL
SELECT 'SVM_sentiment_res' AS method,lag,[sentiment_raw],[sentiment_pos],[sentiment_neg],[sentiment_pos_amp],[sentiment_neg_amp],[sentiment_npr],[sentiment_npr_amp],[sentiment_npnr],[sentiment_sum_pos_neg],[sentiment_npr_alt],[diff_sentiment_raw],[diff_sentiment_pos],[diff_sentiment_neg],[diff_sentiment_pos_amp],[diff_sentiment_neg_amp],[diff_sentiment_npr],[diff_sentiment_npr_amp],[diff_sentiment_npnr],[diff_sentiment_sum_pos_neg],[diff_sentiment_npr_alt]
  FROM [BitcoinTwts].[dbo].Final_SVM_sentiment_res