SELECT        created_at, id_str, user_id_str, full_text, lang, source, truncated, coordinates, place_id, contributors, quote_count, in_reply_to_status_id_str, in_reply_to_user_id_str, in_reply_to_screen_name, reply_count, retweet_count, 
                         favorite_count, favorited, is_quote_status, quoted_status_id_str, retweeted, retweeted_status_id_str, symbols, hashtags, text_url_cleaned
FROM            dbo.BitcoinTweetsIntr
WHERE        (id_str NOT IN
                             (SELECT        id_str
                               FROM            dbo.f1_tickeringSources_t_intr
                               UNION ALL
                               SELECT        id_str
                               FROM            dbo.f2_tickeringUsers_t_intr
                               UNION ALL
                               SELECT        id_str
                               FROM            dbo.f3_usersTweetSame2x2_t_intr
                               UNION ALL
                               SELECT        id_str
                               FROM            dbo.f4_bannedUsers_t_intr
                               UNION ALL
                               SELECT        id_str
                               FROM            dbo.f6_spamHashtags_t_intr
                               UNION ALL
                               SELECT        id_str
                               FROM            dbo.f7_followbackBots_t_intr
                               UNION ALL
                               SELECT        id_str
                               FROM            dbo.f8_bannedOrDeletedAccounts_t_intr
							   ))