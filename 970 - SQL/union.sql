SELECT        status_id, created_at, user_id, text, source, lang, coords_coords AS coordinates,
			  is_quote, favorite_count, retweet_count,
			  quoted_status_id, retweet_status_id,		  
			  hashtags, symbols
FROM dbo.BitcoinTweets
UNION ALL
SELECT id AS status_id, created_at, user_id, full_text AS text, source, lang, coordinates, 
                         is_quote_status as is_quote, favorite_count, retweet_count, 
						 quoted_status_id, retweeted_status_id,
                         hashtags, symbols
FROM dbo.BitcoinTweetsArchived