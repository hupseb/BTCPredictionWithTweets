/****** f1_tickeringSources ******/
/*********************************/
SELECT TOP (100) PERCENT A.cntsources, B.cnt15leftchars,
	   CAST(B.cnt15leftchars AS float) / CAST(A.cntsources AS float) AS percentage, A.source
FROM            dbo.tweetsBySource AS A INNER JOIN
                         dbo.distinctTwts15charsBySource AS B ON A.source = B.source
WHERE        (CAST(B.cnt15leftchars AS float) / CAST(A.cntsources AS float) < 0.0731707317073171)

/****** f2_tickeringUsers ******/
/*******************************/
SELECT TOP (100) PERCENT A.cntusers, B.cnt15leftchars,
	   CAST(B.cnt15leftchars AS float) / CAST(A.cntusers AS float) AS percentage, A.user_id
FROM            dbo.tweetsByUserId AS A INNER JOIN
                         dbo.distinctTwts15charsByUserId AS B ON A.user_id = B.user_id
WHERE        (CAST(B.cnt15leftchars AS float) / CAST(A.cntusers AS float) < 0.0731707317073171)

/****** f3_usersTweetSame2x2 ******/
/**********************************/
SELECT        COUNT(user_id) AS user_id_cnt, user_id
FROM            (SELECT        COUNT(text_url_cleaned) AS sametweets, text_url_cleaned, user_id,
				CONVERT(varchar(16), created_at, 105) AS created_at_day
                          FROM            dbo.duplicateTweets
                          GROUP BY text_url_cleaned, user_id, CONVERT(varchar(16), created_at, 105)
                          HAVING         (COUNT(text_url_cleaned) >= 2)) AS A
GROUP BY user_id
HAVING        (COUNT(user_id) > 2)

/****** f4_bannedUsers  ******/
/*****************************/
SELECT TOP (100) PERCENT [user_id]
FROM [BitcoinTwts].[dbo].[f4_bannedUsers]


/****** f5_lowAmountToFavRate  ******/
/************************************/
SELECT        A.user_id, A.amountTweets, B.sumFav, C.sumRetweet,
			  CAST(B.sumFav AS float) / CAST(A.amountTweets AS float) AS Amount2Favs,
			  CAST(C.sumRetweet AS float) / CAST(A.amountTweets AS float) AS Amount2Retweet
FROM            (SELECT        COUNT(user_id) AS amountTweets, user_id
                          FROM            dbo.BitcoinTweets
                          GROUP BY user_id) AS A INNER JOIN
                             (SELECT        SUM(favorite_count) AS sumFav, user_id
                               FROM            dbo.BitcoinTweets AS BitcoinTweets_2
                               GROUP BY user_id) AS B ON A.user_id = B.user_id INNER JOIN
                             (SELECT        SUM(retweet_count) AS sumRetweet, user_id
                               FROM            dbo.BitcoinTweets AS BitcoinTweets_1
                               GROUP BY user_id) AS C ON A.user_id = C.user_id

/****** f6_spamHashtags  ******/
/******************************/
SELECT        status_id, created_at, user_id, text_url_cleaned, source, lang, coordinates, is_quote, favorite_count,
				retweet_count, quoted_status_id, retweet_status_id, hashtags, symbols
FROM            dbo.BitcoinTweetsUnion
WHERE        (text_url_cleaned LIKE '%#escorts%') OR
                         (text_url_cleaned LIKE '%#airdrop%') OR
                         (text_url_cleaned LIKE '%#casino%') OR
                         (text_url_cleaned LIKE '%#followback%') OR
                         (text_url_cleaned LIKE '%#tokensale%') OR
                         (text_url_cleaned LIKE '%#crowdsale%') OR
                         (text_url_cleaned LIKE '%#bethereum%') OR
                         (text_url_cleaned LIKE '%#freecoins%') OR
                         (text_url_cleaned LIKE '%#freetoken%') OR
                         (text_url_cleaned LIKE '%#freemoney%') OR
						 (text_url_cleaned LIKE '%#ICO%') OR
						  -- ...
                         (text_url_cleaned LIKE '%Bitkong%')

/****** f7_followbackBots  ******/
/********************************/
SELECT        status_id, created_at, user_id, text_url_cleaned, source, lang, coordinates, is_quote, favorite_count,
				retweet_count, quoted_status_id, retweet_status_id, hashtags, symbols
FROM            dbo.BitcoinTweetsUnion
WHERE        (user_id IN
                             (SELECT        user_id
                               FROM            dbo.BitcoinTweets
                               WHERE        (description LIKE '%followback%')
                               UNION ALL
                               SELECT        user_id
                               FROM            dbo.f7_usersOutDesc
                               WHERE        (description LIKE '%followback%')))

/****** f8_bannedOrDeletedAccounts  ******/
/*****************************************/
SELECT        status_id, created_at, user_id, text_url_cleaned, source, lang, coordinates, is_quote, favorite_count,
				retweet_count, quoted_status_id, retweet_status_id, hashtags, symbols
FROM            dbo.BitcoinTweetsUnion
WHERE        (user_id IN
                             (SELECT        user_id
                               FROM            dbo.f8_bannedOrDeletedByTwitter))

/****** BitcoinTweetsUnion  ******/
/*********************************/
SELECT        status_id, created_at, user_id, text_url_cleaned, source, lang, coords_coords AS coordinates, is_quote,
				favorite_count, retweet_count, quoted_status_id, retweet_status_id, hashtags, symbols
FROM            dbo.BitcoinTweets
UNION ALL
SELECT        id_str AS status_id, created_at, user_id_str AS user_id, text_url_cleaned, source, lang, coordinates,
				is_quote_status AS is_quote, favorite_count, retweet_count, quoted_status_id_str AS quoted_status_id, 
                         retweeted_status_id_str AS retweeted_status_id, hashtags, symbols
FROM            dbo.BitcoinTweetsArchived

/****** BitcoinTweetsUnion_Filtered  ******/
/******************************************/
SELECT        status_id, created_at, user_id, text_url_cleaned, source, lang, coordinates, is_quote, favorite_count,
				retweet_count, quoted_status_id, retweet_status_id, hashtags, symbols
FROM            dbo.BitcoinTweetsUnion
WHERE        (status_id NOT IN
                             (SELECT        status_id
                               FROM            dbo.f1_tickeringSources_t
                               UNION ALL
                               SELECT        status_id
                               FROM            dbo.f2_tickeringUsers_t
                               UNION ALL
                               SELECT        status_id
                               FROM            dbo.f3_usersTweetSame2x2_t
                               UNION ALL
                               SELECT        status_id
                               FROM            dbo.f4_bannedUsers_t
                               UNION ALL
                               SELECT        status_id
                               FROM            dbo.f6_spamHashtags_t
                               UNION ALL
                               SELECT        status_id
                               FROM            dbo.f7_followbackBots_t
                               UNION ALL
                               SELECT        status_id
                               FROM            dbo.f8_bannedOrDeletedAccounts_t))