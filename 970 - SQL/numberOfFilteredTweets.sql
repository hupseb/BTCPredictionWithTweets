SELECT        count(status_id) AS numberOf
FROM            dbo.BitcoinTweetsUnion
WHERE        (created_at >= CONVERT(datetime2, '2018-01-28 00:00:00.000')) AND (created_at < CONVERT(datetime2, '2018-06-10 00:00:00.000'))

SELECT        count(status_id) AS numberOfFiltered
FROM            dbo.BitcoinTweetsUnion_Filtered
WHERE        (created_at >= CONVERT(datetime2, '2018-01-28 00:00:00.000')) AND (created_at < CONVERT(datetime2, '2018-06-10 00:00:00.000'))

SELECT        count(status_id) AS numberOfOOS
FROM            dbo.BitcoinTweetsUnion
WHERE        (created_at >= CONVERT(datetime2, '2018-06-10 00:00:00.000')) AND (created_at < CONVERT(datetime2, '2018-10-21 00:00:00.000'))

SELECT        count(status_id) AS numberOfFilteredOOS
FROM            dbo.BitcoinTweetsUnion_Filtered
WHERE        (created_at >= CONVERT(datetime2, '2018-06-10 00:00:00.000')) AND (created_at < CONVERT(datetime2, '2018-10-21 00:00:00.000'))

----------------------------------------------------------------------------------------------------------------------------------------------

SELECT        count(distinct user_id) as distinctActiveUsers
FROM            dbo.BitcoinTweetsUnion
WHERE        (created_at >= CONVERT(datetime2, '2018-01-28 00:00:00.000')) AND (created_at < CONVERT(datetime2, '2018-06-10 00:00:00.000'))

SELECT        count(distinct user_id) as distinctActiveUsersOOS
FROM            dbo.BitcoinTweetsUnion
WHERE        (created_at >= CONVERT(datetime2, '2018-06-10 00:00:00.000')) AND (created_at < CONVERT(datetime2, '2018-10-21 00:00:00.000'))

