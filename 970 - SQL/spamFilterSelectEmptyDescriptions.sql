SELECT [screen_name], [description] FROM [BitcoinAnalysis].[dbo].[usersOutDescription]
WHERE [screen_name] NOT IN (SELECT [screen_name] FROM [usersOutDesc_bannedOrDeleted])
AND [screen_name] NOT IN (SELECT [screen_name] FROM [usersOutDesc]) ORDER BY [screen_name]

--those screen names are confused by user ids --> rtweet is doing a bad job here
--100777001
--112900552
--19899595
--201210060
--253364091
--403097783
--445139099
--023367100
--0720253934
--0x0090
--0x00D
--0x1010101010101
--0x4c41
--0x72d8
--0xca333
--0xcea5ed1015