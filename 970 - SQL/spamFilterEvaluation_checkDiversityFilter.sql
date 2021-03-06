USE [BitcoinAnalysis]

SELECT        TOP (100) PERCENT A.cntusers, B.cnt15leftchars, CAST(B.cnt15leftchars AS float) / CAST(A.cntusers AS float) AS percentage, A.user_id
FROM            dbo.[ldu_tweetsbyuserid 00.] AS A INNER JOIN
                         dbo.[ldu_distinctTwts15chars 00.] AS B ON A.user_id = B.user_id
--WHERE        (CAST(B.cnt15leftchars AS float) / CAST(A.cntusers AS float) < 0.1952)
--WHERE        (CAST(B.cnt15leftchars AS float) / CAST(A.cntusers AS float) < 0.42)
AND A.user_id = 952840735698468864

-- of user shows up hear then filter is working