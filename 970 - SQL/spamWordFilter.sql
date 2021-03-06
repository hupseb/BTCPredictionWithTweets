/****** Script for SelectTopNRows command from SSMS  ******/
USE [BitcoinAnalysis]

SELECT A.cntspamtweets, B.[cntusers] AS totaltweets, CAST(A.cntspamtweets AS float) / CAST(B.[cntusers] AS float) AS spamwordratio, A.[user_id] FROM
(SELECT COUNT([user_id]) as cntspamtweets, [user_id]
  FROM [BitcoinAnalysis].[dbo].[00. BitcoinTweets_d]
  WHERE [text_url_cleaned] LIKE '%#ICO%'
  GROUP BY [user_id]) AS A
INNER JOIN [dbo].[ldu_tweetsbyuserid 00.] AS B
ON A.[user_id] = B.[user_id]
ORDER BY spamwordratio DESC

-- Search in for other spam #hashtags [BitcoinTwts].[dbo].[f1_tickeringSources]
-- #ico
-- #gambling
-- #escort
-- #airdrop

cntspamtweets	totaltweets	spamwordratio	user_id --> HARDCORE SPAM
10	14	0.714285714285714	966666394610761729

cntspamtweets	totaltweets	spamwordratio	user_id --> ACCOUNT SUSPENDED
5	7	0.714285714285714	495047084

cntspamtweets	totaltweets	spamwordratio	user_id --> HARDCORE SPAM
5	7	0.714285714285714	71761089

cntspamtweets	totaltweets	spamwordratio	user_id --> NO SPAM, BUT BULLSHIT POSTS ONLY
5	7	0.714285714285714	947877246114689030

cntspamtweets	totaltweets	spamwordratio	user_id --> SPAM
5	7	0.714285714285714	878292281513484289

cntspamtweets	totaltweets	spamwordratio	user_id --> NO SPAM, BUT BULLSHIT POSTS ONLY
10	14	0.714285714285714	2282106716

cntspamtweets	totaltweets	spamwordratio	user_id --> ACCOUNT SUSPENDED
5	7	0.714285714285714	892005746069520385

cntspamtweets	totaltweets	spamwordratio	user_id --> NO SPAM, BUT BULLSHIT POSTS ONLY
10	14	0.714285714285714	765629910

cntspamtweets	totaltweets	spamwordratio	user_id --> SPAM
5	7	0.714285714285714	822451839538970624

cntspamtweets	totaltweets	spamwordratio	user_id --> SPAM, only LEVELNET not a single tweet about bitcoin
40	56	0.714285714285714	750072191220199424

--------------------------------------------------------------------------------------------------------------
average is 0.711891912 (maybe average + 3% or 4%)
--------------------------------------------------------------------------------------------------------------

cntspamtweets	totaltweets	spamwordratio	user_id
44	62	0.709677419354839	898074505834188800		--> spam, but has some tweets about bitcoin

cntspamtweets	totaltweets	spamwordratio	user_id --> SPAM and "followback" in the description
22	31	0.709677419354839	303129688

cntspamtweets	totaltweets	spamwordratio	user_id --> SPAM, only about "airdrop"
73	103	0.70873786407767	946740932799541249

cntspamtweets	totaltweets	spamwordratio	user_id --> SPAM
17	24	0.708333333333333	97085375

cntspamtweets	totaltweets	spamwordratio	user_id --> SPAM
53	75	0.706666666666667	401708744

cntspamtweets	totaltweets	spamwordratio	user_id --> still SPAM, lot of same tweets
17	25	0.68	848576154944454656

cntspamtweets	totaltweets	spamwordratio	user_id --> SUSPENDED
4	6	0.666666666666667	956158262319104001

cntspamtweets	totaltweets	spamwordratio	user_id --> SPAM, 100% #airdrop
3	5	0.6	3310913682

cntspamtweets	totaltweets	spamwordratio	user_id --> SUSPENDED
14	32	0.4375	2743123834

cntspamtweets	totaltweets	spamwordratio	user_id --> NO SPAM
43	103	0.41747572815534	121909341

cntspamtweets	totaltweets	spamwordratio	user_id --> Bethereum account, only about Bethereum
52	125	0.416	913001325268553728

cntspamtweets	totaltweets	spamwordratio	user_id --> NO SPAM and informative tweets
39	94	0.414893617021277	29748468




cntspamtweets	totaltweets	spamwordratio	user_id --> NO SPAM
19	46	0.41304347826087	2746556975

cntspamtweets	totaltweets	spamwordratio	user_id --> According to botcheck spam, but has only funny pictures
26	63	0.412698412698413	964313040098414592

cntspamtweets	totaltweets	spamwordratio	user_id --> NO SPAM
33	80	0.4125	948955830040317953

cntspamtweets	totaltweets	spamwordratio	user_id --> SPAM
14	34	0.411764705882353	870975957125013504

cntspamtweets	totaltweets	spamwordratio	user_id --> NO SPAM
7	17	0.411764705882353	941141293924630530

cntspamtweets	totaltweets	spamwordratio	user_id --> SPAM, many followback accounts
7	17	0.411764705882353	14232409

cntspamtweets	totaltweets	spamwordratio	user_id --> SPAM, MEXC account
57	140	0.407142857142857	956243570582634496

---------------------------------------------------------------------------------

cntspamtweets	totaltweets	spamwordratio	user_id --> NO SPAM, private consulting account, but no good tweets
16	41	0.390243902439024	25828514

cntspamtweets	totaltweets	spamwordratio	user_id --> SUSPENDED
38	98	0.387755102040816	1694332273

cntspamtweets	totaltweets	spamwordratio	user_id --> SPAM
38	98	0.387755102040816	952262877859098624

---------------------------------------------------------------------------------

cntspamtweets	totaltweets	spamwordratio	user_id --> NO SPAM, even has own twitter account
44	126	0.349206349206349	1388071831

cntspamtweets	totaltweets	spamwordratio	user_id --> SPAM
22	63	0.349206349206349	959784859815182338

cntspamtweets	totaltweets	spamwordratio	user_id --> SPAM
37	106	0.349056603773585	1413062772



