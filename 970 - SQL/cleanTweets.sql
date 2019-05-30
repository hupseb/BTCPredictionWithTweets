USE [iPhoneXReleaseTweets]

--remove links
UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text],'(http|ftp|https)://([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?','')

--remove double spaces
UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'\s\s',' ')

--replace smileys
UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0080>)', N'😀')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0081>)', N'😁')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0082>)', N'😂')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0083>)', N'😃')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0084>)', N'😄')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0085>)', N'😅')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0086>)', N'😆')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0089>)', N'😉')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+008A>)', N'😊')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+008B>)', N'😋')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+008E>)', N'😎')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+008D>)', N'😍')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0098>)', N'😘')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0097>)', N'😗')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0099>)', N'😙')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+009A>)', N'😚')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<U\+263A><U\+FE0F>)', N'☺')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0099><U\+0082>)', N'🙂')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0090>)', N'😐')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0091>)', N'😑')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00B6>)', N'😶')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0099><U\+0084>)', N'🙄')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+008F>)', N'😏')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00A3>)', N'😣')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00A5>)', N'😥')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00AE>)', N'😮')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00AF>)', N'😯')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00AA>)', N'😪')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00AB>)', N'😫')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00B4>)', N'😴')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+008C>)', N'😌')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+009B>)', N'😛')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+009C>)', N'😜')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+009D>)', N'😝')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0092>)', N'😒')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0093>)', N'😓')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0094>)', N'😔')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0095>)', N'😕')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0099><U\+0083>)', N'🙃')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00B2>)', N'😲')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0099><U\+0081>)', N'🙁')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+0096>)', N'😖')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+009E>)', N'😞')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+009F>)', N'😟')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00A4>)', N'😤')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00A2>)', N'😢')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00AD>)', N'😭')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00A6>)', N'😦')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00A7>)', N'😧')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00A8>)', N'😨')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00A9>)', N'😩')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00AC>)', N'😬')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00B0>)', N'😰')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00B1>)', N'😱')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00B3>)', N'😳')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00B5>)', N'😵')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00A1>)', N'😡')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0098><U\+00A0>)', N'😠')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+008E><U\+0089>)', N'🎉')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0099><U\+008C>)', N'🙌')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<U\+2764><U\+FE0F>)', N'❤️')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+008D><U\+00BE>)', N'🍾')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<U\+270C><U\+FE0F>)', N'✌')

UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+008E><U\+008A>)', N'🎊')	
	
UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0><U\+009F><U\+0091><U\+008F>)', N'👏')		
	
--remove left <f0>
UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<f0>)', '')

--remove left unicode chars
UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(<U\+\w\w\w\w>)', '')

--remove double spaces
UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'\s\s',' ')

--replace &amp;
UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(&amp;)', '&')

--replace &lt;
UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(&lt;)', '<')

--replace &gt;
UPDATE [iPhoneXReleaseTweets].[dbo].[Tweets]
SET [text_url_cleaned] = [iPhoneXReleaseTweets].[dbo].RgxReplace([text_url_cleaned],'(&gt;)', '>')


