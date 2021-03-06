USE [iPhoneRelease]
GO
/****** Object:  Trigger [dbo].[textUrlCleaner]    Script Date: 11/4/2017 12:57:40 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
ALTER TRIGGER [dbo].[textUrlCleaner]
   ON  [iPhoneRelease].[dbo].[Tweets]
   AFTER INSERT, UPDATE
AS 
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for trigger here
	UPDATE [iPhoneRelease].dbo.Tweets
	SET [text_url_cleaned] = ([dbo].[RgxReplace]([text],N'(http|ftp|https)://([\w_-]+(?:(?:\.[\w_-]+)+))([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?',N''))

END
