USE [CryptoCurrTweets]
GO

/****** Object:  Table [dbo].[Tweets]    Script Date: 2/6/2018 10:14:53 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[BitcoinTweets](
	[rownames] [nvarchar](512) NULL,
	[status_id] [bigint] NOT NULL,
	[created_at] [datetime] NULL,
	[user_id] [bigint] NULL,
	[screen_name] [nvarchar](64) NULL,
	[text] [nvarchar](4000) NULL,
	[text_url_cleaned] [nvarchar](4000) NULL,
	[source] [nvarchar](128) NULL,
	[reply_to_status_id] [nvarchar](512) NULL,
	[reply_to_user_id] [nvarchar](512) NULL,
	[reply_to_screen_name] [nvarchar](512) NULL,
	[is_quote] [nvarchar](5) NULL,
	[is_retweet] [nvarchar](5) NULL,
	[favorite_count] [int] NULL,
	[retweet_count] [int] NULL,
	[hashtags] [varchar](4000) NULL,
	[symbols] [nvarchar](512) NULL,
	[urls_url] [nvarchar](512) NULL,
	[urls_tco] [nvarchar](512) NULL,
	[urls_expanded_url] [nvarchar](1024) NULL,
	[media_url] [nvarchar](512) NULL,
	[media_tco] [nvarchar](512) NULL,
	[media_expanded_url] [nvarchar](512) NULL,
	[media_type] [nvarchar](512) NULL,
	[ext_media_url] [nvarchar](512) NULL,
	[ext_media_tco] [nvarchar](512) NULL,
	[ext_media_expanded_url] [nvarchar](512) NULL,
	[ext_media_type] [nvarchar](50) NULL,
	[mentions_user_id] [nvarchar](1024) NULL,
	[mentions_screen_name] [nvarchar](1024) NULL,
	[lang] [nvarchar](512) NULL,
	[quoted_status_id] [bigint] NULL,
	[quoted_text] [nvarchar](4000) NULL,
	[retweet_status_id] [bigint] NULL,
	[retweet_text] [nvarchar](4000) NULL,
	[place_url] [nvarchar](512) NULL,
	[place_name] [nvarchar](512) NULL,
	[place_full_name] [nvarchar](512) NULL,
	[place_type] [nvarchar](512) NULL,
	[country] [nvarchar](512) NULL,
	[country_code] [nvarchar](512) NULL,
	[geo_coords] [nvarchar](512) NULL,
	[coords_coords] [nvarchar](512) NULL,
	[bbox_coords] [nvarchar](512) NULL,
 CONSTRAINT [PK_Tweets] PRIMARY KEY CLUSTERED 
(
	[status_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO


