USE [master]

select case [value]
        when 0 then 'You NEED to run the sp_configure.'
        else 'You do NOT need to run the sp_configure.'
        end
from sys.configurations
where name = 'clr enabled';
go

sp_configure @configname=clr_enabled, @configvalue=1
go
reconfigure
go

use master;
grant external access assembly to [desktop-o4hdlva\hupse]; -- The domain\login of the original user.
go

-- This allows the CLR stored procedures to have external_access rights.
alter database [BitcoinTwts] set trustworthy on;
go


USE [BitcoinTwts]
declare @dllPath nvarchar(255) = 'C:\Program Files\Microsoft SQL Server\MSSQL14.SQLEXPRESS\MSSQL\Binn\SqlRegex.dll'
create assembly SqlRegex from @dllPath with permission_set = external_access;
go

 
CREATE FUNCTION dbo.RgxTrim(@Text NVARCHAR(max))
RETURNS NVARCHAR(MAX) 
WITH EXECUTE AS CALLER
EXTERNAL NAME SqlRegex.UserDefinedFunctions.RgxTrim;
GO
 
CREATE FUNCTION [dbo].[RgxReplace](@Text [NVARCHAR](MAX), @pattern [NVARCHAR](MAX), @replacement [NVARCHAR](MAX))
RETURNS [NVARCHAR](MAX) WITH EXECUTE AS CALLER
AS 
EXTERNAL NAME [SqlRegex].[UserDefinedFunctions].[RgxReplace]
GO


