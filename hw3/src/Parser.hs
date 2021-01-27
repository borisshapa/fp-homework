module Parser
  (
    -- * Parser types
    Command(..)

    -- * Parser functions
  , parse
  ) where

import Options.Applicative

data Command
    = Cd FilePath
    | Dir
    | Ls FilePath
    | CreateFolder String
    | Cat String
    | CreateFile String
    | Remove FilePath
    | WriteFile FilePath String
    | FindFile String
    | Information FilePath
    | Help
    deriving (Show, Eq)

-- | Adds metainfoemation to parser result
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- | parse 'cd' command
parseCd :: Parser Command
parseCd = Cd <$> argument str (metavar "DIR")

-- | parse 'dir' command
parseDir :: Parser Command
parseDir = pure Dir

-- | parse 'ls' command
parseLs :: Parser Command
parseLs = Ls <$> argument str (metavar "DIR")

-- | parse 'create-folder' command
parseCreateFolder :: Parser Command
parseCreateFolder = CreateFolder <$> argument str (metavar "FOLDER NAME")

-- | parse 'cat' command
parseCat :: Parser Command
parseCat = Cat <$> argument str (metavar "FILE NAME")

-- | parse 'create-file' command
parseCreateFile :: Parser Command
parseCreateFile = CreateFile <$> argument str (metavar "FILE NAME")

-- | parse 'remove' command
parseRemove :: Parser Command
parseRemove = Remove <$> argument str (metavar "FILE/DIR NAME")

-- | parse 'write-file' command
parseWriteFile :: Parser Command
parseWriteFile = WriteFile
                <$> argument str (metavar "FILE NAME")
                <*> argument str (metavar "STRING")

-- | parse 'find-file' command
parseFindFile :: Parser Command
parseFindFile = FindFile <$> argument str (metavar "FILE NAME")

-- | parse 'information' command
parseInformation :: Parser Command
parseInformation = Information <$> argument str (metavar "FILE/DIR NAME")

-- | parse 'help' command
parseHelp :: Parser Command
parseHelp = pure Help

-- | parse CLI commands
parseCommand :: Parser Command
parseCommand = subparser $
  command "cd" (parseCd `withInfo` "Go to directory") <>
  command "dir" (parseDir 
    `withInfo` "Show the contents of the current directory") <>
  command "ls" (parseLs 
    `withInfo` "Show the contents of the selected directory") <>
  command "create-folder" (parseCreateFolder 
    `withInfo` "Create directory in current") <>
  command "cat" (parseCat `withInfo` "Show file content") <>
  command "create-file" (parseCreateFile 
    `withInfo` "Create an empty file in the current directory") <>
  command "remove" (parseRemove
    `withInfo` "Delete selected directory and file") <>
  command "write-file" (parseWriteFile `withInfo` "Write text to file") <>
  command "find-file" (parseFindFile `withInfo`
    "Search for a file in the current directory and subdirectories") <>
  command "information" (parseInformation
    `withInfo` "Show information about a file or directory") <>
  command "help" (parseHelp `withInfo` "Show usage guide")

-- | parse input
parse :: [String] -> ParserResult Command
parse tokens = execParserPure
  (prefs showHelpOnError)
  (parseCommand `withInfo` "Enter command")
  tokens