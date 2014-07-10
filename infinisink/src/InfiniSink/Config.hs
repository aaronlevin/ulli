{-# LANGUAGE OverloadedStrings #-}

module InfiniSink.Config ( postgresConfigParser
, Config (Config)
, configParser
, infinisinkOpts
, PostgresConfig (PostgresConfig)
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Monoid ((<>))
import Options.Applicative ( fullDesc
                           , info
                           , header
                           , help
                           , helper
                           , long
                           , metavar
                           , Parser
                           , ParserInfo
                           , progDesc
                           , strOption
                           )

-- | Configuration for the app
type AppPort = String
type PGHostname = String
type PGUsername = String
type PGPassword = String
type PGPort = String

data PostgresConfig = PostgresConfig PGHostname PGPort PGUsername PGPassword
                    deriving Show

data Config = Config PostgresConfig AppPort
            deriving Show

postgresConfigParser :: Parser PostgresConfig
postgresConfigParser = PostgresConfig 
                   <$> strOption 
                     ( long "pghostname"
                     <> metavar "HOSTNAME"
                     <> help "hostname for the PostgreSQL database" )
                   <*> strOption
                     ( long "pgport"
                     <> metavar "PORT"
                     <> help "port of the PostgreSQL database" )
                   <*> strOption
                     ( long "pgusername"
                     <> metavar "USERNAME"
                     <> help "username" )
                   <*> strOption
                     ( long "pgpassword"
                     <> metavar "PASSWORD"
                     <> help "password" )


configParser :: Parser Config
configParser = Config <$> postgresConfigParser 
                      <*> strOption
                        ( long "infiniport"
                        <> metavar "INFINI_PORT"
                        <> help "port to run the InfiniSink REST client on")

infinisinkOpts :: ParserInfo Config
infinisinkOpts = info (helper <*> configParser)
                  ( fullDesc
                  <> progDesc "REST Server for InfiniSink"
                  <> header "InfiniSink - REST Client" )
