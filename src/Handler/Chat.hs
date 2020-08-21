{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Chat where

import           Web.Spock
import           Web.Spock.Config
import           Responses
import           Network.HTTP.Types             ( badRequest400 )
import           Data.Text               hiding ( unfoldr
                                                , take
                                                , lines
                                                , length
                                                , map
                                                )
import qualified Data.Text                     as T
import           Data.List                      ( unfoldr )
import           Data.Bson                      ( val
                                                , (=:)
                                                )
import           Data.HVect              hiding ( length
                                                , (!!)
                                                )
import           Data.Time.Clock
import           Model                          ( User(..)
                                                , Lobby(..)
                                                , Chat(..)
                                                )
import           Model.Utils
import           Control.Monad.Trans
import           Controller.Lobby               ( findLobbyById
                                                , generateSalt
                                                )
import           Controller.Utils               ( parseBody )
import           Data.ByteString.Lazy           ( fromStrict
                                                , toStrict
                                                , ByteString
                                                )

import           Data.Aeson                     ( encode )
import           Data.Aeson.Types               ( Parser
                                                , (.:)
                                                )
import           Database.MongoDB               ( Pipe )
import           Control.Monad.Trans.Reader     ( ReaderT )

import           Controller.Utils               ( getCurretISOTime )

handleSendMessage :: (ListContains n User xs) => String -> AppHandle (HVect xs) ()
handleSendMessage _chatID = text $ T.pack _chatID
