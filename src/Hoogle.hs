{-# LANGUAGE OverloadedStrings #-}

-- Подключаем TemplateHaskell, чтобы пользоваться библиотекой Data.Aeson.TH.
{-# LANGUAGE TemplateHaskell #-}

module Hoogle where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid ((<>))

-- Библиотека для автоматической генерации экземпляров класса ToJSON и FromJSON
-- для заданного типа данных на этапе компиляции.
import Data.Aeson.TH (deriveJSON, defaultOptions)

-- Эффективная по времени и памяти реализация текста Unicode. Чтобы
-- не использовать String.
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

-- Библиотека позволяет работать с типом ByteString, используя функции Char.
import Data.ByteString.Char8 (ByteString, unpack, pack)

-- API для работы с протоколом HTTP.
import Network.HTTP.Simple (Request, parseRequest, httpJSON, getResponseBody)
import Network.HTTP.Types.URI (renderSimpleQuery)

-- Структура, формирующая результат.
data HoogleResult = HoogleResult { self :: Text, docs :: Text, location :: Text }
$(deriveJSON defaultOptions ''HoogleResult)

-- Структура ответа.
data HoogleResponse = HoogleResponse { results :: [HoogleResult] }
$(deriveJSON defaultOptions ''HoogleResponse)

-- Функция, которая делает запрос на Hoogle.org  и возвращает ответ.
hoogle :: (MonadThrow m, MonadIO m) => Text -> Int -> m HoogleResponse
hoogle query count = do
  req <- makeRequest (encodeUtf8 query) count
  getResponseBody <$> httpJSON req

-- Функция, которая формирует запрос на Hoogle.org.
makeRequest :: (MonadThrow m) => ByteString -> Int -> m Request
makeRequest queryText count = do
  let qs = [ ("mode", "json"), ("hoogle", queryText), ("start", pack (show 0)), ("count", pack (show count))]
  parseRequest $ unpack $ "http://www.haskell.org/hoogle/" <> renderSimpleQuery True qs
