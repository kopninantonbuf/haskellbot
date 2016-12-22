-- Подключаем расширение Overloaded Strings для того, чтобы можно было
-- использовать библиотеки Data.Text, Data.ByteString.Char8, Network.HTTP.Simple.
{-# LANGUAGE OverloadedStrings #-}

-- Подключаем TemplateHaskell, чтобы пользоваться библиотекой Data.Aeson.TH.
{-# LANGUAGE TemplateHaskell #-}

module Hoogle where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid ((<>))

import qualified Data.List as L (foldl1')

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

-- Структура описания одной функции.
data HoogleResult = HoogleResult { self :: Text, docs :: Text, location :: Text }
$(deriveJSON defaultOptions ''HoogleResult)

-- Структура ответа с Hoogle.org в виде списка описаний функций.
data HoogleResponse = HoogleResponse { results :: [HoogleResult] }
$(deriveJSON defaultOptions ''HoogleResponse)

-- Функция, которая делает запрос на Hoogle.org, получает ответ в формате JSON и
-- возвращает тело ответа в виде массива функций.
hoogle :: (MonadThrow m, MonadIO m) => Text -> Int -> m HoogleResponse
hoogle query count = do
  req <- makeRequest (encodeUtf8 query) count
  getResponseBody <$> httpJSON req

-- Функция, которая формирует запрос на Hoogle.org.
makeRequest :: (MonadThrow m) => ByteString -> Int -> m Request
makeRequest queryText count = do
  let qs = [("mode", "json"), ("hoogle", queryText), ("start", pack (show 0)), ("count", pack (show count))]
  parseRequest $ unpack $ "http://www.haskell.org/hoogle/" <> renderSimpleQuery True qs

-- Функция, проходящая по списку ответов с Hoogle.org и обрабатывающая описание каждой функции с помощью функции formatHoogleResult.
hoogleResults :: [HoogleResult] -> Text
hoogleResults = L.foldl1' (\x y -> x <> "  \n" <> y) . map (("=====================================\n" <>) . hoogleResult)

-- Функция, обрабатывающая описание каждой функции.
hoogleResult :: HoogleResult -> Text
hoogleResult res = "-- " <> self res <> "  \n\nDescription: \n" <> docs res <> " \n" <> location res <> "\n"
