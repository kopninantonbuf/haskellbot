{-# LANGUAGE OverloadedStrings #-}

module Main where

-- программная транзакционная память, воу!
--import Control.Concurrent
import Control.Concurrent.STM 
import Control.Concurrent.STM.TVar (writeTVar, newTVar, readTVar)

-- для обработки ошибок
import Control.Error.Util (hoistMaybe, isJustT)


import Control.Monad --(void, when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)

--модные библиотеки для потоковых данных
import Data.Conduit (Source, (=$=), ($$))
import qualified Data.Conduit.Combinators as DC (concatMap, repeatM, mapM_)

import qualified Data.List as L (find, foldl1')
import Data.Monoid ((<>))

--стринги это не круто, поэтому Data.Text
import Data.Text (pack)
import qualified Data.Text as T (take, drop, length)

--низкоуровневый API для хттп
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

--телеграмный API
import Web.Telegram.API.Bot

--модуль для парсинга http://www.haskell.org/hoogle/
import Hoogle

-- 
type CountFuncUser = TVar Integer

--Список пар, хранящий количество показываемых функций для каждого пользователя
--type ListUserCount = [(T.Text, Integer)]

--Получение настроек по idChat
--getCount :: ListUserCount -> T.Text -> Integer
--getCount arrData idChat = snd $ head $ filter (\x -> x == (idChat, _)) arrData

--atomRead :: TVar Integer -> IO Integer
atomRead = atomically . readTVar
appVH a x = atomically $ readTVar a >>= writeTVar x 

countMy :: Int
countMy = 2 --atomically $ newTVar 5	

myMain x = do
	  shared <- atomically $ newTVar countMy
	  b <- atomically $ newTVar x
          before <- atomRead shared
          --putStrLn $ "Before: " ++ show before
          appVH b shared 
          after <- atomRead shared
          --putStrLn $ "After: " ++ show after
 
main :: IO ()
main = do

  -- менеджер хттп-соединения с сервером телеграма по безопасному протоколу(tls)
  manager <- newManager tlsManagerSettings
  -- токен бота
  let token = Token "bot319624564:AAE_fb6q_eTI942c4K7wpC4kNReC28939RI"
  -- основная функция
  -- нафинги нужны для getUpdates
  -- оператор $$ передает данные из botUpdates в processUpdate, вообще офигеть
  botUpdates token Nothing Nothing Nothing manager $$ DC.mapM_ (processUpdate token manager)

-- приём входящих сообщений
-- offset - идентификатор входящих сообщений (с каждым сообщением увеличивается на 1)
-- limit - макс. кол-во сообщений принятых за раз (= 100 по дефолту)
-- timeout - задержка в секундах для работы getUpdates (= 0 по дефолту)
-- пишут, что надо поставить таймаут больше 0, не знаю зачем, вроде и так работает
-- https://core.telegram.org/bots/API - тут вся инфа, в общем
botUpdates :: (MonadIO m) => Token -> Maybe Int -> Maybe Int -> Maybe Int -> Manager -> Source m Update
botUpdates token offset limit timeout manager = do
  oldOffset <- liftIO $ atomically $ newTVar offset
  -- repeatM запускает getUpdatesBatch несколько раз, пока есть входящие сообщения
  DC.repeatM (getUpdatesBatch oldOffset) =$= DC.concatMap id
    where
      getUpdatesBatch oldOffset = do
        currentOffset <- liftIO $ atomically $ readTVar oldOffset
        -- ответ сервера телеграма (список новых сообщений)
        resp <- liftIO $ getUpdates token currentOffset limit timeout manager
        case resp of
          Left e -> error $ show e
          Right (Response { result = batch }) -> do
            case batch of
              [] -> return []
              _ -> do
                let
                  maxUpdateId = maximum $ map update_id batch
                  newOffset = Just (maxUpdateId + 1)
                liftIO $ atomically $ writeTVar oldOffset newOffset
                return batch

-- обработка входящего сообщения и какая-то дичь с войдами
processUpdate :: (MonadThrow m, MonadIO m) => Token -> Manager -> Update -> m ()
processUpdate token manager update = void $ runMaybeT $ do
    -- msg - сообщение c дополнительной информацией, пришедшее от пользователя телеграма
    msg <- hoistMaybe $ message update
    -- если написать здесь это:
    --liftIO $ print msg
    -- то в консоль выводятся все приходящие сообщения
    -- txt - только само сообщение (что пользователь написал в чат), без другой инфы
    txt <- hoistMaybe $ text msg
    --liftIO $ print txt
    processed <- lift $ tryProcessCommand msg txt
    -- если команда не распознана, то посылаем это сообщение:
    when (not processed) $ do sendReply msg "Команда не найдена"
      where
        -- функция для отправки ответа в чат телеграма
        sendReply msg reply = do
          let
            chatId = pack $ show $ chat_id $ chat msg
            request = sendMessageRequest chatId reply
          void $ liftIO $ sendMessage token request manager

        --функция для отправки картинки в чат
        sendImg msg img = do
          let
            chatId = pack $ show $ chat_id $ chat msg
            request = sendPhotoRequest chatId img
          void $ liftIO $ sendPhoto token request manager

        -- функция возвращает функцию, если есть соответствующая ей команда
        tryProcessCommand msg txt = isJustT $ do
          (cmd, handler) <- hoistMaybe $ L.find (findCmd txt) commands
          handler msg (T.drop (T.length cmd + 2) txt)

        -- функция проверяет - есть ли такая команда (cmd) в списке команд (commands)
        findCmd txt (cmd, _) = T.take (T.length cmd + 1) txt == "/" <> cmd

        -- список команд, которые принимает бот
        commands = [ ("start", startCmd), ("help", helpCmd), ("hoogle", hoogleCmd), ("settings", setConst) ]

        -- старт - магия мемасов
        startCmd msg args = do sendImg msg "https://ipic.su/img/img7/fs/vzhuh.1482187468.jpg"

        -- хелп - справка
        helpCmd msg args =
          do sendReply msg $ "Для того, чтобы воспользоваться ботом необходимо ввести " <>
                            "команду hoogle с параметрами (либо названием функции, " <>
                            "для которой требуется получить описание, либо её сигнатуру)"
       -- команда, позволяющая установить количество функций, выводимых после команды hoogle
        setConst msg args = do 
        	case (T.length args) of
        		0 -> sendReply msg $ "Количество показываемых функций: " <> " "
        		_ -> sendReply msg $ "Задано число показываемых функций: " <> args

        -- команда, которая парсит хугл и возвращает справку по функциям
        hoogleCmd msg args = do
          --t <- atomRead countMy
          HoogleResponse { results = res } <- hoogle args countMy
          case (T.length args) of
            0 -> sendReply msg $ "Введите запрос ( /hoogle запрос )"
            _ -> do
              case (length res) of
                  0 -> sendReply msg $ "Не найдено: " <> args
                  _ -> sendReply msg $ hoogleResults res
        --sendReply msg $ "Введите запрос ( /hoogle запрос )" ++ show $ atomically $ readTVar countMy

--type CountFunc = TVar Integer

--добавить список, хранящий
