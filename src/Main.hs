{-# LANGUAGE OverloadedStrings #-}

module Main where

-- программная транзакционная память, воу!
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (writeTVar, newTVar, readTVar)

-- для обработки ошибок
import Control.Error.Util (hoistMaybe, isJustT)

import Control.Monad (void, when)
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

main :: IO ()
main = do
  -- менеджер хттп-соединения с сервером телеграма по безопасному протоколу(tls)
  manager <- newManager tlsManagerSettings
  -- токен бота
  let token = Token "bot319624564:AAE_fb6q_eTI942c4K7wpC4kNReC28939RI"
  -- основная функция
  -- нафинги нужны для getUpdates
  -- оператор $$ передает данные из updates в processUpdate, вообще офигеть
  updates token Nothing Nothing Nothing manager $$ DC.mapM_ (processUpdate token manager)

-- приём входящих сообщений
-- offset - идентификатор входящих сообщений (с каждым сообщением увеличивается на 1)
-- limit - макс. кол-во сообщений принятых за раз (= 100 по дефолту)
-- timeout - задержка в секундах для работы getUpdates (= 0 по дефолту)
-- пишут, что надо поставить таймаут больше 0, не знаю зачем, вроде и так работает
-- https://core.telegram.org/bots/API - тут вся инфа, в общем
updates :: (MonadIO m) => Token -> Maybe Int -> Maybe Int -> Maybe Int -> Manager -> Source m Update
updates token offset limit timeout manager = do
  offsetHolder <- liftIO $ atomically $ newTVar offset
  -- repeatM запускает getUpdatesBatch несколько раз, пока есть входящие сообщения
  DC.repeatM (getUpdatesBatch offsetHolder) =$= DC.concatMap id
    where
      getUpdatesBatch offsetHolder = do
        currentOffset <- liftIO $ atomically $ readTVar offsetHolder
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
                liftIO $ atomically $ writeTVar offsetHolder newOffset
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
    -- liftIO $ print txt
    processed <- lift $ tryProcessCommand msg txt
    -- если команда не распознана, то посылаем это сообщение:
    when (not processed) $ do sendReply msg "Not recognized"
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
          (cmd, handler) <- hoistMaybe $ L.find (checkCommand txt) commands
          handler msg (T.drop (T.length cmd + 2) txt)
        
        -- функция проверяет - есть ли такая команда (cmd) в списке команд (commands)
        checkCommand txt (cmd, _) = T.take (T.length cmd + 2) txt == "/" <> cmd

        -- список команд, которые принимает бот
        commands = [ ("start", startCmd), ("help", helpCmd), ("hoogle", hoogleCmd) ]
            
        -- старт - магия мемасов
        startCmd msg args = do sendImg msg "https://ipic.su/img/img7/fs/vzhuh.1482187468.jpg"
        -- хелп - надо справку по использованию бота написать
        helpCmd msg args = do sendReply msg "здесь должна быть справка"
        -- команда, которая парсит хугл и возвращает справку по функциям
        -- наше основное заднание, ну вы поняли
        -- делайте давайте
        hoogleCmd msg args = do sendReply msg "хугл"
