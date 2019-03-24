module Hyper.Node.Session.InMemory where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref, modify_, new, read)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (unwrap)
import Hyper.Session (class SessionStore, SessionID(..))

data InMemorySessionStore session = InMemorySessionStore (Ref (Map SessionID session))

foreign import generatedSessionID :: Effect String

instance sessionStoreInMemorySessionStore :: ( Monad m
                                             , MonadEffect m
                                             )
                                          => SessionStore
                                            (InMemorySessionStore session)
                                            m
                                            session where
  newSessionID _ = do
    id <- liftEffect generatedSessionID
    pure (SessionID id)

  get (InMemorySessionStore var) id =
    liftEffect do
      log ("Looking up session: " <> show (unwrap id))
      Map.lookup id <$> read var

  put (InMemorySessionStore var) id session = do
    liftEffect do
      log ("Saving session: " <> unwrap id)
      flip modify_ var $ Map.insert id session

  delete (InMemorySessionStore var) id = do
    liftEffect do
      log ("Deleting session: " <> unwrap id)
      flip modify_ var $ Map.delete id

newInMemorySessionStore
  :: forall session
   . Effect (InMemorySessionStore session)
newInMemorySessionStore = InMemorySessionStore <$> new Map.empty
