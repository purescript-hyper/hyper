module Hyper.Node.Session.InMemory where

import Prelude
import Data.Map as Map
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar', modifyVar, peekVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Map (Map)
import Data.Newtype (unwrap)
import Hyper.Session (class SessionStore, SessionID(..))

data InMemorySessionStore session = InMemorySessionStore (AVar (Map SessionID session))

instance sessionStoreInMemorySessionStore :: ( Monad m
                                             , MonadAff (avar :: AVAR, console :: CONSOLE | e) m
                                             )
                                          => SessionStore
                                            (InMemorySessionStore session)
                                            m
                                            session where
  newSessionID _ =
    pure (SessionID "new-id")

  get (InMemorySessionStore var) id =
    liftAff do
      log ("Looking up session: " <> show (unwrap id))
      Map.lookup id <$> peekVar var

  put (InMemorySessionStore var) id session = do
    liftAff do
      log ("Saving session: " <> unwrap id)
      modifyVar (Map.insert id session) var

  delete (InMemorySessionStore var) id = do
    liftAff do
      log ("Deleting session: " <> unwrap id)
      modifyVar (Map.delete id) var

newInMemorySessionStore
  :: forall e session
   . Aff ( avar ∷ AVAR | e ) (InMemorySessionStore session)
newInMemorySessionStore = InMemorySessionStore <$> makeVar' Map.empty
