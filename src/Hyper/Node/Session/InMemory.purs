module Hyper.Node.Session.InMemory where

import Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (unwrap)
import Hyper.Session (class SessionStore, SessionID(..))

data InMemorySessionStore session = InMemorySessionStore (Ref (Map SessionID session))

foreign import generatedSessionID ::forall eff. Eff (random :: RANDOM | eff) String

instance sessionStoreInMemorySessionStore :: ( Monad m
                                             , MonadEff (ref:: REF, console :: CONSOLE, random :: RANDOM | e) m
                                             )
                                          => SessionStore
                                            (InMemorySessionStore session)
                                            m
                                            session where
  newSessionID _ = do
    id <- liftEff generatedSessionID
    pure (SessionID id)

  get (InMemorySessionStore var) id =
    liftEff do
      log ("Looking up session: " <> show (unwrap id))
      Map.lookup id <$> readRef var

  put (InMemorySessionStore var) id session = do
    liftEff do
      log ("Saving session: " <> unwrap id)
      modifyRef var $ Map.insert id session

  delete (InMemorySessionStore var) id = do
    liftEff do
      log ("Deleting session: " <> unwrap id)
      modifyRef var $ Map.delete id

newInMemorySessionStore
  :: forall e session
   . Eff ( ref∷ REF | e ) (InMemorySessionStore session)
newInMemorySessionStore = InMemorySessionStore <$> newRef Map.empty
