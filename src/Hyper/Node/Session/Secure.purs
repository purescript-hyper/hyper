module Hyper.Node.Session.Secure
       ( SecureSessionStore(..)
       ) where

import Hyper.Session (class SessionStore, newSessionID, get, put, delete)

newtype SecureSessionStore s = SecureSessionStore s

instance sessionStoreEncryptedSessionStore :: SessionStore store m session
                                           => SessionStore (SecureSessionStore store) m session where
  newSessionID (SecureSessionStore store) =
    newSessionID store

  -- TODO: Actually encrypt.
  get (SecureSessionStore store) =
    get store

  -- TODO: Actually encrypt.
  put (SecureSessionStore store) =
    put store

  delete (SecureSessionStore store) =
    delete store
