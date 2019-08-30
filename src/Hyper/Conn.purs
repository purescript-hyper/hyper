module Hyper.Conn where

import Hyper.Middleware (Middleware)

-- | Defines the state of an HTTP request stream. It tracks whether or not
-- | some content has already been read from an HTTP request stream.
-- |
-- | Proper order of computations:
-- | BodyUnread -> BodyRead
foreign import kind RequestState

-- | Indicatess the request's body hasn't been read yet.
foreign import data BodyUnread :: RequestState

-- | Indicatess the request's body has already been read
-- | and can no longer be read again.
foreign import data BodyRead :: RequestState


-- | Defines the state of an HTTP response stream. It tracks whether or not
-- | some content has already been written to an HTTP response stream.
-- |
-- | Proper order of computations. Items marked with an asterisk indicate that
-- | transitioning back to the same state is valid:
-- | StatusLineOpen -> HeadersOpen* -> BodyOpen* -> ResponseEnded
foreign import kind ResponseState

-- | Type indicating that the status-line is ready to be
-- | sent.
foreign import data StatusLineOpen :: ResponseState

-- | Type indicating that headers are ready to be
-- | sent, i.e. the body streaming has not been started.
foreign import data HeadersOpen :: ResponseState

-- | Type indicating that headers have already been
-- | sent, and that the body is currently streaming.
foreign import data BodyOpen :: ResponseState

-- | Type indicating that headers have already been
-- | sent, and that the body stream, and thus the response,
-- | is finished.
foreign import data ResponseEnded :: ResponseState

-- | A `Conn` models the entirety of an HTTP connection, containing the fields
-- | `request`, `response`, and the extensibility point `components`.
type Conn (request :: RequestState -> Type) (requestState :: RequestState)
          (response :: ResponseState -> Type) (responseState :: ResponseState)
          components =
  { request :: request requestState
  , response :: response responseState
  , components :: components
  }

-- | Alias for easily defining all possible transitions allowed
-- | in a middleware's `Conn`'s state transition, including
-- | all or some of the following:
-- | - a request state transition
-- | - a response state transition
-- | - a component type change
type ConnTransition
      m
      (request :: RequestState -> Type)
      (fromRequestState :: RequestState)
      (toRequestState :: RequestState)
      (response :: ResponseState -> Type)
      (fromResponseState :: ResponseState)
      (toResponseState :: ResponseState)
      fromComp
      toComp
      a
  = Middleware
      m
      (Conn request fromRequestState response fromResponseState fromComp)
      (Conn request toRequestState   response toResponseState   toComp)
      a

-- | Defines a Request state transition.
type RequestTransition
      m
      (request :: RequestState -> Type)
      (fromRequestState :: RequestState)
      (toRequestState :: RequestState)
      (response :: ResponseState -> Type)
      (responseState :: ResponseState)
      comp
      a
  = ConnTransition
      m
      request
      fromRequestState
      toRequestState
      response
      responseState
      responseState
      comp
      comp
      a

-- | Defines a Request state transition and allows one to change the type
-- | of the components.
type RequestTransition'
      m
      (request :: RequestState -> Type)
      (fromRequestState :: RequestState)
      (toRequestState :: RequestState)
      (response :: ResponseState -> Type)
      (responseState :: ResponseState)
      fromComp
      toComp
      a
  = ConnTransition
      m
      request
      fromRequestState
      toRequestState
      response
      responseState
      responseState
      fromComp
      toComp
      a

-- | Defines a Response state transition.
type ResponseTransition
      m
      (request :: RequestState -> Type)
      (requestState :: RequestState)
      (response :: ResponseState -> Type)
      (fromResponseState :: ResponseState)
      (toResponseState :: ResponseState)
      comp
      a
  = ConnTransition
      m
      request
      requestState
      requestState
      response
      fromResponseState
      toResponseState
      comp
      comp
      a

-- | Defines a Response state transition and allows one to change the type
-- | of the components.
type ResponseTransition'
      m
      (request :: RequestState -> Type)
      (requestState :: RequestState)
      (response :: ResponseState -> Type)
      (fromResponseState :: ResponseState)
      (toResponseState :: ResponseState)
      fromComp
      toComp
      a
  = ConnTransition
      m
      request
      requestState
      requestState
      response
      fromResponseState
      toResponseState
      fromComp
      toComp
      a

-- | Indicates that no state transition occurs in either the request
-- | or the response.
type NoTransition
      m
      (request :: RequestState -> Type)
      (requestState :: RequestState)
      (response :: ResponseState -> Type)
      (responseState :: ResponseState)
      comp
      a
  = ConnTransition
      m
      request
      requestState
      requestState
      response
      responseState
      responseState
      comp
      comp
      a

-- | Changes the component's type in the Conn Middleware
type ComponentChange
      m
      (request :: RequestState -> Type)
      (requestState :: RequestState)
      (response :: ResponseState -> Type)
      (responseState :: ResponseState)
      fromComp
      toComp
      a
  = ConnTransition
      m
      request
      requestState
      requestState
      response
      responseState
      responseState
      fromComp
      toComp
      a
