module Backend where

import Common.Route
import Obelisk.Backend

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_run = \serve -> serve $ const $ pure (),
      _backend_routeEncoder = fullRouteEncoder
    }
