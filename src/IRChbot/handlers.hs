module IRChbot.Handlers
    ( handlers
    ) where


import qualified IRChbot.Handlers.Hello

--------------------------------------------------------------------------------
handlers :: [UninitializedHandler]
handlers =[IRChbot.Handlers.Hello.Handler]
