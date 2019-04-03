{-
TODO
   * implement POST-ing, postForm
   * convert to lens
   * Payload is now BS, but ToPayload renders to T (lazy text)
      - need to look what headers are (ASCII)
      - body can definitely be BS
-}
module HTTP
   (
     module HTTP.Header
   , module HTTP.Netw
   , module HTTP.Request
   , module HTTP.Response
   , module HTTP.Util
   , module URL
   )
   where


import HTTP.Header
import HTTP.Netw
import HTTP.Request
import HTTP.Response
import HTTP.Util
import URL.ToPayload as URL
