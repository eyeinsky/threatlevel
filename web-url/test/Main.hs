module Main where

import Prelude
import Data.Attoparsec.Text as Atto
import URL

main = print tw3
  where
    t1 = pTest protoP "h-.t+tp://"
    t2 = pTest authenticationP "a:b@"
    t3 = pTest hostP "1.2.3.4"
    t4 = pTest domainP "google.com"

    t5 = pTest portP ":123"
    t6 = pTest pathP "/a/b"
    c = pTest relativePathP "a/b"
    t7 = pTest paramsP "?a=b&c=d"
    t8 = pTest fragmentP "#eeuhtulrc.g"

    tx0 = pTest (authorityP 80) "google.com"
    tx1 = pTest uRLP "http://google.com"
    tx2 = pTest uRLP "https://google.com"
    tx3 = pTest uRLP "http://google.com/a/b/"
    tx4 = pTest uRLP "http://google.com?a=b&c=d"
    tx5 = pTest uRLP "http://google.com#aesonuhnseh"
    txc = pTest uRLP "http://google.com/a/b#aesonuhnseh"

    ty = pTest uRLP "http://domain.com/p1/p2?param1=value1&param2=value2#bla"
    ty2 = pTest uRLP "http://user:password@domain.com/p1/p2?param1=value1&param2=value2#bla"

    tw0 = pTest webURLP "#http://a:b@gmail.com/a/b?c=d&e=f#ensaoeuh"
    tw1 = pTest webURLP "/a/b?eeu&xxx=yyy#ee"
    tw2 = pTest webURLP "a/b?eeu&xxx=yyy#ee"
    tw3 = pTest webURLP "#"

    pTest p t = parseOnly (p <* endOfInput) t
