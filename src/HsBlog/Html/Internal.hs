module HsBlog.Html.Internal where

import Numeric.Natural


-- * Types

newtype Html = Html String
newtype Structure = Structure String
type Title = String


-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content = 
    Html
        ( el "html"
            (el "head" (el "title" (escape title))
                <> el "body" (getStructureString content))
        )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) .escape



instance Semigroup Structure where
    -- (<>) :: Structure -> Structure -> Structure
    (<>) c1 c2 = 
        Structure (getStructureString c1 <> getStructureString c2)

instance Monoid Structure where
    mempty = Structure ""

-- * Render

render :: Html -> String
render html = case html of
    Html str -> str

-- * Utilities

getStructureString :: Structure -> String
getStructureString structure = case structure of
    Structure str -> str

el :: String -> String -> String
el tag content = 
    "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

escape :: String -> String
escape = 
    let
        escapeChar c = 
            case c of
                '<' -> "&lt"
                '>' -> "&gt"
                '&' -> "&amp"
                '"' -> "&quot"
                '\'' -> "&#39"
                _ -> [c]
    in
        concatMap escapeChar