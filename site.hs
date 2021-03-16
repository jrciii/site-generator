--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Text.Pandoc
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Hakyll
import           Text.Regex
import           Data.Text
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.org", "ticklers.org", "contact.org"]) $ do
        route   $ setExtension "html"
        compile $ pandocPostCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "knowledge/*.org" $ do
        route $ setExtension "html"
        compile $ pandocPostCompiler
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/*.org" $ do
        route $ setExtension "html"
        compile $ pandocPostCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*.org"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Posts"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

pandocPostCompiler :: Compiler (Item String)
pandocPostCompiler = pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    orgToHtml

-- Probably need to make this more robust.
orgRegex :: String -> String
orgRegex t = if "http" `isPrefixOf` (pack t)
  then t
  else subRegex (mkRegex "^(.*?)\\.org$") t "\\1.html"
  
orgToHtml :: Pandoc -> Pandoc
orgToHtml = walk $ \inline -> case inline of
  Link attr inline (url, title) -> Link attr inline (pack(orgRegex (unpack url)), title)
  _ -> inline
