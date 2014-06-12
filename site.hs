--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid            ((<>))
import           Hakyll
import qualified Data.Map as M
import           Text.Pandoc.Options
import           Data.Maybe (fromMaybe, isJust)
import           Control.Monad (filterM)
--------------------------------------------------------------------------------
-----RULES-----

main :: IO ()
main = hakyll $ do

    -- Compress CSS
    match ("css/*" 
            .||. "bootstrap/css/*" 
            .||. "highlight/styles/*"
            .||. "fonts/Serif/cmun-serif.css"
            .||. "fonts/Serif Slanted/cmun-serif-slanted.css") $ do
        route   idRoute
        compile compressCssCompiler

    -- Static files
    match ("js/*" 
            .||. "favicon.ico"
            .||. "bootstrap/js/*" 
            .||. "bootstrap/fonts/*" 
            .||. "images/*"
            .||. "images/highlight/*" 
            .||. "highlight/highlight.pack.js"
            .||. "fonts/Serif/*"
            .||. "fonts/Serif-Slanted/*"
            .||. "comments/*"
            .||. "js/MathBox.js/**"
            .||. "posts/**" .&&. (complement "posts/*/*.md")) $ do
        route idRoute
        compile copyFileCompiler

    match "pages/*.md" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes`
                  setExtension "html"
        compile $ pandocCompilerWith myReaderOptions myWriterOptions
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> defaultContext)
            >>= relativizeUrls

    match "posts/*/index.md" $ do
        route $ setExtension ".html"
        compile $ pandocCompilerWith myReaderOptions myWriterOptions
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< onlyPublished =<< loadAll "posts/*/index.md"

            let archiveCtx =
                    listField "posts" (postCtx) (return posts)
                    <> constField "title" "Archives"
                    <> mathCtx
                    <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< onlyPublished =<< loadAll "posts/*/index.md"
            
            let indexCtx =
                    listField "posts" (postCtx) (return posts)
                    <> constField "title" "Home"
                    <> mathCtx
                    <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/index_template.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
----- CONTEXTS ------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
    <> mathCtx
    <> urlstripCtx
    <> defaultContext

-- MathJax
mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
    
-- Gets rid of "/index.html" from posts
urlstripCtx :: Context a
urlstripCtx = field "url" $ \item -> do
    route <- getRoute (itemIdentifier item)
    return $ fromMaybe "/" $ 
        fmap (reverse . drop 10 . reverse) route

-- For filtering lists of items to only be published items
onlyPublished :: MonadMetadata m => [Item a] -> m [Item a]
onlyPublished = filterM isPublished where
    isPublished item = do
        pubfield <- getMetadataField (itemIdentifier item) "published"
        return (isJust pubfield)

myReaderOptions :: ReaderOptions
myReaderOptions = defaultHakyllReaderOptions

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions {
      writerReferenceLinks = True
    , writerHtml5 = True
    , writerHighlight = True
    , writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
    }