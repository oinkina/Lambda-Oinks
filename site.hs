--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid            (mappend,(<>),mconcat)
import           Hakyll
import qualified Data.Map as M
import           Text.Pandoc.Options

--------------------------------------------------------------------------------
-----RULES-----

main :: IO ()
main = hakyll $ do

    -- Compress CSS
    match ("css/*" 
            .||. "bootstrap/css/*" 
            .||. "highlight/styles/*"
            .||. "fonts/Serif/cmun-serif.css"
            .||. "fonts/Serif-Slanted/cmun-serif-slanted.css"
            .||. "/comments/inlineDisqussions.css") $ do
        route   idRoute
        compile compressCssCompiler

    -- Static files
    match ("js/*"
            .||. "bootstrap/js/*" 
            .||. "bootstrap/fonts/*" 
            .||. "images/*"
            .||. "images/highlight/*" 
            .||. "highlight/highlight.pack.js"
            .||. "fonts/Serif/*"
            .||. "fonts/Serif-Slanted/*"
            .||. "comments/*") $ do
        route idRoute
        compile copyFileCompiler

    -- Copy site icon to `favicon.ico`
    match "favicon.ico" $ do
            route   idRoute
            compile copyFileCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension ".html"
        compile $ pandocCompilerWith myReaderOptions myWriterOptions
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> defaultContext)
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension ".html"
        compile $ pandocCompilerWith myReaderOptions myWriterOptions
         -- save immediately after pandoc, but before the templates are applied
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> postCtx)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"

            let archiveCtx =
                    listField "posts" postCtx (return posts)
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
            posts <- recentFirst =<< loadAll "posts/*"
            
            let indexCtx =
                    listField "posts" postCtx (return posts)
                    <> constField "title" "Home"
                    <> mathCtx
                    <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    -- no "route" because not writing to /_site 
    -- just want to use templates elsewhere
    match "templates/*" $ compile templateCompiler

{--
--metadata keywords--
match "posts/*.md" $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    -- use the template with the current content
    >>= loadAndApplyTemplate "templates/post.html"
            (defaultContext <> metaKeywordContext)
--}

--------------------------------------------------------------------------------
----- CONTEXTS ------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
    <> mathCtx
    <> defaultContext


-- MathJax
mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ if "mathjax" `M.member` metadata
             then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
             else ""


myReaderOptions :: ReaderOptions
myReaderOptions = defaultHakyllReaderOptions

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions {
      writerReferenceLinks = True
    , writerHtml5 = True
    , writerHighlight = True
    , writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
    }

{-
-- metaKeywordContext will return a Context containing a String
metaKeywordContext :: Context String
-- can be reached using $metaKeywords$ in the templates
-- Use the current item (markdown file)
metaKeywordContext = field "metaKeywords" $ \item -> do
  -- tags contains the content of the "tags" metadata
  -- inside the item (understand the source)
  tags <- getMetadataField (itemIdentifier item) "tags"
  -- if tags is empty return an empty string
  -- in the other case return
  --   <meta name="keywords" content="$tags$">
  return $ maybe "" showMetaTags tags
    where
      showMetaTags t = "<meta name=\"keywords\" content=\""
                       ++ t ++ "\">\n"
-}