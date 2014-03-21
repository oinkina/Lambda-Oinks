--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad          (forM,forM_)
import           Data.List              (sortBy,isInfixOf)
import           Data.Monoid            (mappend,(<>),mconcat)
import           Data.Ord               (comparing)
import           Hakyll
import           System.Locale          (defaultTimeLocale)
import           System.FilePath.Posix  (takeBaseName,takeDirectory
                                         ,(</>),splitFileName)
import qualified Data.Map as M

--------------------------------------------------------------------------------
-----RULES-----

main :: IO ()
main = hakyll $ do

    -- Compress CSS
    match ("css/*" .||. "bootstrap/css/*") $ do
        route   idRoute
        compile compressCssCompiler

    -- Static files
    match ("bootstrap/js/*" .||. "bootstrap/fonts/*" .||. "images/*") $ do
        route idRoute
        compile $ copyFileCompiler

    -- Copy site icon to `favicon.ico`
    match "images/favicon.ico" $ do
            route   (constRoute "favicon.ico")
            compile copyFileCompiler


    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension ".html"
        compile $ pandocCompiler
         -- save immediately after pandoc, but before the templates are applied
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"

            let archiveCtx =
                    listField "posts" postCtx (return posts)
                    <> constField "title" "Archives"
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
                    <> defaultContext
{--
            let item = teaserField "teaser" "content"
                    <> defaultContext
--}
            do
                x0 <- getResourceBody
                x1 <- applyAsTemplate indexCtx x0
                x2 <- loadAndApplyTemplate "templates/default.html" indexCtx x1
                {--x3 <- loadAndApplyTemplate 
                    "template/postitem.html" 
                    (teaserField "teaser" "content" <> defaultContext)
                    x2--}
                relativizeUrls x2 --x3

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
    <> defaultContext

-- MathJax
mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
  metadata <- getMetadata $ itemIdentifier item
  return $ if "mathjax" `M.member` metadata
           then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
           else ""

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
