--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid            ((<>))
import           Hakyll
import qualified Data.Map as M
import           Text.Pandoc.Options
import           Data.Maybe (fromMaybe, isJust)
import           Control.Monad (filterM)
import qualified Data.Char as Char
--------------------------------------------------------------------------------
-----RULES-----

main :: IO ()
main = hakyllWith config $ do

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
            .||. "posts/**" .&&. (complement postPattern)) $ do
        route idRoute
        compile copyFileCompiler

    -- Compile templates
    match "templates/*" $ compile templateCompiler

    -- Build tags field
    tags <- buildTags postPattern $ fromCapture "posts/tags/*"

    match "pages/*.md" $ do
        route   $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ myPandoc
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> defaultContext)
            >>= relativizeUrls

    match postPattern $ do
        route $ setExtension ".html"
        compile $ do
            let context = postCtx tags
            myPandoc
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders -- h1 -> h2; h2 -> h3; etc
                >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
                >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< onlyPublished =<< loadAll postPattern

            let archiveCtx =
                    listField "posts" (postCtx tags) (return posts)
                 <> constField "title" "Archives"
                 <> mathCtx
                 <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    -- Create tag pages
    tagsRules tags $ \tag pattern -> do
        route   $ gsubRoute " " (const "_") `composeRoutes` setExtension ".html"
        compile $ makeListPage tags pattern (capitalized tag ++ " Posts")

        {--version "rss" $ do
            route   $ setExtension "xml"
            compile $ makeRssFeed tags pattern--}

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx tags
                    <> constField "description" ""

            posts <- fmap (take 10) . recentFirst =<< onlyPublished =<< loadAll "posts/*"
            renderRss myFeedConfiguration feedCtx posts

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< onlyPublished =<< loadAll postPattern
            
            let indexCtx =
                    listField "posts" (postCtx tags) (return posts)
                 <> constField "title" "Home"
                 <> mathCtx
                 <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/index_template.html" indexCtx
                >>= relativizeUrls

    return ()


--------------------------------------------------------------------------------
----- CONTEXTS ------

postCtx :: Tags -> Context String
postCtx tags = dateField "date" "%B %e, %Y"
            <> tagsField "tags" tags
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

-- Creates a list of posts with given tags, pattern, filter.
postList :: Tags
         -> Pattern
         -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern sortFilter = do
    posts        <- sortFilter =<< loadAll pattern
    itemTemplate <- loadBody "templates/postlink.html"
    applyTemplateList itemTemplate (postCtx tags) posts

-- Creates a page with a list of posts in it. We use this for the main
-- blog index, as well as for the "posts tagged X" pages.
makeListPage :: Tags
             -> Pattern
             -> String
             -> Compiler (Item String)
makeListPage tags pattern title = do
    let listCtx = field "postlist" (\_ -> postList tags pattern postFilter)
               <> constField "title"    title
               <> mathCtx
               <> defaultContext
    makeItem ""
        >>= loadAndApplyTemplate "templates/postlist.html" listCtx
        >>= loadAndApplyTemplate "templates/default.html" listCtx
        >>= relativizeUrls

--------------------------------------------------------------------------------
----- CONFIGS ------

-- RSS feed -- 
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Lambda Oinks"
    , feedDescription = "A blog for all things lambda and oinks."
    , feedAuthorName  = "Oinkina"
    , feedAuthorEmail = "lambdaoinks@gmail.com"
    , feedRoot        = "http://oinkina.github.io"
    }

-- Deploy blog with: ./site deploy --
config = defaultConfiguration { deployCommand = "./update.sh" }

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions {
                      writerReferenceLinks = True
                    , writerHtml5 = True
                    , writerHighlight = True
                    , writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
                    }

myPandoc = pandocCompilerWith defaultHakyllReaderOptions myWriterOptions

postPattern = "posts/*/index.md"

postFilter x = recentFirst =<< onlyPublished x

capitalizedWord :: String -> String
capitalizedWord (head:tail) = Char.toUpper head : map Char.toLower tail
capitalizedWord [] = []

capitalized :: String -> String
capitalized = unwords . map capitalizedWord . words